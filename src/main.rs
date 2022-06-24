#![feature(cow_is_borrowed)]
#![feature(new_uninit)]
#![feature(int_roundings)]
#![feature(bigint_helper_methods)]

mod cpu;
mod device;

use cpu::*;
use device::*;

use crossterm::{cursor, style, terminal};
use crossterm::{ExecutableCommand, QueueableCommand};
use getopts::Options;
use ggez::conf::{NumSamples, WindowMode, WindowSetup};
use ggez::event::{EventHandler, KeyCode, KeyMods};
#[allow(unused_imports)]
use ggez::graphics::{
    Color, DrawParam, FilterMode, Font, Image, PxScale, Text, TextFragment, WrapMode,
};
use ggez::{event, graphics, timer, Context, ContextBuilder, GameError, GameResult};
use spin_sleep::LoopHelper;
use std::cell::{RefCell, RefMut};
use std::collections::VecDeque;
use std::io::{Read, Stdout, Write};
use std::rc::Rc;
use std::time::Duration;

const TITLE: &str = "a32emu";
const VERSION: &str = env!("CARGO_PKG_VERSION");
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");

const SCREEN_WIDTH: u16 = 400;
const SCREEN_HEIGHT: u16 = 300;
const SCREEN_SCALE: f32 = 3.0;

const CLOCK_RATE: f64 = 20_000_000.0; // 20 MHz, clock rate is not comparable to hardware because the emulator is not cycle accurate
const FRAME_RATE: f64 = 60.0;
const CYCLES_PER_FRAME: f64 = CLOCK_RATE / FRAME_RATE;
const WHOLE_CYCLES_PER_FRAME: u64 = CYCLES_PER_FRAME as u64;
const FRACT_CYCLES_PER_FRAME: f64 = CYCLES_PER_FRAME - (WHOLE_CYCLES_PER_FRAME as f64);

type Word = u32; // Word size of the CPU
type SWord = i32; // The word size of the CPU as signed (for sign extension)

type SharedRef<T> = Rc<RefCell<T>>;

#[inline]
fn make_shared<T>(obj: T) -> SharedRef<T> {
    Rc::new(RefCell::new(obj))
}

#[inline]
fn clone_shared<T>(other: &SharedRef<T>) -> SharedRef<T> {
    Rc::clone(other)
}

#[inline]
fn borrow_shared<T: ?Sized>(shared: &SharedRef<T>) -> RefMut<'_, T> {
    RefCell::borrow_mut(&shared)
}

fn format_clock_rate(clock_rate: f64) -> String {
    if clock_rate > 999_000_000.0 {
        format!("{:.1} GHz", clock_rate / 1_000_000_000.0)
    } else if clock_rate > 999_000.0 {
        format!("{:.1} MHz", clock_rate / 1_000_000.0)
    } else if clock_rate > 999.0 {
        format!("{:.1} kHz", clock_rate / 1_000.0)
    } else {
        format!("{:.0} Hz", clock_rate)
    }
}

struct Utf8Builder {
    current_count: usize,
    current_input: Vec<u8>,
    output: VecDeque<char>,
}
impl Utf8Builder {
    fn new() -> Self {
        Self {
            current_count: 0,
            current_input: Vec::with_capacity(4),
            output: VecDeque::new(),
        }
    }

    fn push_byte(&mut self, byte: u8) {
        if self.current_count == 0 {
            let c_opt = char::from_u32(byte as u32);
            let is_ascii = match c_opt {
                Some(c) => c.is_ascii(),
                None => false,
            };

            if is_ascii {
                self.output.push_back(c_opt.unwrap());
            } else {
                if (byte & 0b_1110_0000) == 0b_1100_0000 {
                    self.current_count = 2;
                    self.current_input.push(byte);
                } else if (byte & 0b_1111_0000) == 0b_1110_0000 {
                    self.current_count = 3;
                    self.current_input.push(byte);
                } else if (byte & 0b_1111_1000) == 0b_1111_0000 {
                    self.current_count = 4;
                    self.current_input.push(byte);
                } else {
                    self.output.push_back(char::REPLACEMENT_CHARACTER);
                }
            }
        } else {
            if (byte & 0b_1100_0000) == 0b_1000_0000 {
                self.current_input.push(byte);

                if self.current_input.len() == self.current_count {
                    let s = String::from_utf8_lossy(&self.current_input);
                    let mut cs = s.chars();
                    let c = cs.next().unwrap();
                    assert!(cs.next().is_none());
                    assert!(s.is_borrowed());

                    self.current_count = 0;
                    self.current_input.clear();
                    self.output.push_back(c);
                }
            } else {
                self.current_count = 0;
                self.current_input.clear();
                self.output.push_back(char::REPLACEMENT_CHARACTER);
            }
        }
    }

    #[inline]
    fn pop_char(&mut self) -> Option<char> {
        self.output.pop_front()
    }
}

struct HeadlessState {
    cpu: Cpu,
    dma: SharedRef<DmaController>,
    uart: SharedRef<UartController>,
    mem_bus: MemoryBus,
    io_bus: IoBus,
    utf8_builder: Utf8Builder,
}
impl HeadlessState {
    fn new(rom: Vec<u32>) -> Self {
        let mut io_bus = IoBus::new();

        let dma = make_shared(DmaController::new());
        io_bus.map_device(clone_shared(&dma), 0x000, 0x003, 0x003, 0x000);
        let uart = make_shared(UartController::new());
        io_bus.map_device(clone_shared(&uart), 0x004, 0x007, 0x003, 0x000);

        Self {
            cpu: Cpu::new(),
            dma,
            uart,
            mem_bus: MemoryBus::new(rom),
            io_bus,
            utf8_builder: Utf8Builder::new(),
        }
    }

    fn clock(&mut self) -> ClockResult {
        let (result, k_flag) = {
            let result = self.cpu.clock(&mut self.mem_bus, &mut self.io_bus);
            let k_flag = self.cpu.k();
            (result, k_flag)
        };

        {
            let mut dma = borrow_shared(&self.dma);
            if dma.run {
                self.mem_bus
                    .copy(dma.src(), dma.dst(), dma.len(), dma.dir(), k_flag);
                dma.run = false;
            }
        }

        {
            let mut uart = borrow_shared(&self.uart);
            while let Some(byte) = uart.host_read() {
                self.utf8_builder.push_byte(byte);
            }
        }

        while let Some(c) = self.utf8_builder.pop_char() {
            print!("{}", c);
        }

        result
    }
}

struct EmuState {
    cpu: Cpu,
    dma: SharedRef<DmaController>,
    uart: SharedRef<UartController>,
    mem_bus: MemoryBus,
    io_bus: IoBus,
    framebuffer: Framebuffer<{ SCREEN_WIDTH as usize }, { SCREEN_HEIGHT as usize }>,

    stdout: Stdout,
    utf8_builder: Utf8Builder,
    font: Font,
    running: bool,
    halted: bool,
    show_debug_info: bool,
    fractional_cycles: f64,
    loop_helper: LoopHelper,
}
impl EmuState {
    fn new(rom: Vec<u32>, font: Font) -> GameResult<Self> {
        let mut io_bus = IoBus::new();

        let dma = make_shared(DmaController::new());
        io_bus.map_device(clone_shared(&dma), 0x000, 0x003, 0x003, 0x000);
        let uart = make_shared(UartController::new());
        io_bus.map_device(clone_shared(&uart), 0x004, 0x007, 0x003, 0x000);

        terminal::enable_raw_mode()?;

        let mut stdout = std::io::stdout();
        stdout.execute(terminal::EnterAlternateScreen)?;
        stdout.execute(terminal::Clear(terminal::ClearType::All))?;
        stdout.execute(terminal::Clear(terminal::ClearType::Purge))?;
        stdout.execute(cursor::MoveTo(0, 0))?;

        Ok(Self {
            cpu: Cpu::new(),
            dma,
            uart,
            mem_bus: MemoryBus::new(rom),
            io_bus,
            framebuffer: Framebuffer::new(),

            stdout,
            utf8_builder: Utf8Builder::new(),
            font,
            running: false,
            halted: false,
            show_debug_info: false,
            fractional_cycles: 0.0,
            loop_helper: LoopHelper::builder()
                .native_accuracy_ns(1_500_000)
                .report_interval_s(0.5)
                .build_with_target_rate(FRAME_RATE),
        })
    }

    fn reset(&mut self) {
        self.cpu.reset();
        borrow_shared(&self.dma).reset();
        borrow_shared(&self.uart).reset();
    }

    fn process_terminal_input(&mut self) -> GameResult {
        let mut uart = borrow_shared(&self.uart);

        while crossterm::event::poll(Duration::ZERO)? {
            let event = crossterm::event::read()?;
            if let crossterm::event::Event::Key(key_event) = event {
                const ESC_SEQ: [u8; 2] = [0x1B, 0x5B];

                match key_event.code {
                    crossterm::event::KeyCode::Backspace => {}
                    crossterm::event::KeyCode::Enter => {}
                    crossterm::event::KeyCode::Left => {
                        uart.host_write(ESC_SEQ[0]);
                        uart.host_write(ESC_SEQ[1]);
                        uart.host_write(31); // ASCII 1
                        uart.host_write(68); // ASCII D
                    }
                    crossterm::event::KeyCode::Right => {
                        uart.host_write(ESC_SEQ[0]);
                        uart.host_write(ESC_SEQ[1]);
                        uart.host_write(31); // ASCII 1
                        uart.host_write(67); // ASCII C
                    }
                    crossterm::event::KeyCode::Up => {
                        uart.host_write(ESC_SEQ[0]);
                        uart.host_write(ESC_SEQ[1]);
                        uart.host_write(31); // ASCII 1
                        uart.host_write(65); // ASCII A
                    }
                    crossterm::event::KeyCode::Down => {
                        uart.host_write(ESC_SEQ[0]);
                        uart.host_write(ESC_SEQ[1]);
                        uart.host_write(31); // ASCII 1
                        uart.host_write(66); // ASCII B
                    }
                    crossterm::event::KeyCode::Home => {}
                    crossterm::event::KeyCode::End => {}
                    crossterm::event::KeyCode::PageUp => {}
                    crossterm::event::KeyCode::PageDown => {}
                    crossterm::event::KeyCode::Tab => {}
                    crossterm::event::KeyCode::BackTab => {}
                    crossterm::event::KeyCode::Delete => {}
                    crossterm::event::KeyCode::Insert => {}
                    crossterm::event::KeyCode::F(_) => {}
                    crossterm::event::KeyCode::Char(c) => {
                        let mut buffer = [0; 4];
                        let s = c.encode_utf8(&mut buffer);
                        let bytes = s.as_bytes();
                        for b in bytes.iter() {
                            uart.host_write(*b);
                        }
                    }
                    crossterm::event::KeyCode::Null => {}
                    crossterm::event::KeyCode::Esc => {}
                }
            }
        }

        Ok(())
    }

    fn clock(&mut self) {
        let k_flag = {
            let result = self.cpu.clock(&mut self.mem_bus, &mut self.io_bus);
            match result {
                ClockResult::Break => self.running = false,
                ClockResult::Halt => {
                    self.running = false;
                    self.halted = true;
                }
                ClockResult::Error => {
                    self.running = false;
                    self.halted = true;
                }
                _ => {}
            }

            self.cpu.k()
        };

        {
            let mut dma = borrow_shared(&self.dma);
            if dma.run {
                self.mem_bus
                    .copy(dma.src(), dma.dst(), dma.len(), dma.dir(), k_flag);
                dma.run = false;
            }
        }
    }

    fn clock_frame(&mut self) {
        self.fractional_cycles += FRACT_CYCLES_PER_FRAME;
        let cycles_to_add = self.fractional_cycles as u64;
        self.fractional_cycles -= cycles_to_add as f64;
        let cycle_count = WHOLE_CYCLES_PER_FRAME + cycles_to_add;

        for _ in 0..cycle_count {
            self.clock();
            if !self.running {
                break;
            }
        }
    }
}
impl EventHandler<GameError> for EmuState {
    fn update(&mut self, ctx: &mut Context) -> GameResult {
        self.loop_helper.loop_sleep();
        self.loop_helper.loop_start();

        if let Some(fps) = self.loop_helper.report_rate() {
            if self.running {
                graphics::set_window_title(
                    ctx,
                    &format!(
                        "{} v{} - {:.2} fps - {}",
                        TITLE,
                        VERSION,
                        fps,
                        format_clock_rate(fps * CYCLES_PER_FRAME)
                    ),
                );
            } else if self.halted {
                graphics::set_window_title(
                    ctx,
                    &format!("{} v{} - {:.2} fps - halted", TITLE, VERSION, fps),
                );
            } else {
                graphics::set_window_title(
                    ctx,
                    &format!("{} v{} - {:.2} fps", TITLE, VERSION, fps),
                );
            }
        }

        self.process_terminal_input()?;

        if self.running {
            self.clock_frame();
        }

        {
            let mut uart = borrow_shared(&self.uart);
            while let Some(byte) = uart.host_read() {
                self.utf8_builder.push_byte(byte);
            }
        }

        while let Some(c) = self.utf8_builder.pop_char() {
            self.stdout.queue(style::Print(c))?;
        }
        self.stdout.flush()?;

        timer::yield_now();
        Ok(())
    }

    fn draw(&mut self, ctx: &mut Context) -> GameResult {
        graphics::clear(ctx, Color::BLACK);

        {
            self.mem_bus.draw_framebuffer(&mut self.framebuffer, 0, 0);
            let framebuffer_pixels = self.framebuffer.as_pixels();

            let mut screen =
                Image::from_rgba8(ctx, SCREEN_WIDTH, SCREEN_HEIGHT, framebuffer_pixels)?;
            let filter = if SCREEN_SCALE.fract() == 0.0 {
                FilterMode::Nearest
            } else {
                FilterMode::Linear
            };
            screen.set_filter(filter);
            screen.set_wrap(WrapMode::Clamp, WrapMode::Clamp);

            let params = DrawParam::default()
                .dest([2.0, 2.0])
                .scale([SCREEN_SCALE, SCREEN_SCALE]);
            graphics::draw(ctx, &screen, params)?;
        }

        if self.show_debug_info {
            const TEXT_SCALE: PxScale = PxScale { x: 20.0, y: 20.0 };
            const TEXT_BACK_COLOR: graphics::Color = graphics::Color::new(0.0, 0.0, 0.0, 1.0);
            const TEXT_FRONT_COLOR: graphics::Color = graphics::Color::new(0.9, 0.9, 0.9, 1.0);
            const STACK_HOFFSET: f32 = TEXT_SCALE.x * 0.5 * 22.0;
            const MEM_VOFFSET: f32 = TEXT_SCALE.y * 8.0;
            const MEM_HOFFSET: f32 = TEXT_SCALE.x * 0.5 * 112.0;

            let cpu_info = format!("{}", self.cpu);
            let cpu_info_frag = TextFragment::new(cpu_info)
                .font(self.font)
                .scale(TEXT_SCALE);
            let cpu_info_text = Text::new(cpu_info_frag);
            graphics::draw(
                ctx,
                &cpu_info_text,
                DrawParam::default()
                    .dest([11.0, 9.0])
                    .color(TEXT_BACK_COLOR),
            )?;
            graphics::draw(
                ctx,
                &cpu_info_text,
                DrawParam::default()
                    .dest([10.0, 8.0])
                    .color(TEXT_FRONT_COLOR),
            )?;

            let mut stack_info = String::new();
            if self.cpu.sp() == 0 {
                stack_info.push_str(&format!("{:0>8X} >   <uninit>", self.cpu.sp()));
            } else if self.cpu.sp() == 0x02_000000 {
                stack_info.push_str(&format!("{:0>8X} >   <empty> ", self.cpu.sp()));
            } else {
                const MAX_STACK_VALUES: usize = 20;
                let start = usize::min(
                    (self.cpu.sp() as usize) + (MAX_STACK_VALUES * 4),
                    0x01_FFFFFC,
                );
                let end = self.cpu.sp() as usize;

                let mut addr = start;
                while addr > end {
                    let value = self.mem_bus.read32(addr, true);
                    stack_info.push_str(&format!("             0x{:0>8X}\n", value));
                    addr -= 4;
                }

                let value = self.mem_bus.read32(addr, true);
                stack_info.push_str(&format!("{:0>8X} >   0x{:0>8X}", addr, value));
            }
            let stack_info_frag = TextFragment::new(stack_info)
                .font(self.font)
                .scale(TEXT_SCALE);
            let stack_info_text = Text::new(stack_info_frag);
            graphics::draw(
                ctx,
                &stack_info_text,
                DrawParam::default()
                    .dest([
                        (SCREEN_WIDTH as f32) * SCREEN_SCALE - STACK_HOFFSET - 11.0,
                        9.0,
                    ])
                    .color(TEXT_BACK_COLOR),
            )?;
            graphics::draw(
                ctx,
                &stack_info_text,
                DrawParam::default()
                    .dest([
                        (SCREEN_WIDTH as f32) * SCREEN_SCALE - STACK_HOFFSET - 10.0,
                        8.0,
                    ])
                    .color(TEXT_FRONT_COLOR),
            )?;

            let mut mem_info = String::new();
            {
                const MEMORY_OFFSET: usize = 0x00_007700;

                let mut addr: usize = MEMORY_OFFSET;
                while addr < (MEMORY_OFFSET + 0x100) {
                    mem_info.push_str(&format!("{:0>8X} | ", addr));

                    let mut text = String::new();
                    for _ in 0..8 {
                        let value = self.mem_bus.read32(addr, true);
                        mem_info.push_str(&format!("{:0>8X} ", value));

                        let ascii = value.to_le_bytes();
                        for i in 0..4 {
                            let b = ascii[i];
                            let c = if (b >= 0x20) && (b < 0x7F) {
                                unsafe { char::from_u32_unchecked(b as u32) }
                            } else {
                                '.'
                            };
                            text.push(c);
                        }

                        addr += 4;
                    }

                    mem_info.push_str("| ");
                    mem_info.push_str(&text);
                    mem_info.push('\n');
                }
            }
            let mem_info_frag = TextFragment::new(mem_info)
                .font(self.font)
                .scale(TEXT_SCALE);
            let mem_info_text = Text::new(mem_info_frag);
            graphics::draw(
                ctx,
                &mem_info_text,
                DrawParam::default()
                    .dest([
                        (SCREEN_WIDTH as f32) * SCREEN_SCALE - MEM_HOFFSET - 11.0,
                        (SCREEN_HEIGHT as f32) * SCREEN_SCALE - MEM_VOFFSET - 9.0,
                    ])
                    .color(TEXT_BACK_COLOR),
            )?;
            graphics::draw(
                ctx,
                &mem_info_text,
                DrawParam::default()
                    .dest([
                        (SCREEN_WIDTH as f32) * SCREEN_SCALE - MEM_HOFFSET - 10.0,
                        (SCREEN_HEIGHT as f32) * SCREEN_SCALE - MEM_VOFFSET - 8.0,
                    ])
                    .color(TEXT_FRONT_COLOR),
            )?;
        }

        graphics::present(ctx)?;
        timer::yield_now();

        Ok(())
    }

    fn key_down_event(
        &mut self,
        ctx: &mut Context,
        keycode: KeyCode,
        _keymods: KeyMods,
        _repeat: bool,
    ) {
        match keycode {
            KeyCode::Escape => ggez::event::quit(ctx),
            KeyCode::Space => {
                if !self.halted {
                    self.running = !self.running
                }
            }
            KeyCode::D => self.show_debug_info = !self.show_debug_info,
            KeyCode::C => {
                if !self.halted && !self.running {
                    self.clock();
                }
            }
            KeyCode::R => {
                self.running = false;
                self.halted = false;
                self.reset();
            }
            _ => {}
        }
    }

    fn quit_event(&mut self, _ctx: &mut Context) -> bool {
        let _ = terminal::disable_raw_mode();
        let _ = self.stdout.execute(terminal::LeaveAlternateScreen);
        let _ = self
            .stdout
            .execute(terminal::Clear(terminal::ClearType::All));
        let _ = self
            .stdout
            .execute(terminal::Clear(terminal::ClearType::Purge));
        let _ = self.stdout.execute(cursor::MoveTo(0, 0));
        let _ = self.stdout.execute(cursor::Show);

        false
    }
}

fn print_usage(opts: Options) {
    let brief = format!("Usage: a32emu [--headless [--max-cycles INT]] [--rom FILE]");
    println!("{}", opts.usage(&brief));
}

fn main() -> std::result::Result<(), Box<dyn std::error::Error>> {
    let args: Vec<String> = std::env::args().collect();
    let mut opts = Options::new();
    opts.reqopt("", "rom", "Binary file to load into kernel memory", "FILE");
    opts.optflag("", "headless", "Run in headless mode");
    opts.optopt("", "max-cycles", "Max cycles before abort", "INT");

    let result = opts.parse(args);
    if let Err(_) = result {
        print_usage(opts);
    }
    let matches = result?;

    let headless = matches.opt_present("headless");

    const DEFAULT_MAX_CYCLES: u64 = 1_000_000;
    let max_cycles: u64 = matches
        .opt_get_default("max-cycles", DEFAULT_MAX_CYCLES)
        .unwrap_or(DEFAULT_MAX_CYCLES);

    let rom: Vec<u32> = match matches.opt_str("rom") {
        Some(path) => {
            let mut rom_file = std::fs::File::open(path)?;
            let mut rom_vec: Vec<u32> = Vec::new();

            loop {
                let mut buffer: [u8; 4] = [0; 4];
                match rom_file.read_exact(&mut buffer) {
                    Ok(_) => rom_vec.push(u32::from_ne_bytes(buffer)),
                    Err(_) => break,
                }
            }

            rom_vec
        }
        None => unreachable!("rom argument is required"),
    };

    if headless {
        let mut state = HeadlessState::new(rom);
        let mut cycles: u64 = 0;

        loop {
            match state.clock() {
                ClockResult::Halt => break,
                ClockResult::Error => Err("CPU error")?,
                _ => {}
            }

            cycles += 1;
            if cycles > max_cycles {
                Err("CPU did not stop within cycle limit")?;
            }
        }

        Ok(())
    } else {
        let window_setup = WindowSetup::default()
            .title(&format!("{} v{}", TITLE, VERSION))
            .vsync(false)
            .srgb(true)
            .samples(NumSamples::One);
        let window_mode = WindowMode::default().dimensions(
            (SCREEN_WIDTH as f32) * SCREEN_SCALE + 4.0,
            (SCREEN_HEIGHT as f32) * SCREEN_SCALE + 4.0,
        );
        let builder = ContextBuilder::new(TITLE, AUTHOR)
            .window_setup(window_setup)
            .window_mode(window_mode);

        let (mut ctx, event_loop) = builder.build()?;

        const FONT_BYTES: &[u8] = include_bytes!("../res/SourceCodePro-Bold.ttf");
        let font = Font::new_glyph_font_bytes(&mut ctx, FONT_BYTES)?;

        let state = EmuState::new(rom, font)?;
        event::run(ctx, event_loop, state);
    }
}
