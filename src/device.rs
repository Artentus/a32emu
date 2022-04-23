use crate::{borrow_shared, SharedRef, Word};
use bytemuck::{Pod, Zeroable};
use lru::LruCache;
use std::collections::VecDeque;
use std::io::{Cursor, Read};
use std::num::Wrapping;

const KERNEL_ROM: &[u8] = include_bytes!("../res/kernel_rom.bin");

macro_rules! read_mem {
    ($t:ty, $mem:expr, $addr:expr) => {{
        let mem_t: &[$t] = bytemuck::cast_slice($mem);
        let addr = $addr / std::mem::size_of::<$t>();
        <$t>::from_le(mem_t[addr])
    }};
}

macro_rules! write_mem {
    ($t:ty, $mem:expr, $addr:expr, $value:expr) => {{
        let mem_t: &mut [$t] = bytemuck::cast_slice_mut($mem);
        let addr = $addr / std::mem::size_of::<$t>();
        mem_t[addr] = $value.to_le();
    }};
}

struct Ram {
    size: usize,
    mem: Box<[u32]>,
}
impl Ram {
    #[inline]
    fn new_zeroed(size: usize) -> Self {
        Self {
            size,
            mem: unsafe { Box::new_zeroed_slice(size / 4).assume_init() },
        }
    }

    fn new_init(size: usize, mut mem: Vec<u32>) -> Self {
        assert!(mem.len() <= (size / 4));

        while mem.len() < (size / 4) {
            mem.push(0);
        }

        Self {
            size,
            mem: mem.into_boxed_slice(),
        }
    }

    fn new_default_kernel(size: usize) -> Self {
        assert!(KERNEL_ROM.len() <= size);

        let mut mem = Vec::with_capacity(size / 4);
        let mut cursor = Cursor::new(KERNEL_ROM);

        loop {
            let mut bytes: [u8; 4] = [0; 4];
            match cursor.read_exact(&mut bytes) {
                Ok(_) => mem.push(u32::from_ne_bytes(bytes)),
                Err(_) => break,
            }
        }

        Self::new_init(size, mem)
    }

    #[inline]
    fn read32(&self, addr: usize) -> u32 {
        read_mem!(u32, self.mem.as_ref(), addr % self.size)
    }

    #[inline]
    fn read16(&self, addr: usize) -> u16 {
        read_mem!(u16, self.mem.as_ref(), addr % self.size)
    }

    #[inline]
    fn read8(&self, addr: usize) -> u8 {
        read_mem!(u8, self.mem.as_ref(), addr % self.size)
    }

    #[inline]
    fn write32(&mut self, addr: usize, value: u32) {
        write_mem!(u32, self.mem.as_mut(), addr % self.size, value);
    }

    #[inline]
    fn write16(&mut self, addr: usize, value: u16) {
        write_mem!(u16, self.mem.as_mut(), addr % self.size, value);
    }

    #[inline]
    fn write8(&mut self, addr: usize, value: u8) {
        write_mem!(u8, self.mem.as_mut(), addr % self.size, value);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Zeroable, Pod)]
#[repr(transparent)]
struct Bitmap([u32; 8]);
include!("bitmaps.rs");

#[derive(Debug, Clone, Copy, PartialEq, Eq, Zeroable, Pod)]
#[repr(C, align(4))]
struct Color {
    r: u8,
    g: u8,
    b: u8,
    a: u8,
}
impl Color {
    const fn new(r: u8, g: u8, b: u8) -> Self {
        Self {
            r,
            g,
            b,
            a: u8::MAX,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Zeroable, Pod)]
#[repr(transparent)]
struct Palette([Color; 16]);
impl Palette {
    const EGA: Self = Self([
        Color::new(0x00, 0x00, 0x00), // Black
        Color::new(0x00, 0x00, 0xAA), // Dark Blue
        Color::new(0x00, 0xAA, 0x00), // Dark Green
        Color::new(0x00, 0xAA, 0xAA), // Dark Cyan
        Color::new(0xAA, 0x00, 0x00), // Dark Red
        Color::new(0xAA, 0x00, 0xAA), // Dark Magenta
        Color::new(0xAA, 0x55, 0x00), // Brown
        Color::new(0xAA, 0xAA, 0xAA), // Light Gray
        Color::new(0x55, 0x55, 0x55), // Dark Gray
        Color::new(0x55, 0x55, 0xFF), // Blue
        Color::new(0x55, 0xFF, 0x55), // Green
        Color::new(0x55, 0xFF, 0xFF), // Cyan
        Color::new(0xFF, 0x55, 0x55), // Red
        Color::new(0xFF, 0x55, 0xFF), // Magenta
        Color::new(0xFF, 0xFF, 0x55), // Yellow
        Color::new(0xFF, 0xFF, 0xFF), // White
    ]);
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Zeroable, Pod)]
#[repr(C, align(4))]
struct Tile {
    value: u16,
    _padding: u16,
}

#[repr(transparent)]
pub struct Framebuffer<const WIDTH: usize, const HEIGHT: usize> {
    pixels: Box<[Color]>,
}
impl<const WIDTH: usize, const HEIGHT: usize> Framebuffer<WIDTH, HEIGHT> {
    pub fn new() -> Self {
        const BLACK: Color = Color {
            r: u8::MIN,
            g: u8::MIN,
            b: u8::MAX,
            a: u8::MAX,
        };

        Self {
            pixels: vec![BLACK; WIDTH * HEIGHT].into_boxed_slice(),
        }
    }

    #[inline]
    pub fn as_pixels<'a>(&'a self) -> &'a [u8] {
        bytemuck::cast_slice(&self.pixels)
    }

    fn blit(&mut self, bitmap: &Bitmap, palette: &Palette, x: isize, y: isize) {
        if (x >= 0) && (y >= 0) && ((x + 8) <= (WIDTH as isize)) && ((y + 8) <= (HEIGHT as isize)) {
            // No bounds checking required

            for by in 0..8 {
                let fy = y + (by as isize);

                let mut row = u32::from_le(bitmap.0[by]);
                for bx in 0..8 {
                    let fx = x + (bx as isize);

                    let palette_index = (row & 0xF) as usize;
                    let mut pixel = palette.0[palette_index];
                    pixel.a = u8::MAX;
                    self.pixels[(fy as usize) * WIDTH + (fx as usize)] = pixel;

                    row >>= 4;
                }
            }
        } else if ((x + 8) > 0)
            && ((y + 8) > 0)
            && (x < (WIDTH as isize))
            && (y < (HEIGHT as isize))
        {
            // We need to bounds check

            for by in 0..8 {
                let fy = y + (by as isize);

                if (fy >= 0) && (fy < (HEIGHT as isize)) {
                    let mut row = bitmap.0[by];
                    for bx in 0..8 {
                        let fx = x + (bx as isize);

                        if (fx >= 0) && (fx < (WIDTH as isize)) {
                            let palette_index = (row & 0xF) as usize;
                            let mut pixel = palette.0[palette_index];
                            pixel.a = u8::MAX;
                            self.pixels[(fy as usize) * WIDTH + (fx as usize)] = pixel;
                        }

                        row >>= 4;
                    }
                }
            }
        } else {
            // No pixels are inside the framebuffer
        }
    }
}

const BITMAP_COUNT: usize = 1024;
const PALETTE_COUNT: usize = 64;
const TILEMAP_SIZE: usize = 128;
const TILE_COUNT: usize = TILEMAP_SIZE * TILEMAP_SIZE;

const BITMAP_BLOCK: usize = 0;
const PALETTE_BLOCK: usize = 1;
const TILE_BLOCK: usize = 2;

const BITMAP_MEM_SIZE: usize = BITMAP_COUNT * std::mem::size_of::<Bitmap>();
const PALETTE_MEM_SIZE: usize = PALETTE_COUNT * std::mem::size_of::<Palette>();
const TILE_MEM_SIZE: usize = TILE_COUNT * std::mem::size_of::<Tile>();

struct Vram {
    bitmap_mem: Box<[Bitmap]>,
    palette_mem: Box<[Palette]>,
    tile_mem: Box<[Tile]>,
}
impl Vram {
    fn new() -> Self {
        let mut bitmap_mem = unsafe { Box::new_zeroed_slice(BITMAP_COUNT).assume_init() };

        bitmap_mem[0x00] = Bitmap::EMPTY;
        for i in 0x01..0x20 {
            bitmap_mem[i] = Bitmap::REPLACEMENT_CHAR;
        }

        bitmap_mem[0x20] = Bitmap::SPACE;
        bitmap_mem[0x21] = Bitmap::EXCLAMATION_MARK;
        bitmap_mem[0x22] = Bitmap::DOUBLE_QUOTATION_MARK;
        bitmap_mem[0x23] = Bitmap::POUND;
        bitmap_mem[0x24] = Bitmap::DOLLAR_SIGN;
        bitmap_mem[0x25] = Bitmap::PERCENT_SIGN;
        bitmap_mem[0x26] = Bitmap::AMPERSAND;
        bitmap_mem[0x27] = Bitmap::SINGLE_QUOTATION_MARK;
        bitmap_mem[0x28] = Bitmap::LEFT_PARENTHESIS;
        bitmap_mem[0x29] = Bitmap::RIGHT_PARENTHESIS;
        bitmap_mem[0x2A] = Bitmap::ASTERISK;
        bitmap_mem[0x2B] = Bitmap::PLUS_SIGN;
        bitmap_mem[0x2C] = Bitmap::COMMA;
        bitmap_mem[0x2D] = Bitmap::HYPHEN;
        bitmap_mem[0x2E] = Bitmap::PERIOD;
        bitmap_mem[0x2F] = Bitmap::SLASH;

        bitmap_mem[0x30] = Bitmap::DIGIT_0;
        bitmap_mem[0x31] = Bitmap::DIGIT_1;
        bitmap_mem[0x32] = Bitmap::DIGIT_2;
        bitmap_mem[0x33] = Bitmap::DIGIT_3;
        bitmap_mem[0x34] = Bitmap::DIGIT_4;
        bitmap_mem[0x35] = Bitmap::DIGIT_5;
        bitmap_mem[0x36] = Bitmap::DIGIT_6;
        bitmap_mem[0x37] = Bitmap::DIGIT_7;
        bitmap_mem[0x38] = Bitmap::DIGIT_8;
        bitmap_mem[0x39] = Bitmap::DIGIT_9;
        bitmap_mem[0x3A] = Bitmap::COLON;
        bitmap_mem[0x3B] = Bitmap::SEMICOLON;
        bitmap_mem[0x3C] = Bitmap::LESS_THAN_SIGN;
        bitmap_mem[0x3D] = Bitmap::EQUAL_SIGN;
        bitmap_mem[0x3E] = Bitmap::GREATER_THAN_SIGN;
        bitmap_mem[0x3F] = Bitmap::QUESTION_MARK;

        bitmap_mem[0x40] = Bitmap::AT_SIGN;
        bitmap_mem[0x41] = Bitmap::UPPERCASE_A;
        bitmap_mem[0x42] = Bitmap::UPPERCASE_B;
        bitmap_mem[0x43] = Bitmap::UPPERCASE_C;
        bitmap_mem[0x44] = Bitmap::UPPERCASE_D;
        bitmap_mem[0x45] = Bitmap::UPPERCASE_E;
        bitmap_mem[0x46] = Bitmap::UPPERCASE_F;
        bitmap_mem[0x47] = Bitmap::UPPERCASE_G;
        bitmap_mem[0x48] = Bitmap::UPPERCASE_H;
        bitmap_mem[0x49] = Bitmap::UPPERCASE_I;
        bitmap_mem[0x4A] = Bitmap::UPPERCASE_J;
        bitmap_mem[0x4B] = Bitmap::UPPERCASE_K;
        bitmap_mem[0x4C] = Bitmap::UPPERCASE_L;
        bitmap_mem[0x4D] = Bitmap::UPPERCASE_M;
        bitmap_mem[0x4E] = Bitmap::UPPERCASE_N;
        bitmap_mem[0x4F] = Bitmap::UPPERCASE_O;

        bitmap_mem[0x50] = Bitmap::UPPERCASE_P;
        bitmap_mem[0x51] = Bitmap::UPPERCASE_Q;
        bitmap_mem[0x52] = Bitmap::UPPERCASE_R;
        bitmap_mem[0x53] = Bitmap::UPPERCASE_S;
        bitmap_mem[0x54] = Bitmap::UPPERCASE_T;
        bitmap_mem[0x55] = Bitmap::UPPERCASE_U;
        bitmap_mem[0x56] = Bitmap::UPPERCASE_V;
        bitmap_mem[0x57] = Bitmap::UPPERCASE_W;
        bitmap_mem[0x58] = Bitmap::UPPERCASE_X;
        bitmap_mem[0x59] = Bitmap::UPPERCASE_Y;
        bitmap_mem[0x5A] = Bitmap::UPPERCASE_Z;
        bitmap_mem[0x5B] = Bitmap::LEFT_SQUARE_BRACKET;
        bitmap_mem[0x5C] = Bitmap::BACKSLASH;
        bitmap_mem[0x5D] = Bitmap::RIGHT_SQUARE_BRACKET;
        bitmap_mem[0x5E] = Bitmap::CIRCUMFLEX;
        bitmap_mem[0x5F] = Bitmap::UNDERSCORE;

        bitmap_mem[0x60] = Bitmap::GRAVE;
        bitmap_mem[0x61] = Bitmap::LOWERCASE_A;
        bitmap_mem[0x62] = Bitmap::LOWERCASE_B;
        bitmap_mem[0x63] = Bitmap::LOWERCASE_C;
        bitmap_mem[0x64] = Bitmap::LOWERCASE_D;
        bitmap_mem[0x65] = Bitmap::LOWERCASE_E;
        bitmap_mem[0x66] = Bitmap::LOWERCASE_F;
        bitmap_mem[0x67] = Bitmap::LOWERCASE_G;
        bitmap_mem[0x68] = Bitmap::LOWERCASE_H;
        bitmap_mem[0x69] = Bitmap::LOWERCASE_I;
        bitmap_mem[0x6A] = Bitmap::LOWERCASE_J;
        bitmap_mem[0x6B] = Bitmap::LOWERCASE_K;
        bitmap_mem[0x6C] = Bitmap::LOWERCASE_L;
        bitmap_mem[0x6D] = Bitmap::LOWERCASE_M;
        bitmap_mem[0x6E] = Bitmap::LOWERCASE_N;
        bitmap_mem[0x6F] = Bitmap::LOWERCASE_O;

        bitmap_mem[0x70] = Bitmap::LOWERCASE_P;
        bitmap_mem[0x71] = Bitmap::LOWERCASE_Q;
        bitmap_mem[0x72] = Bitmap::LOWERCASE_R;
        bitmap_mem[0x73] = Bitmap::LOWERCASE_S;
        bitmap_mem[0x74] = Bitmap::LOWERCASE_T;
        bitmap_mem[0x75] = Bitmap::LOWERCASE_U;
        bitmap_mem[0x76] = Bitmap::LOWERCASE_V;
        bitmap_mem[0x77] = Bitmap::LOWERCASE_W;
        bitmap_mem[0x78] = Bitmap::LOWERCASE_X;
        bitmap_mem[0x79] = Bitmap::LOWERCASE_Y;
        bitmap_mem[0x7A] = Bitmap::LOWERCASE_Z;
        bitmap_mem[0x7B] = Bitmap::LEFT_CURLY_BRACKET;
        bitmap_mem[0x7C] = Bitmap::VERTICAL_BAR;
        bitmap_mem[0x7D] = Bitmap::RIGHT_CURLY_BRACKET;
        bitmap_mem[0x7E] = Bitmap::TILDE;
        bitmap_mem[0x7F] = Bitmap::REPLACEMENT_CHAR;

        Self {
            bitmap_mem,
            palette_mem: vec![Palette::EGA; PALETTE_COUNT].into_boxed_slice(),
            tile_mem: unsafe { Box::new_zeroed_slice(TILE_COUNT).assume_init() },
        }
    }

    #[inline]
    fn read32(&self, _addr: usize) -> u32 {
        0 // VRAM is write-only to the CPU
    }

    #[inline]
    fn read16(&self, _addr: usize) -> u16 {
        0 // VRAM is write-only to the CPU
    }

    #[inline]
    fn read8(&self, _addr: usize) -> u8 {
        0 // VRAM is write-only to the CPU
    }

    fn write32(&mut self, addr: usize, value: u32) {
        match (addr >> 16) & 0x3 {
            BITMAP_BLOCK => {
                write_mem!(u32, self.bitmap_mem.as_mut(), addr % BITMAP_MEM_SIZE, value)
            }
            PALETTE_BLOCK => write_mem!(
                u32,
                self.palette_mem.as_mut(),
                addr % PALETTE_MEM_SIZE,
                value
            ),
            TILE_BLOCK => write_mem!(u32, self.tile_mem.as_mut(), addr % TILE_MEM_SIZE, value),
            _ => {}
        }
    }

    fn write16(&mut self, addr: usize, value: u16) {
        match (addr >> 16) & 0x3 {
            BITMAP_BLOCK => {
                write_mem!(u16, self.bitmap_mem.as_mut(), addr % BITMAP_MEM_SIZE, value)
            }
            PALETTE_BLOCK => write_mem!(
                u16,
                self.palette_mem.as_mut(),
                addr % PALETTE_MEM_SIZE,
                value
            ),
            TILE_BLOCK => write_mem!(u16, self.tile_mem.as_mut(), addr % TILE_MEM_SIZE, value),
            _ => {}
        }
    }

    fn write8(&mut self, addr: usize, value: u8) {
        match (addr >> 16) & 0x3 {
            BITMAP_BLOCK => write_mem!(u8, self.bitmap_mem.as_mut(), addr % BITMAP_MEM_SIZE, value),
            PALETTE_BLOCK => write_mem!(
                u8,
                self.palette_mem.as_mut(),
                addr % PALETTE_MEM_SIZE,
                value
            ),
            TILE_BLOCK => write_mem!(u8, self.tile_mem.as_mut(), addr % TILE_MEM_SIZE, value),
            _ => {}
        }
    }

    fn draw_to_framebuffer<const WIDTH: usize, const HEIGHT: usize>(
        &self,
        framebuffer: &mut Framebuffer<WIDTH, HEIGHT>,
        offset_h: usize,
        offset_v: usize,
    ) {
        let tile_width = WIDTH.div_ceil(8) + 1;
        let tile_height = HEIGHT.div_ceil(8) + 1;

        let start_tile_x = offset_h / 8;
        let start_tile_y = offset_v / 8;
        let pixel_offset_x = (offset_h % 8) as isize;
        let pixel_offset_y = (offset_v % 8) as isize;

        for ty in 0..tile_height {
            let py = (ty * 8) as isize - pixel_offset_y;
            let ty = (ty + start_tile_y) % TILEMAP_SIZE;

            for tx in 0..tile_width {
                let px = (tx * 8) as isize - pixel_offset_x;
                let tx = (tx + start_tile_x) % TILEMAP_SIZE;

                let tile = self.tile_mem[ty * TILEMAP_SIZE + tx];
                let bitmap_index = (tile.value & 0x03FF) as usize;
                let palette_index = (tile.value >> 10) as usize;
                let bitmap = &self.bitmap_mem[bitmap_index];
                let palette = &self.palette_mem[palette_index];

                framebuffer.blit(bitmap, palette, px, py);
            }
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CopyDirection {
    Forward,
    Backward,
}

const KRAM_SIZE: usize = 0x8000; // 15 bit
const SRAM_SIZE: usize = 0x100000; // 20 bit

const KRAM_BANK: u8 = 0x00;
const SRAM_BANK: u8 = 0x01;
const VRAM_BANK: u8 = 0x02;

macro_rules! read_k {
    ($kram:expr, $read:ident, $addr:expr, $k:expr) => {
        if $k {
            $kram.$read($addr)
        } else {
            0
        }
    };
}

macro_rules! write_k {
    ($kram:expr, $write:ident, $addr:expr, $value:expr, $k:expr) => {
        if $k {
            $kram.$write($addr, $value);
        }
    };
}

pub struct MemoryBus {
    kram: Ram,
    sram: Ram,
    vram: Vram,
}
impl MemoryBus {
    pub fn new(rom: Option<Vec<u32>>) -> Self {
        let kram = if let Some(rom) = rom {
            Ram::new_init(KRAM_SIZE, rom)
        } else {
            Ram::new_default_kernel(KRAM_SIZE)
        };

        Self {
            kram,
            sram: Ram::new_zeroed(SRAM_SIZE),
            vram: Vram::new(),
        }
    }

    pub fn read32(&self, addr: usize, k_flag: bool) -> u32 {
        let msb = (addr >> 24) as u8;
        let addr = addr & 0x00FFFFFF;
        match msb {
            KRAM_BANK => read_k!(self.kram, read32, addr, k_flag),
            SRAM_BANK => self.sram.read32(addr),
            VRAM_BANK => self.vram.read32(addr),
            _ => 0,
        }
    }

    pub fn read16(&self, addr: usize, k_flag: bool) -> u16 {
        let msb = (addr >> 24) as u8;
        let addr = addr & 0x00FFFFFF;
        match msb {
            KRAM_BANK => read_k!(self.kram, read16, addr, k_flag),
            SRAM_BANK => self.sram.read16(addr),
            VRAM_BANK => self.vram.read16(addr),
            _ => 0,
        }
    }

    pub fn read8(&self, addr: usize, k_flag: bool) -> u8 {
        let msb = (addr >> 24) as u8;
        let addr = addr & 0x00FFFFFF;
        match msb {
            KRAM_BANK => read_k!(self.kram, read8, addr, k_flag),
            SRAM_BANK => self.sram.read8(addr),
            VRAM_BANK => self.vram.read8(addr),
            _ => 0,
        }
    }

    pub fn write32(&mut self, addr: usize, value: u32, k_flag: bool) {
        let msb = (addr >> 24) as u8;
        let addr = addr & 0x00FFFFFF;
        match msb {
            KRAM_BANK => write_k!(self.kram, write32, addr, value, k_flag),
            SRAM_BANK => self.sram.write32(addr, value),
            VRAM_BANK => self.vram.write32(addr, value),
            _ => {}
        }
    }

    pub fn write16(&mut self, addr: usize, value: u16, k_flag: bool) {
        let msb = (addr >> 24) as u8;
        let addr = addr & 0x00FFFFFF;
        match msb {
            KRAM_BANK => write_k!(self.kram, write16, addr, value, k_flag),
            SRAM_BANK => self.sram.write16(addr, value),
            VRAM_BANK => self.vram.write16(addr, value),
            _ => {}
        }
    }

    pub fn write8(&mut self, addr: usize, value: u8, k_flag: bool) {
        let msb = (addr >> 24) as u8;
        let addr = addr & 0x00FFFFFF;
        match msb {
            KRAM_BANK => write_k!(self.kram, write8, addr, value, k_flag),
            SRAM_BANK => self.sram.write8(addr, value),
            VRAM_BANK => self.vram.write8(addr, value),
            _ => {}
        }
    }

    pub fn copy(&mut self, src: usize, dst: usize, len: usize, dir: CopyDirection, k_flag: bool) {
        let mut src = Wrapping(src);
        let mut dst = Wrapping(dst);
        let mut len = len;

        while len > 0 {
            let value = self.read32(src.0, k_flag);
            self.write32(dst.0, value, k_flag);

            match dir {
                CopyDirection::Forward => {
                    src += Wrapping(std::mem::size_of::<u32>());
                    dst += Wrapping(std::mem::size_of::<u32>());
                }
                CopyDirection::Backward => {
                    src -= Wrapping(std::mem::size_of::<u32>());
                    dst -= Wrapping(std::mem::size_of::<u32>());
                }
            }

            len -= 1;
        }
    }

    #[inline]
    pub fn draw_framebuffer<const WIDTH: usize, const HEIGHT: usize>(
        &self,
        framebuffer: &mut Framebuffer<WIDTH, HEIGHT>,
        offset_h: usize,
        offset_v: usize,
    ) {
        self.vram
            .draw_to_framebuffer(framebuffer, offset_h, offset_v);
    }
}

pub trait Device {
    fn read(&mut self, addr: usize) -> Word;
    fn write(&mut self, addr: usize, value: Word);
}

struct DeviceMapping {
    device: SharedRef<dyn Device>,
    start: usize,
    end: usize,
    mask: usize,
    offset: usize,
}

#[inline]
const fn addr_in_range(addr: usize, start: usize, end: usize) -> bool {
    (start <= addr) && (end >= addr)
}

#[inline]
const fn calc_device_addr(mapping: &DeviceMapping, addr: usize) -> usize {
    ((addr - mapping.start) & mapping.mask) + mapping.offset
}

pub struct IoBus {
    mappings: Vec<DeviceMapping>,
    cache: LruCache<usize, usize, ahash::RandomState>,
}
impl IoBus {
    #[inline]
    pub fn new() -> Self {
        const CACHE_CAPACITY: usize = 1024;

        Self {
            mappings: Vec::new(),
            cache: LruCache::new(CACHE_CAPACITY),
        }
    }

    pub fn map_device<T: 'static + Device>(
        &mut self,
        device: SharedRef<T>,
        start: usize,
        end: usize,
        mask: usize,
        offset: usize,
    ) {
        assert!(start < end);

        for m in self.mappings.iter() {
            let start_in_range = addr_in_range(start, m.start, m.end);
            let end_in_range = addr_in_range(end, m.start, m.end);
            assert!(!start_in_range && !end_in_range);
        }

        let mapping = DeviceMapping {
            device,
            start,
            end,
            mask,
            offset,
        };

        self.mappings.push(mapping);
    }

    fn find_mapping_slow(&self, addr: usize) -> Option<usize> {
        for (i, m) in self.mappings.iter().enumerate() {
            if addr_in_range(addr, m.start, m.end) {
                return Some(i);
            }
        }

        None
    }

    fn get_mapping(&mut self, addr: usize) -> Option<usize> {
        if let Some(index) = self.cache.get(&addr) {
            Some(*index)
        } else {
            if let Some(index) = self.find_mapping_slow(addr) {
                self.cache.put(addr, index);
                Some(index)
            } else {
                None
            }
        }
    }

    pub fn read(&mut self, addr: usize, k_flag: bool) -> Word {
        if k_flag {
            if let Some(index) = self.get_mapping(addr) {
                let mapping = self.mappings.get(index).expect("Cache desync");
                let device_addr = calc_device_addr(mapping, addr);
                borrow_shared(&mapping.device).read(device_addr)
            } else {
                0
            }
        } else {
            0
        }
    }

    pub fn write(&mut self, addr: usize, value: Word, k_flag: bool) {
        if k_flag {
            if let Some(index) = self.get_mapping(addr) {
                let mapping = self.mappings.get(index).expect("Cache desync");
                let device_addr = calc_device_addr(mapping, addr);
                borrow_shared(&mapping.device).write(device_addr, value);
            }
        }
    }
}

pub struct DmaController {
    regs: [Word; 4],
    pub run: bool,
}
impl DmaController {
    #[inline]
    pub fn new() -> Self {
        Self {
            regs: [0; 4],
            run: false,
        }
    }

    #[inline]
    pub fn src(&self) -> usize {
        self.regs[0] as usize
    }

    #[inline]
    pub fn dst(&self) -> usize {
        self.regs[1] as usize
    }

    #[inline]
    pub fn len(&self) -> usize {
        self.regs[2] as usize
    }

    #[inline]
    pub fn dir(&self) -> CopyDirection {
        if self.regs[3] == 0 {
            CopyDirection::Forward
        } else {
            CopyDirection::Backward
        }
    }

    pub fn reset(&mut self) {
        self.regs = [0; 4];
        self.run = false;
    }
}
impl Device for DmaController {
    fn read(&mut self, addr: usize) -> Word {
        self.regs[addr & 0x3]
    }

    fn write(&mut self, addr: usize, value: Word) {
        let addr = addr & 0x3;

        self.regs[addr] = if addr == 0x2 {
            value & 0x3FFFFFFF
        } else if addr == 0x3 {
            self.run = true;
            value & 0x1
        } else {
            value & 0xFFFFFFFC
        };
    }
}

pub struct UartController {
    rx: VecDeque<u8>,
    tx: VecDeque<u8>,
}
impl UartController {
    #[inline]
    pub fn new() -> Self {
        Self {
            rx: VecDeque::new(),
            tx: VecDeque::new(),
        }
    }

    pub fn reset(&mut self) {
        self.rx.clear();
        self.tx.clear();
    }

    #[inline]
    pub fn host_read(&mut self) -> Option<u8> {
        self.tx.pop_front()
    }

    #[inline]
    pub fn host_write(&mut self, data: u8) {
        self.rx.push_back(data);
    }
}
impl Device for UartController {
    fn read(&mut self, addr: usize) -> Word {
        match addr {
            0x0 => self.rx.pop_front().unwrap_or_default() as Word,
            0x1 => 0,
            0x2 => usize::min(self.rx.len(), 0xFF) as Word,
            0x3 => usize::min(self.tx.len(), 0xFF) as Word,
            _ => unreachable!(),
        }
    }

    fn write(&mut self, addr: usize, value: Word) {
        if addr == 0x1 {
            self.tx.push_back(value as u8);
        }
    }
}
