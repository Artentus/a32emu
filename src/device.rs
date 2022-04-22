use crate::{borrow_shared, SharedRef, Word};
use lru::LruCache;
use std::borrow::Cow;
use std::collections::VecDeque;
use std::io::{Cursor, Read};
use std::num::Wrapping;

const KERNEL_ROM: &[u8] = include_bytes!("../res/kernel_rom.bin");

#[inline]
fn transmute_with_len<'a, I, O>(slice: &'a [I]) -> &'a [O] {
    let in_size = std::mem::size_of::<I>();
    let out_size = std::mem::size_of::<O>();

    let ptr = slice.as_ptr();
    unsafe { std::slice::from_raw_parts(ptr as *const O, slice.len() * in_size / out_size) }
}

#[inline]
fn transmute_with_len_mut<'a, I, O>(slice: &'a mut [I]) -> &'a mut [O] {
    let in_size = std::mem::size_of::<I>();
    let out_size = std::mem::size_of::<O>();

    let ptr = slice.as_mut_ptr();
    unsafe { std::slice::from_raw_parts_mut(ptr as *mut O, slice.len() * in_size / out_size) }
}

macro_rules! read_mem {
    ($t:ty, $mem:expr, $addr:expr) => {{
        let mem_t: &[$t] = transmute_with_len($mem);
        let addr = $addr / std::mem::size_of::<$t>();
        <$t>::from_le(mem_t[addr])
    }};
}

macro_rules! write_mem {
    ($t:ty, $mem:expr, $addr:expr, $value:expr) => {{
        let mem_t: &mut [$t] = transmute_with_len_mut($mem);
        let addr = $addr / std::mem::size_of::<$t>();
        mem_t[addr] = $value.to_le();
    }};
}

pub struct Ram {
    size: usize,
    mem: Box<[u32]>,
}
impl Ram {
    #[inline]
    pub fn new_zeroed(size: usize) -> Self {
        Self {
            size,
            mem: unsafe { Box::new_zeroed_slice(size / 4).assume_init() },
        }
    }

    pub fn new_init(size: usize, mut mem: Vec<u32>) -> Self {
        assert!(mem.len() <= (size / 4));

        while mem.len() < (size / 4) {
            mem.push(0);
        }

        Self {
            size,
            mem: mem.into_boxed_slice(),
        }
    }

    pub fn new_default_kernel(size: usize) -> Self {
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
    pub fn read32(&self, addr: usize) -> u32 {
        read_mem!(u32, self.mem.as_ref(), addr % self.size)
    }

    #[inline]
    pub fn read16(&self, addr: usize) -> u16 {
        read_mem!(u16, self.mem.as_ref(), addr % self.size)
    }

    #[inline]
    pub fn read8(&self, addr: usize) -> u8 {
        read_mem!(u8, self.mem.as_ref(), addr % self.size)
    }

    #[inline]
    pub fn write32(&mut self, addr: usize, value: u32) {
        write_mem!(u32, self.mem.as_mut(), addr % self.size, value);
    }

    #[inline]
    pub fn write16(&mut self, addr: usize, value: u16) {
        write_mem!(u16, self.mem.as_mut(), addr % self.size, value);
    }

    #[inline]
    pub fn write8(&mut self, addr: usize, value: u8) {
        write_mem!(u8, self.mem.as_mut(), addr % self.size, value);
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
const SRAM_BANK: u8 = 0x00;

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
    pub kram: Ram,
    pub sram: Ram,
}
impl MemoryBus {
    pub fn new(rom: Option<Vec<u32>>) -> Self {
        if let Some(rom) = rom {
            Self {
                kram: Ram::new_init(KRAM_SIZE, rom),
                sram: Ram::new_zeroed(SRAM_SIZE),
            }
        } else {
            Self {
                kram: Ram::new_default_kernel(KRAM_SIZE),
                sram: Ram::new_zeroed(SRAM_SIZE),
            }
        }
    }

    pub fn read32(&self, addr: usize, k_flag: bool) -> u32 {
        let msb = (addr >> 24) as u8;
        match msb {
            KRAM_BANK => read_k!(self.kram, read32, addr, k_flag),
            SRAM_BANK => self.sram.read32(addr),
            _ => 0,
        }
    }

    pub fn read16(&self, addr: usize, k_flag: bool) -> u16 {
        let msb = (addr >> 24) as u8;
        match msb {
            KRAM_BANK => read_k!(self.kram, read16, addr, k_flag),
            SRAM_BANK => self.sram.read16(addr),
            _ => 0,
        }
    }

    pub fn read8(&self, addr: usize, k_flag: bool) -> u8 {
        let msb = (addr >> 24) as u8;
        match msb {
            KRAM_BANK => read_k!(self.kram, read8, addr, k_flag),
            SRAM_BANK => self.sram.read8(addr),
            _ => 0,
        }
    }

    pub fn write32(&mut self, addr: usize, value: u32, k_flag: bool) {
        let msb = (addr >> 24) as u8;
        match msb {
            KRAM_BANK => write_k!(self.kram, write32, addr, value, k_flag),
            SRAM_BANK => self.sram.write32(addr, value),
            _ => {}
        }
    }

    pub fn write16(&mut self, addr: usize, value: u16, k_flag: bool) {
        let msb = (addr >> 24) as u8;
        match msb {
            KRAM_BANK => write_k!(self.kram, write16, addr, value, k_flag),
            SRAM_BANK => self.sram.write16(addr, value),
            _ => {}
        }
    }

    pub fn write8(&mut self, addr: usize, value: u8, k_flag: bool) {
        let msb = (addr >> 24) as u8;
        match msb {
            KRAM_BANK => write_k!(self.kram, write8, addr, value, k_flag),
            SRAM_BANK => self.sram.write8(addr, value),
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
