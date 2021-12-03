use crate::{borrow_shared, SharedRef, Word};
use align_data::include_transmute;
use lru::LruCache;
use std::collections::VecDeque;
use std::num::Wrapping;

pub const ROM_SIZE: usize = 0x1000;
pub const RAM_SIZE: usize = 0x1000000;

static KERNEL_ROM: [u32; ROM_SIZE / 4] = unsafe { include_transmute!("../res/kernel_rom.bin") };

macro_rules! read {
    ($t: ty, $mem: expr, $addr: expr) => {{
        let mem_t: &[$t] = unsafe { std::mem::transmute($mem) };

        <$t>::from_le(mem_t[$addr])
    }};
}

macro_rules! write {
    ($t: ty, $mem: expr, $addr: expr, $value: expr) => {{
        let mem_t: &mut [$t] = unsafe { std::mem::transmute($mem) };

        mem_t[$addr] = $value.to_le();
    }};
}

pub struct Rom {
    mem: &'static [u32],
}
impl Rom {
    #[inline]
    pub fn new(mem: &'static [u32]) -> Self {
        Self { mem }
    }

    #[inline]
    pub fn read32(&self, addr: usize) -> u32 {
        read!(u32, self.mem, (addr % ROM_SIZE) >> 2)
    }

    #[inline]
    pub fn read16(&self, addr: usize) -> u16 {
        read!(u16, self.mem, (addr % ROM_SIZE) >> 1)
    }

    #[inline]
    pub fn read8(&self, addr: usize) -> u8 {
        read!(u8, self.mem, (addr % ROM_SIZE) >> 0)
    }
}
impl Default for Rom {
    fn default() -> Self {
        Self { mem: &KERNEL_ROM }
    }
}

pub struct Ram {
    mem: Box<[u32]>,
}
impl Ram {
    #[inline]
    pub fn new() -> Self {
        Self {
            mem: vec![0; RAM_SIZE / 4].into_boxed_slice(),
        }
    }

    #[inline]
    pub fn read32(&self, addr: usize) -> u32 {
        read!(u32, self.mem.as_ref(), (addr % RAM_SIZE) >> 2)
    }

    #[inline]
    pub fn read16(&self, addr: usize) -> u16 {
        read!(u16, self.mem.as_ref(), (addr % RAM_SIZE) >> 1)
    }

    #[inline]
    pub fn read8(&self, addr: usize) -> u8 {
        read!(u8, self.mem.as_ref(), (addr % RAM_SIZE) >> 0)
    }

    #[inline]
    pub fn write32(&mut self, addr: usize, value: u32) {
        write!(u32, self.mem.as_mut(), (addr % RAM_SIZE) >> 2, value);
    }

    #[inline]
    pub fn write16(&mut self, addr: usize, value: u16) {
        write!(u16, self.mem.as_mut(), (addr % RAM_SIZE) >> 1, value);
    }

    #[inline]
    pub fn write8(&mut self, addr: usize, value: u8) {
        write!(u8, self.mem.as_mut(), (addr % RAM_SIZE) >> 0, value);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CopyDirection {
    Forward,
    Backward,
}

pub struct MemoryBus {
    pub rom: Rom,
    pub ram: Ram,
}
impl MemoryBus {
    pub fn new(rom: Option<&'static [u32]>) -> Self {
        if let Some(rom) = rom {
            Self {
                rom: Rom::new(rom),
                ram: Ram::new(),
            }
        } else {
            Self {
                rom: Rom::default(),
                ram: Ram::new(),
            }
        }
    }

    pub fn read32(&self, addr: usize) -> u32 {
        let msb = (addr >> 24) as u8;
        match msb {
            0x00 => self.rom.read32(addr),
            0x01 => self.ram.read32(addr),
            _ => 0,
        }
    }

    pub fn read16(&self, addr: usize) -> u16 {
        let msb = (addr >> 24) as u8;
        match msb {
            0x00 => self.rom.read16(addr),
            0x01 => self.ram.read16(addr),
            _ => 0,
        }
    }

    pub fn read8(&self, addr: usize) -> u8 {
        let msb = (addr >> 24) as u8;
        match msb {
            0x00 => self.rom.read8(addr),
            0x01 => self.ram.read8(addr),
            _ => 0,
        }
    }

    pub fn write32(&mut self, addr: usize, value: u32) {
        let msb = (addr >> 24) as u8;
        match msb {
            0x01 => self.ram.write32(addr, value),
            _ => {}
        }
    }

    pub fn write16(&mut self, addr: usize, value: u16) {
        let msb = (addr >> 24) as u8;
        match msb {
            0x01 => self.ram.write16(addr, value),
            _ => {}
        }
    }

    pub fn write8(&mut self, addr: usize, value: u8) {
        let msb = (addr >> 24) as u8;
        match msb {
            0x01 => self.ram.write8(addr, value),
            _ => {}
        }
    }

    pub fn copy(&mut self, src: usize, dst: usize, len: usize, dir: CopyDirection) {
        let mut src = Wrapping(src);
        let mut dst = Wrapping(dst);
        let mut len = len;

        while len > 0 {
            let value = self.read32(src.0);
            self.write32(dst.0, value);

            match dir {
                CopyDirection::Forward => {
                    src += Wrapping(4);
                    dst += Wrapping(4);
                }
                CopyDirection::Backward => {
                    src -= Wrapping(4);
                    dst -= Wrapping(4);
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

    pub fn read(&mut self, addr: usize) -> Word {
        if let Some(index) = self.get_mapping(addr) {
            let mapping = self.mappings.get(index).expect("Cache desync");
            let device_addr = calc_device_addr(mapping, addr);
            borrow_shared(&mapping.device).read(device_addr)
        } else {
            0
        }
    }

    pub fn write(&mut self, addr: usize, value: Word) {
        if let Some(index) = self.get_mapping(addr) {
            let mapping = self.mappings.get(index).expect("Cache desync");
            let device_addr = calc_device_addr(mapping, addr);
            borrow_shared(&mapping.device).write(device_addr, value);
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
