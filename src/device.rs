use std::collections::VecDeque;

use crate::{borrow_shared, SharedRef, Word};
use align_data::include_aligned;
use lru::LruCache;

const ROM_SIZE: usize = 0x1000;
const RAM_SIZE: usize = 0x1000000;

static KERNEL_ROM: &[u8; ROM_SIZE] = include_aligned!(u32, "../res/kernel_rom.bin");

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
    mem: &'static [u8],
}
impl Rom {
    #[inline]
    pub fn new() -> Self {
        Self { mem: KERNEL_ROM }
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

    pub fn copy(&mut self, src: usize, dst: usize, len: usize) {
        let src = src >> 2;
        let dst = dst >> 2;

        let src_end = usize::min(src + len, RAM_SIZE / 4);
        let dst_end = usize::min(dst + len, RAM_SIZE / 4);
        let src_len = src_end - src;
        let dst_len = dst_end - dst;
        let len = usize::min(src_len, dst_len);

        if len > 0 {
            unsafe {
                let src_ptr = self.mem.as_ptr().offset(src as isize);
                let dst_ptr = self.mem.as_mut_ptr().offset(dst as isize);
                std::ptr::copy(src_ptr, dst_ptr, len);
            }
        }
    }
}

pub struct MemoryBus {
    pub rom: Rom,
    pub ram: Ram,
}
impl MemoryBus {
    #[inline]
    pub fn new() -> Self {
        Self {
            rom: Rom::new(),
            ram: Ram::new(),
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

    pub fn copy(&mut self, src: usize, dst: usize, len: usize) {
        let src_msb = (src >> 24) as u8;
        let dst_msb = (dst >> 24) as u8;

        if (src_msb == 0x01) && (dst_msb == 0x01) {
            self.ram.copy(src, dst, len);
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

    pub fn src(&self) -> usize {
        if (self.regs[3] & 0x1) == 0 {
            self.regs[0] as usize
        } else {
            (self.regs[0] - (self.regs[2] * 4) + 4) as usize
        }
    }

    pub fn dst(&self) -> usize {
        if (self.regs[3] & 0x1) == 0 {
            self.regs[1] as usize
        } else {
            (self.regs[1] - (self.regs[2] * 4) + 4) as usize
        }
    }

    pub fn len(&self) -> usize {
        self.regs[3] as usize
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
        self.regs[addr] = value;
        if addr == 0x3 {
            self.run = true;
        }
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
