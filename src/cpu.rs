use crate::device::{IoBus, MemoryBus};
use crate::{DWord, SWord, Word};
use std::fmt::Display;
use std::num::Wrapping;

const REG_COUNT: usize = 32;
const SYSCALL_ADDRESS: Word = 0x00000FF0;

type Register = u8;
type Flag = bool;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AluOperation {
    Add,
    Addc,
    Sub,
    Subb,
    And,
    Or,
    Xor,
    Shl,
    Asr,
    Lsr,
    Mull,
    Mulh,
    Smull,
    Smulh,
    None,
}
impl AluOperation {
    fn decode(op: Word) -> Self {
        match op & 0xF {
            0x0 => Self::Add,
            0x1 => Self::Addc,
            0x2 => Self::Sub,
            0x3 => Self::Subb,
            0x4 => Self::And,
            0x5 => Self::Or,
            0x6 => Self::Xor,
            0x7 => Self::Shl,
            0x8 => Self::Asr,
            0x9 => Self::Lsr,
            0xA => Self::Mull,
            0xB => Self::Mulh,
            0xC => Self::Smull,
            0xD => Self::Smulh,
            _ => Self::None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AluSource {
    Register(Register),
    Immediate,
    ProgramCounter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct AluInstruction {
    op: AluOperation,
    lhs: AluSource,
    rhs: AluSource,
    dst: Register,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MemoryMode {
    Bits32,
    Bits16,
    Bits8,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct LoadInstruction {
    mode: MemoryMode,
    signed: bool,
    base: AluSource,
    offset: AluSource,
    dst: Register,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct StoreInstruction {
    mode: MemoryMode,
    base: AluSource,
    offset: AluSource,
    src: Register,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum JumpCondition {
    Never,
    Carry,
    Zero,
    Sign,
    Overflow,
    NotCarry,
    NotZero,
    NotSign,
    NotOverflow,
    UnsignedLessEqual,
    UnsignedGreater,
    SignedLess,
    SignedGreaterEqual,
    SignedLessEqual,
    SignedGreater,
    Always,
}
impl JumpCondition {
    fn decode(op: Word) -> Self {
        match op & 0xF {
            0x0 => Self::Never,
            0x1 => Self::Carry,
            0x2 => Self::Zero,
            0x3 => Self::Sign,
            0x4 => Self::Overflow,
            0x5 => Self::NotCarry,
            0x6 => Self::NotZero,
            0x7 => Self::NotSign,
            0x8 => Self::NotOverflow,
            0x9 => Self::UnsignedLessEqual,
            0xA => Self::UnsignedGreater,
            0xB => Self::SignedLess,
            0xC => Self::SignedGreaterEqual,
            0xD => Self::SignedLessEqual,
            0xE => Self::SignedGreater,
            0xF => Self::Always,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct JumpInstruction {
    con: JumpCondition,
    base: AluSource,
    offset: AluSource,
    indirect: bool,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct InInstruction {
    base: AluSource,
    offset: AluSource,
    dst: Register,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
struct OutInstruction {
    base: AluSource,
    offset: AluSource,
    src: Register,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Nop,
    Brk,
    Hlt,
    Err,
    Alu(AluInstruction),
    Load(LoadInstruction),
    Store(StoreInstruction),
    Jump(JumpInstruction),
    In(InInstruction),
    Out(OutInstruction),
    Sys,
    Clrk,
}
impl Instruction {
    fn decode(code: Word) -> Self {
        let grp = code & 0x7;
        let op = (code >> 3) & 0xF;
        let reg1 = ((code >> 7) & 0x1F) as Register;
        let reg2 = ((code >> 12) & 0x1F) as Register;
        let reg3 = ((code >> 17) & 0x1F) as Register;

        match grp {
            0x0 => match op & 0x3 {
                0x0 => Self::Nop,
                0x1 => Self::Brk,
                0x2 => Self::Hlt,
                0x3 => Self::Err,
                _ => unreachable!(),
            },
            0x1 => {
                let inst = AluInstruction {
                    op: AluOperation::decode(op),
                    lhs: AluSource::Register(reg2),
                    rhs: AluSource::Register(reg3),
                    dst: reg1,
                };

                Self::Alu(inst)
            }
            0x2 => {
                let inst = AluInstruction {
                    op: AluOperation::decode(op),
                    lhs: AluSource::Register(reg2),
                    rhs: AluSource::Immediate,
                    dst: reg1,
                };

                Self::Alu(inst)
            }
            0x3 => {
                let is_imm = (op & 0x1) != 0;
                let offset = if is_imm {
                    AluSource::Immediate
                } else {
                    AluSource::Register(reg3)
                };

                if (op >> 2) == 0x3 {
                    let mode = if (op & 0x2) == 0 {
                        MemoryMode::Bits8
                    } else {
                        MemoryMode::Bits16
                    };

                    let inst = LoadInstruction {
                        mode,
                        signed: true,
                        base: AluSource::Register(reg2),
                        offset,
                        dst: reg1,
                    };

                    Self::Load(inst)
                } else {
                    let is_st = (op & 0x2) != 0;

                    let mode = match op >> 2 {
                        0x0 => MemoryMode::Bits32,
                        0x1 => MemoryMode::Bits8,
                        0x2 => MemoryMode::Bits16,
                        _ => unreachable!(),
                    };

                    if is_st {
                        let inst = StoreInstruction {
                            mode,
                            base: AluSource::Register(reg1),
                            offset,
                            src: reg2,
                        };

                        Self::Store(inst)
                    } else {
                        let inst = LoadInstruction {
                            mode,
                            signed: false,
                            base: AluSource::Register(reg2),
                            offset,
                            dst: reg1,
                        };

                        Self::Load(inst)
                    }
                }
            }
            0x4 => {
                let inst = match op & 0x3 {
                    0x0 => {
                        JumpInstruction {
                            con: JumpCondition::Always,
                            base: AluSource::Register(reg2),
                            offset: AluSource::Register(reg3),
                            indirect: false,
                        }
                    }
                    0x1 => {
                        JumpInstruction {
                            con: JumpCondition::Always,
                            base: AluSource::Register(reg2),
                            offset: AluSource::Immediate,
                            indirect: false,
                        }
                    }
                    0x2 => {
                        JumpInstruction {
                            con: JumpCondition::Always,
                            base: AluSource::Register(reg2),
                            offset: AluSource::Register(reg3),
                            indirect: true,
                        }
                    }
                    0x3 => {
                        JumpInstruction {
                            con: JumpCondition::Always,
                            base: AluSource::Register(reg2),
                            offset: AluSource::Immediate,
                            indirect: true,
                        }
                    }
                    _ => unreachable!(),
                };

                Self::Jump(inst)
            }
            0x5 => {
                let inst = JumpInstruction {
                    con: JumpCondition::decode(op),
                    base: AluSource::ProgramCounter,
                    offset: AluSource::Immediate,
                    indirect: false,
                };

                Self::Jump(inst)
            }
            0x6 => {
                let is_imm = (op & 0x1) != 0;
                let is_out = (op & 0x2) != 0;

                let offset = if is_imm {
                    AluSource::Immediate
                } else {
                    AluSource::Register(reg3)
                };

                if is_out {
                    let inst = OutInstruction {
                        base: AluSource::Register(reg1),
                        offset,
                        src: reg2,
                    };

                    Self::Out(inst)
                } else {
                    let inst = InInstruction {
                        base: AluSource::Register(reg2),
                        offset,
                        dst: reg1,
                    };

                    Self::In(inst)
                }
            }
            0x7 => {
                if (op & 0x1) == 0 {
                    Self::Sys
                } else {
                    Self::Clrk
                }
            },
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ClockResult {
    Continue,
    Break,
    Halt,
    Error,
}

pub struct Cpu {
    pc: Wrapping<Word>,
    regs: [Word; REG_COUNT],

    c: Flag,
    z: Flag,
    s: Flag,
    o: Flag,
    k: Flag,

    inst: Instruction,
    imm: Word,
}
impl Cpu {
    pub const fn new() -> Self {
        Self {
            pc: Wrapping(0),
            regs: [0; REG_COUNT],
            c: false,
            z: false,
            s: false,
            o: false,
            k: true,
            inst: Instruction::Nop,
            imm: 0,
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub fn pc(&self) -> Word {
        self.pc.0
    }

    #[allow(dead_code)]
    #[inline]
    pub fn ra(&self) -> Word {
        self.regs[1]
    }

    #[allow(dead_code)]
    #[inline]
    pub fn bp(&self) -> Word {
        self.regs[2]
    }

    #[allow(dead_code)]
    #[inline]
    pub fn sp(&self) -> Word {
        self.regs[3]
    }

    pub fn reset(&mut self) {
        self.pc = Wrapping(0);
        self.regs = [0; REG_COUNT];
        self.c = false;
        self.z = false;
        self.s = false;
        self.o = false;
        self.k = true;
        self.inst = Instruction::Nop;
        self.imm = 0;
    }

    #[inline]
    const fn read_reg(&self, reg: Register) -> Word {
        self.regs[reg as usize]
    }

    #[inline]
    fn write_reg(&mut self, reg: Register, value: Word) {
        if reg > 0 {
            self.regs[reg as usize] = value;
        }
    }

    fn read_source(&self, src: AluSource) -> Word {
        match src {
            AluSource::Register(reg) => self.read_reg(reg),
            AluSource::Immediate => self.imm,
            AluSource::ProgramCounter => self.pc.0,
        }
    }

    fn exec_alu(&mut self, op: AluOperation, lhs: AluSource, rhs: AluSource, set_flags: bool) -> Word {
        let lhs_val = self.read_source(lhs);
        let rhs_val = self.read_source(rhs);

        let mut c: Flag = self.c;
        let mut z: Flag = self.z;
        let mut s: Flag = self.s;
        let mut o: Flag = self.o;

        macro_rules! set_flags {
            ($lhs:expr, $rhs:expr, $result: ident) => {
                let $result = $result as Word;
                z = $result == 0;
                s = ($result >> 31) != 0;

                let lhs_s = ($lhs >> 31) != 0;
                let rhs_s = ($rhs >> 31) != 0;
                o = (lhs_s == rhs_s) && (s != lhs_s);
            };
        }

        let result = match op {
            AluOperation::Add => {
                let result = (lhs_val as DWord) + (rhs_val as DWord) + 0;
                c = (result >> 32) != 0;
                set_flags!(lhs_val, rhs_val, result);
                result
            }
            AluOperation::Addc => {
                let cv = if c { 1 } else { 0 };
                let result = (lhs_val as DWord) + (rhs_val as DWord) + cv;
                c = (result >> 32) != 0;
                set_flags!(lhs_val, rhs_val, result);
                result
            }
            AluOperation::Sub => {
                let rhs_val = !rhs_val;

                let result = (lhs_val as DWord) + (rhs_val as DWord) + 1;
                c = (result >> 32) != 0;
                set_flags!(lhs_val, rhs_val, result);
                result
            }
            AluOperation::Subb => {
                let rhs_val = !rhs_val;

                let cv = if c { 1 } else { 0 };
                let result = (lhs_val as DWord) + (rhs_val as DWord) + cv;
                c = (result >> 32) != 0;
                set_flags!(lhs_val, rhs_val, result);
                result
            }
            AluOperation::And => {
                let result = lhs_val & rhs_val;
                z = result == 0;
                result
            }
            AluOperation::Or => {
                let result = lhs_val | rhs_val;
                z = result == 0;
                result
            }
            AluOperation::Xor => {
                let result = lhs_val ^ rhs_val;
                z = result == 0;
                result
            }
            AluOperation::Shl => {
                let result = lhs_val << rhs_val;
                z = result == 0;
                result
            }
            AluOperation::Asr => {
                let result = ((lhs_val as SWord) >> rhs_val) as Word;
                z = result == 0;
                result
            }
            AluOperation::Lsr => {
                let result = lhs_val >> rhs_val;
                z = result == 0;
                result
            }
            AluOperation::Mull => todo!(),
            AluOperation::Mulh => todo!(),
            AluOperation::Smull => todo!(),
            AluOperation::Smulh => todo!(),
            AluOperation::None => 0,
        };

        if set_flags {
            self.c = c;
            self.z = z;
            self.s = s;
            self.o = o;
        }

        result
    }

    fn jump(&mut self, dst: Word, con: JumpCondition) {
        let do_jump = match con {
            JumpCondition::Never => false,
            JumpCondition::Carry => self.c,
            JumpCondition::Zero => self.z,
            JumpCondition::Sign => self.s,
            JumpCondition::Overflow => self.o,
            JumpCondition::NotCarry => !self.c,
            JumpCondition::NotZero => !self.z,
            JumpCondition::NotSign => !self.s,
            JumpCondition::NotOverflow => !self.o,
            JumpCondition::UnsignedLessEqual => !self.c || self.z,
            JumpCondition::UnsignedGreater => self.c && !self.z,
            JumpCondition::SignedLess => self.s != self.o,
            JumpCondition::SignedGreaterEqual => self.s == self.o,
            JumpCondition::SignedLessEqual => (self.s != self.o) || self.z,
            JumpCondition::SignedGreater => (self.s == self.o) && !self.z,
            JumpCondition::Always => true,
        };

        if do_jump {
            self.pc = Wrapping(dst);
        }
    }

    pub fn clock(&mut self, mem: &mut MemoryBus, io: &mut IoBus) -> ClockResult {
        let inst = mem.read32(self.pc.0 as usize);
        self.inst = Instruction::decode(inst);
        self.pc += Wrapping(4);

        self.imm = if (inst & 0x80000000) != 0 {
            let ui = mem.read32(self.pc.0 as usize);
            self.pc += Wrapping(4);

            (ui << 14) | ((inst >> 17) & 0x00003FFF)
        } else {
            (((inst << 1) as SWord) >> 18) as Word
        };

        match self.inst {
            Instruction::Nop => ClockResult::Continue,
            Instruction::Brk => ClockResult::Break,
            Instruction::Hlt => ClockResult::Halt,
            Instruction::Err => ClockResult::Error,
            Instruction::Alu(inst) => {
                let result = self.exec_alu(inst.op, inst.lhs, inst.rhs, true);
                self.write_reg(inst.dst, result);

                ClockResult::Continue
            }
            Instruction::Load(inst) => {
                let addr = self.exec_alu(AluOperation::Addr, inst.base, inst.offset, false) as usize;
                let value = if self.k || (addr >= 0x01000000) {
                    match inst.mode {
                        MemoryMode::Bits32 => mem.read32(addr) as Word,
                        MemoryMode::Bits16 => {
                            if inst.signed {
                                let sval = mem.read16(addr) as i16;
                                (sval as SWord) as Word
                            } else {
                                mem.read16(addr) as Word
                            }
                        }
                        MemoryMode::Bits8 => {
                            if inst.signed {
                                let sval = mem.read8(addr) as i8;
                                (sval as SWord) as Word
                            } else {
                                mem.read8(addr) as Word
                            }
                        }
                    }
                } else {
                    0
                };

                self.write_reg(inst.dst, value);

                ClockResult::Continue
            }
            Instruction::Store(inst) => {
                let addr = self.exec_alu(AluOperation::Addr, inst.base, inst.offset, false) as usize;
                if self.k || (addr >= 0x01000000) {
                    let value = self.read_reg(inst.src);
                    match inst.mode {
                        MemoryMode::Bits32 => mem.write32(addr, value as u32),
                        MemoryMode::Bits16 => mem.write16(addr, value as u16),
                        MemoryMode::Bits8 => mem.write8(addr, value as u8),
                    }
                }

                ClockResult::Continue
            }
            Instruction::Jump(inst) => {
                let mut addr = self.exec_alu(AluOperation::Addr, inst.base, inst.offset, false);
                if inst.indirect {
                    addr = mem.read32(addr as usize) as Word
                }
                self.jump(addr, inst.con);

                ClockResult::Continue
            }
            Instruction::In(inst) => {
                let value = if self.k {
                    let addr = self.exec_alu(AluOperation::Addr, inst.base, inst.offset, false) as usize;
                    io.read(addr)
                } else {
                    0
                };

                self.write_reg(inst.dst, value);

                ClockResult::Continue
            }
            Instruction::Out(inst) => {
                if self.k {
                    let addr = self.exec_alu(AluOperation::Addr, inst.base, inst.offset, false) as usize;
                    let value = self.read_reg(inst.src);
                    io.write(addr, value);
                }

                ClockResult::Continue
            }
            Instruction::Sys => {
                self.k = true;
                self.pc = Wrapping(SYSCALL_ADDRESS);

                ClockResult::Continue
            }
            Instruction::Clrk => {
                self.k = false;

                ClockResult::Continue
            }
        }
    }
}
impl Display for Cpu {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let c_val = if self.c { 1 } else { 0 };
        let z_val = if self.z { 1 } else { 0 };
        let s_val = if self.s { 1 } else { 0 };
        let o_val = if self.o { 1 } else { 0 };
        let k_val = if self.k { 1 } else { 0 };

        writeln!(f, "PC: {:0>8X}", self.pc.0)?;
        writeln!(f)?;

        writeln!(f, "C Z S O K")?;
        writeln!(f, "{} {} {} {} {}", c_val, z_val, s_val, o_val, k_val)?;
        writeln!(f)?;

        #[rustfmt::skip]
        const REG_NAMES: [&str; REG_COUNT - 1] = [
            "ra", "bp", "sp",
            "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
            "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "t8", "t9",
            "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9",
        ];

        for i in 0..(REG_COUNT - 1) {
            let base_name = format!("R{}", i + 1);
            let abi_name = format!("({})", REG_NAMES[i]);

            writeln!(
                f,
                "{:<3} {:<4}: {:0>8X}",
                base_name, abi_name, self.regs[i]
            )?;
        }

        Ok(())
    }
}
