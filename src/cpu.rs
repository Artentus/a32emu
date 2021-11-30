use crate::device::{IoBus, MemoryBus};
use crate::{DWord, SWord, Word};
use std::fmt::Display;
use std::num::Wrapping;

const REG_COUNT: usize = 32;

type Register = u8;
type Flag = bool;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AluOperation {
    Addr,
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
    None,
}
impl AluOperation {
    fn decode(op: Word) -> Self {
        match op & 0xF {
            0x0 => Self::Addr,
            0x1 => Self::Add,
            0x2 => Self::Addc,
            0x3 => Self::Sub,
            0x4 => Self::Subb,
            0x5 => Self::And,
            0x6 => Self::Or,
            0x7 => Self::Xor,
            0x8 => Self::Shl,
            0x9 => Self::Asr,
            0xA => Self::Lsr,
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
    Alu(AluInstruction),
    Load(LoadInstruction),
    Store(StoreInstruction),
    Jump(JumpInstruction),
    In(InInstruction),
    Out(OutInstruction),
}
impl Instruction {
    fn decode(code: Word) -> Self {
        let grp = code & 0x7;
        let op = (code >> 3) & 0xF;
        let reg1 = ((code >> 7) & 0x1F) as Register;
        let reg2 = ((code >> 12) & 0x1F) as Register;
        let reg3 = ((code >> 17) & 0x1F) as Register;

        match grp {
            0x0 => {
                if (op & 0x1) == 0 {
                    Self::Nop
                } else {
                    Self::Brk
                }
            }
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
                let inst = AluInstruction {
                    op: AluOperation::decode(op),
                    lhs: AluSource::Immediate,
                    rhs: AluSource::Register(reg2),
                    dst: reg1,
                };

                Self::Alu(inst)
            }
            0x4 => {
                let is_imm = (op & 0x1) != 0;
                let is_st = (op & 0x2) != 0;

                let mode = match op >> 2 {
                    0x0 => MemoryMode::Bits32,
                    0x1 => MemoryMode::Bits8,
                    0x2 => MemoryMode::Bits16,
                    0x3 => return Self::Nop,
                    _ => unreachable!(),
                };

                let offset = if is_imm {
                    AluSource::Immediate
                } else {
                    AluSource::Register(reg3)
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
                        base: AluSource::Register(reg2),
                        offset,
                        dst: reg1,
                    };

                    Self::Load(inst)
                }
            }
            0x5 => {
                let jump_kind = reg1;

                if (jump_kind & 0x4) == 0 {
                    match jump_kind & 0x3 {
                        0x0 => {
                            let inst = JumpInstruction {
                                con: JumpCondition::decode(op),
                                base: AluSource::Register(reg2),
                                offset: AluSource::Register(reg3),
                                indirect: false,
                            };

                            Self::Jump(inst)
                        }
                        0x1 => {
                            let inst = JumpInstruction {
                                con: JumpCondition::decode(op),
                                base: AluSource::Register(reg2),
                                offset: AluSource::Register(reg3),
                                indirect: true,
                            };

                            Self::Jump(inst)
                        }
                        0x2 => {
                            let inst = JumpInstruction {
                                con: JumpCondition::decode(op),
                                base: AluSource::Register(reg2),
                                offset: AluSource::Immediate,
                                indirect: false,
                            };

                            Self::Jump(inst)
                        }
                        0x3 => {
                            let inst = JumpInstruction {
                                con: JumpCondition::decode(op),
                                base: AluSource::Register(reg2),
                                offset: AluSource::Immediate,
                                indirect: true,
                            };

                            Self::Jump(inst)
                        }
                        _ => unreachable!(),
                    }
                } else {
                    let inst = JumpInstruction {
                        con: JumpCondition::decode(op),
                        base: AluSource::ProgramCounter,
                        offset: AluSource::Immediate,
                        indirect: false,
                    };

                    Self::Jump(inst)
                }
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
            0x7 => Self::Nop,
            _ => unreachable!(),
        }
    }
}

pub struct Cpu {
    pc: Wrapping<Word>,
    regs: [Word; REG_COUNT],

    c: Flag,
    z: Flag,
    s: Flag,
    o: Flag,

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

    fn exec_alu(&mut self, op: AluOperation, lhs: AluSource, rhs: AluSource) -> Word {
        let lhs_val = self.read_source(lhs);
        let rhs_val = self.read_source(rhs);

        macro_rules! set_flags {
            ($result: ident) => {
                let $result = $result as Word;
                self.z = $result == 0;
                self.s = ($result >> 31) != 0;

                let lhs_s = (lhs_val >> 31) != 0;
                let rhs_s = (rhs_val >> 31) != 0;
                self.o = (lhs_s == rhs_s) && (self.s != lhs_s);
            };
        }

        match op {
            AluOperation::Addr => (Wrapping(lhs_val) + Wrapping(rhs_val)).0,
            AluOperation::Add => {
                let result = (lhs_val as DWord) + (rhs_val as DWord) + 0;
                self.c = (result >> 32) != 0;
                set_flags!(result);
                result
            }
            AluOperation::Addc => {
                let c = if self.c { 1 } else { 0 };
                let result = (lhs_val as DWord) + (rhs_val as DWord) + c;
                self.c = (result >> 32) != 0;
                set_flags!(result);
                result
            }
            AluOperation::Sub => {
                let result = (lhs_val as DWord) + ((!rhs_val) as DWord) + 1;
                self.c = (result >> 32) != 0;
                set_flags!(result);
                result
            }
            AluOperation::Subb => {
                let c = if self.c { 1 } else { 0 };
                let result = (lhs_val as DWord) + ((!rhs_val) as DWord) + c;
                self.c = (result >> 32) != 0;
                set_flags!(result);
                result
            }
            AluOperation::And => {
                let result = lhs_val & rhs_val;
                self.z = result == 0;
                result
            }
            AluOperation::Or => {
                let result = lhs_val | rhs_val;
                self.z = result == 0;
                result
            }
            AluOperation::Xor => {
                let result = lhs_val ^ rhs_val;
                self.z = result == 0;
                result
            }
            AluOperation::Shl => {
                let result = lhs_val << rhs_val;
                self.z = result == 0;
                result
            }
            AluOperation::Asr => {
                let result = ((lhs_val as SWord) >> rhs_val) as Word;
                self.z = result == 0;
                result
            }
            AluOperation::Lsr => {
                let result = lhs_val >> rhs_val;
                self.z = result == 0;
                result
            }
            AluOperation::None => 0,
        }
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

    pub fn clock(&mut self, mem: &mut MemoryBus, io: &mut IoBus) -> bool {
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
            Instruction::Nop => false,
            Instruction::Brk => true,
            Instruction::Alu(inst) => {
                let result = self.exec_alu(inst.op, inst.lhs, inst.rhs);
                self.write_reg(inst.dst, result);

                false
            }
            Instruction::Load(inst) => {
                let addr = self.exec_alu(AluOperation::Addr, inst.base, inst.offset) as usize;
                let value = match inst.mode {
                    MemoryMode::Bits32 => mem.read32(addr) as Word,
                    MemoryMode::Bits16 => mem.read16(addr) as Word,
                    MemoryMode::Bits8 => mem.read8(addr) as Word,
                };
                self.write_reg(inst.dst, value);

                false
            }
            Instruction::Store(inst) => {
                let addr = self.exec_alu(AluOperation::Addr, inst.base, inst.offset) as usize;
                let value = self.read_reg(inst.src);
                match inst.mode {
                    MemoryMode::Bits32 => mem.write32(addr, value as u32),
                    MemoryMode::Bits16 => mem.write16(addr, value as u16),
                    MemoryMode::Bits8 => mem.write8(addr, value as u8),
                }

                false
            }
            Instruction::Jump(inst) => {
                let mut addr = self.exec_alu(AluOperation::Addr, inst.base, inst.offset);
                if inst.indirect {
                    addr = mem.read32(addr as usize) as Word
                }
                self.jump(addr, inst.con);

                false
            }
            Instruction::In(inst) => {
                let addr = self.exec_alu(AluOperation::Addr, inst.base, inst.offset) as usize;
                let value = io.read(addr);
                self.write_reg(inst.dst, value);

                false
            }
            Instruction::Out(inst) => {
                let addr = self.exec_alu(AluOperation::Addr, inst.base, inst.offset) as usize;
                let value = self.read_reg(inst.src);
                io.write(addr, value);

                false
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

        writeln!(f, "PC: 0x{:0>8X}", self.pc.0)?;
        writeln!(f)?;

        writeln!(f, "C Z S O")?;
        writeln!(f, "{} {} {} {}", c_val, z_val, s_val, o_val)?;
        writeln!(f)?;

        for i in 0..REG_COUNT {
            if i > 9 {
                writeln!(f, "R{}:  0x{:0>8X}", i, self.regs[i])?;
            } else {
                writeln!(f, "R{}:   0x{:0>8X}", i, self.regs[i])?;
            }
        }

        Ok(())
    }
}
