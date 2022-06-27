use crate::device::{IoBus, MemoryBus};
use crate::{SWord, Word};
use std::fmt::Display;

const REG_COUNT: usize = 32;
const SYSCALL_ADDRESS: Word = 0x00_007FF0;

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
    Lsr,
    Asr,
    Mul,
    Mulhuu,
    Mulhss,
    Mulhsu,
    Csub,
    Slc,
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
            0x8 => Self::Lsr,
            0x9 => Self::Asr,
            0xA => Self::Mul,
            0xB => Self::Mulhuu,
            0xC => Self::Mulhss,
            0xD => Self::Mulhsu,
            0xE => Self::Csub,
            0xF => Self::Slc,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum AluRhs {
    Register(Register),
    Immediate,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum MemoryMode {
    Bits32,
    Bits16,
    Bits8,
    Io,
}
impl MemoryMode {
    fn decode(op: Word) -> Self {
        match op & 0x3 {
            0x0 => Self::Bits32,
            0x1 => Self::Bits8,
            0x2 => Self::Bits16,
            0x3 => Self::Io,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum BranchCondition {
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
impl BranchCondition {
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
enum UpperImmediateKind {
    Load,
    AddPc,
}
impl UpperImmediateKind {
    fn decode(op: Word) -> Self {
        match op & 0x1 {
            0x0 => Self::Load,
            0x1 => Self::AddPc,
            _ => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum Instruction {
    Nop,
    Brk,
    Hlt,
    Err,
    Alu {
        op: AluOperation,
        lhs: Register,
        rhs: AluRhs,
        dst: Register,
    },
    Load {
        mode: MemoryMode,
        signed: bool,
        base: Register,
        dst: Register,
    },
    Store {
        mode: MemoryMode,
        base: Register,
        src: Register,
    },
    Jump {
        base: Register,
        indirect: bool,
    },
    Branch {
        condition: BranchCondition,
    },
    UpperImmediate {
        kind: UpperImmediateKind,
        dst: Register,
    },
    Sys,
    Clrk,
}
impl Instruction {
    fn decode(code: Word) -> Self {
        const GRP_MISC: Word = 0b000;
        const GRP_ALU_REG: Word = 0b001;
        const GRP_ALU_IMM: Word = 0b010;
        const GRP_LD_ST: Word = 0b011;
        const GRP_JMP: Word = 0b100;
        const GRP_BRA: Word = 0b101;
        const GRP_UI: Word = 0b110;
        const GRP_SYS: Word = 0b111;

        let grp = code & 0x7;
        let op = (code >> 3) & 0xF;
        let rd = ((code >> 7) & 0x1F) as Register;
        let rs1 = ((code >> 12) & 0x1F) as Register;
        let rs2 = ((code >> 17) & 0x1F) as Register;

        match grp {
            GRP_MISC => {
                const OP_NOP: Word = 0x0;
                const OP_BRK: Word = 0x1;
                const OP_HLT: Word = 0x2;
                const OP_ERR: Word = 0x3;

                match op & 0x3 {
                    OP_NOP => Self::Nop,
                    OP_BRK => Self::Brk,
                    OP_HLT => Self::Hlt,
                    OP_ERR => Self::Err,
                    _ => unreachable!(),
                }
            }
            GRP_ALU_REG => Self::Alu {
                op: AluOperation::decode(op),
                lhs: rs1,
                rhs: AluRhs::Register(rs2),
                dst: rd,
            },
            GRP_ALU_IMM => Self::Alu {
                op: AluOperation::decode(op),
                lhs: rs1,
                rhs: AluRhs::Immediate,
                dst: rd,
            },
            GRP_LD_ST => {
                let mode = MemoryMode::decode(op);

                if (op & 0x8) == 0 {
                    Self::Load {
                        mode,
                        signed: (op & 0x4) != 0,
                        base: rs1,
                        dst: rd,
                    }
                } else {
                    Self::Store {
                        mode,
                        base: rd,
                        src: rs1,
                    }
                }
            }
            GRP_JMP => Self::Jump {
                base: rs1,
                indirect: (op & 0x1) != 0,
            },
            GRP_BRA => Self::Branch {
                condition: BranchCondition::decode(op),
            },
            GRP_UI => Self::UpperImmediate {
                kind: UpperImmediateKind::decode(op),
                dst: rd,
            },
            GRP_SYS => {
                const OP_SYS: Word = 0x0;
                const OP_CLRK: Word = 0x1;

                match op & 0x1 {
                    OP_SYS => Self::Sys,
                    OP_CLRK => Self::Clrk,
                    _ => unreachable!(),
                }
            }
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
    pc: Word,
    regs: [Word; REG_COUNT],

    c: Flag,
    z: Flag,
    s: Flag,
    o: Flag,
    k: Flag,

    inst: Instruction,
    imm15: Word,
    imm22: Word,
    u_imm: Word,
}
impl Cpu {
    pub const fn new() -> Self {
        Self {
            pc: 0,
            regs: [0; REG_COUNT],
            c: false,
            z: false,
            s: false,
            o: false,
            k: true,
            inst: Instruction::Nop,
            imm15: 0,
            imm22: 0,
            u_imm: 0,
        }
    }

    #[allow(dead_code)]
    #[inline]
    pub fn pc(&self) -> Word {
        self.pc
    }

    #[allow(dead_code)]
    #[inline]
    pub fn ra(&self) -> Word {
        self.regs[1]
    }

    #[allow(dead_code)]
    #[inline]
    pub fn sp(&self) -> Word {
        self.regs[2]
    }

    #[allow(dead_code)]
    #[inline]
    pub fn k(&self) -> Flag {
        self.k
    }

    pub fn reset(&mut self) {
        self.pc = 0;
        self.regs = [0; REG_COUNT];
        self.c = false;
        self.z = false;
        self.s = false;
        self.o = false;
        self.k = true;
        self.inst = Instruction::Nop;
        self.imm15 = 0;
        self.imm22 = 0;
        self.u_imm = 0;
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

    fn read_rhs(&self, rhs: AluRhs) -> Word {
        match rhs {
            AluRhs::Register(reg) => self.read_reg(reg),
            AluRhs::Immediate => self.imm15,
        }
    }

    fn exec_alu(&mut self, op: AluOperation, lhs: Register, rhs: AluRhs) -> Word {
        let lhs_val = self.read_reg(lhs);
        let rhs_val = self.read_rhs(rhs);

        let mut c: Flag = self.c;
        let z: Flag;
        let mut s: Flag = self.s;
        let mut o: Flag = self.o;

        macro_rules! set_flags {
            ($lhs:expr, $rhs:expr, $result: ident) => {
                z = $result == 0;
                s = ($result >> 31) != 0;

                let lhs_s = ($lhs >> 31) != 0;
                let rhs_s = ($rhs >> 31) != 0;
                o = (lhs_s == rhs_s) && (s != lhs_s);
            };
        }

        let result = match op {
            AluOperation::Add => {
                let (result, c_out) = lhs_val.carrying_add(rhs_val, false);
                c = c_out;
                set_flags!(lhs_val, rhs_val, result);
                result
            }
            AluOperation::Addc => {
                let (result, c_out) = lhs_val.carrying_add(rhs_val, c);
                c = c_out;
                set_flags!(lhs_val, rhs_val, result);
                result
            }
            AluOperation::Sub => {
                let rhs_val = !rhs_val;
                let (result, c_out) = lhs_val.carrying_add(rhs_val, true);
                c = c_out;
                set_flags!(lhs_val, rhs_val, result);
                result
            }
            AluOperation::Subb => {
                let rhs_val = !rhs_val;
                let (result, c_out) = lhs_val.carrying_add(rhs_val, c);
                c = c_out;
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
            AluOperation::Lsr => {
                let result = lhs_val >> rhs_val;
                z = result == 0;
                result
            }
            AluOperation::Asr => {
                let result = ((lhs_val as SWord) >> rhs_val) as Word;
                z = result == 0;
                result
            }
            AluOperation::Mul => {
                let result = lhs_val.widening_mul(rhs_val).0;
                z = result == 0;
                result
            }
            AluOperation::Mulhuu => {
                let result = lhs_val.widening_mul(rhs_val).1;
                z = result == 0;
                result
            }
            AluOperation::Mulhss => {
                let full = ((lhs_val as SWord) as i64) * ((rhs_val as SWord) as i64);
                let result = (full >> 32) as Word;
                z = result == 0;
                result
            }
            AluOperation::Mulhsu => {
                let full = ((lhs_val as SWord) as i64) * (rhs_val as i64);
                let result = (full >> 32) as Word;
                z = result == 0;
                result
            }
            AluOperation::Csub => {
                if lhs_val >= rhs_val {
                    let result = lhs_val - rhs_val;
                    c = true;
                    z = result == 0;
                    result
                } else {
                    c = false;
                    z = lhs_val == 0;
                    lhs_val
                }
            }
            AluOperation::Slc => {
                let c_in = if c { 1 } else { 0 };
                let result = (lhs_val << 1) | c_in;
                c = (lhs_val >> 31) != 0;
                z = result == 0;
                result
            }
        };

        self.c = c;
        self.z = z;
        self.s = s;
        self.o = o;

        result
    }

    fn do_branch(&mut self, condition: BranchCondition) -> bool {
        match condition {
            BranchCondition::Never => false,
            BranchCondition::Carry => self.c,
            BranchCondition::Zero => self.z,
            BranchCondition::Sign => self.s,
            BranchCondition::Overflow => self.o,
            BranchCondition::NotCarry => !self.c,
            BranchCondition::NotZero => !self.z,
            BranchCondition::NotSign => !self.s,
            BranchCondition::NotOverflow => !self.o,
            BranchCondition::UnsignedLessEqual => !self.c || self.z,
            BranchCondition::UnsignedGreater => self.c && !self.z,
            BranchCondition::SignedLess => self.s != self.o,
            BranchCondition::SignedGreaterEqual => self.s == self.o,
            BranchCondition::SignedLessEqual => (self.s != self.o) || self.z,
            BranchCondition::SignedGreater => (self.s == self.o) && !self.z,
            BranchCondition::Always => true,
        }
    }

    pub fn clock(&mut self, mem: &mut MemoryBus, io: &mut IoBus) -> ClockResult {
        const PC_INC: Word = std::mem::size_of::<Word>() as Word;

        let inst = mem.read32(self.pc as usize, self.k);
        self.inst = Instruction::decode(inst);

        self.imm15 = ((inst as SWord) >> 17) as Word;
        self.imm22 = ((((inst & 0x8000_0000) as SWord) >> 10) as Word)
            | ((inst & 0x7FF8_0000) >> 17)
            | ((inst & 0x0007_F000) << 2);
        self.u_imm =
            (inst & 0x8000_0000) | ((inst & 0x6000_0000) >> 17) | ((inst & 0x1FFF_F000) << 2);

        match self.inst {
            Instruction::Nop => {
                self.pc = self.pc.wrapping_add(PC_INC);
                ClockResult::Continue
            }
            Instruction::Brk => {
                self.pc = self.pc.wrapping_add(PC_INC);
                ClockResult::Break
            }
            Instruction::Hlt => {
                self.pc = self.pc.wrapping_add(PC_INC);
                ClockResult::Halt
            }
            Instruction::Err => {
                self.pc = self.pc.wrapping_add(PC_INC);
                ClockResult::Error
            }
            Instruction::Alu { op, lhs, rhs, dst } => {
                let result = self.exec_alu(op, lhs, rhs);
                self.write_reg(dst, result);

                self.pc = self.pc.wrapping_add(PC_INC);
                ClockResult::Continue
            }
            Instruction::Load {
                mode,
                signed,
                base,
                dst,
            } => {
                let addr = self.read_reg(base).wrapping_add(self.imm15) as usize;

                let value = match mode {
                    MemoryMode::Bits32 => mem.read32(addr, self.k) as Word,
                    MemoryMode::Bits16 => {
                        if signed {
                            let sval = mem.read16(addr, self.k) as i16;
                            (sval as SWord) as Word
                        } else {
                            mem.read16(addr, self.k) as Word
                        }
                    }
                    MemoryMode::Bits8 => {
                        if signed {
                            let sval = mem.read8(addr, self.k) as i8;
                            (sval as SWord) as Word
                        } else {
                            mem.read8(addr, self.k) as Word
                        }
                    }
                    MemoryMode::Io => io.read(addr, self.k),
                };

                self.write_reg(dst, value);

                self.pc = self.pc.wrapping_add(PC_INC);
                ClockResult::Continue
            }
            Instruction::Store { mode, base, src } => {
                let addr = self.read_reg(base).wrapping_add(self.imm15) as usize;
                let value = self.read_reg(src);

                match mode {
                    MemoryMode::Bits32 => mem.write32(addr, value as u32, self.k),
                    MemoryMode::Bits16 => mem.write16(addr, value as u16, self.k),
                    MemoryMode::Bits8 => mem.write8(addr, value as u8, self.k),
                    MemoryMode::Io => io.write(addr, value, self.k),
                }

                self.pc = self.pc.wrapping_add(PC_INC);
                ClockResult::Continue
            }
            Instruction::Jump { base, indirect } => {
                let mut addr = self.read_reg(base).wrapping_add(self.imm15);
                if indirect {
                    addr = mem.read32(addr as usize, self.k) as Word
                }

                self.pc = addr & 0xFFFF_FFFC;
                ClockResult::Continue
            }
            Instruction::Branch { condition } => {
                if self.do_branch(condition) {
                    let addr = self.pc.wrapping_add(self.imm22);
                    self.pc = addr & 0xFFFF_FFFC;
                } else {
                    self.pc = self.pc.wrapping_add(PC_INC);
                }

                ClockResult::Continue
            }
            Instruction::UpperImmediate { kind, dst } => {
                let value = match kind {
                    UpperImmediateKind::Load => self.u_imm,
                    UpperImmediateKind::AddPc => self.pc.wrapping_add(self.u_imm),
                };

                self.write_reg(dst, value);

                self.pc = self.pc.wrapping_add(PC_INC);
                ClockResult::Continue
            }
            Instruction::Sys => {
                self.k = true;

                self.pc = SYSCALL_ADDRESS;
                ClockResult::Continue
            }
            Instruction::Clrk => {
                self.k = false;

                self.pc = self.pc.wrapping_add(PC_INC);
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

        writeln!(f, "PC: {:0>8X}", self.pc)?;
        writeln!(f)?;

        writeln!(f, "C Z S O K")?;
        writeln!(f, "{} {} {} {} {}", c_val, z_val, s_val, o_val, k_val)?;
        writeln!(f)?;

        #[rustfmt::skip]
        const REG_NAMES: [&str; REG_COUNT - 1] = [
            "ra", "sp",
            "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
            "t0", "t1", "t2", "t3", "t4", "t5", "t6", "t7",
            "s0", "s1", "s2", "s3", "s4", "s5", "s6", "s7", "s8", "s9", "s10", "s11", "s12",
        ];

        for i in 0..(REG_COUNT - 1) {
            let base_name = format!("R{}", i + 1);
            let abi_name = format!("({})", REG_NAMES[i]);

            writeln!(f, "{:<3} {:<4}: {:0>8X}", base_name, abi_name, self.regs[i])?;
        }

        Ok(())
    }
}
