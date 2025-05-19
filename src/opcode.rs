
use serde::{Serialize, Deserialize};
use std::fmt::Display;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum VMError {
    ProcessEnd,
    CatchFire,
    Explosion,
    TempestH,
    IceOver,
    RockStone,
    Zap,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum RegIndex {
    C0 = 0, C1 = 1, C2 = 2, C3 = 3,
    C4 = 4, C5 = 5, C6 = 6, C7 = 7,
    R0 = 8, R1 = 9, R2 = 10, R3 = 11,
    R4 = 12, R5 = 13, R6 = 14, Ri = 15,
}
impl RegIndex {
    pub fn is_c_reg(&self) -> bool {
        match self {
            Self::C0 => true, Self::C1 => true, Self::C2 => true, Self::C3 => true,
            Self::C4 => true, Self::C5 => true, Self::C6 => true, Self::C7 => true,
            Self::R0 => false, Self::R1 => false, Self::R2 => false, Self::R3 => false,
            Self::R4 => false, Self::R5 => false, Self::R6 => false, Self::Ri => false,
        }
    }
    pub fn is_r_reg(&self) -> bool {
        match self {
            Self::C0 => false, Self::C1 => false, Self::C2 => false, Self::C3 => false,
            Self::C4 => false, Self::C5 => false, Self::C6 => false, Self::C7 => false,
            Self::R0 => true, Self::R1 => true, Self::R2 => true, Self::R3 => true,
            Self::R4 => true, Self::R5 => true, Self::R6 => true, Self::Ri => true,
        }
    }
}
impl std::fmt::Display for RegIndex {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::C0 => f.write_str("c0"),
            Self::C1 => f.write_str("c1"),
            Self::C2 => f.write_str("c2"),
            Self::C3 => f.write_str("c3"),
            Self::C4 => f.write_str("c4"),
            Self::C5 => f.write_str("c5"),
            Self::C6 => f.write_str("c6"),
            Self::C7 => f.write_str("c7"),
            Self::R0 => f.write_str("r0"),
            Self::R1 => f.write_str("r1"),
            Self::R2 => f.write_str("r2"),
            Self::R3 => f.write_str("r3"),
            Self::R4 => f.write_str("r4"),
            Self::R5 => f.write_str("r5"),
            Self::R6 => f.write_str("r6"),
            Self::Ri => f.write_str("ri"),
        }
    }
}
impl From<u8> for RegIndex {
    fn from(value: u8) -> Self {
        match value & 15 {
            0 => Self::C0, 1 => Self::C1, 2 => Self::C2, 3 => Self::C3,
            4 => Self::C4, 5 => Self::C5, 6 => Self::C6, 7 => Self::C7,
            8 => Self::R0, 9 => Self::R1, 10 => Self::R2, 11 => Self::R3,
            12 => Self::R4, 13 => Self::R5, 14 => Self::R6, 15 => Self::Ri,
            _ => unreachable!()
        }
    }
}
impl From<u16> for RegIndex {
    fn from(value: u16) -> Self { RegIndex::from(value as u8) }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Param {
    Lit(u8),
    RegH(RegIndex),
    RegL(RegIndex),
    Reg(RegIndex),
}
impl Display for Param {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Lit(v) => {
                write!(f, "#{}", v)
            }
            Self::RegH(r) => {
                write!(f, "{:?}H", r)
            }
            Self::RegL(r) => {
                write!(f, "{:?}L", r)
            }
            Self::Reg(r) => {
                write!(f, "{:?}", r)
            }
        }
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Swizzle {
    X, Y, Z, W
}
impl From<u8> for Swizzle {
    fn from(value: u8) -> Self {
        match value & 3 {
            0 => Self::X, 1 => Self::Y,
            2 => Self::Z, 3 => Self::W,
            _ => unreachable!()
        }
    }
}
impl Display for Swizzle {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(match self {
            Self::X => "x", Self::Y => "y", Self::Z => "z", Self::W => "w"
        })
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Opcode {
    Invalid,
    // System:
    Halt, Sleep(Param), Nop,
    // Word selection opcodes:
    WMove(RegIndex, RegIndex, Swizzle),
    WSwap(RegIndex, RegIndex, Swizzle),
    WAdd(RegIndex, RegIndex, Swizzle),
    WSub(RegIndex, RegIndex, Swizzle),
    // dear future me: get out while you still can... there is no escape otherwise
    // later meisaka... "oh... that's what you meant... wish you made it more obvious"
    // Extra:
    Extra2, Extra3,
    Move(RegIndex, RegIndex, u8), Swizzle(RegIndex, Swizzle, Swizzle, Swizzle, Swizzle),
    Load(RegIndex, RegIndex, u8), LoadInc(RegIndex, RegIndex, u8),
    Gather(RegIndex, RegIndex, u8), GatherInc(RegIndex, RegIndex, u8),
    Store(RegIndex, RegIndex, u8), StoreInc(RegIndex, RegIndex, u8),
    Scatter(RegIndex, RegIndex, u8), ScatterInc(RegIndex, RegIndex, u8),
    Skip1, Skip2, Skip3, Skip4,
    // psudo-instructions that perform the address increment function without memory access:
    // this is not the same as a standard increment!
    IncA1x1(RegIndex), IncA1x2(RegIndex), IncA1x3(RegIndex), IncA1x4(RegIndex),
    IncA2x1(RegIndex), IncA3x1(RegIndex), IncA4x1(RegIndex),
    // Math8:
    Add8(RegIndex, RegIndex), Sub8(RegIndex, RegIndex), RSub8(RegIndex, RegIndex),
    Eq8(RegIndex, RegIndex),
    Carry8(RegIndex, RegIndex),
    LessU8(RegIndex, RegIndex),
    GreaterU8(RegIndex, RegIndex),
    NotEq8(RegIndex, RegIndex),
    AddSat8(RegIndex, RegIndex), SubSat8(RegIndex, RegIndex), RSubSat8(RegIndex, RegIndex),
    GreaterEqU8(RegIndex, RegIndex),
    AddOver8(RegIndex, RegIndex), SubOver8(RegIndex, RegIndex), RSubOver8(RegIndex, RegIndex),
    LessEqU8(RegIndex, RegIndex),
    // Math16:
    Add16(RegIndex, RegIndex), Sub16(RegIndex, RegIndex), RSub16(RegIndex, RegIndex),
    Eq16(RegIndex, RegIndex),
    Carry16(RegIndex, RegIndex), LessU16(RegIndex, RegIndex), GreaterU16(RegIndex, RegIndex),
    NotEq16(RegIndex, RegIndex),
    AddSat16(RegIndex, RegIndex), SubSat16(RegIndex, RegIndex), RSubSat16(RegIndex, RegIndex),
    GreaterEqU16(RegIndex, RegIndex),
    AddOver16(RegIndex, RegIndex), SubOver16(RegIndex, RegIndex), RSubOver16(RegIndex, RegIndex),
    LessEqU16(RegIndex, RegIndex),
    // Shift8, Shift16,
    LShift8(RegIndex, RegIndex),
    RLogiShift8(RegIndex, RegIndex),
    RArithShift8(RegIndex, RegIndex),
    LRotate8(RegIndex, RegIndex),
    LIShift8(RegIndex, RegIndex),
    RILogiShift8(RegIndex, RegIndex),
    RIArithShift8(RegIndex, RegIndex),
    RRotate8(RegIndex, RegIndex),
    LShiftLit8(u8, RegIndex),
    RLogiShiftLit8(u8, RegIndex),
    RArithShiftLit8(u8, RegIndex),
    RotateLit8(u8, RegIndex),
    LShift16(RegIndex, RegIndex),
    RLogiShift16(RegIndex, RegIndex),
    RArithShift16(RegIndex, RegIndex),
    LRotate16(RegIndex, RegIndex),
    LIShift16(RegIndex, RegIndex),
    RILogiShift16(RegIndex, RegIndex),
    RIArithShift16(RegIndex, RegIndex),
    RRotate16(RegIndex, RegIndex),
    LShiftLit16(u8, RegIndex),
    RLogiShiftLit16(u8, RegIndex),
    RArithShiftLit16(u8, RegIndex),
    RotateLit16(u8, RegIndex),
    // BitOp:
    SetOne(RegIndex), SetAll(RegIndex), Swap(RegIndex, RegIndex),
    NotSrc(RegIndex, RegIndex), NotDest(RegIndex),
    And(RegIndex, RegIndex), Or(RegIndex, RegIndex), Xor(RegIndex, RegIndex),
    Nand(RegIndex, RegIndex), Nor(RegIndex, RegIndex), Xnor(RegIndex, RegIndex),
    AndNot(RegIndex, RegIndex) /*(Dest Clears Src)*/,
    AndNotS(RegIndex, RegIndex) /*(Src Clears Dest)*/,
    OrNot(RegIndex, RegIndex), OrNotS(RegIndex, RegIndex),
    // SpecOp:
    HAdd(RegIndex, RegIndex),
    MultSat(RegIndex, RegIndex),
    MultLow(RegIndex, RegIndex),
    MultHi(RegIndex, RegIndex),
    Divide(RegIndex, RegIndex),
    RecpDivide(RegIndex, RegIndex),
    Extra14, Extra15,
}
impl Opcode {
    pub fn parse(value: u16) -> Self {
        let src_val = ((value >> 8) & 0b1111) as u8;
        let src: RegIndex = (src_val as u8).into();
        let dst_val = ((value >> 12) & 0b1111) as u8;
        let dst = dst_val.into();
        let opt = ((value >> 4) & 0b1111) as u8;
        match value & 15 {
            0 => match opt {
                0 => Self::Halt,
                1 => Self::Sleep(
                    match dst_val & 3 {
                        0 => Param::Lit(src_val),
                        1 => Param::RegL(src),
                        2 => Param::RegH(src),
                        3 => Param::Reg(src),
                        _ => Param::Lit(src_val),
                    }
                ),
                2..=15 => Self::Invalid,
                _ => unreachable!()
            },
            1 => { // WSelect opcodes
                match opt & 3 {
                    0 => Self::WMove(src, dst, (opt >> 2).into()),
                    1 => Self::WSwap(src, dst, (opt >> 2).into()),
                    2 => Self::WAdd(src, dst, (opt >> 2).into()),
                    3 => Self::WSub(src, dst, (opt >> 2).into()),
                    _ => unreachable!()
                }
            }
            2 => Self::Extra2, 3 => Self::Extra3,
            4 => Self::Move(src, dst, opt), 5 => Self::Swizzle(dst,
                opt.into(), (opt >> 2).into(),
                src_val.into(), (src_val >> 2).into()
                ),
            6 => match opt & 3 {
                0 => Self::Load(src, dst, 3u8.wrapping_sub(opt >> 2)),
                1 if (src, dst) == (RegIndex::Ri, RegIndex::C0) => match opt >> 2 {
                    0 => Self::Skip4,
                    1 => Self::Skip3,
                    2 => Self::Skip2,
                    3 => Self::Skip1,
                    _ => unreachable!("skip width"),
                }
                1 if (src.is_r_reg(), dst) == (true, RegIndex::C0) => match opt >> 2 {
                    0 => Self::IncA1x4(src),
                    1 => Self::IncA1x3(src),
                    2 => Self::IncA1x2(src),
                    3 => Self::IncA1x1(src),
                    _ => unreachable!("skip width"),
                }
                1 => Self::LoadInc(src, dst, 3u8.wrapping_sub(opt >> 2)),
                2 => Self::Gather(src, dst, 3u8.wrapping_sub(opt >> 2)),
                3 if (src.is_r_reg(), dst) == (true, RegIndex::C0) => match opt >> 2 {
                    0 => Self::IncA2x1(src),
                    1 => Self::IncA3x1(src),
                    2 => Self::IncA4x1(src),
                    3 => Self::IncA1x1(src),
                    _ => unreachable!("skip width"),
                }
                3 if (src, dst, opt >> 2) == (RegIndex::Ri, RegIndex::C0, 3) => {
                    Self::Skip1
                }
                3 => Self::GatherInc(src, dst, 3u8.wrapping_sub(opt >> 2)),
                _ => unreachable!()
            }
            7 => match opt & 3 {
                0 => Self::Store(src, dst, 3u8.wrapping_sub(opt >> 2)),
                1 => Self::StoreInc(src, dst, 3u8.wrapping_sub(opt >> 2)),
                2 => Self::Scatter(src, dst, 3u8.wrapping_sub(opt >> 2)),
                3 => Self::ScatterInc(src, dst, 3u8.wrapping_sub(opt >> 2)),
                _ => unreachable!()
            }
            8 => /* Math8 */ match opt {
                0x0 => Self::Add8(src, dst),
                0x1 => Self::Sub8(src, dst),
                0x2 => Self::RSub8(src, dst),
                0x3 => Self::Eq8(src, dst),
                0x4 => Self::Carry8(src, dst),
                0x5 => Self::LessU8(src, dst),
                0x6 => Self::GreaterU8(src, dst),
                0x7 => Self::NotEq8(src, dst),
                0x8 => Self::AddSat8(src, dst),
                0x9 => Self::SubSat8(src, dst),
                0xa => Self::RSubSat8(src, dst),
                0xb => Self::GreaterEqU8(src, dst),
                0xc => Self::AddOver8(src, dst),
                0xd => Self::SubOver8(src, dst),
                0xe => Self::RSubOver8(src, dst),
                0xf => Self::LessEqU8(src, dst),
                _ => Self::Invalid
            },
            9 => /* Math16 */ match opt {
                0x0 => Self::Add16(src, dst),
                0x1 => Self::Sub16(src, dst),
                0x2 => Self::RSub16(src, dst),
                0x3 => Self::Eq16(src, dst),
                0x4 => Self::Carry16(src, dst),
                0x5 => Self::LessU16(src, dst),
                0x6 => Self::GreaterU16(src, dst),
                0x7 => Self::NotEq16(src, dst),
                0x8 => Self::AddSat16(src, dst),
                0x9 => Self::SubSat16(src, dst),
                0xa => Self::RSubSat16(src, dst),
                0xb => Self::GreaterEqU16(src, dst),
                0xc => Self::AddOver16(src, dst),
                0xd => Self::SubOver16(src, dst),
                0xe => Self::RSubOver16(src, dst),
                0xf => Self::LessEqU16(src, dst),
                _ => Self::Invalid
            },
            10 => /* Shift8 */ match opt & 15 {
                0b0000 => Self::LShift8(src, dst),
                0b0001 => Self::RLogiShift8(src, dst),
                0b0010 => Self::RArithShift8(src, dst),
                0b0011 => Self::LRotate8(src, dst),
                0b0100 => Self::LIShift8(src, dst),
                0b0101 => Self::RILogiShift8(src, dst),
                0b0110 => Self::RIArithShift8(src, dst),
                0b0111 => Self::RRotate8(src, dst),
                0b1000 => Self::LShiftLit8(src_val, dst),
                0b1001 => Self::RLogiShiftLit8(src_val, dst),
                0b1010 => Self::RArithShiftLit8(src_val, dst),
                0b1011 => Self::RotateLit8(src_val, dst),
                0b1100 => Self::LShiftLit8(8u8.wrapping_sub(src_val) & 7, dst),
                0b1101 => Self::RLogiShiftLit8(8u8.wrapping_sub(src_val) & 7, dst),
                0b1110 => Self::RArithShiftLit8(8u8.wrapping_sub(src_val) & 7, dst),
                0b1111 => Self::RotateLit8(8u8.wrapping_sub(src_val) & 7, dst),
                _ => Self::Invalid
            }
            11 => /* Shift16 */ match opt & 15 {
                0b0000 => Self::LShift16(src, dst),
                0b0001 => Self::RLogiShift16(src, dst),
                0b0010 => Self::RArithShift16(src, dst),
                0b0011 => Self::LRotate16(src, dst),
                0b0100 => Self::LIShift16(src, dst),
                0b0101 => Self::RILogiShift16(src, dst),
                0b0110 => Self::RIArithShift16(src, dst),
                0b0111 => Self::RRotate16(src, dst),
                0b1000 => Self::LShiftLit16(src_val, dst),
                0b1001 => Self::RLogiShiftLit16(src_val, dst),
                0b1010 => Self::RArithShiftLit16(src_val, dst),
                0b1011 => Self::RotateLit16(src_val, dst),
                0b1100 => Self::LShiftLit16(16u8.wrapping_sub(src_val) & 15, dst),
                0b1101 => Self::RLogiShiftLit16(16u8.wrapping_sub(src_val) & 15, dst),
                0b1110 => Self::RArithShiftLit16(16u8.wrapping_sub(src_val) & 15, dst),
                0b1111 => Self::RotateLit16(16u8.wrapping_sub(src_val) & 15, dst),
                _ => Self::Invalid
            }
            12 => /* BitOp */ match opt {
                0b0000 => Self::SetOne(dst),
                0b1111 => Self::SetAll(dst),
                0b1000 => Self::And(src, dst),
                0b1110 => Self::Or(src, dst),
                0b0110 => Self::Xor(src, dst),
                0b0111 => Self::Nand(src, dst),
                0b0001 => Self::Nor(src, dst),
                0b1001 => Self::Xnor(src, dst),
                0b1010 => Self::Swap(src, dst),
                0b1100 => Self::Nop,
                0b0101 => Self::NotSrc(src, dst),
                0b0011 => Self::NotDest(dst),
                0b0010 => Self::AndNot(src, dst) /*(Dest Clears Src)*/,
                0b0100 => Self::AndNotS(src, dst) /*(Src Clears Dest)*/,
                0b1011 => Self::OrNot(src, dst),
                0b1101 => Self::OrNotS(src, dst),
                _ => Self::Invalid
            }
            13 => /* SpecOp */ match opt {
                0x0 => Self::HAdd(src, dst),
                0x1 => Self::MultSat(src, dst),
                0x2 => Self::MultLow(src, dst),
                0x3 => Self::MultHi(src, dst),
                0x4 => Self::Divide(src, dst),
                0x5 => Self::RecpDivide(src, dst),
                _ => Self::Invalid
            }
            14 => Self::Extra14, 15 => Self::Extra15,
            16..=u16::MAX => unreachable!(),
        }
    }
}

impl Display for Opcode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Opcode::Nop => write!(f, "Nop"),
            Opcode::Invalid => write!(f, "Invalid:ProcessEnd"),
            Opcode::Halt => write!(f, "Halt:CatchFire"),
            Opcode::Sleep(kind) => {
                write!(f, "Sleep ")?;
                match kind {
                    Param::Lit(_) => write!(f, "{}", kind),
                    Param::RegL(_) |
                    Param::RegH(_) |
                    Param::Reg(_) => write!(f, "{}.x", kind)
                }
            }
            // WSelect
            Self::WMove(src, dst, sel) => write!(f, "WMove.{} {:?}, {:?}", sel, dst, src),
            Self::WSwap(src, dst, sel) => write!(f, "WSwap.{} {:?}, {:?}", sel, dst, src),
            Self::WAdd(src, dst, sel) => write!(f, "WAdd.{} {:?}, {:?}", sel, dst, src),
            Self::WSub(src, dst, sel) => write!(f, "WSub.{} {:?}, {:?}", sel, dst, src),
            Opcode::Extra2 => write!(f, "{:?} VMError::{:?}", self, VMError::Explosion),
            Opcode::Extra3 => write!(f, "{:?} VMError::{:?}", self, VMError::Zap),
            Opcode::HAdd(src, dst) => {
                write!(f, "HAdd {0:?} = {1:?}", dst, src)
            }
            Opcode::MultSat(src, dst) => write!(f, "MultSat {0:?}, {1:?}", dst, src),
            Opcode::MultLow(src, dst) => write!(f, "MultLow {0:?}, {1:?}", dst, src),
            Opcode::MultHi(src, dst) => write!(f, "MultHi {0:?}, {1:?}", dst, src),
            Opcode::Divide(src, dst) => write!(f, "Divide {0:?}, {1:?}", dst, src),
            Opcode::RecpDivide(src, dst) => write!(f, "RecpDivide {0:?}, {1:?}", dst, src),
            Opcode::Extra14 => write!(f, "{:?} VMError::{:?}", self, VMError::IceOver),
            Opcode::Extra15 => write!(f, "{:?} VMError::{:?}", self, VMError::RockStone),
            Opcode::Move(src, dst, opt) => {
                if *opt == 0 {
                    write!(f, "Move {0:?}, {1:?}", dst, src)
                } else if *opt != 0xf {
                    write!(f, "Move {0:?}.{2}{3}{4}{5}, {1:?}.{2}{3}{4}{5}", dst, src,
                        if opt & 1 == 0 {"x"} else {""},
                        if opt & 2 == 0 {"y"} else {""},
                        if opt & 4 == 0 {"z"} else {""},
                        if opt & 8 == 0 {"w"} else {""}
                    )
                } else {
                    write!(f, "NopMove")
                }
            }
            Opcode::Swizzle(dst, tx, ty, tz, tw) => {
                write!(f, "Swizzle {0:?} = {0:?}.{1}{2}{3}{4}", dst, tx, ty, tz, tw)
            }
            Opcode::Load(src, dst, scale) => {
                write!(f, "Load{} {:?}, [{:?}]", 1 + *scale, dst, src)
            }
            Opcode::LoadInc(RegIndex::Ri, RegIndex::C0, scale) => {
                write!(f, "Skip{}", 1 + *scale)
            }
            Opcode::LoadInc(RegIndex::Ri, dst, scale) => {
                write!(f, "LoadInl {:?}, {}#", dst, 1 + *scale)
            }
            Opcode::LoadInc(src, dst, scale) => {
                write!(f, "LoadInc{} {:?}, [{:?}]", 1 + *scale, dst, src)
            }
            Opcode::Gather(src, dst, scale) => {
                write!(f, "Gather{} {:?}, [{:?}]", 1 + *scale, dst, src)
            }
            Opcode::GatherInc(src, dst, scale) => {
                write!(f, "GatherInc{} {:?}, [{:?}]", 1 + *scale, dst, src)
            }
            Opcode::Store(src, dst, scale) => {
                write!(f, "Store{} [{2:?}], {1:?}", 1 + *scale, dst, src)
            }
            Opcode::StoreInc(src, dst, scale) => {
                write!(f, "StoreInc{} [{2:?}], {1:?}", 1 + *scale, dst, src)
            }
            Opcode::Scatter(src, dst, scale) => {
                write!(f, "Scatter{} [{2:?}], {1:?}", 1 + *scale, dst, src)
            }
            Opcode::ScatterInc(src, dst, scale) => {
                write!(f, "ScatterInc{} [{2:?}], {1:?}", 1 + *scale, dst, src)
            }
            Opcode::Skip1 => write!(f, "Skip1"),
            Opcode::Skip2 => write!(f, "Skip2"),
            Opcode::Skip3 => write!(f, "Skip3"),
            Opcode::Skip4 => write!(f, "Skip4"),
            Opcode::IncA1x1(src) => write!(f, "IncA1x1 {}", src),
            Opcode::IncA1x2(src) => write!(f, "IncA1x2 {}", src),
            Opcode::IncA1x3(src) => write!(f, "IncA1x3 {}", src),
            Opcode::IncA1x4(src) => write!(f, "IncA1x4 {}", src),
            Opcode::IncA2x1(src) => write!(f, "IncA2x1 {}", src),
            Opcode::IncA3x1(src) => write!(f, "IncA3x1 {}", src),
            Opcode::IncA4x1(src) => write!(f, "IncA4x1 {}", src),
            Opcode::Add8(src, dst) => write!(f, "Add8 {:?}, {:?}", dst, src),
            Opcode::Sub8(src, dst) => write!(f, "Sub8 {:?}, {:?}", dst, src),
            Opcode::RSub8(src, dst) => write!(f, "RSub8 {:?}, {:?}", dst, src),
            Opcode::Eq8(src, dst) => write!(f, "Eq8 {:?}, {:?}", dst, src),
            Opcode::NotEq8(src, dst) => write!(f, "NotEq8 {:?}, {:?}", dst, src),
            Opcode::Carry8(src, dst) => write!(f, "Carry8 {:?}, {:?}", dst, src),
            Opcode::LessU8(src, dst) => write!(f, "LessU8 {:?}, {:?}", dst, src),
            Opcode::GreaterU8(src, dst) => write!(f, "GreaterU8 {:?}, {:?}", dst, src),
            Opcode::AddSat8(src, dst) => write!(f, "AddSat8 {:?}, {:?}", dst, src),
            Opcode::SubSat8(src, dst) => write!(f, "SubSat8 {:?}, {:?}", dst, src),
            Opcode::RSubSat8(src, dst) => write!(f, "RSubSat8 {:?}, {:?}", dst, src),
            Opcode::GreaterEqU8(src, dst) => write!(f, "GreaterEqU8 {:?}, {:?}", dst, src),
            Opcode::AddOver8(src, dst) => write!(f, "AddOver8 {:?}, {:?}", dst, src),
            Opcode::SubOver8(src, dst) => write!(f, "SubOver8 {:?}, {:?}", dst, src),
            Opcode::RSubOver8(src, dst) => write!(f, "RSubOver8 {:?}, {:?}", dst, src),
            Opcode::LessEqU8(src, dst) => write!(f, "LessEqU8 {:?}, {:?}", dst, src),
            Opcode::Add16(src, dst) => write!(f, "Add16 {:?}, {:?}", dst, src),
            Opcode::Sub16(src, dst) => write!(f, "Sub16 {:?}, {:?}", dst, src),
            Opcode::RSub16(src, dst) => write!(f, "RSub16 {:?}, {:?}", dst, src),
            Opcode::Eq16(src, dst) => write!(f, "Eq16 {:?}, {:?}", dst, src),
            Opcode::NotEq16(src, dst) => write!(f, "NotEq16 {:?}, {:?}", dst, src),
            Opcode::Carry16(src, dst) => write!(f, "Carry16 {:?}, {:?}", dst, src),
            Opcode::LessU16(src, dst) => write!(f, "LessU16 {:?}, {:?}", dst, src),
            Opcode::LessEqU16(src, dst) => write!(f, "LessEqU16 {:?}, {:?}", dst, src),
            Opcode::GreaterU16(src, dst) => write!(f, "GreaterU16 {:?}, {:?}", dst, src),
            Opcode::GreaterEqU16(src, dst) => write!(f, "GreaterEqU16 {:?}, {:?}", dst, src),
            Opcode::AddSat16(src, dst) => write!(f, "AddSat16 {:?}, {:?}", dst, src),
            Opcode::SubSat16(src, dst) => write!(f, "SubSat16 {:?}, {:?}", dst, src),
            Opcode::RSubSat16(src, dst) => write!(f, "RSubSat16 {:?}, {:?}", dst, src),
            Opcode::AddOver16(src, dst) => write!(f, "AddOver16 {:?}, {:?}", dst, src),
            Opcode::SubOver16(src, dst) => write!(f, "SubOver16 {:?}, {:?}", dst, src),
            Opcode::RSubOver16(src, dst) => write!(f, "RSubOver16 {:?}, {:?}", dst, src),
            Opcode::SetAll(dst) => write!(f, "SetAll {:?}", dst),
            Opcode::SetOne(dst) => write!(f, "SetOne {:?}", dst),
            Opcode::Swap(src, dst) => write!(f, "Swap {:?} = {:?}", dst, src),
            Opcode::NotSrc(src, dst) => write!(f, "NotS {:?} = ~{:?}", dst, src),
            Opcode::NotDest(dst) => write!(f, "Not {0:?} = ~{0:?}", dst),
            Opcode::Xor(src, dst) => {
                if dst == src { write!(f, "Zero {:?}", dst) }
                else { write!(f, "Xor {0:?} = {1:?} ^ {0:?}", dst, src) }
            }
            Opcode::Xnor(src, dst) => write!(f, "Xnor {0:?} = {1:?} ^ ~{0:?}", dst, src),
            Opcode::Or(src, dst) => write!(f, "Or {0:?} = {1:?} | {0:?}", dst, src),
            Opcode::Nor(src, dst) => write!(f, "Nor {0:?} =~({1:?} | {0:?})", dst, src),
            Opcode::And(src, dst) => write!(f, "And {0:?} = {1:?} & {0:?}", dst, src),
            Opcode::Nand(src, dst) => write!(f, "Nand {0:?} =~({1:?} & {0:?})", dst, src),
            Opcode::AndNot(src, dst) => write!(f, "AndNot {0:?} = {1:?} & ~{0:?}", dst, src),
            Opcode::AndNotS(src, dst) => write!(f, "AndNotS {0:?} = ~{1:?} & {0:?}", dst, src),
            Opcode::OrNot(src, dst) => write!(f, "OrNot {0:?} = {1:?} | ~{0:?}", dst, src),
            Opcode::OrNotS(src, dst) => write!(f, "OrNotS {0:?} = ~{1:?} | {0:?}", dst, src),
            Self::LShift8(src, dst) => write!(f, "LShift8 {:?} <<= {:?}", dst, src),
            Self::RLogiShift8(src, dst) => write!(f, "RLogiShift8 {:?} >>>= {:?}", dst, src),
            Self::RArithShift8(src, dst) => write!(f, "RArithShift8 {:?} >>= {:?}", dst, src),
            Self::LRotate8(src, dst) => write!(f, "LRotate8 {:?} <_<= {:?}", dst, src),
            Self::LIShift8(src, dst) => write!(f, "LIShift8 {:?} <<= 8-{:?}", dst, src),
            Self::RILogiShift8(src, dst) => write!(f, "RILogiShift8 {:?} >>>= 8-{:?}", dst, src),
            Self::RIArithShift8(src, dst) => write!(f, "RIArithShift8 {:?} >>= 8-{:?}", dst, src),
            Self::RRotate8(src, dst) => write!(f, "RRotate8 {:?} >_>= 8-{:?}", dst, src),
            Self::LShiftLit8(src_val, dst) => write!(f, "LShiftLit8 {:?} <<= {:?}", dst, src_val),
            Self::RLogiShiftLit8(src_val, dst) => write!(f, "RLogiShiftLit8 {:?} >>>= {:?}", dst, src_val),
            Self::RArithShiftLit8(src_val, dst) => write!(f, "RArithShiftLit8 {:?} >>= {:?}", dst, src_val),
            Self::RotateLit8(src_val, dst) => write!(f, "LRotateLit8 {:?} <_<= {:?}", dst, src_val),
            Self::LShift16(src, dst) => write!(f, "LShift16 {:?} <<= {:?}", dst, src),
            Self::RLogiShift16(src, dst) => write!(f, "RLogiShift16 {:?} >>>= {:?}", dst, src),
            Self::RArithShift16(src, dst) => write!(f, "RArithShift16 {:?} >>= {:?}", dst, src),
            Self::LRotate16(src, dst) => write!(f, "LRotate16 {:?} <_<= {:?}", dst, src),
            Self::LIShift16(src, dst) => write!(f, "LIShift16 {:?} <<= 16-{:?}", dst, src),
            Self::RILogiShift16(src, dst) => write!(f, "RILogiShift16 {:?} >>>= 16-{:?}", dst, src),
            Self::RIArithShift16(src, dst) => write!(f, "RIArithShift16 {:?} >>= 16-{:?}", dst, src),
            Self::RRotate16(src, dst) => write!(f, "LRotate16 {:?} <_<= 16-{:?}", dst, src),
            Self::LShiftLit16(src_val, dst) => write!(f, "LShiftLit16 {:?} <<= {:?}", dst, src_val),
            Self::RLogiShiftLit16(src_val, dst) => write!(f, "RLogiShiftLit16 {:?} >>>= {:?}", dst, src_val),
            Self::RArithShiftLit16(src_val, dst) => write!(f, "RArithShiftLit16 {:?} >>= {:?}", dst, src_val),
            Self::RotateLit16(src_val, dst) => write!(f, "LRotateLit16 {:?} <_<= {:?}", dst, src_val),
        }
    }
}

