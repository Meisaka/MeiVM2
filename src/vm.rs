use std::{
    collections::{HashMap, VecDeque}, fmt::Display, sync::Arc
};

const VM_INIT_USER_COUNT: usize = 128;
const VM_INIT_PROC_COUNT: usize = 128;
const WORD_DELAY_PRIV_TO_SHARED: u32 = 64;
const WORD_DELAY_PRIV_FROM_SHARED: u32 = 16;

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
struct VMRegister {
    x: u16, y: u16, z: u16, w: u16,
}
impl Display for VMRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04x} {:04x} {:04x} {:04x}", self.x, self.y, self.z, self.w)
    }
}
fn inc_addr(addr: u16) -> u16 {
    if addr < 128 {
        if addr + 1 >= 128 { 64 } else { addr + 1 }
    } else if addr >= 2048 {
        (addr.wrapping_add(1) & 0b111_1111_1111) + 2048
    } else {
        addr
    }
}

fn is_priv_mem(addr: u16) -> bool {
    addr < 256
}

impl VMRegister {
    fn inc_addr(&mut self) {
        self.x = inc_addr(self.x);
    }
    fn index_mut(&mut self, addr: u8) -> &mut u16 {
        match addr & 3 {
            0 => &mut self.x, 1 => &mut self.y, 2 => &mut self.z, 3 => &mut self.w,
            _ => unreachable!()
        }
    }
    fn index(&self, addr: u8) -> u16 {
        match addr & 3 {
            0 => self.x, 1 => self.y, 2 => self.z, 3 => self.w, _ => unreachable!()
        }
    }
    fn index8(&self, addr: u8) -> u8 {
        (match addr & 7 {
            0 => self.x, 1 => self.x >> 8,
            2 => self.y, 3 => self.y >> 8,
            4 => self.z, 5 => self.z >> 8,
            6 => self.w, 7 => self.w >> 8,
            _ => unreachable!()
        }) as u8
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum RegIndex {
    C0 = 0, C1 = 1, C2 = 2, C3 = 3,
    C4 = 4, C5 = 5, C6 = 6, C7 = 7,
    R0 = 8, R1 = 9, R2 = 10, R3 = 11,
    R4 = 12, R5 = 13, R6 = 14, Ri = 15,
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
enum Param {
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
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum Swizzle {
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
enum Opcode {
    Invalid,
    // System:
    Halt, Sleep(Param), Nop,
    // dear future me: get out while you still can... there is no escape otherwise
    // Extra:
    Extra1, Extra2, Extra3,
    Move(RegIndex, RegIndex, u8), Swizzle(RegIndex, Swizzle, Swizzle, Swizzle, Swizzle),
    Load(RegIndex, RegIndex, Swizzle), LoadInc(RegIndex, RegIndex, Swizzle),
    LoadGather(RegIndex, RegIndex, Swizzle), LoadGatherInc(RegIndex, RegIndex, Swizzle),
    Store(RegIndex, RegIndex, Swizzle), StoreInc(RegIndex, RegIndex, Swizzle),
    StoreScatter(RegIndex, RegIndex, Swizzle), StoreScatterInc(RegIndex, RegIndex, Swizzle),
    // Math8:
    Add8(RegIndex, RegIndex), Sub8(RegIndex, RegIndex),
    AddSat8(RegIndex, RegIndex), AddCarry8(RegIndex, RegIndex), AddSign8(RegIndex, RegIndex),
    SubSat8(RegIndex, RegIndex),
    CompareCarry8(RegIndex, RegIndex), CompareEq8(RegIndex, RegIndex),
    SubRev8(RegIndex, RegIndex),
    SubSatRev8(RegIndex, RegIndex),
    CompareCarryRev8(RegIndex, RegIndex),
    // Math16:
    Add16(RegIndex, RegIndex), Sub16(RegIndex, RegIndex),
    AddSat16(RegIndex, RegIndex), AddCarry16(RegIndex, RegIndex), AddSign16(RegIndex, RegIndex),
    SubSat16(RegIndex, RegIndex),
    CompareCarry16(RegIndex, RegIndex), CompareEq16(RegIndex, RegIndex),
    SubRev16(RegIndex, RegIndex),
    SubSatRev16(RegIndex, RegIndex),
    CompareCarryRev16(RegIndex, RegIndex),
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
    SetZero(RegIndex), SetOne(RegIndex), Src(RegIndex, RegIndex), NotSrc(RegIndex, RegIndex), NotDest(RegIndex),
    And(RegIndex, RegIndex), Or(RegIndex, RegIndex), Xor(RegIndex, RegIndex),
    Nand(RegIndex, RegIndex), Nor(RegIndex, RegIndex), Xnor(RegIndex, RegIndex),
    SrcAndNotDest(RegIndex, RegIndex) /*(Dest Clears Src)*/,
    NotSrcAndDest(RegIndex, RegIndex) /*(Src Clears Dest)*/,
    SrcOrNotDest(RegIndex, RegIndex), NotSrcOrDest(RegIndex, RegIndex),
    // SpecOp:
    HAdd(RegIndex, RegIndex), DotProd(RegIndex, RegIndex),
    Extra14, Extra15,
}
impl Opcode {
    fn parse(value: u16) -> Self {
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
            1 => Self::Extra1, 2 => Self::Extra2, 3 => Self::Extra3,
            4 => Self::Move(src, dst, opt), 5 => Self::Swizzle(dst,
                opt.into(), (opt >> 2).into(),
                src_val.into(), (src_val >> 2).into()
                ),
            6 => match opt & 3 {
                0 => Self::Load(src, dst, (3u8.wrapping_sub(opt >> 2)).into()),
                1 => Self::LoadInc(src, dst, (3u8.wrapping_sub(opt >> 2)).into()),
                2 => Self::LoadGather(src, dst, (3u8.wrapping_sub(opt >> 2)).into()),
                3 => Self::LoadGatherInc(src, dst, (3u8.wrapping_sub(opt >> 2)).into()),
                _ => unreachable!()
            }
            7 => match opt & 3 {
                0 => Self::Store(src, dst, (3u8.wrapping_sub(opt >> 2)).into()),
                1 => Self::StoreInc(src, dst, (3u8.wrapping_sub(opt >> 2)).into()),
                2 => Self::StoreScatter(src, dst, (3u8.wrapping_sub(opt >> 2)).into()),
                3 => Self::StoreScatterInc(src, dst, (3u8.wrapping_sub(opt >> 2)).into()),
                _ => unreachable!()
            }
            8 => /* Math8 */ match opt {
                0b0000 => Self::Add8(src, dst),
                0b0001 => Self::AddSat8(src, dst),
                0b0010 => Self::AddCarry8(src, dst),
                0b0011 => Self::AddSign8(src, dst),
                0b0100 => Self::Sub8(src, dst),
                0b0101 => Self::SubSat8(src, dst),
                0b0110 => Self::CompareCarry8(src, dst),
                0b0111 => Self::CompareEq8(src, dst),
                0b1000 => Self::SubRev8(src, dst),
                0b1001 => Self::SubSatRev8(src, dst),
                0b1010 => Self::CompareCarryRev8(src, dst),
                _ => Self::Invalid
            },
            9 => /* Math16 */ match opt {
                0b0000 => Self::Add16(src, dst),
                0b0001 => Self::AddSat16(src, dst),
                0b0010 => Self::AddCarry16(src, dst),
                0b0011 => Self::AddSign16(src, dst),
                0b0100 => Self::Sub16(src, dst),
                0b0101 => Self::SubSat16(src, dst),
                0b0110 => Self::CompareCarry16(src, dst),
                0b0111 => Self::CompareEq16(src, dst),
                0b1000 => Self::SubRev16(src, dst),
                0b1001 => Self::SubSatRev16(src, dst),
                0b1010 => Self::CompareCarryRev16(src, dst),
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
                0b0000 => Self::SetZero(dst),
                0b1111 => Self::SetOne(dst),
                0b1000 => Self::And(src, dst),
                0b1110 => Self::Or(src, dst),
                0b0110 => Self::Xor(src, dst),
                0b0111 => Self::Nand(src, dst),
                0b0001 => Self::Nor(src, dst),
                0b1001 => Self::Xnor(src, dst),
                0b1010 => Self::Src(src, dst),
                0b1100 => Self::Nop,
                0b0101 => Self::NotSrc(src, dst),
                0b0011 => Self::NotDest(dst),
                0b0010 => Self::SrcAndNotDest(src, dst) /*(Dest Clears Src)*/,
                0b0100 => Self::NotSrcAndDest(src, dst) /*(Src Clears Dest)*/,
                0b1011 => Self::SrcOrNotDest(src, dst),
                0b1101 => Self::NotSrcOrDest(src, dst),
                _ => Self::Invalid
            }
            13 => /* SpecOp */ match opt {
                0b0000 => Self::HAdd(src, dst),
                0b0001 => Self::DotProd(src, dst),
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
            Opcode::Extra1 | Opcode::Extra2
                => write!(f, "{:?} VMError::{:?}", self, VMError::Explosion),
            Opcode::Extra3 => write!(f, "{:?} VMError::{:?}", self, VMError::Zap),
            Opcode::HAdd(src, dst) => {
                write!(f, "HAdd {0:?} = {1:?}", dst, src)
            }
            Opcode::DotProd(src, dst) => {
                write!(f, "DotProd {0:?} = {0:?} dot {1:?}", dst, src)
            }
            Opcode::Extra14 => write!(f, "{:?} VMError::{:?}", self, VMError::IceOver),
            Opcode::Extra15 => write!(f, "{:?} VMError::{:?}", self, VMError::RockStone),
            Opcode::Move(src, dst, opt) => {
                if *opt == 0 {
                    write!(f, "Move {0:?} = {1:?}", dst, src)
                } else if *opt != 0xf {
                    write!(f, "Move {0:?}.{2}{3}{4}{5} = {1:?}.{2}{3}{4}{5}", dst, src,
                        if opt & 1 == 0 {"x"} else {""},
                        if opt & 2 == 0 {"y"} else {""},
                        if opt & 4 == 0 {"z"} else {""},
                        if opt & 8 == 0 {"w"} else {""}
                    )
                } else {
                    write!(f, "Move Nop")
                }
            }
            Opcode::Swizzle(dst, tx, ty, tz, tw) => {
                write!(f, "Swizzle {0:?}.xyzw = {0:?}.{1}{2}{3}{4}", dst, tx, ty, tz, tw)
            }
            Opcode::Load(src, dst, scale) => {
                write!(f, "Load {:?}.x{}{}{} = [{:?}]", dst,
                    if *scale > Swizzle::X {"y"} else {""},
                    if *scale > Swizzle::Y {"z"} else {""},
                    if *scale > Swizzle::Z {"w"} else {""},
                    src)
            }
            Opcode::LoadInc(src, dst, scale) => {
                write!(f, "LoadInc {:?}.x{}{}{} = [{:?}+]", dst,
                    if *scale > Swizzle::X {"y"} else {""},
                    if *scale > Swizzle::Y {"z"} else {""},
                    if *scale > Swizzle::Z {"w"} else {""},
                    src)
            }
            Opcode::LoadGather(src, dst, scale) => {
                write!(f, "LoadGather {:?}.x{1}{2}{3} = [*{4:?}]", dst,
                    if *scale > Swizzle::X {"y"} else {""},
                    if *scale > Swizzle::Y {"z"} else {""},
                    if *scale > Swizzle::Z {"w"} else {""},
                    src)
            }
            Opcode::LoadGatherInc(src, dst, scale) => {
                write!(f, "LoadGatherInc {:?}.x{1}{2}{3} = [*{4:?}+]", dst,
                    if *scale > Swizzle::X {"y"} else {""},
                    if *scale > Swizzle::Y {"z"} else {""},
                    if *scale > Swizzle::Z {"w"} else {""},
                    src)
            }
            Opcode::Store(src, dst, scale) => {
                write!(f, "Store [{4:?}] = {0:?}.x{1}{2}{3}", dst,
                    if *scale > Swizzle::X {"y"} else {""},
                    if *scale > Swizzle::Y {"z"} else {""},
                    if *scale > Swizzle::Z {"w"} else {""},
                    src)
            }
            Opcode::StoreInc(src, dst, scale) => {
                write!(f, "StoreInc [{4:?}+] = {0:?}.x{1}{2}{3}", dst,
                    if *scale > Swizzle::X {"y"} else {""},
                    if *scale > Swizzle::Y {"z"} else {""},
                    if *scale > Swizzle::Z {"w"} else {""},
                    src)
            }
            Opcode::StoreScatter(src, dst, scale) => {
                write!(f, "StoreScatter [*{4:?}] = {0:?}.x{1}{2}{3}", dst,
                    if *scale > Swizzle::X {"y"} else {""},
                    if *scale > Swizzle::Y {"z"} else {""},
                    if *scale > Swizzle::Z {"w"} else {""},
                    src)
            }
            Opcode::StoreScatterInc(src, dst, scale) => {
                write!(f, "StoreScatterInc [*{4:?}+] = {0:?}.x{1}{2}{3}", dst,
                    if *scale > Swizzle::X {"y"} else {""},
                    if *scale > Swizzle::Y {"z"} else {""},
                    if *scale > Swizzle::Z {"w"} else {""},
                    src)
            }
            Opcode::Add8(src, dst) => write!(f, "Add8 {:?} += {:?}", dst, src),
            Opcode::AddSat8(src, dst) => write!(f, "AddSat8 {:?} += {:?}", dst, src),
            Opcode::AddCarry8(src, dst) => write!(f, "AddCarry8 {:?} += {:?}", dst, src),
            Opcode::AddSign8(src, dst) => write!(f, "AddSign8 {:?} += {:?}", dst, src),
            Opcode::Sub8(src, dst) => write!(f, "Sub8 {0:?} = {1:?} - {0:?}", dst, src),
            Opcode::SubSat8(src, dst) => write!(f, "SubSat8 {:?}, {:?}", dst, src),
            Opcode::CompareCarry8(src, dst) => write!(f, "CompareCarry8 {:?}, {:?}", dst, src),
            Opcode::CompareEq8(src, dst) => write!(f, "CompareEq8 {:?} == {:?}", dst, src),
            Opcode::SubRev8(src, dst) => write!(f, "SubRev8 {0:?} = {0:?} - {1:?}", dst, src),
            Opcode::SubSatRev8(src, dst) => write!(f, "SubSatRev8 {:?}, {:?}", dst, src),
            Opcode::CompareCarryRev8(src, dst) => write!(f, "CompareCarryRev8 {:?}, {:?}", dst, src),
            Opcode::Add16(src, dst) => write!(f, "Add16 {:?} += {:?}", dst, src),
            Opcode::AddSat16(src, dst) => write!(f, "AddSat16 {:?} += {:?}", dst, src),
            Opcode::AddCarry16(src, dst) => write!(f, "AddCarry16 {:?} += {:?}", dst, src),
            Opcode::AddSign16(src, dst) => write!(f, "AddSign16 {:?} += {:?}", dst, src),
            Opcode::Sub16(src, dst) => write!(f, "Sub16 {:?}, {:?}", dst, src),
            Opcode::SubSat16(src, dst) => write!(f, "SubSat16 {:?}, {:?}", dst, src),
            Opcode::CompareCarry16(src, dst) => write!(f, "CompareCarry16 {:?}, {:?}", dst, src),
            Opcode::CompareEq16(src, dst) => write!(f, "CompareEq16 {:?} == {:?}", dst, src),
            Opcode::SubRev16(src, dst) => write!(f, "SubRev16 {:?}, {:?}", dst, src),
            Opcode::SubSatRev16(src, dst) => write!(f, "SubSatRev16 {:?}, {:?}", dst, src),
            Opcode::CompareCarryRev16(src, dst) => write!(f, "CompareCarryRev16 {:?}, {:?}", dst, src),
            Opcode::SetOne(dst) => write!(f, "SetOne {:?}", dst),
            Opcode::SetZero(dst) => write!(f, "SetZero {:?}", dst),
            Opcode::Src(src, dst) => write!(f, "Src {:?} = {:?}", dst, src),
            Opcode::NotSrc(src, dst) => write!(f, "NotSrc {:?} = ~{:?}", dst, src),
            Opcode::NotDest(dst) => write!(f, "NotDest {0:?} = ~{0:?}", dst),
            Opcode::Xor(src, dst) => write!(f, "Xor {0:?} = {1:?} ^ {0:?}", dst, src),
            Opcode::Xnor(src, dst) => write!(f, "Xnor {0:?} = {1:?} ^ ~{0:?}", dst, src),
            Opcode::Or(src, dst) => write!(f, "Or {0:?} = {1:?} | {0:?}", dst, src),
            Opcode::Nor(src, dst) => write!(f, "Nor {0:?} =~({1:?} | {0:?})", dst, src),
            Opcode::And(src, dst) => write!(f, "And {0:?} = {1:?} & {0:?}", dst, src),
            Opcode::Nand(src, dst) => write!(f, "Nand {0:?} =~({1:?} & {0:?})", dst, src),
            Opcode::SrcAndNotDest(src, dst) => write!(f, "SrcAndNotDest {0:?} = {1:?} & ~{0:?}", dst, src),
            Opcode::NotSrcAndDest(src, dst) => write!(f, "NotSrcAndDest {0:?} = ~{1:?} & {0:?}", dst, src),
            Opcode::SrcOrNotDest(src, dst) => write!(f, "Nand {0:?} = {1:?} | ~{0:?}", dst, src),
            Opcode::NotSrcOrDest(src, dst) => write!(f, "Nand {0:?} = ~{1:?} | {0:?}", dst, src),
            Self::LShift8(src, dst) => write!(f, "LShift8 {:?} <<= {:?}", dst, src),
            Self::RLogiShift8(src, dst) => write!(f, "RLogiShift8 {:?} >>>= {:?}", dst, src),
            Self::RArithShift8(src, dst) => write!(f, "RArithShift8 {:?} >>= {:?}", dst, src),
            Self::LRotate8(src, dst) => write!(f, "LRotate8 {:?} ><<= {:?}", dst, src),
            Self::LIShift8(src, dst) => write!(f, "LIShift8 {:?} <<= 8-{:?}", dst, src),
            Self::RILogiShift8(src, dst) => write!(f, "RILogiShift8 {:?} >>>= 8-{:?}", dst, src),
            Self::RIArithShift8(src, dst) => write!(f, "RIArithShift8 {:?} >>= 8-{:?}", dst, src),
            Self::RRotate8(src, dst) => write!(f, "RRotate8 {:?} ><<= 8-{:?}", dst, src),
            Self::LShiftLit8(src_val, dst) => write!(f, "LShiftLit8 {:?} <<= {:?}", dst, src_val),
            Self::RLogiShiftLit8(src_val, dst) => write!(f, "RLogiShiftLit8 {:?} >>>= {:?}", dst, src_val),
            Self::RArithShiftLit8(src_val, dst) => write!(f, "RArithShiftLit8 {:?} >>= {:?}", dst, src_val),
            Self::RotateLit8(src_val, dst) => write!(f, "LRotateLit8 {:?} ><<= {:?}", dst, src_val),
            Self::LShift16(src, dst) => write!(f, "LShift16 {:?} <<= {:?}", dst, src),
            Self::RLogiShift16(src, dst) => write!(f, "RLogiShift16 {:?} >>>= {:?}", dst, src),
            Self::RArithShift16(src, dst) => write!(f, "RArithShift16 {:?} >>= {:?}", dst, src),
            Self::LRotate16(src, dst) => write!(f, "LRotate16 {:?} ><<= {:?}", dst, src),
            Self::LIShift16(src, dst) => write!(f, "LIShift16 {:?} <<= 16-{:?}", dst, src),
            Self::RILogiShift16(src, dst) => write!(f, "RILogiShift16 {:?} >>>= 16-{:?}", dst, src),
            Self::RIArithShift16(src, dst) => write!(f, "RIArithShift16 {:?} >>= 16-{:?}", dst, src),
            Self::RRotate16(src, dst) => write!(f, "LRotate16 {:?} ><<= 16-{:?}", dst, src),
            Self::LShiftLit16(src_val, dst) => write!(f, "LShiftLit16 {:?} <<= {:?}", dst, src_val),
            Self::RLogiShiftLit16(src_val, dst) => write!(f, "RLogiShiftLit16 {:?} >>>= {:?}", dst, src_val),
            Self::RArithShiftLit16(src_val, dst) => write!(f, "RArithShiftLit16 {:?} >>= {:?}", dst, src_val),
            Self::RotateLit16(src_val, dst) => write!(f, "LRotateLit16 {:?} ><<= {:?}", dst, src_val),
        }
    }
}

#[derive(Debug)]
enum DeferredOp {
    DelayLoad(u32, Swizzle, Swizzle, RegIndex, Option<RegIndex>),
    DelayGather(u32, Swizzle, Swizzle, RegIndex, Option<RegIndex>),
    DelayStore(u32, Swizzle, Swizzle, Option<RegIndex>),
    DelayScatter(u32, Swizzle, Swizzle, Option<RegIndex>),
}

struct VMProc {
    cval: [VMRegister; 8],
    reg: [VMRegister; 7],
    ins_ptr: VMRegister,
    priv_mem: [u16; 64],
    sleep_for: u32,
    lval: VMRegister,
    rval: VMRegister,
    defer: Option<DeferredOp>,
    is_running: bool,
}
impl Display for VMProc {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sleep_for > 0 {
            write!(f, "SLEEP {}", self.sleep_for)?
        } else if let Some(defer) = &self.defer {
            write!(f, "DEFER {:?}", defer)?
        } else {
            f.write_str(if self.is_running {"RUN "} else {"HALT"})?
        }
        write!(f, "ria={:4x}", self.ins_ptr.x)
    }
}
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
enum VMError {
    ProcessEnd,
    CatchFire,
    Explosion,
    TempestH,
    IceOver,
    RockStone,
    Zap,
}

impl VMProc {
    fn new() -> Self {
        Self {
            cval: [
                VMRegister::default(),
                VMRegister::default(),
                VMRegister::default(),
                VMRegister::default(),
                VMRegister{x:1, y:1, z:1, w:1},
                VMRegister{x:0b1_1111, y:0b11_1111, z:0b1_1111, w:0},
                VMRegister{x:11, y:5, z:0, w:0},
                VMRegister{x:0x800, y:0x040, z:0xffff, w:0xffff},
            ],
            lval: VMRegister::default(),
            rval: VMRegister::default(),
            defer: None,
            reg: [VMRegister::default(); 7],
            ins_ptr: VMRegister{x:0x40, y:0, z:0, w:0}, // reg15.x is instruction pointer
            priv_mem: [0; 64],
            sleep_for: 0,
            is_running: false,
        }
    }
    fn reg_index_mut(&mut self, index: RegIndex) -> Option<&mut VMRegister> {
        match index {
            RegIndex::C0 | RegIndex::C1 | RegIndex::C2 | RegIndex::C3
            | RegIndex::C4 | RegIndex::C5 | RegIndex::C6 | RegIndex::C7
                => None,
            RegIndex::R0 => Some(&mut self.reg[0]),
            RegIndex::R1 => Some(&mut self.reg[1]),
            RegIndex::R2 => Some(&mut self.reg[2]),
            RegIndex::R3 => Some(&mut self.reg[3]),
            RegIndex::R4 => Some(&mut self.reg[4]),
            RegIndex::R5 => Some(&mut self.reg[5]),
            RegIndex::R6 => Some(&mut self.reg[6]),
            RegIndex::Ri => Some(&mut self.ins_ptr),
        }
    }
    fn reg_index_priv_mut(&mut self, index: RegIndex) -> &mut VMRegister {
        match index {
            RegIndex::C0 => &mut self.cval[0],
            RegIndex::C1 => &mut self.cval[1],
            RegIndex::C2 => &mut self.cval[2],
            RegIndex::C3 => &mut self.cval[3],
            RegIndex::C4 => &mut self.cval[4],
            RegIndex::C5 => &mut self.cval[5],
            RegIndex::C6 => &mut self.cval[6],
            RegIndex::C7 => &mut self.cval[7],
            RegIndex::R0 => &mut self.reg[0],
            RegIndex::R1 => &mut self.reg[1],
            RegIndex::R2 => &mut self.reg[2],
            RegIndex::R3 => &mut self.reg[3],
            RegIndex::R4 => &mut self.reg[4],
            RegIndex::R5 => &mut self.reg[5],
            RegIndex::R6 => &mut self.reg[6],
            RegIndex::Ri => &mut self.ins_ptr,
        }
    }
    fn reg_index(&self, index: RegIndex) -> &VMRegister {
        match index {
            RegIndex::C0 => &self.cval[0],
            RegIndex::C1 => &self.cval[1],
            RegIndex::C2 => &self.cval[2],
            RegIndex::C3 => &self.cval[3],
            RegIndex::C4 => &self.cval[4],
            RegIndex::C5 => &self.cval[5],
            RegIndex::C6 => &self.cval[6],
            RegIndex::C7 => &self.cval[7],
            RegIndex::R0 => &self.reg[0],
            RegIndex::R1 => &self.reg[1],
            RegIndex::R2 => &self.reg[2],
            RegIndex::R3 => &self.reg[3],
            RegIndex::R4 => &self.reg[4],
            RegIndex::R5 => &self.reg[5],
            RegIndex::R6 => &self.reg[6],
            RegIndex::Ri => &self.ins_ptr,
        }
    }
    fn dump(&self) -> () {
        println!("VM is {}", if self.is_running { "running" } else { "halted" });
        let mut ri = 0u32;
        let mut mi = 0usize;
        for rc in self.cval.chunks_exact(2) {
            println!("VM c{}: {}  c{}: {}  {:02x}: {}", ri, rc[0], ri + 1, rc[1],
                mi + 0x40, Opcode::parse(self.priv_mem[mi]));
            ri += 2;
            mi += 1;
        }
        ri = 0;
        for rc in self.reg.chunks_exact(2) {
            println!("VM r{}: {}  r{}: {}  {:02x}: {}", ri, rc[0], ri + 1, rc[1],
                mi + 0x40, Opcode::parse(self.priv_mem[mi]));
            ri += 2;
            mi += 1;
        }
        println!("VM r6: {}  ri: {}  {:02x}: {}", self.reg[6], self.ins_ptr,
            mi + 0x40, Opcode::parse(self.priv_mem[mi]));
        mi += 1;
        for (mc, m) in self.priv_mem.chunks_exact(8).enumerate() {
            println!("VM m{:03x}: {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x}     {:02x}: {}",
                0x40 + mc * 8, m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7],
                mi + 0x40, Opcode::parse(self.priv_mem[mi]));
            mi += 1;
        }
    }
    fn read_mem(&self, vm: &SimulationVM, addr: u16) -> u16 {
        if addr < 64 {
            self.reg_index((addr >> 2).into()).index(addr as u8)
        } else if addr < 128 {
            self.priv_mem[addr as usize - 64]
        } else if addr >= 2048 {
            vm.memory[addr as usize & 2047].0
        } else { 0 }
    }
    fn write_priv(&mut self, addr: u16, value: u16) {
        if addr < 64 {
            let reg = self.reg_index_priv_mut((addr >> 2).into());
            *reg.index_mut(addr as u8) = value;
        } else if addr < 128 {
            self.priv_mem[addr as usize - 64] = value;
        }
    }
    fn write_mem(&mut self, vm: &mut SimulationVM, addr: u16, value: u16) {
        if addr < 64 {
            if let Some(reg) = self.reg_index_mut((addr >> 2).into()) {
                *reg.index_mut(addr as u8) = value;
            }
        } else if addr < 128 {
            self.priv_mem[addr as usize - 64] = value;
        } else if addr >= 2048 {
            vm.memory[addr as usize & 2047].0 = value;
        } else {}
    }
    fn run(&mut self, vm: &mut SimulationVM) -> Result<(), VMError> {
        if self.sleep_for > 0 {
            self.sleep_for -= 1;
            return Ok(())
        }
        if let Some(op) = self.defer.take() {
            let mut lval = self.lval.clone();
            let mut rval = self.rval.clone();
            match match op {
                DeferredOp::DelayLoad(delay, stage, limit, result, inc_reg) => {
                    let next_stage = match stage {
                        Swizzle::X => {
                            rval.x = self.read_mem(vm, lval.x); lval.inc_addr();
                            Swizzle::Y
                        }
                        Swizzle::Y => {
                            rval.y = self.read_mem(vm, lval.x); lval.inc_addr();
                            Swizzle::Z
                        }
                        Swizzle::Z => {
                            rval.z = self.read_mem(vm, lval.x); lval.inc_addr();
                            Swizzle::W
                        }
                        Swizzle::W => {
                            rval.w = self.read_mem(vm, lval.x); lval.inc_addr();
                            Swizzle::W
                        }
                    };
                    if stage >= limit {
                        Ok((inc_reg, Some(result)))
                    } else {
                        self.sleep_for = delay;
                        Err(DeferredOp::DelayLoad(delay, next_stage, limit, result, inc_reg))
                    }
                }
                DeferredOp::DelayGather(delay, stage, limit, result, inc_reg) => {
                    let next_stage = match stage {
                        Swizzle::X => {
                            self.rval.x = self.read_mem(vm, self.lval.x); self.lval.x = inc_addr(self.lval.x);
                            Swizzle::Y
                        }
                        Swizzle::Y => {
                            self.rval.y = self.read_mem(vm, self.lval.y); self.lval.y = inc_addr(self.lval.y);
                            Swizzle::Z
                        }
                        Swizzle::Z => {
                            self.rval.z = self.read_mem(vm, self.lval.z); self.lval.z = inc_addr(self.lval.z);
                            Swizzle::W
                        }
                        Swizzle::W => {
                            self.rval.w = self.read_mem(vm, self.lval.w); self.lval.w = inc_addr(self.lval.w);
                            Swizzle::W
                        }
                    };
                    if stage >= limit {
                        Ok((inc_reg, Some(result)))
                    } else {
                        self.sleep_for = delay;
                        Err(DeferredOp::DelayGather(delay, next_stage, limit, result, inc_reg))
                    }
                }
                DeferredOp::DelayStore(delay, stage, limit, inc_reg) => {
                    let next_stage = match stage {
                        Swizzle::X => {
                            self.write_mem(vm, lval.x, rval.x); lval.inc_addr();
                            Swizzle::Y
                        }
                        Swizzle::Y => {
                            self.write_mem(vm, lval.x, rval.y); lval.inc_addr();
                            Swizzle::Z
                        }
                        Swizzle::Z => {
                            self.write_mem(vm, lval.x, rval.z); lval.inc_addr();
                            Swizzle::W
                        }
                        Swizzle::W => {
                            self.write_mem(vm, lval.x, rval.w); lval.inc_addr();
                            Swizzle::W
                        }
                    };
                    self.sleep_for = delay;
                    if stage >= limit {
                        Ok((inc_reg, None))
                    } else {
                        Err(DeferredOp::DelayStore(delay, next_stage, limit, inc_reg))
                    }
                }
                DeferredOp::DelayScatter(delay, stage, limit, inc_reg) => {
                    let next_stage = match stage {
                        Swizzle::X => {
                            self.write_mem(vm, lval.x, rval.x); lval.x = inc_addr(lval.x);
                            Swizzle::Y
                        }
                        Swizzle::Y => {
                            self.write_mem(vm, lval.y, rval.y); lval.y = inc_addr(lval.y);
                            Swizzle::Z
                        }
                        Swizzle::Z => {
                            self.write_mem(vm, lval.z, rval.z); lval.z = inc_addr(lval.z);
                            Swizzle::W
                        }
                        Swizzle::W => {
                            self.write_mem(vm, lval.w, rval.w); lval.w = inc_addr(lval.w);
                            Swizzle::W
                        }
                    };
                    self.sleep_for = delay;
                    if stage >= limit {
                        Ok((inc_reg, None))
                    } else {
                        Err(DeferredOp::DelayScatter(delay, next_stage, limit, inc_reg))
                    }
                }
            } {
                Err(defer) => {
                    self.lval = lval;
                    self.rval = rval;
                    self.defer = Some(defer);
                }
                Ok((maybe_lreg, maybe_rreg)) => {
                    // handle src register write-back
                    if let Some(index) = maybe_lreg {
                        if let Some(wreg) = self.reg_index_mut(index) {
                            *wreg = lval;
                        }
                    }
                    // write back any result
                    if let Some(index) = maybe_rreg {
                        if let Some(wreg) = self.reg_index_mut(index) {
                            *wreg = rval;
                        }
                    }
                }
            }
            return Ok(())
        }
        macro_rules! operation16 {
            ($l:pat_param, $r:ident => $b:expr) => {
                VMRegister{
                x: { let ($l, $r) = (self.lval.x, self.rval.x); ($b) as u16},
                y: { let ($l, $r) = (self.lval.y, self.rval.y); ($b) as u16},
                z: { let ($l, $r) = (self.lval.z, self.rval.z); ($b) as u16},
                w: { let ($l, $r) = (self.lval.w, self.rval.w); ($b) as u16},
                }
            };
        }
        macro_rules! operation8 {
            ($l:pat_param, $r:ident => $b:expr) => {
                VMRegister{
                x: ({ let ($l, $r) = (self.lval.index8(0), self.rval.index8(0)); ($b) as u8 } as u16)
                | (({ let ($l, $r) = (self.lval.index8(1), self.rval.index8(1)); ($b) as u8 } as u16) << 8),
                y: ({ let ($l, $r) = (self.lval.index8(2), self.rval.index8(2)); ($b) as u8 } as u16)
                | (({ let ($l, $r) = (self.lval.index8(3), self.rval.index8(3)); ($b) as u8 } as u16) << 8),
                z: ({ let ($l, $r) = (self.lval.index8(4), self.rval.index8(4)); ($b) as u8 } as u16)
                | (({ let ($l, $r) = (self.lval.index8(5), self.rval.index8(5)); ($b) as u8 } as u16) << 8),
                w: ({ let ($l, $r) = (self.lval.index8(6), self.rval.index8(6)); ($b) as u8 } as u16)
                | (({ let ($l, $r) = (self.lval.index8(7), self.rval.index8(7)); ($b) as u8 } as u16) << 8),
                }
            }
        }
        let addr = self.ins_ptr.x;
        let is_priv_exec = is_priv_mem(addr);
        self.ins_ptr.inc_addr();
        let value = self.read_mem(vm, addr);
        let opc = Opcode::parse(value);
        let src = ((value >> 8) & 0b1111) as u8;
        let dst = ((value >> 12) & 0b1111) as u8;
        let opt = ((value >> 4) & 0b1111) as u8;
        self.lval = self.reg_index(src.into()).clone();
        self.rval = self.reg_index(dst.into()).clone();
        Ok(if let Some(wval) = match opc {
            Opcode::Nop => None,
            Opcode::Invalid => Err(VMError::ProcessEnd)?,
            Opcode::Halt => Err(VMError::CatchFire)?,
            Opcode::Sleep(_) => {
                self.sleep_for = match dst & 3 {
                    0 => src as u32,
                    1 => self.lval.x as u32 & 0xff,
                    2 => self.lval.x as u32 >> 8,
                    3 => self.lval.x as u32,
                    _ => src as u32,
                };
                None
            }
            Opcode::Extra1 | Opcode::Extra2 => Err(VMError::TempestH)?,
            Opcode::Extra3 => Err(VMError::Zap)?,
            Opcode::HAdd(_, _) => {
                let v = self.lval.x
                    .saturating_add(self.lval.y)
                    .saturating_add(self.lval.z)
                    .saturating_add(self.lval.w);
                Some(VMRegister{ x: v, y: v, z: v, w: v })
            }
            Opcode::DotProd(_, _) => {
                self.lval.x = self.lval.x.saturating_mul(self.rval.x);
                self.lval.y = self.lval.y.saturating_mul(self.rval.y);
                self.lval.z = self.lval.z.saturating_mul(self.rval.z);
                self.lval.w = self.lval.w.saturating_mul(self.rval.w);
                let v = self.lval.x
                    .saturating_add(self.lval.y)
                    .saturating_add(self.lval.z)
                    .saturating_add(self.lval.w);
                Some(VMRegister{ x: v, y: v, z: v, w: v })
            }
            Opcode::Extra14 => Err(VMError::IceOver)?,
            Opcode::Extra15 => Err(VMError::RockStone)?,
            Opcode::Move(..) => {
                if opt & 1 == 0 { self.rval.x = self.lval.x }
                if opt & 2 == 0 { self.rval.y = self.lval.y }
                if opt & 4 == 0 { self.rval.z = self.lval.z }
                if opt & 8 == 0 { self.rval.w = self.lval.w }
                Some(self.rval.clone())
            }
            Opcode::Swizzle(..) => Some(VMRegister{
                x: self.rval.index(opt),
                y: self.rval.index(opt >> 2),
                z: self.rval.index(src),
                w: self.rval.index(src >> 2)
            }),
            Opcode::Load(src_reg, dst_reg, scale) |
            Opcode::LoadInc(src_reg, dst_reg, scale) => {
                if is_priv_exec ^ is_priv_mem(self.lval.x) {
                    let delay = 
                        if is_priv_exec { WORD_DELAY_PRIV_FROM_SHARED }
                        else { WORD_DELAY_PRIV_TO_SHARED };
                    self.defer = Some(DeferredOp::DelayLoad(
                        delay, Swizzle::X, scale, dst_reg,
                        if let Opcode::Load(..) = opc { None } else { Some(src_reg) }
                        ));
                    self.sleep_for = delay;
                    None
                } else {
                    self.rval.x = self.read_mem(vm, self.lval.x); self.lval.inc_addr();
                    if scale > Swizzle::X { self.rval.y = self.read_mem(vm, self.lval.x); self.lval.inc_addr(); }
                    if scale > Swizzle::Y { self.rval.z = self.read_mem(vm, self.lval.x); self.lval.inc_addr(); }
                    if scale > Swizzle::Z { self.rval.w = self.read_mem(vm, self.lval.x); self.lval.inc_addr(); }
                    let lval = self.lval.clone();
                    if let (Opcode::LoadInc(..), Some(wreg)) = (opc, self.reg_index_mut(src.into())) {
                        *wreg = lval;
                    }
                    Some(self.rval)
                }
            }
            Opcode::LoadGather(src_reg, dst_reg, scale) |
            Opcode::LoadGatherInc(src_reg, dst_reg, scale) => {
                if is_priv_exec ^ is_priv_mem(self.lval.x) {
                    let delay = 
                        if is_priv_exec { WORD_DELAY_PRIV_FROM_SHARED }
                        else { WORD_DELAY_PRIV_TO_SHARED };
                    self.defer = Some(DeferredOp::DelayGather(
                        delay, Swizzle::X, scale, dst_reg,
                        if let Opcode::LoadGather(..) = opc { None } else { Some(src_reg) }
                        ));
                    self.sleep_for = delay;
                    None
                } else {
                    self.rval.x = self.read_mem(vm, self.lval.x); self.lval.x = inc_addr(self.lval.x);
                    if scale > Swizzle::X { self.rval.y = self.read_mem(vm, self.lval.y); self.lval.y = inc_addr(self.lval.y); }
                    if scale > Swizzle::Y { self.rval.z = self.read_mem(vm, self.lval.z); self.lval.z = inc_addr(self.lval.z); }
                    if scale > Swizzle::Z { self.rval.w = self.read_mem(vm, self.lval.w); self.lval.w = inc_addr(self.lval.w); }
                    let lval = self.lval.clone();
                    if let (Opcode::LoadGatherInc(..), Some(wreg)) = (opc, self.reg_index_mut(src.into())) {
                        *wreg = lval;
                    }
                    Some(self.rval)
                }
            }
            Opcode::Store(src_reg, _, scale) |
            Opcode::StoreInc(src_reg, _, scale) => {
                if is_priv_exec ^ is_priv_mem(self.lval.x) {
                    self.defer = Some(DeferredOp::DelayStore(
                        if is_priv_exec { WORD_DELAY_PRIV_TO_SHARED }
                        else { WORD_DELAY_PRIV_FROM_SHARED },
                        Swizzle::X, scale,
                        if let Opcode::Store(..) = opc { None } else { Some(src_reg) }
                        ));
                } else {
                    self.write_mem(vm, self.lval.x, self.rval.x); self.lval.inc_addr();
                    if scale > Swizzle::X { self.write_mem(vm, self.lval.x, self.rval.y); self.lval.inc_addr(); }
                    if scale > Swizzle::Y { self.write_mem(vm, self.lval.x, self.rval.z); self.lval.inc_addr(); }
                    if scale > Swizzle::Z { self.write_mem(vm, self.lval.x, self.rval.w); self.lval.inc_addr(); }
                    let lval = self.lval.clone();
                    if let (Opcode::StoreInc(..), Some(wreg)) = (opc, self.reg_index_mut(src.into())) {
                        *wreg = lval;
                    }
                }
                None
            }
            Opcode::StoreScatter(src_reg, _, scale) |
            Opcode::StoreScatterInc(src_reg, _, scale) => {
                if is_priv_exec ^ is_priv_mem(self.lval.x) {
                    self.defer = Some(DeferredOp::DelayScatter(
                        if is_priv_exec { WORD_DELAY_PRIV_TO_SHARED }
                        else { WORD_DELAY_PRIV_FROM_SHARED },
                        Swizzle::X, scale,
                        if let Opcode::StoreScatter(..) = opc { None } else { Some(src_reg) }
                        ));
                } else {
                    self.write_mem(vm, self.lval.x, self.rval.x); self.lval.x = inc_addr(self.lval.x);
                    if scale > Swizzle::X { self.write_mem(vm, self.lval.y, self.rval.y); self.lval.y = inc_addr(self.lval.y); }
                    if scale > Swizzle::Y { self.write_mem(vm, self.lval.z, self.rval.z); self.lval.z = inc_addr(self.lval.z); }
                    if scale > Swizzle::Z { self.write_mem(vm, self.lval.w, self.rval.w); self.lval.w = inc_addr(self.lval.w); }
                    let lval = self.lval.clone();
                    if let (Opcode::StoreScatterInc(..), Some(wreg)) = (opc, self.reg_index_mut(src.into())) {
                        *wreg = lval;
                    }
                }
                None
            }
            Opcode::Add8                    (..) => Some( operation8!(lval, rval => lval.wrapping_add(rval)) ),
            Opcode::AddSat8                 (..) => Some( operation8!(lval, rval => lval.saturating_add(rval)) ),
            Opcode::AddCarry8               (..) => Some( operation8!(lval, rval => lval.checked_add(rval).map_or(1, |_| 0)) ),
            Opcode::AddSign8                (..) => Some( operation8!(lval, rval => if lval.wrapping_add(rval) > 0x7f {0xff} else {0})),
            Opcode::Sub8                    (..) => Some( operation8!(lval, rval => lval.wrapping_sub(rval))),
            Opcode::SubSat8                 (..) => Some( operation8!(lval, rval => (lval as i8).saturating_sub(rval as i8))),
            Opcode::CompareCarry8           (..) => Some( operation8!(lval, rval => lval.checked_sub(rval).map_or(0xff, |_| 0))),
            Opcode::CompareEq8              (..) => Some( operation8!(lval, rval => if lval != rval {0xff} else {0})),
            Opcode::SubRev8                 (..) => Some( operation8!(lval, rval => rval.wrapping_sub(lval))),
            Opcode::SubSatRev8              (..) => Some( operation8!(lval, rval => (rval as i8).saturating_sub(lval as i8))),
            Opcode::CompareCarryRev8        (..) => Some( operation8!(lval, rval => rval.checked_sub(lval).map_or(0xff, |_| 0))),
            Opcode::Add16                   (..) => Some(operation16!(lval, rval => lval.wrapping_add(rval))),
            Opcode::AddSat16                (..) => Some(operation16!(lval, rval => lval.saturating_add(rval))),
            Opcode::AddCarry16              (..) => Some(operation16!(lval, rval => lval.checked_add(rval).map_or(1, |_| 0))),
            Opcode::AddSign16               (..) => Some(operation16!(lval, rval => if lval.wrapping_add(rval) > 0x7fff {0xffff} else {0})),
            Opcode::Sub16                   (..) => Some(operation16!(lval, rval => lval.wrapping_sub(rval))),
            Opcode::SubSat16                (..) => Some(operation16!(lval, rval => (lval as i16).saturating_sub(rval as i16))),
            Opcode::CompareCarry16          (..) => Some(operation16!(lval, rval => lval.checked_sub(rval).map_or(0xffff, |_| 0))),
            Opcode::CompareEq16             (..) => Some(operation16!(lval, rval => if lval != rval {0xffff} else {0})),
            Opcode::SubRev16                (..) => Some(operation16!(lval, rval => rval.wrapping_sub(lval))),
            Opcode::SubSatRev16             (..) => Some(operation16!(lval, rval => (rval as i16).wrapping_sub(lval as i16))),
            Opcode::CompareCarryRev16       (..) => Some(operation16!(lval, rval => rval.checked_sub(lval).map_or(0xffff, |_| 0))),
            Opcode::SetOne                  (..) => Some(VMRegister{ x: 0xffff, y: 0xffff, z: 0xffff, w: 0xffff }),
            Opcode::SetZero                 (..) => Some(VMRegister{ x: 0, y: 0, z: 0, w: 0}),
            Opcode::Src                     (..) => Some(self.lval),
            Opcode::NotSrc                  (..) => Some(VMRegister{ x: !self.lval.x, y: !self.lval.y, z: !self.lval.z, w: !self.lval.w, }),
            Opcode::NotDest                 (..) => Some(VMRegister{ x: !self.rval.x, y: !self.rval.y, z: !self.rval.z, w: !self.rval.w, }),
            Opcode::Xor                     (..) => Some(operation16!(lval, rval =>   lval ^ rval)),
            Opcode::Xnor                    (..) => Some(operation16!(lval, rval =>  !lval ^ rval)),
            Opcode::Or                      (..) => Some(operation16!(lval, rval =>   lval | rval)),
            Opcode::Nor                     (..) => Some(operation16!(lval, rval => !(lval | rval))),
            Opcode::And                     (..) => Some(operation16!(lval, rval =>   lval & rval)),
            Opcode::Nand                    (..) => Some(operation16!(lval, rval => !(lval & rval))),
            Opcode::SrcAndNotDest           (..) => Some(operation16!(lval, rval =>   lval & !rval)),
            Opcode::NotSrcAndDest           (..) => Some(operation16!(lval, rval =>  !lval & rval)),
            Opcode::SrcOrNotDest            (..) => Some(operation16!(lval, rval =>   lval | !rval)),
            Opcode::NotSrcOrDest            (..) => Some(operation16!(lval, rval =>  !lval | rval)),
            Opcode::LShift8               (_, _) => Some( operation8!(lval, rval => rval << (lval & 7))),
            Opcode::RLogiShift8           (_, _) => Some( operation8!(lval, rval => rval >> (lval & 7))),
            Opcode::RArithShift8          (_, _) => Some( operation8!(lval, rval => (rval as i8) >> (lval & 7))),
            Opcode::LRotate8              (_, _) => Some( operation8!(lval, rval => rval.rotate_left(lval as u32 & 7))),
            Opcode::LIShift8              (_, _) => Some( operation8!(lval, rval => rval << (8u8.wrapping_sub(lval) & 7))),
            Opcode::RILogiShift8          (_, _) => Some( operation8!(lval, rval => rval >> (8u8.wrapping_sub(lval) & 7))),
            Opcode::RIArithShift8         (_, _) => Some( operation8!(lval, rval => (rval as i8) >> (8u8.wrapping_sub(lval) & 7))),
            Opcode::RRotate8              (_, _) => Some( operation8!(lval, rval => rval.rotate_right(lval as u32 & 7))),
            Opcode::LShiftLit8      (src_val, _) => Some( operation8!(   _, rval => rval << src_val)),
            Opcode::RLogiShiftLit8  (src_val, _) => Some( operation8!(   _, rval => rval >> src_val)),
            Opcode::RArithShiftLit8 (src_val, _) => Some( operation8!(   _, rval => (rval as i8) >> src_val)),
            Opcode::RotateLit8      (src_val, _) => Some( operation8!(   _, rval => rval.rotate_left(src_val as u32))),
            Opcode::LShift16              (_, _) => Some(operation16!(lval, rval => rval << (lval & 15))),
            Opcode::RLogiShift16          (_, _) => Some(operation16!(lval, rval => rval >> (lval & 15))),
            Opcode::RArithShift16         (_, _) => Some(operation16!(lval, rval => (rval as i16) >> (lval & 15))),
            Opcode::LRotate16             (_, _) => Some(operation16!(lval, rval => rval.rotate_left(lval as u32 & 15))),
            Opcode::LIShift16             (_, _) => Some(operation16!(lval, rval => rval << (16u16.wrapping_sub(lval) & 15))),
            Opcode::RILogiShift16         (_, _) => Some(operation16!(lval, rval => rval >> (16u16.wrapping_sub(lval) & 15))),
            Opcode::RIArithShift16        (_, _) => Some(operation16!(lval, rval => (rval as i16) >> (16u16.wrapping_sub(lval) & 15))),
            Opcode::RRotate16             (_, _) => Some(operation16!(lval, rval => rval.rotate_left(16u16.wrapping_sub(lval) as u32 & 15))),
            Opcode::LShiftLit16     (src_val, _) => Some(operation16!(   _, rval => rval << src_val)),
            Opcode::RLogiShiftLit16 (src_val, _) => Some(operation16!(   _, rval => rval >> src_val)),
            Opcode::RArithShiftLit16(src_val, _) => Some(operation16!(   _, rval => (rval as i16) >> src_val)),
            Opcode::RotateLit16     (src_val, _) => Some(operation16!(   _, rval => rval.rotate_right(src_val as u32))),
        } {
            if let Some(wreg) = self.reg_index_mut(dst.into()) {
                *wreg = wval;
            }
        })
    }
}

type MemoryType = [(u16, u16); 2048];
type VMProcType = Box<VMProc>;
pub struct VMUser {
    proc: VMProcType,
}
impl VMUser {
    fn new() -> Self {
        Self {
            proc: Box::new(VMProc::new()),
        }
    }
}

pub struct SimulationVM {
    users: HashMap<u64, Box<VMUser>>,
    processes: VecDeque<*mut VMProc>,
    memory: MemoryType,
}
pub trait VMUserWrite {
    fn user_write(&mut self, user: u64, addr: u16, value: u16);
}
unsafe impl Send for SimulationVM {}

use std::iter::Iterator;
pub struct VMThreads<'a> {
    vm: &'a SimulationVM,
    iter: std::collections::vec_deque::Iter<'a, *mut VMProc>
}
impl Iterator for VMThreads<'_> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let proc = unsafe { self.iter.next()?.as_ref()? };
        let val = proc.read_mem(self.vm, proc.ins_ptr.x);
        Some(format!("{} {:4x}: {}", proc, val, Opcode::parse(val)))
    }
}
impl VMUserWrite for SimulationVM {
    fn user_write(&mut self, user: u64, addr: u16, value: u16) {
        let user = self.make_user(user);
        user.proc.write_priv(addr, value);
    }
}

impl SimulationVM {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            users: HashMap::with_capacity(VM_INIT_USER_COUNT),
            processes: VecDeque::with_capacity(VM_INIT_PROC_COUNT),
            memory: [(0,0); 2048],
        })
    }
    pub fn make_user<'a>(&'a mut self, user: u64) -> &'a mut Box<VMUser> {
        if !self.users.contains_key(&user) {
            self.users.insert(user, Box::new(VMUser::new()));
        }
        self.users.get_mut(&user).unwrap()
    }
    pub fn user_run(&mut self, user: u64) {
        let user = self.make_user(user);
        let proc_addr: *mut VMProc = Box::as_mut(&mut user.proc);
        user.proc.is_running = true;
        if !self.processes.contains(&proc_addr) {
            self.processes.push_back(proc_addr);
        }
    }
    pub fn sys_halt_all(&mut self) {
        for (_, user) in self.users.iter_mut() {
            user.proc.is_running = false;
        }
        for (v, _) in self.memory.iter_mut() {
            *v = 0;
        }
    }
    pub fn user_halt(&mut self, user: u64) {
        let user = self.make_user(user);
        user.proc.is_running = false;
    }
    pub fn user_reset(&mut self, user: u64) {
        let user = self.make_user(user);
        *user.proc = VMProc::new();
    }
    pub fn user_restart(&mut self, user: u64) {
        let user = self.make_user(user);
        user.proc.ins_ptr.x = 0x40;
    }
    pub fn user_dump(&mut self, user: u64) {
        let user = self.make_user(user);
        user.proc.dump();
    }
    pub fn memory_invalidate(&mut self) {
        for m in self.memory.iter_mut() {
            m.1 = !m.0
        }
    }
    pub fn threads(&mut self) -> VMThreads {
        VMThreads{
            vm: self,
            iter: self.processes.iter()
        }
    }
    pub fn tick(&mut self, tick_count: usize) -> Arc<[u8]> {
        let queue_size = self.processes.len();
        let mut ticks = 0;
        while ticks < tick_count {
            let mut proc_index = 0;
            while proc_index < queue_size {
                if let Some(proc) = self.processes.pop_front() {
                    let proc_ref = unsafe { &mut *proc };
                    if proc_ref.is_running {
                        match proc_ref.run(self) {
                            Ok(()) => { self.processes.push_back(proc); },
                            Err(_) => { proc_ref.is_running = false; }
                        }
                    }
                    proc_index += 1;
                } else { break }
            }
            ticks += 1;
        }
        let mut out: Vec<u8> = Vec::new();
        let mut offset = 0u32;
        let mut runlength = 0u32;
        let mut runpos = 0usize;
        for (val, old) in self.memory.iter_mut() {
            const MEMSIZE: u32 = 1;
            if val == old {
                let mut newoffset = offset + MEMSIZE;
                if runlength > 0 {
                    out[runpos] = runlength as u8;
                    runlength = 0;
                }
                if newoffset > u8::MAX as u32 {
                    out.push(offset as u8); // skip forward
                    out.push(0); // no data
                    newoffset -= offset;
                }
                offset = newoffset;
            } else {
                *old = *val;
                let mut newlength = runlength + MEMSIZE;
                if runlength == 0 {
                    out.push(offset as u8);
                    runpos = out.len(); out.push(0u8); // placeholder for runlength
                } else if newlength > u8::MAX as u32 {
                    out.push(0u8); // no offset from the last block
                    out[runpos] = runlength as u8;
                    newlength -= runlength;
                    runpos = out.len(); out.push(0u8); // placeholder for runlength
                }
                out.push(*val as u8);
                out.push((*val >> 8) as u8);
                offset = 0;
                runlength = newlength;
            }
        }
        if runlength > 0 {
            out[runpos] = runlength as u8;
        }
        Arc::from(out)
    }
}

pub fn vm_write(split: &mut std::str::SplitWhitespace, sim_vm: &mut dyn VMUserWrite, user_id: u64, start_addr: u16) -> () {
    let mut val: u16 = 0;
    let mut addr = start_addr;
    let order = [12u32,8,4,0];
    let mut ofs_index = 0u32;
    let mut command_break = false;
    let mut last_written = 0u16;
    for omnom in split {
        for bite in omnom.chars() {
            if let Some(hex_val) = match bite {
                '0'..='9' => Some((bite as u8 - b'0') as u16),
                // abcdef
                'A'..='F' => Some((bite as u8 - b'A' + 10) as u16),
                'a'..='f' => Some((bite as u8 - b'a' + 10) as u16),
                // uvwxyz
                'U'..='Z' => Some((bite as u8 - b'u' + 10) as u16),
                'u'..='z' => Some((bite as u8 - b'u' + 10) as u16),
                // skip forward in the address space
                // by N
                'ᚢ' => {
                    let ofs = order[ofs_index as usize];
                    val >>= ofs;
                    addr = addr.wrapping_add(val);
                    ofs_index = 0;
                    val = 0;
                    None
                }
                // skip forward a word without writting
                // and without affecting input
                'ᚨ' => {
                    addr = addr.wrapping_add(1);
                    None
                }
                // write 0 words, N times
                'ᚠ' => {
                    let ofs = order[ofs_index as usize];
                    val >>= ofs;
                    if val == 0 { val = 1; }
                    while val > 0 {
                        sim_vm.user_write(user_id, addr, 0);
                        addr = addr.wrapping_add(1);
                        val -= 1;
                    }
                    ofs_index = 0;
                    val = 0;
                    None
                }
                // repeat the "last written" value 1 or N times
                'ᚱ' => {
                    let ofs = order[ofs_index as usize];
                    val >>= ofs;
                    if val == 0 { val = 1; }
                    while val > 0 {
                        sim_vm.user_write(user_id, addr, last_written);
                        addr = addr.wrapping_add(1);
                        val -= 1;
                    }
                    ofs_index = 0;
                    val = 0;
                    None
                }
                // right align and write current value
                '×' => {
                    let ofs = order[ofs_index as usize];
                    val >>= ofs;
                    last_written = val;
                    sim_vm.user_write(user_id, addr, val);
                    addr = addr.wrapping_add(1);
                    ofs_index = 0;
                    None
                },
                // left align and write current value
                'ᚲ' => {
                    last_written = val;
                    sim_vm.user_write(user_id, addr, val);
                    addr = addr.wrapping_add(1);
                    ofs_index = 0;
                    None
                }
                // alternate hex data, 0-F equiv
                // ᚺᚾ ᛁᛃ ᛈᛇ ᛉᛊ ᛏᛒ ᛖᛗ ᛚᛜ ᛞᛟ
                'ᚺ' => Some(0), 'ᚾ' => Some(1), 'ᛁ' => Some(2), 'ᛃ' => Some(3),
                'ᛈ' => Some(4), 'ᛇ' => Some(5), 'ᛉ' => Some(6), 'ᛊ' => Some(7),
                'ᛏ' => Some(8), 'ᛒ' => Some(9), 'ᛖ' => Some(10), 'ᛗ' => Some(11),
                'ᛚ' => Some(12), 'ᛜ' => Some(13), 'ᛞ' => Some(14), 'ᛟ' => Some(15),
                '!' => { command_break = true; None }
                _ => None
            } {
                let ofs = order[ofs_index as usize];
                val |= hex_val << ofs;
                ofs_index += 1;
            }
            if ofs_index >= 4 {
                last_written = val;
                sim_vm.user_write(user_id, addr, val);
                val = 0;
                addr += 1;
                ofs_index = 0;
            }
        }
        if command_break { break }
    }
    if ofs_index > 0 {
        sim_vm.user_write(user_id, addr, val);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    #[test]
    fn saturates() {
        let a: u8 = 0x60u8;
        let b: u8 = 0xe0u8;
        assert_eq!(0x80u16, (b as i8).saturating_sub(a as i8) as u8 as u16);
        assert_eq!(0xffu16, b.saturating_add(a) as u16);
        assert_eq!(0x40u16, b.wrapping_add(a) as u16);
    }
    #[test]
    fn shifts() {
        let rval = VMRegister{ x: 0x1111, y:0x8181, z:0, w:0 };
        let lval = VMRegister{ x: 0x0104, y:0x0407, z:0, w:0 };
        let x =
           (((rval.index8(0) as i8) >> (lval.index8(0) & 7)) as u8 as u16)
        | ((((rval.index8(1) as i8) >> (lval.index8(1) & 7)) as u8 as u16) << 8);
        let y =
           (((rval.index8(2) as i8) >> (lval.index8(2) & 7)) as u8 as u16)
        | ((((rval.index8(3) as i8) >> (lval.index8(3) & 7)) as u8 as u16) << 8);
        assert_eq!(x, 0x0801);
        assert_eq!(y, 0xf8ff);
    }
    #[test]
    fn ins_parse() -> anyhow::Result<()> {
        let op = Opcode::parse(0x3215);
        if let Opcode::Swizzle(dst, tx, ty, tz, tw) = op {
            writeln!(std::io::stderr(), "{}", op)?;
            assert_eq!(dst, RegIndex::C3, "destination field parsed wrong");
        } else {
            panic!("parsed wrong opcode");
        }
        Ok(())
    }
    struct TestVM {
        expectations: Vec<Option<u16>>,
    }
    impl VMUserWrite for TestVM {
        fn user_write(&mut self, user: u64, addr: u16, value: u16) {
            let expect_value = self.expectations.get(addr as usize).unwrap_or(&None);
            assert_eq!((addr, expect_value), (addr, &Some(value)));
        }
    }
    impl TestVM {
        fn expect_at(&mut self, addr: u16, val: u16) {
            if addr as usize >= self.expectations.len() {
                self.expectations.resize_with(1usize + addr as usize, || None);
            }
            self.expectations[addr as usize] = Some(val);
        }
    }

    #[test]
    fn test_write() {
        let mut vm = TestVM{expectations:vec![
            Some(0xdead), Some(0xbeef),
        ]};
        let mut split =
            "deadbeef! ".split_whitespace();
        vm_write(&mut split, &mut vm, 0, 0);
        split = "dead beef! ".split_whitespace();
        vm_write(&mut split, &mut vm, 0, 0);
        split = "dead beef ! ".split_whitespace();
        vm_write(&mut split, &mut vm, 0, 0);
        split = "deadbeef!".split_whitespace();
        vm_write(&mut split, &mut vm, 0, 0);
        split = "deadbeef !".split_whitespace();
        vm_write(&mut split, &mut vm, 0, 0);
        vm = TestVM{expectations:vec![
            Some(0x0123), Some(0x4567),
            Some(0x89ab), Some(0xcdef),
        ]};
        split = "ᚺᚾ ᛁᛃ ᛈᛇ ᛉᛊ ᛏᛒ ᛖᛗ ᛚᛜ ᛞᛟ".split_whitespace();
        vm_write(&mut split, &mut vm, 0, 0);
    }
}

