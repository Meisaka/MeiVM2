mod persist;
mod register;
mod opcode;

use std::{
    collections::{HashMap, VecDeque}, fmt::Display, sync::Arc
};
use serde::{
    de::{self, Visitor},
    ser::{self, Serializer},
    Serialize, Deserialize,
};
use register::VMRegister;
use opcode::{ Opcode, VMError, RegIndex, Swizzle };
pub use persist::{write_persist, read_persist};

const VM_INIT_USER_COUNT: usize = 128;
const VM_INIT_PROC_COUNT: usize = 128;
const MEM_PRIV_NV_START: u16 = 0x0040;
const MEM_PRIV_NV_START_U: usize = MEM_PRIV_NV_START as usize;
const MEM_PRIV_NVT_END: u16 = 0x0100;
const MEM_PRIV_NV_END: u16 = 0x0300;
const MEM_PRIV_NV_SIZE: usize = 192;
const MEM_PRIV_IO_START: u16 = 0x0300;
const MEM_PRIV_IO_END: u16 = 0x0400;
const MEM_PRIV_RA_START: u16 = 0x400;
const MEM_PRIV_RA_END: u16 = 0x800;
const MEM_PRIV_V_START: u16 = 0x800;
const MEM_PRIV_V_END: u16 = 0x1000;
const MEM_PRIV_AREA_END: u16 = 0x1000;
const MEM_SHARED_START: u16 = 0x1000;
const MEM_SHARED_SIZE: u16 = 0x2000;
const MEM_SHARED_END: u16 = MEM_SHARED_START + MEM_SHARED_SIZE;
const MEM_SHARED_START_U: usize = MEM_SHARED_START as usize;
const MEM_SHARED_SIZE_U: usize = MEM_SHARED_SIZE as usize;
const WORD_DELAY_PRIV_TO_SHARED: u32 = 64;
const WORD_DELAY_PRIV_FROM_SHARED: u32 = 16;

fn inc_addr(addr: u16) -> u16 {
    let next = addr.wrapping_add(1);
    if addr < MEM_PRIV_NV_END {
        if next >= MEM_PRIV_NV_END { MEM_PRIV_NV_START } else { next }
    } else if addr < MEM_PRIV_IO_END {
        if next >= MEM_PRIV_IO_END { MEM_PRIV_IO_START } else { next }
    } else if addr < MEM_PRIV_RA_END {
        if next >= MEM_PRIV_RA_END { MEM_PRIV_RA_START } else { next }
    } else if addr < MEM_PRIV_V_END {
        if next >= MEM_PRIV_V_END { MEM_PRIV_V_START } else { next }
    } else if addr < MEM_SHARED_END {
        if next >= MEM_SHARED_END { MEM_SHARED_START } else { next }
    } else {
        MEM_SHARED_START
    }
}

fn is_priv_mem(addr: u16) -> bool {
    addr < MEM_PRIV_AREA_END
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
enum DeferredOp {
    DelayLoad(u32, Swizzle, Swizzle, RegIndex, Option<RegIndex>),
    DelayGather(u32, Swizzle, Swizzle, RegIndex, Option<RegIndex>),
    DelayStore(u32, Swizzle, Swizzle, Option<RegIndex>),
    DelayScatter(u32, Swizzle, Swizzle, Option<RegIndex>),
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct VMProc {
    pub cval: [VMRegister; 8],
    pub reg: [VMRegister; 7],
    pub ins_ptr: VMRegister,
    #[serde(deserialize_with = "deserialize_vmproc_mem", serialize_with = "serialize_vmproc_mem")]
    pub priv_mem: [u16; MEM_PRIV_NV_SIZE],
    sleep_for: u32,
    lval: VMRegister,
    rval: VMRegister,
    defer: Option<DeferredOp>,
    pub is_running: bool,
}

fn serialize_vmproc_mem<S>(value: &[u16; MEM_PRIV_NV_SIZE], serializer: S) -> Result<S::Ok, S::Error> where S: Serializer {
    let v: &[u8; MEM_PRIV_NV_SIZE * 2] = unsafe { std::mem::transmute(value) };
    serializer.serialize_bytes(v)
}
fn deserialize_vmproc_mem<'de, D>(deserializer: D) -> Result<[u16; MEM_PRIV_NV_SIZE], D::Error>
    where D: de::Deserializer<'de> {
    struct LoadMem;
    impl<'de> Visitor<'de> for LoadMem {
        type Value = [u16; MEM_PRIV_NV_SIZE];
        fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
            write!(formatter, "array of bytes")
        }
        fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
            where E: de::Error {
            let mut out = [0u16; MEM_PRIV_NV_SIZE];
            for (index, block) in v.chunks(2).enumerate() {
                if index >= out.len() { break }
                if block.len() == 2 { out[index] = (block[0] as u16) | ((block[1] as u16) << 8); }
                else if block.len() == 1 { out[index] = block[0] as u16; }
            }
            Ok(out)
        }
        fn visit_seq<A>(self, mut seq: A) -> Result<Self::Value, A::Error>
            where A: de::SeqAccess<'de> {
            let mut out = [0u16; MEM_PRIV_NV_SIZE];
            let mut index = 0;
            while let Some(elem) = seq.next_element()? {
                out[index] = elem;
                index += 1;
                if index >= out.len() { break }
            }
            Ok(out)
        }
    }
    deserializer.deserialize_bytes(LoadMem)
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

impl VMProc {
    fn new(proc_id: u16) -> Self {
        Self {
            cval: [
                VMRegister::default(),
                VMRegister::default(),
                VMRegister::default(),
                VMRegister::default(),
                VMRegister{x:1, y:1, z:1, w:1},
                VMRegister{x:0b1_1111, y:0b11_1111, z:0b1_1111, w:0},
                VMRegister{x:11, y:5, z:0, w:0},
                VMRegister{
                    x:MEM_SHARED_START, y:MEM_PRIV_NV_START,
                    z:MEM_PRIV_IO_START, w:MEM_PRIV_IO_START + proc_id * 4},
            ],
            lval: VMRegister::default(),
            rval: VMRegister::default(),
            defer: None,
            reg: [VMRegister::default(); 7],
            ins_ptr: VMRegister{x:MEM_PRIV_NV_START, y:0, z:0, w:0}, // reg15.x is instruction pointer
            priv_mem: [0; MEM_PRIV_NV_SIZE],
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
                mi + MEM_PRIV_NV_START_U, Opcode::parse(self.priv_mem[mi]));
            ri += 2;
            mi += 1;
        }
        ri = 0;
        for rc in self.reg.chunks_exact(2) {
            println!("VM r{}: {}  r{}: {}  {:02x}: {}", ri, rc[0], ri + 1, rc[1],
                mi + MEM_PRIV_NV_START_U, Opcode::parse(self.priv_mem[mi]));
            ri += 2;
            mi += 1;
        }
        println!("VM r6: {}  ri: {}  {:02x}: {}", self.reg[6], self.ins_ptr,
            mi + MEM_PRIV_NV_START_U, Opcode::parse(self.priv_mem[mi]));
        mi += 1;
        for (mc, m) in self.priv_mem.chunks_exact(8).enumerate() {
            println!("VM m{:03x}: {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x} {:04x}     {:02x}: {}",
                MEM_PRIV_NV_START_U + mc * 8, m[0], m[1], m[2], m[3], m[4], m[5], m[6], m[7],
                mi + MEM_PRIV_NV_START_U, Opcode::parse(self.priv_mem[mi]));
            mi += 1;
        }
    }
    fn read_mem(&self, vm: &SimulationVM, addr: u16) -> u16 {
        if addr < 0x40 {
            self.reg_index((addr >> 2).into()).index(addr as u8)
        } else if addr < MEM_PRIV_NVT_END {
            self.priv_mem[addr as usize - MEM_PRIV_NV_START_U]
        } else if addr >= MEM_SHARED_START && addr < MEM_SHARED_END {
            vm.memory[(addr as usize).wrapping_sub(MEM_SHARED_START_U)].0
        } else { 0 }
    }
    fn write_mem(&mut self, vm: &mut SimulationVM, addr: u16, value: u16) {
        if addr < 0x40 {
            if let Some(reg) = self.reg_index_mut((addr >> 2).into()) {
                *reg.index_mut(addr as u8) = value;
            }
        } else if addr < MEM_PRIV_NVT_END {
            self.priv_mem[addr as usize - MEM_PRIV_NV_START_U] = value;
        } else if addr >= MEM_SHARED_START && addr < MEM_SHARED_END {
            vm.memory[(addr as usize).wrapping_sub(MEM_SHARED_START_U)].0 = value;
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
            #[cfg(test)]
            eprintln!("Deferred: {:?} {lval} {rval}", op);
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
            Opcode::WMove(_, _, sel) => {
                self.rval.x = match sel {
                    Swizzle::X => self.lval.x,
                    Swizzle::Y => self.lval.y,
                    Swizzle::Z => self.lval.z,
                    Swizzle::W => self.lval.w,
                };
                Some(self.rval)
            }
            Opcode::WSwap(src, _, sel) => {
                let mut lval = self.lval;
                match sel {
                    Swizzle::X => std::mem::swap(&mut self.rval.x, &mut lval.x),
                    Swizzle::Y => std::mem::swap(&mut self.rval.x, &mut lval.y),
                    Swizzle::Z => std::mem::swap(&mut self.rval.x, &mut lval.z),
                    Swizzle::W => std::mem::swap(&mut self.rval.x, &mut lval.w),
                };
                self.reg_index_mut(src).map(|l| *l = lval );
                Some(self.rval)
            }
            Opcode::WAdd(_, _, sel) => {
                self.rval.x = self.rval.x.wrapping_add(match sel {
                    Swizzle::X => self.lval.x,
                    Swizzle::Y => self.lval.y,
                    Swizzle::Z => self.lval.z,
                    Swizzle::W => self.lval.w,
                });
                Some(self.rval)
            }
            Opcode::WSub(_, _, sel) => {
                self.rval.x = self.rval.x.wrapping_sub(match sel {
                    Swizzle::X => self.lval.x,
                    Swizzle::Y => self.lval.y,
                    Swizzle::Z => self.lval.z,
                    Swizzle::W => self.lval.w,
                });
                Some(self.rval)
            }
            Opcode::Extra2 => Err(VMError::TempestH)?,
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
                #[cfg(test)]
                eprintln!("priv:pc:{},s:{} {:?} {} {}", is_priv_exec, is_priv_mem(self.lval.x), opc, self.lval, self.rval);
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
            Opcode::Add8          (..) => Some( operation8!(lval, rval => lval.wrapping_add(rval)) ),
            Opcode::Sub8          (..) => Some( operation8!(lval, rval => lval.wrapping_sub(rval)) ),
            Opcode::RSub8         (..) => Some( operation8!(lval, rval => rval.wrapping_sub(lval)) ),
            Opcode::Eq8           (..) => Some( operation8!(lval, rval => if lval == rval {0xff} else {0})),
            Opcode::NotEq8        (..) => Some( operation8!(lval, rval => if lval != rval {0xff} else {0})),
            Opcode::Carry8        (..) => Some( operation8!(lval, rval => lval.checked_add(rval).map_or(0xff, |_| 0)) ),
            Opcode::LessU8        (..) => Some( operation8!(lval, rval => lval.checked_sub(rval).map_or(0xff, |_| 0)) ),
            Opcode::LessEqU8      (..) => Some( operation8!(lval, rval => rval.checked_sub(lval).map_or(0, |_| 0xff))),
            Opcode::GreaterU8     (..) => Some( operation8!(lval, rval => rval.checked_sub(lval).map_or(0xff, |_| 0)) ),
            Opcode::GreaterEqU8   (..) => Some( operation8!(lval, rval => lval.checked_sub(rval).map_or(0, |_| 0xff)) ),
            Opcode::AddOver8      (..) => Some( operation8!(lval, rval => (lval as i8).checked_add(rval as i8).map_or(0xff, |_| 0)) ),
            Opcode::SubOver8      (..) => Some( operation8!(lval, rval => (lval as i8).checked_sub(rval as i8).map_or(0xff, |_| 0)) ),
            Opcode::RSubOver8     (..) => Some( operation8!(lval, rval => (rval as i8).checked_sub(lval as i8).map_or(0xff, |_| 0)) ),
            Opcode::AddSat8       (..) => Some( operation8!(lval, rval => (lval as i8).saturating_add(rval as i8)) ),
            Opcode::SubSat8       (..) => Some( operation8!(lval, rval => (lval as i8).saturating_sub(rval as i8)) ),
            Opcode::RSubSat8      (..) => Some( operation8!(lval, rval => (rval as i8).saturating_sub(lval as i8)) ),

            Opcode::Add16         (..) => Some(operation16!(lval, rval => lval.wrapping_add(rval))),
            Opcode::Sub16         (..) => Some(operation16!(lval, rval => lval.wrapping_sub(rval))),
            Opcode::RSub16        (..) => Some(operation16!(lval, rval => rval.wrapping_sub(lval))),
            Opcode::Eq16          (..) => Some(operation16!(lval, rval => if lval == rval {0xffff} else {0})),
            Opcode::NotEq16       (..) => Some(operation16!(lval, rval => if lval != rval {0xffff} else {0})),
            Opcode::Carry16       (..) => Some(operation16!(lval, rval => lval.checked_add(rval).map_or(0xffff, |_| 0))),
            Opcode::LessU16       (..) => Some(operation16!(lval, rval => lval.checked_sub(rval).map_or(0xffff, |_| 0))),
            Opcode::LessEqU16     (..) => Some(operation16!(lval, rval => rval.checked_sub(lval).map_or(0, |_| 0xffff))),
            Opcode::GreaterU16    (..) => Some(operation16!(lval, rval => rval.checked_sub(lval).map_or(0xffff, |_| 0))),
            Opcode::GreaterEqU16  (..) => Some(operation16!(lval, rval => lval.checked_sub(rval).map_or(0, |_| 0xffff))),
            Opcode::AddOver16     (..) => Some(operation16!(lval, rval => (lval as i16).checked_add(rval as i16).map_or(0xffff, |_| 0)) ),
            Opcode::SubOver16     (..) => Some(operation16!(lval, rval => (lval as i16).checked_sub(rval as i16).map_or(0xffff, |_| 0)) ),
            Opcode::RSubOver16    (..) => Some(operation16!(lval, rval => (rval as i16).checked_sub(lval as i16).map_or(0xffff, |_| 0)) ),
            Opcode::AddSat16      (..) => Some(operation16!(lval, rval => (lval as i16).saturating_add(rval as i16))),
            Opcode::SubSat16      (..) => Some(operation16!(lval, rval => (lval as i16).saturating_sub(rval as i16))),
            Opcode::RSubSat16     (..) => Some(operation16!(lval, rval => (rval as i16).saturating_sub(lval as i16))),
            Opcode::SetAll        (..) => Some(VMRegister{ x: 0xffff, y: 0xffff, z: 0xffff, w: 0xffff }),
            Opcode::SetOne        (..) => Some(VMRegister{ x: 1, y: 1, z: 1, w: 1}),
            Opcode::Swap          (..) => {
                let rval = self.rval;
                self.reg_index_mut(src.into()).map(|r| *r = rval );
                Some(self.lval)
            },
            Opcode::NotSrc        (..) => Some(VMRegister{ x: !self.lval.x, y: !self.lval.y, z: !self.lval.z, w: !self.lval.w, }),
            Opcode::NotDest       (..) => Some(VMRegister{ x: !self.rval.x, y: !self.rval.y, z: !self.rval.z, w: !self.rval.w, }),
            Opcode::Xor           (..) => Some(operation16!(lval, rval =>   lval ^ rval)),
            Opcode::Xnor          (..) => Some(operation16!(lval, rval =>  !lval ^ rval)),
            Opcode::Or            (..) => Some(operation16!(lval, rval =>   lval | rval)),
            Opcode::Nor           (..) => Some(operation16!(lval, rval => !(lval | rval))),
            Opcode::And           (..) => Some(operation16!(lval, rval =>   lval & rval)),
            Opcode::Nand          (..) => Some(operation16!(lval, rval => !(lval & rval))),
            Opcode::AndNot        (..) => Some(operation16!(lval, rval =>   lval & !rval)),
            Opcode::AndNotS       (..) => Some(operation16!(lval, rval =>  !lval & rval)),
            Opcode::OrNot         (..) => Some(operation16!(lval, rval =>   lval | !rval)),
            Opcode::OrNotS        (..) => Some(operation16!(lval, rval =>  !lval | rval)),
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

pub type MemoryType = [(u16, u16); MEM_SHARED_SIZE_U];
pub type VMProcType = Box<VMProc>;

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct VMShip {
    pub x: f32, pub y: f32,
    vel_x: f32, vel_y: f32,
    #[serde(default)]
    accel_x: f32,
    #[serde(default)]
    accel_y: f32,
    pub heading: f32,
    spin: f32,
    pub color: u16,
    #[serde(default)]
    pub color_mix: u16,
}
impl Default for VMShip {
    fn default() -> Self {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let direction = rng.gen_range(0.0..core::f32::consts::TAU);
        let speed: f32 = rng.gen_range(10.0..100.0);
        Self {
            x: rng.gen_range(0.0..1920.0),
            y: rng.gen_range(0.0..256.0),
            vel_x: direction.cos() * speed, vel_y: direction.sin() * speed,
            accel_x: 0.0, accel_y: 0.0,
            spin: rng.gen_range(-20.0..20.0),
            heading: rng.gen_range(0.0..1.0),
            color_mix: 0,
            color: 0xffff,
        }
    }
}
impl Display for VMShip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "VMShip([{}, {}], [{}, {}] {:4x} {:4x})",
            self.x, self.y, self.vel_x, self.vel_y,
            (self.heading * 32768.0) as u16, self.color
            )
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct VMUserContext {
    #[serde(default)]
    pub ship: VMShip,
    #[serde(default)]
    pub user_login: String,
    #[serde(default)]
    pub user_name: String,
    #[serde(default)]
    pub user_color: u32,
    #[serde(default)]
    pub user_color_loaded: bool,
}

impl VMUserContext {
    pub fn dump(&self) {
        println!("Context: {} {:6x}", self.user_login, self.user_color);
        println!("Ship: {}", self.ship);
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct VMUser {
    #[serde(default = "default_thread0")]
    pub proc: VMProcType,
    #[serde(default = "default_thread1")]
    pub agent: VMProcType,
    #[serde(flatten)]
    pub context: VMUserContext,
}
fn default_thread0() -> Box<VMProc> {
    Box::new(VMProc::new(0))
}
fn default_thread1() -> Box<VMProc> {
    Box::new(VMProc::new(1))
}

impl VMUser {
    fn new() -> Self {
        Self {
            proc: default_thread0(),
            agent: default_thread1(),
            context: VMUserContext {
                ship: VMShip::default(),
                user_login: String::default(),
                user_name: String::default(),
                user_color: 0xffffff,
                user_color_loaded: false,
            }
        }
    }
}

pub struct SimulationVM {
    users: HashMap<u64, Box<VMUser>>,
    processes: VecDeque<*mut VMUser>,
    memory: MemoryType,
}
impl<'de> Deserialize<'de> for SimulationVM {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: de::Deserializer<'de> {
        struct VisitSimulationVM;
        impl<'de> de::Visitor<'de> for VisitSimulationVM {
            type Value = SimulationVM;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "struct SimulationVM")
            }
            fn visit_map<A>(self, mut map: A) -> Result<Self::Value, A::Error>
                where
                    A: de::MapAccess<'de>, {
                let mut users: HashMap<u64, Box<VMUser>> = HashMap::new();
                let mut memory = [(0,0); MEM_SHARED_SIZE_U];
                while let Some(key) = map.next_key()? {
                    match key {
                        "memory" => {
                            let memory_val: &[u8] = map.next_value()?;
                            for (index, &b) in memory_val.iter().enumerate() {
                                memory[index / 2].0 |= if (index & 1) != 0 { (b as u16) << 8 } else { b as u16 };
                            }
                        }
                        "users" => { users = map.next_value()?; }
                        _ => {}
                    }
                }
                let mut vm = SimulationVM {
                    users,
                    processes: VecDeque::with_capacity(VM_INIT_PROC_COUNT),
                    memory,
                };
                for user in vm.users.values_mut() {
                    if user.proc.is_running || user.agent.is_running {
                        let user_addr: *mut VMUser = Box::as_mut(user);
                        if !vm.processes.contains(&user_addr) {
                            vm.processes.push_back(user_addr);
                        }
                    }
                }
                Ok(vm)
            }
        }
        deserializer.deserialize_struct("SimulationVM", &["memory", "users"], VisitSimulationVM)
    }
}

impl Serialize for SimulationVM {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        use ser::SerializeStruct;
        let mut out = serializer.serialize_struct("SimulationVM", 2)?;
        struct MemBlock([u8; MEM_SHARED_SIZE_U * 2]);
        let mut memory_block = [0u8; MEM_SHARED_SIZE_U * 2];
        for (index, (w, _)) in self.memory.iter().enumerate() {
            memory_block[index * 2] = *w as u8;
            memory_block[index * 2 + 1] = (*w >> 8) as u8;
        }
        impl Serialize for MemBlock {
            fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
            where S: Serializer { serializer.serialize_bytes(&self.0) }
        }
        out.serialize_field("memory", &MemBlock(memory_block))?;
        out.serialize_field("users", &self.users)?;
        out.end()
    }
}

pub trait VMWritePriv {
    fn write_priv(&mut self, addr: u16, value: u16);
}
unsafe impl Send for SimulationVM {}

use std::iter::Iterator;
pub struct VMThreads<'a> {
    vm: &'a SimulationVM,
    iter: std::collections::vec_deque::Iter<'a, *mut VMUser>
}
impl Iterator for VMThreads<'_> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let user = unsafe { self.iter.next()?.as_ref()? };
        let val = user.proc.read_mem(self.vm, user.proc.ins_ptr.x);
        Some(format!("{} {:4x}: {}", user.proc, val, Opcode::parse(val)))
    }
}

impl VMWritePriv for VMProc {
    fn write_priv(&mut self, addr: u16, value: u16) {
        if addr < 0x40 {
            let reg = self.reg_index_priv_mut((addr >> 2).into());
            *reg.index_mut(addr as u8) = value;
        } else if addr < MEM_PRIV_NVT_END {
            self.priv_mem[addr as usize - MEM_PRIV_NV_START_U] = value;
        }
    }
}

impl SimulationVM {
    pub fn new() -> Box<Self> {
        Box::new(Self {
            users: HashMap::with_capacity(VM_INIT_USER_COUNT),
            processes: VecDeque::with_capacity(VM_INIT_PROC_COUNT),
            memory: [(0,0); MEM_SHARED_SIZE_U],
        })
    }
    pub fn find_user<'a>(&'a mut self, user: u64) -> Option<&'a mut Box<VMUser>> {
        if !self.users.contains_key(&user) {
            self.users.insert(user, Box::new(VMUser::new()));
        }
        self.users.get_mut(&user)
    }
    pub fn make_user<'a>(&'a mut self, user: u64) -> &'a mut Box<VMUser> {
        if !self.users.contains_key(&user) {
            self.users.insert(user, Box::new(VMUser::new()));
        }
        self.users.get_mut(&user).unwrap()
    }
    pub fn sys_halt_all(&mut self) {
        for (_, user) in self.users.iter_mut() {
            user.proc.is_running = false;
            user.agent.is_running = false;
        }
        for (v, _) in self.memory.iter_mut() {
            *v = 0;
        }
    }
    pub fn user_run(&mut self, user: u64) {
        let user = self.make_user(user);
        let user_addr: *mut VMUser = Box::as_mut(user);
        user.proc.is_running = true;
        if !self.processes.contains(&user_addr) {
            self.processes.push_back(user_addr);
        }
    }
    pub fn user_halt(&mut self, user: u64) {
        let user = self.make_user(user);
        user.proc.is_running = false;
    }
    pub fn user_reset(&mut self, user: u64) {
        let user = self.make_user(user);
        *user.proc = VMProc::new(0);
    }
    pub fn user_restart(&mut self, user: u64) {
        let user = self.make_user(user);
        user.proc.ins_ptr.x = 0x40;
    }
    pub fn agent_run(&mut self, user: u64) {
        let user = self.make_user(user);
        let user_addr: *mut VMUser = Box::as_mut(user);
        user.agent.is_running = true;
        if !self.processes.contains(&user_addr) {
            self.processes.push_back(user_addr);
        }
    }
    pub fn agent_halt(&mut self, user: u64) {
        let user = self.make_user(user);
        user.agent.is_running = false;
    }
    pub fn agent_reset(&mut self, user: u64) {
        let user = self.make_user(user);
        *user.agent = VMProc::new(1);
    }
    pub fn agent_restart(&mut self, user: u64) {
        let user = self.make_user(user);
        user.agent.ins_ptr.x = 0x40;
    }
    pub fn user_new(&mut self, user: u64) -> &mut Box<VMUser> {
        self.make_user(user)
    }
    pub fn user_find(&mut self, user: u64) -> Option<&mut Box<VMUser>> {
        self.find_user(user)
    }
    pub fn user_new_with_update(&mut self, user: u64, user_name: String, user_login: String, user_color: u32) {
        let user = self.make_user(user);
        user.context.user_name = user_name;
        user.context.user_login = user_login;
        user.context.user_color = user_color;
        if !user.context.user_color_loaded {
            user.context.user_color_loaded = true;
            let (r, g, b) = ((user_color >> 16) as u8, (user_color >> 8) as u8, user_color as u8);
            user.context.ship.color =
                (((r & 0b011111) as u16) << 11) |
                (((g & 0b111111) as u16) << 5) |
                ((b & 0b011111) as u16);
        }
    }
    pub fn user_update_if_exists(&mut self, user: u64, user_name: String, user_login: String, user_color: u32) {
        let Some(user) = self.find_user(user) else { return };
        user.context.user_name = user_name;
        user.context.user_login = user_login;
        user.context.user_color = user_color;
        if !user.context.user_color_loaded {
            user.context.user_color_loaded = true;
            let (r, g, b) = ((user_color >> 16) as u8, (user_color >> 8) as u8, user_color as u8);
            user.context.ship.color =
                (((r & 0b011111) as u16) << 11) |
                (((g & 0b111111) as u16) << 5) |
                ((b & 0b011111) as u16);
        }
    }
    pub fn user_dump(&mut self, user: u64) {
        let user = self.make_user(user);
        user.context.dump();
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
    pub fn tick(&mut self, tick_count: usize) -> Arc<Vec<u8>> {
        let queue_size = self.processes.len();
        let mut ticks = 0;
        let delta_time = 0.018f32;
        let delta_time_s = delta_time * delta_time;
        for (_, user) in self.users.iter_mut() {
            let ship = &mut user.context.ship;
            ship.x += ship.vel_x * delta_time + ship.accel_x * delta_time_s;
            ship.y += ship.vel_y * delta_time + ship.accel_y * delta_time_s;
            ship.vel_x += ship.accel_x * delta_time;
            ship.vel_y += ship.accel_y * delta_time;
            ship.heading += ship.spin * delta_time;
            const MARGIN: f32 = 16.0;
            const RIGHT_EDGE: f32 = 1920.0 + MARGIN * 2.0;
            const BOTTOM_EDGE: f32 = 256.0 + MARGIN * 2.0;
            if ship.heading < 0.0 { ship.heading += 1.0 }
            if ship.heading >= 1.0 { ship.heading -= 1.0 }
            if ship.x < 0.0 { ship.x += RIGHT_EDGE }
            if ship.x > RIGHT_EDGE { ship.x -= RIGHT_EDGE }
            if ship.y < 0.0 { ship.y += BOTTOM_EDGE }
            if ship.y > BOTTOM_EDGE { ship.y -= BOTTOM_EDGE }
        }
        while ticks < tick_count {
            let mut proc_index = 0;
            while proc_index < queue_size {
                if let Some(user_proc) = self.processes.pop_front() {
                    let user_ref = unsafe { &mut *user_proc };
                    let still_running = if user_ref.proc.is_running {
                        match user_ref.proc.run(self) {
                            Ok(()) => true,
                            Err(_) => { user_ref.proc.is_running = false; false }
                        }
                    } else {false} | if user_ref.agent.is_running {
                        match user_ref.agent.run(self) {
                            Ok(()) => true,
                            Err(_) => { user_ref.agent.is_running = false; false }
                        }
                    } else {false};
                    if still_running {
                        // reschedule
                        self.processes.push_back(user_proc);
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
            runlength = 0;
        }
        out.push(offset as u8); //offset = 0;
        runpos = out.len(); out.push(0u8); // placeholder for runlength
        for (_, user) in self.users.iter() {
            let ship = &user.context.ship;
            let x = ship.x as i16 as u16;
            let y = ship.y as i16 as u16;
            let h = (ship.heading * 32768.0) as i16 as u16;
            let c = ship.color;
            out.push(x as u8);
            out.push((x >> 8) as u8);
            out.push(y as u8);
            out.push((y >> 8) as u8);
            out.push(h as u8);
            out.push((h >> 8) as u8);
            out.push(c as u8);
            out.push((c >> 8) as u8);
            runlength += 4;
        }
        if runlength > 0 {
            out[runpos] = runlength as u8;
            //runlength = 0;
        }
        Arc::new(out)
    }
}

pub fn vm_write(split: &mut std::str::SplitWhitespace, vmproc: &mut dyn VMWritePriv, start_addr: u16) -> () {
    let mut val: u16 = 0;
    let mut addr = start_addr;
    let order = [12u32,8,4,0];
    let unorder = [0, 12u32,8,4,0];
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
                'U'..='Z' => Some((bite as u8 - b'U' + 10) as u16),
                'u'..='z' => Some((bite as u8 - b'u' + 10) as u16),
                // skip forward in the address space
                // by N
                'ᚢ' => {
                    let ofs = unorder[ofs_index as usize];
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
                    let ofs = unorder[ofs_index as usize];
                    val >>= ofs;
                    if val == 0 { val = 1; }
                    while val > 0 {
                        vmproc.write_priv(addr, 0);
                        addr = addr.wrapping_add(1);
                        val -= 1;
                    }
                    ofs_index = 0;
                    val = 0;
                    None
                }
                // repeat the "last written" value 1 or N times
                'ᚱ' => {
                    let ofs = unorder[ofs_index as usize];
                    val >>= ofs;
                    if val == 0 { val = 1; }
                    while val > 0 {
                        vmproc.write_priv(addr, last_written);
                        addr = addr.wrapping_add(1);
                        val -= 1;
                    }
                    ofs_index = 0;
                    val = 0;
                    None
                }
                // right align and write current value
                '×' => {
                    let ofs = unorder[ofs_index as usize];
                    val >>= ofs;
                    last_written = val;
                    vmproc.write_priv(addr, val);
                    addr = addr.wrapping_add(1);
                    ofs_index = 0;
                    val = 0;
                    None
                },
                // left align and write current value
                'ᚲ' => {
                    last_written = val;
                    vmproc.write_priv(addr, val);
                    addr = addr.wrapping_add(1);
                    ofs_index = 0;
                    val = 0;
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
                vmproc.write_priv(addr, val);
                val = 0;
                addr += 1;
                ofs_index = 0;
            }
        }
        if command_break { break }
    }
    if ofs_index > 0 {
        vmproc.write_priv(addr, val);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
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
        assert_eq!(Opcode::parse(0x3214), Opcode::Move(RegIndex::C2, RegIndex::C3, 1));
        assert_eq!(Opcode::parse(0x3215), Opcode::Swizzle(RegIndex::C3, Swizzle::Y, Swizzle::X, Swizzle::Z, Swizzle::X));
        assert_eq!(Opcode::parse(0x3206), Opcode::Load(RegIndex::C2, RegIndex::C3, Swizzle::W));
        assert_eq!(Opcode::parse(0x32C6), Opcode::Load(RegIndex::C2, RegIndex::C3, Swizzle::X));
        assert_eq!(Opcode::parse(0x3216), Opcode::LoadInc(RegIndex::C2, RegIndex::C3, Swizzle::W));
        assert_eq!(Opcode::parse(0x3226), Opcode::LoadGather(RegIndex::C2, RegIndex::C3, Swizzle::W));
        assert_eq!(Opcode::parse(0x3236), Opcode::LoadGatherInc(RegIndex::C2, RegIndex::C3, Swizzle::W));
        assert_eq!(Opcode::parse(0x3208), Opcode::Add8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x3218), Opcode::Sub8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x3228), Opcode::RSub8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x3238), Opcode::Eq8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x3248), Opcode::Carry8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x3258), Opcode::LessU8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x3268), Opcode::GreaterU8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x3278), Opcode::NotEq8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x3288), Opcode::AddSat8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x3298), Opcode::SubSat8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x32a8), Opcode::RSubSat8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x32b8), Opcode::GreaterEqU8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x32c8), Opcode::AddOver8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x32d8), Opcode::SubOver8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x32e8), Opcode::RSubOver8(RegIndex::C2, RegIndex::C3));
        assert_eq!(Opcode::parse(0x32f8), Opcode::LessEqU8(RegIndex::C2, RegIndex::C3));
        //
        assert_eq!(Opcode::parse(0x3209), Opcode::Add16(RegIndex::C2, RegIndex::C3));
        Ok(())
    }
    fn vm_set_shared(vm: &mut SimulationVM, to_shared: &[u16], offset: usize) {
        for (index, &value) in to_shared.iter().enumerate() {
            vm.memory[index + offset].0 = value;
        }
    }
    fn vm_wait_run(vm: &mut SimulationVM) -> (&[(u16, u16); MEM_SHARED_SIZE_U], &Box<VMProc>) {
        vm.user_run(0);
        let mut t = 0;
        while t < 10000 {
            vm.tick(40);
            let u = vm.users.get(&0).expect("thread 0");
            if !u.proc.is_running { break }
            t += 40;
        }
        let u = vm.users.get(&0).expect("thread 0");
        assert!(!u.proc.is_running);
        (&vm.memory, &u.proc)
    }
    fn vm_setup(to_write: &[u16], to_code: &[u16]) -> Box<SimulationVM> {
        let mut vm = SimulationVM::new();
        let user = vm.user_new(0);
        for (index, &value) in to_write.iter().enumerate() {
            user.proc.write_priv(index as u16, value);
        }
        for (index, &value) in to_code.iter().enumerate() {
            user.proc.write_priv(0x40 + index as u16, value);
        }
        vm
    }
    #[test]
    fn ins_wselect() {
        let to_write = &[
            1,2,4,8,
            0x10,0x10,0x10,0x10,
        ];
        let to_code = &[
            0x8001, // WMove.x r0, c0
            0x8935, // Swizzle.zyxw r0
            0x8041, // WMove.y r0, c0
            0x8935, // Swizzle.zyxw r0
            0x8081, // WMove.z r0, c0
            0x8935, // Swizzle.zyxw r0
            0x80c1, // WMove.w r0, c0
            //0x8935, // Swizzle.zyxw r0
            0x9021, // WAdd.x r1, c0
            0x9061, // WAdd.y r1, c0
            0x90a1, // WAdd.z r1, c0
            0x90e1, // WAdd.w r1, c0
            0xa104, // Move r2, c1
            0xa031, // WSub.x r2, c0
            0xa071, // WSub.y r2, c0
            0xa0b1, // WSub.z r2, c0
            0xa0f1, // WSub.w r2, c0
            0xf074, // Move.w ri, c0
            0xb104, // Move r3, c1
            0xc004, // Move r4, c0
            0xd004, // Move r5, c0
            0xe004, // Move r6, c0
            0xbc11, // WSwap.x r3, r4
            0xbd51, // WSwap.y r3, r5
            0xbe91, // WSwap.z r3, r6
            0xbfd1, // WSwap.w r3, r7
        ];
        let mut vm = vm_setup(to_write, to_code);
        let to_shared = &[ ];
        vm_set_shared(&mut vm, to_shared, 0);
        let (_, proc) = vm_wait_run(&mut vm);
        let reg = proc.reg;
        let pc = proc.ins_ptr;
        assert_eq!(reg[0], VMRegister{x:8, y:4, z:2, w:1});
        assert_eq!(reg[1], VMRegister{x:15, y:0, z:0, w:0});
        assert_eq!(reg[2], VMRegister{x:1, y:16, z:16, w:16});
        assert_eq!(reg[3], VMRegister{x:8, y:16, z:16, w:16});
        assert_eq!(reg[4], VMRegister{x:16, y:2, z:4, w:8});
        assert_eq!(reg[5], VMRegister{x:1, y:1, z:4, w:8});
        assert_eq!(reg[6], VMRegister{x:1, y:2, z:2, w:8});
        assert_eq!(    pc, VMRegister{x:pc.x, y:0, z:0, w:4});
    }
    #[test]
    fn ins_mem_load() {
        let to_write = &[
            0,1,1,1,
            0x20, 2, 2, 2, // -> r0
            0x40, 3, 3, 3,
            MEM_SHARED_START, 0,0,0,
            0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
            // r0
            4,4,4,4,
        ];
        let to_code = &[
            0x9006, // Load [c0], r1  move c0 to r1 via memory load
                    // r1 should be 0,1,1,1
            0xa106, // Load [c1], r2  move r0 to r2 via memory load
                    // r2 should be 4,4,4,4
            0xb206, // Load [c2], r3  move priv ins to r3
                    // r3 should be to_code[0..4]
            0xc306, // Load [c3], r4  move shared ins to r4 via cross load
                    // r4 should be to_shared[0..4]
            0xf304, // Move ri = c3
        ];
        let mut vm = vm_setup(to_write, to_code);
        let to_shared = &[
            0xd306, // Load [c3], r5  move shared ins to r5
                    // r5 should be to_shared[0..4]
            0xe106, // Load [c1], r6  move r0 to r6 via cross memory load
                    // r6 should be 4,4,4,4
            0x8206, // Load [c2], r0  move priv ins to r0 via cross memory load
                    // r0 should be to_code[0..4]
            0xf006, // Load [c0], ri  move c0 to r7 via cross memory load
                    // r7 should be 1,1,1,1 after halt
        ];
        vm_set_shared(&mut vm, to_shared, 0);
        let (_, proc) = vm_wait_run(&mut vm);
        let reg = proc.reg;
        let pc = proc.ins_ptr;
        // r1 should be 0,1,1,1
        // r2 should be 4,4,4,4
        // r3 should be to_code[0..4]
        // r4 should be to_shared[0..4]
        // r5 should be to_shared[0..4]
        // r6 should be 4,4,4,4
        // r0 should be to_code[0..4]
        // r7 should be 1,1,1,1 after halt
        assert_eq!(reg[1], VMRegister{x:0, y:1, z:1, w:1});
        assert_eq!(reg[2], VMRegister{x:4, y:4, z:4, w:4});
        assert_eq!(reg[3], VMRegister{x:to_code[0], y:to_code[1], z:to_code[2], w:to_code[3]});
        assert_eq!(reg[4], VMRegister{x:to_shared[0], y:to_shared[1], z:to_shared[2], w:to_shared[3]});
        assert_eq!(    pc, VMRegister{x:1, y:1, z:1, w:1});
        assert_eq!(reg[6], VMRegister{x:4, y:4, z:4, w:4});
        assert_eq!(reg[0], VMRegister{x:to_code[0], y:to_code[1], z:to_code[2], w:to_code[3]});
        assert_eq!(reg[5], VMRegister{x:to_shared[0], y:to_shared[1], z:to_shared[2], w:to_shared[3]});
    }
    #[test]
    fn ins_mem_store() {
        let to_write = &[
            // stored into shared:
            0x0307, // Store [c3], c0
            0x0407, // Store [c4], c0
            0, 0, // Halt
            MEM_SHARED_START, MEM_SHARED_START,
            MEM_SHARED_START, MEM_SHARED_START,
            MEM_PRIV_NV_START+0x20, MEM_PRIV_NV_START+0x20,
            MEM_PRIV_NV_START+0x20, MEM_PRIV_NV_START+0x20,
            MEM_SHARED_START+0x4, MEM_SHARED_START+0x4,
            MEM_SHARED_START+0x4, MEM_SHARED_START+0x4,
            MEM_PRIV_NV_START+0x24, MEM_PRIV_NV_START+0x24,
            MEM_PRIV_NV_START+0x24, MEM_PRIV_NV_START+0x24,
        ];
        let to_code = &[
            0x0107, // Store [c1], c0
            0x0207, // Store [c2], c0
            0xf104, // Move ri = c1
        ];
        let mut vm = vm_setup(to_write, to_code);
        let (vmm, proc) = vm_wait_run(&mut vm);
        assert!(proc.ins_ptr.x > MEM_SHARED_START);
        eprintln!("ins: {:x}", proc.ins_ptr.x);
        assert_eq!(
            (vmm[0x000].0, vmm[0x001].0, vmm[0x004].0, vmm[0x005].0),
            (to_write[0], to_write[1], to_write[0], to_write[1]) );
        let m = &proc.priv_mem;
        assert_eq!(m[0x20], to_write[0]);
        assert_eq!(m[0x21], to_write[1]);
        assert_eq!(m[0x24], to_write[0]);
        assert_eq!(m[0x25], to_write[1]);
    }
    #[test]
    fn ins_mem_store_inc() {
        let to_write = &[
            // stored into shared:
            0x0817, // Store [r0+], c0
            0x0917, // Store [r1+], c0
            0, 0, // halt
            MEM_SHARED_START,0,0,0, 0,0,0,0, 0,0,0,0,
            0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
            // r8
            MEM_SHARED_START, MEM_SHARED_START,
            MEM_SHARED_START, MEM_SHARED_START,
            MEM_PRIV_NV_START+0x20, MEM_PRIV_NV_START+0x20,
            MEM_PRIV_NV_START+0x20, MEM_PRIV_NV_START+0x20,
        ];
        let to_code = &[
            0x0817, // Store [r0+], c0
            0x0917, // Store [r1+], c0
            0xf104, // move ri = c1
        ];
        let mut vm = vm_setup(to_write, to_code);
        let (vmm, proc) = vm_wait_run(&mut vm);
        assert!(proc.ins_ptr.x > MEM_SHARED_START);
        eprintln!("ins: {:x}", proc.ins_ptr.x);
        assert_eq!(
            (vmm[0x000].0, vmm[0x001].0, vmm[0x004].0, vmm[0x005].0),
            (to_write[0], to_write[1], to_write[0], to_write[1]) );
        let m = &proc.priv_mem;
        assert_eq!(m[0x20], to_write[0]);
        assert_eq!(m[0x21], to_write[1]);
        assert_eq!(m[0x24], to_write[0]);
        assert_eq!(m[0x25], to_write[1]);
        assert_eq!(proc.reg[0].x, to_write[0x20]+8);
        assert_eq!(proc.reg[0].y, to_write[0x21]);
        assert_eq!(proc.reg[0].z, to_write[0x22]);
        assert_eq!(proc.reg[0].w, to_write[0x23]);
        assert_eq!(proc.reg[1].x, to_write[0x24]+8);
        assert_eq!(proc.reg[1].y, to_write[0x25]);
        assert_eq!(proc.reg[1].z, to_write[0x26]);
        assert_eq!(proc.reg[1].w, to_write[0x27]);
    }
    #[test]
    fn ins_mem_scatter() {
        let to_write = &[
            // stored into shared:
            0x0a27, // Store *[r2], c0
            0x0b27, // Store *[r3], c0
            0, 0, // halt
            MEM_SHARED_START,0,0,0, 0,0,0,0, 0,0,0,0,
            0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
            // r8
            MEM_SHARED_START+0x0, MEM_SHARED_START+0x1,
            MEM_SHARED_START+0x2, MEM_SHARED_START+0x3,
            MEM_PRIV_NV_START+0x20, MEM_PRIV_NV_START+0x21,
            MEM_PRIV_NV_START+0x22, MEM_PRIV_NV_START+0x23,
            MEM_SHARED_START+0x4, MEM_SHARED_START+0x5,
            MEM_SHARED_START+0x6, MEM_SHARED_START+0x7,
            MEM_PRIV_NV_START+0x24, MEM_PRIV_NV_START+0x25,
            MEM_PRIV_NV_START+0x26, MEM_PRIV_NV_START+0x27,
        ];
        let to_code = &[
            0x0827, // Store *[r0], c0
            0x0927, // Store *[r1], c0
            0xf104, // Move ri = c1
        ];
        let mut vm = vm_setup(to_write, to_code);
        let (vmm, proc) = vm_wait_run(&mut vm);
        assert!(proc.ins_ptr.x > MEM_SHARED_START);
        eprintln!("ins: {:x}", proc.ins_ptr.x);
        assert_eq!(
            (vmm[0x000].0, vmm[0x001].0, vmm[0x004].0, vmm[0x005].0),
            (to_write[0], to_write[1], to_write[0], to_write[1]) );
        let m = &proc.priv_mem;
        assert_eq!(m[0x20], to_write[0]);
        assert_eq!(m[0x21], to_write[1]);
        assert_eq!(m[0x24], to_write[0]);
        assert_eq!(m[0x25], to_write[1]);
        assert_eq!(proc.reg[0].x, to_write[0x20]);
        assert_eq!(proc.reg[0].y, to_write[0x21]);
        assert_eq!(proc.reg[0].z, to_write[0x22]);
        assert_eq!(proc.reg[0].w, to_write[0x23]);
        assert_eq!(proc.reg[1].x, to_write[0x24]);
        assert_eq!(proc.reg[1].y, to_write[0x25]);
        assert_eq!(proc.reg[1].z, to_write[0x26]);
        assert_eq!(proc.reg[1].w, to_write[0x27]);
    }
    #[test]
    fn ins_mem_scatter_inc() {
        let to_write = &[
            // stored into shared:
            0x0a37, // Store *[r2+], c0
            0x0b37, // Store *[r3+], c0
            0, 0, // halt
            MEM_SHARED_START,0,0,0, 0,0,0,0, 0,0,0,0,
            0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0,
            // r8
            MEM_SHARED_START+0x0, MEM_SHARED_START+0x1,
            MEM_SHARED_START+0x2, MEM_SHARED_START+0x3,
            MEM_PRIV_NV_START+0x20, MEM_PRIV_NV_START+0x21,
            MEM_PRIV_NV_START+0x22, MEM_PRIV_NV_START+0x23,
            MEM_SHARED_START+0x4, MEM_SHARED_START+0x5,
            MEM_SHARED_START+0x6, MEM_SHARED_START+0x7,
            MEM_PRIV_NV_START+0x24, MEM_PRIV_NV_START+0x25,
            MEM_PRIV_NV_START+0x26, MEM_PRIV_NV_START+0x27,
        ];
        let to_code = &[
            0x0837, // Store *[r0+], c0
            0x0937, // Store *[r1+], c0
            0xf104, // Move ri = c1
        ];
        let mut vm = vm_setup(to_write, to_code);
        let (vmm, proc) = vm_wait_run(&mut vm);
        assert!(proc.ins_ptr.x > MEM_SHARED_START);
        eprintln!("ins: {:x}", proc.ins_ptr.x);
        assert_eq!(
            (vmm[0x000].0, vmm[0x001].0, vmm[0x004].0, vmm[0x005].0),
            (to_write[0], to_write[1], to_write[0], to_write[1]) );
        let m = &proc.priv_mem;
        assert_eq!(m[0x20], to_write[0]);
        assert_eq!(m[0x21], to_write[1]);
        assert_eq!(m[0x24], to_write[0]);
        assert_eq!(m[0x25], to_write[1]);
        assert_eq!(proc.reg[0].x, to_write[0x20]+1);
        assert_eq!(proc.reg[0].y, to_write[0x21]+1);
        assert_eq!(proc.reg[0].z, to_write[0x22]+1);
        assert_eq!(proc.reg[0].w, to_write[0x23]+1);
        assert_eq!(proc.reg[1].x, to_write[0x24]+1);
        assert_eq!(proc.reg[1].y, to_write[0x25]+1);
        assert_eq!(proc.reg[1].z, to_write[0x26]+1);
        assert_eq!(proc.reg[1].w, to_write[0x27]+1);
    }
    struct TestVM {
        expectations: Vec<Option<u16>>,
    }
    impl VMWritePriv for TestVM {
        fn write_priv(&mut self, addr: u16, value: u16) {
            let expect_value = self.expectations.get(addr as usize).unwrap_or(&None);
            assert_eq!((addr, expect_value), (addr, &Some(value)));
        }
    }

    fn test_write_run(test_string: &str, exp: Vec<Option<u16>>) {
        let mut vm = TestVM{expectations: exp};
        let mut split = test_string.split_whitespace();
        vm_write(&mut split, &mut vm, 0);
    }
    #[test]
    fn test_write_simple() {
        let beef = vec![ Some(0xdead), Some(0xbeef), ];
        test_write_run("deadbeef! ", beef.clone());
        test_write_run("dead beef! ", beef.clone());
        test_write_run("dead beef ! ", beef.clone());
        test_write_run("deadbeef!", beef.clone());
        test_write_run("deadbeef !", beef.clone());
        test_write_run("ᚺᚾ ᛁᛃ ᛈᛇ ᛉᛊ ᛏᛒ ᛖᛗ ᛚᛜ ᛞᛟ",
            vec![ Some(0x0123), Some(0x4567), Some(0x89ab), Some(0xcdef), ]);
    }
    #[test]
    fn test_write_runic_basic() {
        test_write_run("ᛖ×ᚨᛒᚱᛁᚢᛞᚲ!",
            vec![
            Some(0x000a), None,
            Some(0x000a), Some(0x000a), Some(0x000a),
            Some(0x000a), Some(0x000a), Some(0x000a),
            Some(0x000a), Some(0x000a), Some(0x000a),
            None, None, Some(0xe000),
        ]);
    }
    #[test]
    fn test_write_runic_skips() {
        // skip forward in the address space
        // by N
        test_write_run("12341ᚢ4531",
            vec![ Some(0x1234), None, Some(0x4531) ]);
        // skip forward a word without writting
        // and without affecting input
        test_write_run("123456ᚨ78",
            vec![ Some(0x1234), None, Some(0x5678) ]);
    }
    #[test]
    fn test_write_runic_fills() {
        // write 0 words, N times
        test_write_run("4ᚠ", vec![Some(0), Some(0), Some(0), Some(0)]);
        test_write_run("12ᚠ", vec![
            Some(0), Some(0), Some(0), Some(0),
            Some(0), Some(0), Some(0), Some(0),
            Some(0), Some(0), Some(0), Some(0),
            Some(0), Some(0), Some(0), Some(0),
            Some(0), Some(0),
        ]);
        test_write_run("4ᚠ1234", vec![Some(0), Some(0), Some(0), Some(0), Some(0x1234)]);
        // repeat the "last written" value {1}N times
        test_write_run("12345ᚱ", vec![
            Some(0x1234), Some(0x1234), Some(0x1234),
            Some(0x1234), Some(0x1234), Some(0x1234),
        ]);
        // repeat the "last written" value 1 times
        test_write_run("1234ᚱ", vec![
            Some(0x1234), Some(0x1234),
        ]);
        // the ᚠ rune does not affect "last written"
        test_write_run("12344ᚠ2ᚱ", vec![
            Some(0x1234), // inital sets "last"
            Some(0), Some(0), Some(0), Some(0), // fill zero
            Some(0x1234), Some(0x1234), // repeat 2 of "last"
        ]);
    }
    #[test]
    fn test_write_runic_short_l() {
        // left align and write current value
        test_write_run("12ᚲ3400", vec![Some(0x1200), Some(0x3400)]);
        // left align and write current value
        test_write_run("1ᚲ3400", vec![Some(0x1000), Some(0x3400)]);
        // left align and write current value
        test_write_run("125ᚲ3400", vec![Some(0x1250), Some(0x3400)]);
    }
    #[test]
    fn test_write_runic_short_r() {
        // right align and write current value
        test_write_run("12×3400", vec![Some(0x0012), Some(0x3400)]);
        // right align and write current value
        test_write_run("1×3400", vec![Some(0x0001), Some(0x3400)]);
        // right align and write current value
        test_write_run("125×3400", vec![Some(0x0125), Some(0x3400)]);
    }

    fn reflective_persist_test(state: &SimulationVM) {
        let the_data = write_persist(state).expect("serialization without errors");
        let mut reflected: SimulationVM = read_persist(the_data.as_slice()).expect("deserialization without errors");
        reflected.memory_invalidate();
        assert_eq!(&state.users, &reflected.users);
        assert_eq!(&state.memory, &reflected.memory);
    }
    #[test]
    fn persist_vm() {
        let mut users = HashMap::new();
        users.insert(9230529035839, Box::new(VMUser{
            proc: Box::new(VMProc {
                cval: [VMRegister{x:342, y: 92, z: 1000, w: 32905}; 8],
                reg: [VMRegister{x:2, y: 1, z: 3, w: 6}; 7],
                lval: VMRegister{x:342, y: 92, z: 1000, w: 32905},
                rval: VMRegister{x:493, y: 0, z: 0, w: 0x8000},
                ins_ptr: VMRegister{x: 1010, y: 230, z: 2333, w: 0xffff},
                sleep_for: 22,
                defer: Some(DeferredOp::DelayGather(340, Swizzle::W, Swizzle::X, RegIndex::R4, Some(RegIndex::R5))),
                priv_mem: [0; MEM_PRIV_NV_SIZE],
                is_running: true,
            }),
            agent: Box::new(VMProc {
                cval: [VMRegister{x:342, y: 92, z: 1000, w: 32905}; 8],
                reg: [VMRegister{x:2, y: 1, z: 3, w: 6}; 7],
                lval: VMRegister{x:342, y: 92, z: 1000, w: 32905},
                rval: VMRegister{x:493, y: 0, z: 0, w: 0x8000},
                ins_ptr: VMRegister{x: 1010, y: 230, z: 2333, w: 0xffff},
                sleep_for: 22,
                defer: Some(DeferredOp::DelayGather(340, Swizzle::W, Swizzle::X, RegIndex::R4, Some(RegIndex::R5))),
                priv_mem: [0; MEM_PRIV_NV_SIZE],
                is_running: true,
            }),
            context: VMUserContext {
                ship: VMShip::default(),
                user_color_loaded: true,
                user_color: 0xcccccc,
                user_name: String::from("Test"),
                user_login: String::from("test"),
            }
        }));
        let mut state = SimulationVM {
            users, processes: VecDeque::new(),
            memory: [(4, 4); MEM_SHARED_SIZE_U]
        };
        state.memory_invalidate();
        reflective_persist_test(&state);
    }
}

