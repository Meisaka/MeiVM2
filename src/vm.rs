pub mod persist;
pub mod register;
pub mod opcode;

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
fn is_nta_mem(addr: u16) -> bool {
    addr > MEM_PRIV_NV_END && addr < MEM_PRIV_AREA_END
}
fn is_shared_mem(addr: u16) -> bool {
    addr > MEM_PRIV_AREA_END
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq)]
enum DeferredOp {
    DelayLoad(u8, RegIndex),
    DelayStore(u8),
    WriteBack(RegIndex),
    WriteBack2(RegIndex, RegIndex),
}

#[derive(Debug, Serialize, Deserialize, PartialEq, Eq, Clone, Copy)]
pub enum MemoryProtect {
    Off = 0,
    ReadOnly = 1,
    ReadZero = 2,
    AccessException = 3,
}
impl From<u16> for MemoryProtect {
    fn from(value: u16) -> Self {
        match value & 3 {
            0 => Self::Off,
            1 => Self::ReadOnly,
            2 => Self::ReadZero,
            3 => Self::AccessException,
            _ => unreachable!("MemoryProtect")
        }
    }
}
impl Default for MemoryProtect {
    fn default() -> Self { Self::Off }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct VMProtections {
    pub pma_from_nta: MemoryProtect,
    pub pma_from_sma: MemoryProtect,
    pub nta_from_sma: MemoryProtect,
    pub reg_saves: bool,
    pub core_control: bool,
    pub in_pma: bool, // PC in private mem
    pub in_nta: bool, // PC in private mem, but not thread private
    pub in_shared: bool, // PC in shared mem
    pub exec_halt_inval: bool,
    pub exec_sleep: bool,
}

impl From<u16> for VMProtections {
    fn from(value: u16) -> Self {
        Self {
            pma_from_nta: MemoryProtect::from(value >> 0),
            pma_from_sma: MemoryProtect::from(value >> 2),
            nta_from_sma: MemoryProtect::from(value >> 4),
            reg_saves:       0 != value & (1<<6),
            core_control:    0 != value & (1<<7),
            in_pma:          0 != value & (1<<8),
            in_nta:          0 != value & (1<<9),
            in_shared:       0 != value & (1<<10),
            exec_halt_inval: 0 != value & (1<<12),
            exec_sleep:      0 != value & (1<<13),
        }
    }
}

impl VMProtections {
    pub fn to_u16(&self) -> u16 {
        ((self.pma_from_nta as u16) << 0)
        | ((self.pma_from_sma as u16) << 2)
        | ((self.nta_from_sma as u16) << 4)
        | if self.reg_saves       {1<<6} else {0}
        | if self.core_control    {1<<7} else {0}
        | if self.in_pma          {1<<8} else {0}
        | if self.in_nta          {1<<9} else {0}
        | if self.in_shared       {1<<10} else {0}
        | if self.exec_halt_inval {1<<12} else {0}
        | if self.exec_sleep      {1<<13} else {0}
    }
}
impl ser::Serialize for VMProtections {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where S: Serializer {
        serializer.serialize_u16(self.to_u16())
    }
}
impl<'de> de::Deserialize<'de> for VMProtections {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where D: de::Deserializer<'de> {
        struct Visit;
        impl<'de> de::Visitor<'de> for Visit {
            type Value = VMProtections;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "an integer containing protection bitflags")
            }
            fn visit_i64<E>(self, v: i64) -> Result<Self::Value, E>
                where E: de::Error, {
                Ok(VMProtections::from(v as u16))
            }
            fn visit_u64<E>(self, v: u64) -> Result<Self::Value, E>
                where E: de::Error, {
                Ok(VMProtections::from(v as u16))
            }
        }
        deserializer.deserialize_u16(Visit)
    }
}

pub fn default_module_selects() -> [u16; 7] {
    [0x1000, 0x1001, 0, 0x4000, 0x4040, 0x4050, 0x4051]
}
pub fn default_except_register() -> VMRegister {
    VMRegister{x:MEM_PRIV_IO_START + 0x08, y:0, z:0, w:0}
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct WaveProc {
    pub cval: [VMRegister; 8],
    pub reg: [VMRegister; 7],
    pub ins_ptr: VMRegister,
    #[serde(deserialize_with = "deserialize_vmproc_mem", serialize_with = "serialize_vmproc_mem")]
    pub priv_mem: [u16; MEM_PRIV_NV_SIZE],
    sleep_for: u32,
    lval: VMRegister,
    rval: VMRegister,
    defer: Option<DeferredOp>,
    #[serde(default)]
    pub core_id: u16,
    #[serde(default)]
    pub current_ins_addr: u16,
    #[serde(default)]
    pub is_running: bool,
    #[serde(default)]
    pub save_ri: VMRegister,
    #[serde(default = "default_except_register")]
    pub save_except: VMRegister,
    #[serde(default)]
    pub bank1_select: u16,
    #[serde(default)]
    pub protect: VMProtections,
    #[serde(default = "default_module_selects")]
    pub mod_selects: [u16; 7],
    #[serde(default)]
    pub con_store: VMConsts,
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

impl Display for WaveProc {
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

pub struct Cycle<'v> {
    vm: &'v mut SimulationVM,
    user: *mut VMUser,
}

impl WaveProc {
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
                    z:MEM_PRIV_IO_START, w:MEM_PRIV_IO_START + 0x08},
            ],
            lval: VMRegister::default(),
            rval: VMRegister::default(),
            core_id: (proc_id & 0xff) + 1,
            defer: None,
            reg: [VMRegister::default(); 7],
            ins_ptr: VMRegister{x:MEM_PRIV_NV_START, y:0, z:0, w:0}, // reg15.x is instruction pointer
            priv_mem: [0; MEM_PRIV_NV_SIZE],
            sleep_for: 0,
            is_running: false,
            current_ins_addr: 0,
            bank1_select: 0,
            save_ri: VMRegister::default(),
            save_except: default_except_register(),
            protect: VMProtections::default(),
            mod_selects: default_module_selects(),
            con_store: VMConsts::default(),
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
    fn dump(&self) {
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
    fn read_mem(&mut self, ctx: &mut Cycle, addr: u16) -> u16 {
        match addr {
            0..0x40 => self.reg_index((addr >> 2).into()).index(addr as u8),
            MEM_PRIV_NV_START..MEM_PRIV_NVT_END =>
                self.priv_mem[addr as usize - MEM_PRIV_NV_START_U],
            MEM_PRIV_NVT_END..MEM_PRIV_NV_END => 0,
            MEM_PRIV_IO_START..MEM_PRIV_IO_END =>
                read_io(self, ctx, addr - MEM_PRIV_IO_START),
            MEM_PRIV_RA_START..MEM_PRIV_RA_END => 0,
            MEM_PRIV_V_START..MEM_PRIV_V_END => 0,
            MEM_SHARED_START..MEM_SHARED_END =>
                ctx.vm.memory[(addr as usize).wrapping_sub(MEM_SHARED_START_U)].0,
            _ => 0
        }
    }
    fn write_mem(&mut self, ctx: &mut Cycle, addr: u16, value: u16) {
        match addr {
            0..0x40 =>
            if let Some(reg) = self.reg_index_mut((addr >> 2).into()) {
                *reg.index_mut(addr as u8) = value;
            },
            MEM_PRIV_NV_START..MEM_PRIV_NVT_END => {
                self.priv_mem[addr as usize - MEM_PRIV_NV_START_U] = value;
            }
            MEM_PRIV_NVT_END..MEM_PRIV_NV_END => {
            }
            MEM_PRIV_IO_START..MEM_PRIV_IO_END => {
                write_io(self, ctx, addr - MEM_PRIV_IO_START, value);
            }
            MEM_PRIV_RA_START..MEM_PRIV_RA_END => {
            }
            MEM_PRIV_V_START..MEM_PRIV_V_END => {
            }
            MEM_SHARED_START..MEM_SHARED_END => {
                ctx.vm.memory[(addr as usize).wrapping_sub(MEM_SHARED_START_U)].0 = value;
            }
            _ => {}
        }
    }

    fn exception(&mut self, failure_flavour: VMError) -> Result<(), VMError> {
        self.save_ri = self.ins_ptr;
        self.protect.in_pma = false;
        self.ins_ptr = self.save_except;
        self.save_except = default_except_register();
        if self.ins_ptr.x >= MEM_PRIV_IO_START
            && self.ins_ptr.x < MEM_PRIV_IO_START + 0x20 {
            return Err(failure_flavour)
        }
        return Ok(())
    }

    fn cycle(&mut self, ctx: &mut Cycle) -> Result<(), VMError> {
        if self.sleep_for > 0 {
            self.sleep_for -= 1;
            return Ok(())
        }
        if let Some(op) = self.defer.take() {
            let is_nta_exec = is_nta_mem(self.current_ins_addr);
            let is_shared_exec = is_shared_mem(self.current_ins_addr);
            #[cfg(test)]
            eprintln!("Deferred: {:x?} {} {}", op, self.lval, self.rval);
            match match op {
                DeferredOp::DelayLoad(mut stage, result) => 'op:{
                    debug_assert!(stage != 0);
                    let addr =
                        if 0 != stage & 1 { self.lval.x } else
                        if 0 != stage & 2 { self.lval.y } else
                        if 0 != stage & 4 { self.lval.z } else
                        if 0 != stage & 8 { self.lval.w } else {0};
                    // access permission and delay
                    let (delay, access) =
                    if is_shared_exec == is_shared_mem(addr) {
                        if 0x40 != (stage & 0xc0) {
                            if 0 != stage & 1 { self.rval.x = self.read_mem(ctx, self.lval.x); }
                            if 0 != stage & 2 { self.rval.y = self.read_mem(ctx, self.lval.y); }
                            if 0 != stage & 4 { self.rval.z = self.read_mem(ctx, self.lval.z); }
                            if 0 != stage & 8 { self.rval.w = self.read_mem(ctx, self.lval.w); }
                            break 'op Ok(Some(result))
                        }
                        (0, MemoryProtect::Off)
                    } else if is_shared_exec {
                        (WORD_DELAY_PRIV_TO_SHARED, MemoryProtect::Off)
                    } else if is_nta_exec {
                        if is_priv_mem(addr) {
                            (1, self.protect.pma_from_nta)
                        } else {
                            (WORD_DELAY_PRIV_FROM_SHARED, MemoryProtect::Off)
                        }
                    } else { // private exec (probably)
                        if is_shared_mem(addr) { // from shared
                            (WORD_DELAY_PRIV_FROM_SHARED, self.protect.pma_from_sma)
                        } else { // from NTA
                            (1, self.protect.pma_from_nta)
                        }
                    };
                    self.sleep_for = delay;
                    if 0 == stage & 0x80 { // Init pre-load delay (if any)
                        break 'op Err(DeferredOp::DelayLoad(stage | 0x80, result))
                    }
                    let mem_val =
                    match access {
                        MemoryProtect::Off
                        | MemoryProtect::ReadOnly => self.read_mem(ctx, addr),
                        MemoryProtect::ReadZero => 0,
                        MemoryProtect::AccessException => {
                            self.ins_ptr.x = self.current_ins_addr;
                            return self.exception(VMError::TempestH)
                        }
                    };
                    if 0 != stage & 1 { self.rval.x = mem_val; stage &= !1; } else
                    if 0 != stage & 2 { self.rval.y = mem_val; stage &= !2; } else
                    if 0 != stage & 4 { self.rval.z = mem_val; stage &= !4; } else
                    if 0 != stage & 8 { self.rval.w = mem_val; stage &= !8; }
                    else { break 'op Ok(Some(result)) }
                    Err(DeferredOp::DelayLoad(stage, result))
                }
                DeferredOp::DelayStore(mut stage) => 'op:{
                    let addr =
                        if 0 != stage & 1 { self.lval.x } else
                        if 0 != stage & 2 { self.lval.y } else
                        if 0 != stage & 4 { self.lval.z } else
                        if 0 != stage & 8 { self.lval.w } else {0};
                    // access permission and delay
                    let (delay, access) =
                    if is_shared_exec == is_shared_mem(addr) {
                        if 0x40 != (stage & 0xc0) {
                            if 0 != stage & 1 { self.write_mem(ctx, self.lval.x, self.rval.x); }
                            if 0 != stage & 2 { self.write_mem(ctx, self.lval.y, self.rval.y); }
                            if 0 != stage & 4 { self.write_mem(ctx, self.lval.z, self.rval.z); }
                            if 0 != stage & 8 { self.write_mem(ctx, self.lval.w, self.rval.w); }
                            break 'op Ok(None)
                        }
                        (0, MemoryProtect::Off)
                    } else if is_shared_exec {
                        if is_priv_mem(addr) {
                            (WORD_DELAY_PRIV_FROM_SHARED, self.protect.pma_from_sma)
                        } else {
                            (WORD_DELAY_PRIV_FROM_SHARED, self.protect.nta_from_sma)
                        }
                    } else if is_nta_exec {
                        if is_priv_mem(addr) { // nta to priv
                            (1, self.protect.pma_from_nta)
                        } else { // nta to shared
                            (WORD_DELAY_PRIV_TO_SHARED, MemoryProtect::Off)
                        }
                    } else { // private exec (probably)
                        if is_shared_mem(addr) { // priv to shared
                            (WORD_DELAY_PRIV_TO_SHARED, MemoryProtect::Off)
                        } else { // priv to nta
                            (1, MemoryProtect::Off)
                        }
                    };
                    self.sleep_for = delay;
                    if 0 == stage & 0x80 { // Init pre-load delay (if any)
                        break 'op Err(DeferredOp::DelayStore(stage | 0x80))
                    }
                    let mem_val =
                    if 0 != stage & 1 { stage &= !1; self.rval.x } else
                    if 0 != stage & 2 { stage &= !2; self.rval.y } else
                    if 0 != stage & 4 { stage &= !4; self.rval.z } else
                    if 0 != stage & 8 { stage &= !8; self.rval.w }
                    else { break 'op Ok(None) };
                    match access {
                        MemoryProtect::Off => self.write_mem(ctx, addr, mem_val),
                        MemoryProtect::ReadOnly | MemoryProtect::ReadZero => {}
                        MemoryProtect::AccessException => {
                            self.ins_ptr.x = self.current_ins_addr;
                            return self.exception(VMError::TempestH)
                        }
                    }
                    Err(DeferredOp::DelayStore(stage))
                }
                DeferredOp::WriteBack(index) => Ok(Some(index)),
                DeferredOp::WriteBack2(index_l, index_r) => {
                    let lval = self.lval.clone();
                    if let Some(wreg) = self.reg_index_mut(index_l) {
                        *wreg = lval;
                    }
                    Ok(Some(index_r))
                }
            } {
                Err(defer) => {
                    self.defer = Some(defer);
                }
                Ok(Some(index)) => {
                    // write back any result
                    let rval = self.rval.clone();
                    if let Some(wreg) = self.reg_index_mut(index) {
                        *wreg = rval;
                    }
                }
                Ok(None) => {}
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
        let is_nta_exec = is_nta_mem(addr);
        let is_shm_exec = is_shared_mem(addr);
        if (self.protect.in_shared && is_shm_exec)
            || (self.protect.in_pma && is_priv_exec)
            || (self.protect.in_nta && is_nta_exec) {
            return self.exception(VMError::Explosion);
        }
        let value = self.read_mem(ctx, addr);
        self.current_ins_addr = addr;
        self.ins_ptr.inc_addr();
        let opc = Opcode::parse(value);
        let src = ((value >> 8) & 0b1111) as u8;
        let dst = ((value >> 12) & 0b1111) as u8;
        let opt = ((value >> 4) & 0b1111) as u8;
        self.lval = self.reg_index(src.into()).clone();
        self.rval = self.reg_index(dst.into()).clone();
        if let Some(wval) = match opc {
            Opcode::Nop => None,
            Opcode::Invalid => {
                self.ins_ptr.x = addr;
                return if self.protect.exec_halt_inval {
                    self.exception(VMError::Zap)
                } else { Err(VMError::Zap) }
            }
            Opcode::Halt => {
                self.ins_ptr.x = addr;
                return if self.protect.exec_halt_inval {
                    self.exception(VMError::CatchFire)
                } else { Err(VMError::CatchFire) }
            }
            Opcode::Sleep(_) => {
                if self.protect.exec_sleep {
                    self.ins_ptr.x = addr;
                    return self.exception(VMError::IceOver)
                }
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
            Opcode::Extra2 => {
                self.ins_ptr.x = addr;
                return if self.protect.exec_halt_inval {
                    self.exception(VMError::TempestH)
                } else { Err(VMError::TempestH) }
            }
            Opcode::Extra3 => {
                self.ins_ptr.x = addr;
                return if self.protect.exec_halt_inval {
                    self.exception(VMError::Zap)
                } else { Err(VMError::Zap) }
            }
            Opcode::HAdd(_, _) => {
                let v = self.lval.x
                    .saturating_add(self.lval.y)
                    .saturating_add(self.lval.z)
                    .saturating_add(self.lval.w);
                Some(VMRegister{ x: v, y: v, z: v, w: v })
            }
            Opcode::MultSat(_, _) => Some(VMRegister{
                x: self.lval.x.saturating_mul(self.rval.x),
                y: self.lval.y.saturating_mul(self.rval.y),
                z: self.lval.z.saturating_mul(self.rval.z),
                w: self.lval.w.saturating_mul(self.rval.w),
            }),
            Opcode::MultLow(_, _) => Some(VMRegister{
                x: self.lval.x.wrapping_mul(self.rval.x),
                y: self.lval.y.wrapping_mul(self.rval.y),
                z: self.lval.z.wrapping_mul(self.rval.z),
                w: self.lval.w.wrapping_mul(self.rval.w),
            }),
            Opcode::MultHi(_, _) => Some(VMRegister{
                x: ((self.lval.x as u32).saturating_mul(self.rval.x as u32) >> 16) as u16,
                y: ((self.lval.y as u32).saturating_mul(self.rval.y as u32) >> 16) as u16,
                z: ((self.lval.z as u32).saturating_mul(self.rval.z as u32) >> 16) as u16,
                w: ((self.lval.w as u32).saturating_mul(self.rval.w as u32) >> 16) as u16,
            }),
            Opcode::Divide(_, _) => {
                let quo = VMRegister{
                    x: (self.lval.x).checked_div(self.rval.x).unwrap_or(0xffff),
                    y: (self.lval.y).checked_div(self.rval.y).unwrap_or(0xffff),
                    z: (self.lval.z).checked_div(self.rval.z).unwrap_or(0xffff),
                    w: (self.lval.w).checked_div(self.rval.w).unwrap_or(0xffff),
                };
                let remain = VMRegister{
                    x: (self.lval.x).checked_rem(self.rval.x).unwrap_or(self.lval.x),
                    y: (self.lval.y).checked_rem(self.rval.y).unwrap_or(self.lval.y),
                    z: (self.lval.z).checked_rem(self.rval.z).unwrap_or(self.lval.z),
                    w: (self.lval.w).checked_rem(self.rval.w).unwrap_or(self.lval.w),
                };
                self.rval = quo;
                self.lval = remain;
                self.sleep_for = 18;
                self.defer = Some(DeferredOp::WriteBack2(src.into(), dst.into()));
                None
            }
            Opcode::RecpDivide(_, _) => {
                let quo = VMRegister{
                    x: (0x10000u32).checked_div(self.lval.x as u32).unwrap_or(0xffff) as u16,
                    y: (0x10000u32).checked_div(self.lval.y as u32).unwrap_or(0xffff) as u16,
                    z: (0x10000u32).checked_div(self.lval.z as u32).unwrap_or(0xffff) as u16,
                    w: (0x10000u32).checked_div(self.lval.w as u32).unwrap_or(0xffff) as u16,
                };
                self.rval = quo;
                self.sleep_for = 18;
                self.defer = Some(DeferredOp::WriteBack(dst.into()));
                None
            }
            Opcode::Extra14 => {
                self.ins_ptr.x = addr;
                return if self.protect.exec_halt_inval {
                    self.exception(VMError::IceOver)
                } else { Err(VMError::IceOver) }
            }
            Opcode::Extra15 => {
                self.ins_ptr.x = addr;
                return if self.protect.exec_halt_inval {
                    self.exception(VMError::RockStone)
                } else { Err(VMError::RockStone) }
            }
            Opcode::Move(..) => {
                if opt & 1 == 0 { self.rval.x = self.lval.x }
                if opt & 2 == 0 { self.rval.y = self.lval.y }
                if opt & 4 == 0 { self.rval.z = self.lval.z }
                if opt & 8 == 0 { self.rval.w = self.lval.w }
                Some(self.rval)
            }
            Opcode::Swizzle(..) => Some(VMRegister{
                x: self.rval.index(opt),
                y: self.rval.index(opt >> 2),
                z: self.rval.index(src),
                w: self.rval.index(src >> 2)
            }),
            Opcode::Load(src_reg, _, scale) |
            Opcode::LoadInc(src_reg, _, scale) |
            Opcode::Gather(src_reg, _, scale) |
            Opcode::GatherInc(src_reg, _, scale) |
            Opcode::Store(src_reg, _, scale) |
            Opcode::StoreInc(src_reg, _, scale) |
            Opcode::Scatter(src_reg, _, scale) |
            Opcode::ScatterInc(src_reg, _, scale) => {
                let mut lval = self.lval;
                let mut is_align = false;
                match opc {
                    Opcode::Load(..) | Opcode::LoadInc(..) |
                    Opcode::Store(..) | Opcode::StoreInc(..) => {
                        is_align = (self.lval.x & 0x3) == 0;
                        if scale > 0 { self.lval.y = lval.inc_addr() }
                        if scale > 1 { self.lval.z = lval.inc_addr() }
                        if scale > 2 { self.lval.w = lval.inc_addr() }
                        lval.inc_addr();
                    }
                    Opcode::GatherInc(..) |
                    Opcode::ScatterInc(..) => {
                        lval.x = inc_addr(lval.x);
                        if scale > 0 { lval.y = inc_addr(lval.y) }
                        if scale > 1 { lval.z = inc_addr(lval.z) }
                        if scale > 2 { lval.w = inc_addr(lval.w) }
                    }
                    Opcode::Gather(..) |
                    Opcode::Scatter(..) => {}
                    _ => unreachable!("did you add load/store opcodes?")
                }
                let defer_op = match opc {
                    Opcode::Load(_, dst_reg, _) |
                    Opcode::LoadInc(_, dst_reg, _) |
                    Opcode::Gather(_, dst_reg, _) |
                    Opcode::GatherInc(_, dst_reg, _) => {
                        #[cfg(test)]
                        eprintln!("priv:pc:{},s:{} {:?} {} {}", is_priv_exec, is_priv_mem(self.lval.x), opc, self.lval, self.rval);
                        DeferredOp::DelayLoad(
                            if is_align {0x40} else {0}
                            | (0x2u8 << scale).wrapping_sub(1), dst_reg)
                    }
                    Opcode::Store(..) |
                    Opcode::StoreInc(..) |
                    Opcode::Scatter(..) |
                    Opcode::ScatterInc(..) => {
                        DeferredOp::DelayStore(
                            if is_align {0x40} else {0}
                            | (0x2u8 << scale).wrapping_sub(1) as u8)
                    }
                    _ => unreachable!("did you add load/store opcodes?")
                };
                self.defer = Some(defer_op);
                match (opc, self.reg_index_mut(src_reg)) {
                    (Opcode::LoadInc(..), Some(wreg)) |
                    (Opcode::GatherInc(..), Some(wreg)) |
                    (Opcode::StoreInc(..), Some(wreg)) |
                    (Opcode::ScatterInc(..), Some(wreg)) => {
                        *wreg = lval;
                    }
                    _ => {}
                }
                None
            }
            Opcode::IncA1x1(..) => {
                if let Some(s_val) = self.reg_index_mut(src.into()) {
                    s_val.inc_addr();
                }
                None
            }
            Opcode::IncA1x2(..) => {
                if let Some(s_val) = self.reg_index_mut(src.into()) {
                    s_val.inc_addr();
                    s_val.inc_addr();
                }
                None
            }
            Opcode::IncA1x3(..) => {
                if let Some(s_val) = self.reg_index_mut(src.into()) {
                    s_val.inc_addr();
                    s_val.inc_addr();
                    s_val.inc_addr();
                }
                None
            }
            Opcode::IncA1x4(..) => {
                if let Some(s_val) = self.reg_index_mut(src.into()) {
                    s_val.inc_addr();
                    s_val.inc_addr();
                    s_val.inc_addr();
                    s_val.inc_addr();
                }
                None
            }
            Opcode::IncA2x1(..) => {
                if let Some(s_val) = self.reg_index_mut(src.into()) {
                    s_val.x = inc_addr(s_val.x);
                    s_val.y = inc_addr(s_val.y);
                }
                None
            }
            Opcode::IncA3x1(..) => {
                if let Some(s_val) = self.reg_index_mut(src.into()) {
                    s_val.x = inc_addr(s_val.x);
                    s_val.y = inc_addr(s_val.y);
                    s_val.z = inc_addr(s_val.z);
                }
                None
            }
            Opcode::IncA4x1(..) => {
                if let Some(s_val) = self.reg_index_mut(src.into()) {
                    s_val.x = inc_addr(s_val.x);
                    s_val.y = inc_addr(s_val.y);
                    s_val.z = inc_addr(s_val.z);
                    s_val.w = inc_addr(s_val.w);
                }
                None
            }
            Opcode::Skip1 => {
                self.ins_ptr.inc_addr();
                None
            }
            Opcode::Skip2 => {
                self.ins_ptr.inc_addr();
                self.ins_ptr.inc_addr();
                None
            }
            Opcode::Skip3 => {
                self.ins_ptr.inc_addr();
                self.ins_ptr.inc_addr();
                self.ins_ptr.inc_addr();
                None
            }
            Opcode::Skip4 => {
                #[cfg(test)]
                eprintln!("Skip4 {}", self.ins_ptr);
                self.ins_ptr.inc_addr();
                self.ins_ptr.inc_addr();
                self.ins_ptr.inc_addr();
                self.ins_ptr.inc_addr();
                #[cfg(test)]
                eprintln!("Skip4 {}", self.ins_ptr);
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
            Opcode::SetAll        (..) => {
                #[cfg(test)]
                eprintln!("{} SetAll {}", self.ins_ptr, self.rval);
                Some(VMRegister{ x: 0xffff, y: 0xffff, z: 0xffff, w: 0xffff })
            }
            Opcode::SetOne        (..) => {
                #[cfg(test)]
                eprintln!("{} SetOne {}", self.ins_ptr, self.rval);
                Some(VMRegister{ x: 1, y: 1, z: 1, w: 1})
            }
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
            Opcode::LShiftLit8      (src_val, _) => Some( operation8!(   _, rval => rval.wrapping_shl(src_val as u32))),
            Opcode::RLogiShiftLit8  (src_val, _) => Some( operation8!(   _, rval => rval.wrapping_shr(src_val as u32))),
            Opcode::RArithShiftLit8 (src_val, _) => Some( operation8!(   _, rval => (rval as i8).wrapping_shr(src_val as u32))),
            Opcode::RotateLit8      (src_val, _) => Some( operation8!(   _, rval => rval.rotate_left(src_val as u32))),
            Opcode::LShift16              (_, _) => Some(operation16!(lval, rval => rval << (lval & 15))),
            Opcode::RLogiShift16          (_, _) => Some(operation16!(lval, rval => rval >> (lval & 15))),
            Opcode::RArithShift16         (_, _) => Some(operation16!(lval, rval => (rval as i16) >> (lval & 15))),
            Opcode::LRotate16             (_, _) => Some(operation16!(lval, rval => rval.rotate_left(lval as u32 & 15))),
            Opcode::LIShift16             (_, _) => Some(operation16!(lval, rval => rval << (16u16.wrapping_sub(lval) & 15))),
            Opcode::RILogiShift16         (_, _) => Some(operation16!(lval, rval => rval >> (16u16.wrapping_sub(lval) & 15))),
            Opcode::RIArithShift16        (_, _) => Some(operation16!(lval, rval => (rval as i16) >> (16u16.wrapping_sub(lval) & 15))),
            Opcode::RRotate16             (_, _) => Some(operation16!(lval, rval => rval.rotate_left(16u16.wrapping_sub(lval) as u32 & 15))),
            Opcode::LShiftLit16     (src_val, _) => Some(operation16!(   _, rval => rval.wrapping_shl(src_val as u32))),
            Opcode::RLogiShiftLit16 (src_val, _) => Some(operation16!(   _, rval => rval.wrapping_shr(src_val as u32))),
            Opcode::RArithShiftLit16(src_val, _) => Some(operation16!(   _, rval => (rval as i16).wrapping_shr(src_val as u32))),
            Opcode::RotateLit16     (src_val, _) => Some(operation16!(   _, rval => rval.rotate_right(src_val as u32))),
        } {
            if let Some(wreg) = self.reg_index_mut(dst.into()) {
                *wreg = wval;
            }
        }
        Ok(())
    }
}

pub type MemoryType = [(u16, u16); MEM_SHARED_SIZE_U];
pub type VMProcType = Box<WaveProc>;

#[derive(Debug, Default, Serialize, Deserialize, PartialEq)]
#[serde(default)]
pub struct FlightModule {
    pub req_vel_x: u16,
    pub req_vel_y: u16,
    pub req_vel_z: u16,
    pub save_7: u16,
    #[serde(skip)]
    pub current_vel_x: u16,
    #[serde(skip)]
    pub current_vel_y: u16,
    pub req_heading: u16,
    pub save_d: u16,
    pub save_e: u16,
    pub save_f: u16,
    #[serde(skip)]
    pub current_compass: u16,
    pub engine: u16,
    pub save_15: u16,
    pub save_16: u16,
    pub save_17: u16,
    pub color: u16,
    pub color_mix: u16, // TODO
}
impl Display for FlightModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "RRV[{:4x}, {:4x}] RH[{:4x}] CRV[{:4x} {:4x}] E:Y{}{},X{}{},H{}{}",
            self.req_vel_x, self.req_vel_y, self.req_heading,
            self.current_vel_x,
            self.current_vel_y,
            if (self.engine & 1) != 0 {'+'} else {' '},
            if (self.engine & 2) != 0 {'-'} else {' '},
            if (self.engine & 4) != 0 {'+'} else {' '},
            if (self.engine & 8) != 0 {'-'} else {' '},
            if (self.engine & 16) != 0 {'E'} else {'D'},
            if (self.engine & 32) != 0 {'A'} else {'R'},
            )
    }
}
impl ModuleBank for FlightModule {
    fn read_bank(&mut self, _: &mut Cycle, offset: u8) -> u16 {
        match offset {
            0 => 1, 1 => 0x4000,
            0x4 => self.req_vel_x,
            0x5 => self.req_vel_y,
            0x6 => self.req_vel_z,
            0x7 => self.save_7,
            0x8 => self.current_vel_x,
            0x9 => self.current_vel_y,
            0xc => self.req_heading,
            0xd => self.save_d,
            0xe => self.save_e,
            0xf => self.save_f,
            0x10 => self.current_compass,
            0x14 => self.engine,
            0x15 => self.save_15,
            0x16 => self.save_16,
            0x17 => self.save_17,
            0x1c => self.color,
            0x1d => self.color_mix,
            _ => 0
        }
    }
    fn write_bank(&mut self, _: &mut Cycle, offset: u8, value: u16) {
        match offset {
            0x04 => { self.req_vel_x = value; },
            0x05 => { self.req_vel_y = value; },
            0x06 => { self.req_vel_z = value; },
            0x07 => { self.save_7 = value; },
            0x0c => { self.req_heading = value; },
            0x0d => { self.save_d = value; },
            0x0e => { self.save_e = value; },
            0x0f => { self.save_f = value; },
            0x14 => { self.engine = value; },
            0x15 => { self.save_15 = value; },
            0x16 => { self.save_16 = value; },
            0x17 => { self.save_17 = value; },
            0x1c => { self.color = value; },
            0x1d => { self.color_mix = value; },
            _ => {}
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize, PartialEq)]
#[serde(default)]
pub struct NavModule {
    #[serde(skip)]
    pub current_ship_x: u16,
    #[serde(skip)]
    pub current_ship_y: u16,
    pub target_screen_x: u16,
    pub target_screen_y: u16,
    pub target_screen_z: u16, // just storage
    pub target_screen_w: u16, // just storage
    pub target_select: u16, // done: [0] TODO: [1 2 N>2]
    pub target_id_1: u16, // TODO
    pub target_id_2: u16, // TODO
    pub target_id_3: u16, // TODO
    #[serde(skip)]
    pub target_rel_dist_x: u16,
    #[serde(skip)]
    pub target_rel_dist_y: u16,
    #[serde(skip)]
    pub target_rel_vel_x: u16, // TODO
    #[serde(skip)]
    pub target_rel_vel_y: u16, // TODO
    #[serde(skip)]
    pub target_abs_heading_to: u16,
    #[serde(skip)]
    pub target_abs_heading_fro: u16,
    #[serde(skip)]
    pub target_rel_heading_to: u16,
    #[serde(skip)]
    pub target_rel_heading_fro: u16,
}
impl Display for NavModule {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<TODO NAV>")
    }
}
impl ModuleBank for NavModule {
    fn read_bank(&mut self, _: &mut Cycle, offset: u8) -> u16 {
        match offset {
            0 => 1, 1 => 0x4050,
            0x4 => self.current_ship_x,
            0x5 => self.current_ship_y,
            0x8 => self.target_screen_x,
            0x9 => self.target_screen_y,
            0xa => self.target_screen_z,
            0xb => self.target_screen_w,
            0xc => self.target_select,
            0xd => self.target_id_1,
            0xe => self.target_id_2,
            0xf => self.target_id_3,
            0x10 => self.target_rel_dist_x,
            0x11 => self.target_rel_dist_y,
            0x14 => self.target_rel_vel_x,
            0x15 => self.target_rel_vel_y,
            0x18 => self.target_abs_heading_to,
            0x1a => self.target_abs_heading_fro,
            0x1c => self.target_rel_heading_to,
            0x1e => self.target_rel_heading_fro,
            _ => 0
        }
    }
    fn write_bank(&mut self, _: &mut Cycle, offset: u8, value: u16) {
        match offset {
            0x08 => { self.target_screen_x = value; },
            0x09 => { self.target_screen_y = value; },
            0x0a => { self.target_screen_z = value; },
            0x0b => { self.target_screen_w = value; },
            _ => {}
        }
    }
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
#[serde(default)]
pub struct VMShip {
    pub x: f32, pub y: f32,
    pub vel_x: f32, pub vel_y: f32,
    accel_x: f32,
    accel_y: f32,
    pub heading: f32,
    pub spin: f32,
    #[serde(flatten)]
    pub flight: FlightModule,
    #[serde(flatten)]
    pub nav: NavModule,
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
            spin: rng.gen_range(-5.0..5.0),
            heading: rng.gen_range(0.0..1.0),
            flight: FlightModule {
                color: 0xffff,
                ..Default::default()
            },
            nav: NavModule {
                ..Default::default()
            },
        }
    }
}
impl Display for VMShip {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "VMShip([{}, {}], [{}, {}] {:4x} {:4x})",
            self.x, self.y, self.vel_x, self.vel_y,
            (self.heading * 32768.0) as u16, self.flight.color
            )
    }
}
impl VMShip {
    pub fn set_color(&mut self, color: u32) {
        let (r, g, b) = ((color >> 16) as u8, (color >> 8) as u8, color as u8);
        self.flight.color =
            (((r & 0b0011111000) as u16) << 8) |
            (((g & 0b0011111100) as u16) << 3) |
            (((b & 0b0011111000) as u16) >> 3);
    }
}

pub trait ModuleBank {
    fn read_bank(&mut self, ctx: &mut Cycle, offset: u8) -> u16;
    fn write_bank(&mut self, ctx: &mut Cycle, offset: u8, value: u16);
}

#[derive(Debug, Default, Serialize, Deserialize, PartialEq)]
pub struct VMConsts {
    pub mem: [u16; 8*4],
}
impl ModuleBank for VMConsts {
    fn read_bank(&mut self, _: &mut Cycle, offset: u8) -> u16 {
        self.mem.get(offset as usize).map_or(0, |&v| v)
    }

    fn write_bank(&mut self, _: &mut Cycle, offset: u8, value: u16) {
        if let Some(slot) = self.mem.get_mut(offset as usize) {
            *slot = value;
        }
    }
}

#[derive(Debug, Default, Serialize, Deserialize, PartialEq)]
#[serde(default)]
pub struct VMThreadContext {
}

impl ModuleBank for WaveProc {
    fn read_bank(&mut self, _: &mut Cycle, offset: u8) -> u16 {
        match offset {
            0x0 => (if self.is_running {1} else {0}
                | if self.sleep_for > 0 {1<<1} else {0}),
            0x1 => self.core_id,
            0x2 => self.protect.to_u16(),
            0x3 => 0, // protection register future use maybe
            0x4 => 0, // CCRL
            0x5..0x8 => 0, // CUID
            0x8 => self.save_except.x,
            0x9 => self.save_except.y,
            0xa => self.save_except.z,
            0xb => self.save_except.w,
            0xc => self.save_ri.x,
            0xd => self.save_ri.y,
            0xe => self.save_ri.z,
            0xf => self.save_ri.w,
            0x10 => 0,
            0x11 => self.bank1_select, // thread 0 bank
            0x12 => self.bank1_select, // thread 0 bank
            0x13 => 0xf000,
            0x18 => self.core_id,
            0x19..0x20 => self.mod_selects[offset as usize - 0x19],
            _ => 0,
        }
    }

    fn write_bank(&mut self, ctx: &mut Cycle, offset: u8, value: u16) {
        let reg_enable = !self.protect.reg_saves || !self.is_running;
        match offset {
            // Thread 0
            0x0 => if (value & 1) != 0 && !self.is_running {
                self.is_running = true;
                if !ctx.vm.processes.contains(&ctx.user) {
                    ctx.vm.processes.push_back(ctx.user);
                }
            }
            0x2 => if !self.is_running {
                self.protect = VMProtections::from(value);
            }
            0x4 => if !self.protect.core_control || !self.is_running { // Const
                self.sleep_for += 8;
                let meme = &self.con_store.mem;
                self.cval[0] = VMRegister{x:meme[ 0], y:meme[ 1], z:meme[ 2], w:meme[ 3]};
                self.cval[1] = VMRegister{x:meme[ 4], y:meme[ 5], z:meme[ 6], w:meme[ 7]};
                self.cval[2] = VMRegister{x:meme[ 8], y:meme[ 9], z:meme[10], w:meme[11]};
                self.cval[3] = VMRegister{x:meme[12], y:meme[13], z:meme[14], w:meme[15]};
                self.cval[4] = VMRegister{x:meme[16], y:meme[17], z:meme[18], w:meme[19]};
                self.cval[5] = VMRegister{x:meme[20], y:meme[21], z:meme[22], w:meme[23]};
                self.cval[6] = VMRegister{x:meme[24], y:meme[25], z:meme[26], w:meme[27]};
                self.cval[7] = VMRegister{x:meme[28], y:meme[29], z:meme[30], w:meme[31]};
            }
            0x08 => if reg_enable { self.save_except.x = value; },
            0x09 => if reg_enable { self.save_except.y = value; },
            0x0a => if reg_enable { self.save_except.z = value; },
            0x0b => if reg_enable { self.save_except.w = value; },
            0x0c => if reg_enable { self.save_ri.x = value; },
            0x0d => if reg_enable { self.save_ri.y = value; },
            0x0e => if reg_enable { self.save_ri.z = value; },
            0x0f => if reg_enable { self.save_ri.w = value; },
            0x11 => if reg_enable { self.bank1_select = value; }, // thread 0 bank
            0x12 => if reg_enable { self.bank1_select = value; },
            0x19..0x20 => if reg_enable {
                self.mod_selects[offset as usize - 0x19] = value;
            }
            _ => {},
        }
    }
}

enum IOModule {
    Thread,
    Module(u16),
}

#[derive(Debug, Serialize, Deserialize, PartialEq)]
pub struct VMUser {
    #[serde(default)]
    pub eid: u32,
    #[serde(default = "default_thread0")]
    pub proc: VMProcType,
    #[serde(default = "default_thread1")]
    pub agent: VMProcType,
    pub ship: VMShip,
    pub user_login: String,
    pub user_name: String,
    pub user_color: u32,
    pub user_color_loaded: bool,
    #[serde(skip)]
    pub ident_time: u32,
    #[serde(skip)]
    pub requested_fields: u32,
}
fn default_thread0() -> Box<WaveProc> {
    Box::new(WaveProc::new(0))
}
fn default_thread1() -> Box<WaveProc> {
    Box::new(WaveProc::new(1))
}

impl Default for VMUser {
    fn default() -> Self {
        Self {
            eid: 0,
            proc: default_thread0(),
            agent: default_thread1(),
            ship: VMShip::default(),
            user_color: 0xffffff,
            user_color_loaded: false,
            ident_time: 0,
            requested_fields: 0,
            user_name: String::default(),
            user_login: String::default(),
        }
    }
}

impl VMUser {
    pub fn dump(&self) {
        println!("Context: {} {:6x}", self.user_login, self.user_color);
        println!("Ship: {}", self.ship);
        println!("Flight: {}", self.ship.flight);
        println!("Nav: {}", self.ship.nav);
    }
    fn io_decode<'p>(&'p mut self, proc: &'p mut WaveProc, addr: u16) -> Option<(&'p mut dyn ModuleBank, u8)> {
        let (io, offset) = match addr {
            0x00..0x20 => (IOModule::Thread, addr as u8),
            0x20..0x40 => (IOModule::Module(proc.mod_selects[0]), (addr - 0x20) as u8),
            0x40..0x60 => (IOModule::Module(proc.mod_selects[1]), (addr - 0x40) as u8),
            0x60..0x80 => (IOModule::Module(proc.mod_selects[2]), (addr - 0x60) as u8),
            0x80..0xa0 => (IOModule::Module(proc.mod_selects[3]), (addr - 0x80) as u8),
            0xa0..0xc0 => (IOModule::Module(proc.mod_selects[4]), (addr - 0xa0) as u8),
            0xc0..0xe0 => (IOModule::Module(proc.mod_selects[5]), (addr - 0xc0) as u8),
            0xe0..0x100 => (IOModule::Module(proc.mod_selects[6]), (addr - 0xe0) as u8),
            _ => return None,
        };
        Some((match io {
            IOModule::Thread => proc,
            IOModule::Module(0x0001) => self.proc.as_mut(),
            IOModule::Module(0x0002) => self.agent.as_mut(),
            IOModule::Module(0x1001) => &mut self.proc.con_store,
            IOModule::Module(0x1002) => &mut self.agent.con_store,
            IOModule::Module(0x4000) => &mut self.ship.flight,
            IOModule::Module(0x4050) => &mut self.ship.nav,
            IOModule::Module(_) => return None,
        }, offset))
    }
}

pub fn read_io(proc: &mut WaveProc, ctx: &mut Cycle, addr: u16) -> u16 {
    unsafe {
        if let Some((thing, offset)) = (*ctx.user).io_decode(proc, addr) {
            thing.read_bank(ctx, offset)
        } else { 0 }
    }
}

pub fn write_io(proc: &mut WaveProc, ctx: &mut Cycle, addr: u16, value: u16) {
    unsafe {
        if let Some((thing, offset)) = (*ctx.user).io_decode(proc, addr) {
            thing.write_bank(ctx, offset, value)
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

pub trait VMAccessPriv {
    fn read_priv(&self, addr: u16) -> u16;
    fn write_priv(&mut self, addr: u16, value: u16);
}
unsafe impl Send for SimulationVM {}

use std::iter::Iterator;
pub struct VMThreads<'a> {
    iter: std::collections::vec_deque::Iter<'a, *mut VMUser>
}
impl Iterator for VMThreads<'_> {
    type Item = String;

    fn next(&mut self) -> Option<Self::Item> {
        let user = unsafe { self.iter.next()?.as_ref()? };
        let val = user.proc.read_priv(user.proc.ins_ptr.x);
        Some(format!("{} {:4x}: {}", user.proc, val, Opcode::parse(val)))
    }
}

impl VMAccessPriv for WaveProc {
    fn read_priv(&self, addr: u16) -> u16 {
        if addr < 0x40 {
            self.reg_index((addr >> 2).into()).index(addr as u8)
        } else if addr < MEM_PRIV_NVT_END {
            self.priv_mem[addr as usize - MEM_PRIV_NV_START_U]
        } else { 0 }
    }
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
    pub fn find_user(&mut self, user: u64) -> Option<&mut Box<VMUser>> {
        self.users.get_mut(&user)
    }
    pub fn find_user_eid(&mut self, eid: u32) -> Option<&mut Box<VMUser>> {
        self.users.values_mut().find(|user| user.eid == eid)
    }
    pub fn make_user(&mut self, user: u64) -> &mut Box<VMUser> {
        self.users.entry(user).or_insert_with(|| {
            Box::new(VMUser::default())
        });
        self.users.get_mut(&user).unwrap()
    }
    pub fn sys_clear_shared(&mut self) {
        for (v, _) in self.memory.iter_mut() {
            *v = 0;
        }
    }
    pub fn sys_clear_col(&mut self, mut col: usize) {
        col <<= 5;
        for (index, (v, _)) in self.memory.iter_mut().enumerate() {
            if (index & 0x00e0) == col {
                *v = 0;
            }
        }
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
        *user.proc = WaveProc::new(0);
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
        *user.agent = WaveProc::new(1);
    }
    pub fn agent_restart(&mut self, user: u64) {
        let user = self.make_user(user);
        user.agent.ins_ptr.x = 0x40;
    }
    pub fn user_new(&mut self, user: u64) -> &mut Box<VMUser> {
        self.make_user(user)
    }
    pub fn user_new_with_update(&mut self, user: u64, user_name: String, user_login: String, user_color: u32) {
        let user = self.make_user(user);
        user.user_name = user_name;
        user.user_login = user_login;
        user.user_color = user_color;
        if !user.user_color_loaded {
            user.user_color_loaded = true;
            user.ship.set_color(user_color);
        }
    }
    pub fn user_dump(&mut self, user: u64) {
        let user = self.make_user(user);
        user.dump();
        user.proc.dump();
    }
    pub fn memory_invalidate(&mut self) {
        for m in self.memory.iter_mut() {
            m.1 = !m.0
        }
    }
    pub fn threads(&mut self) -> VMThreads {
        VMThreads{
            iter: self.processes.iter()
        }
    }
    pub fn tick(&mut self, tick_count: usize) {
        use rand::Rng;
        let mut rng = rand::thread_rng();
        let queue_size = self.processes.len();
        let mut ticks = 0;
        let delta_time = 0.018f32;
        let delta_time_s = delta_time * delta_time;
        let users = &raw const self.users;
        for (_, user) in self.users.iter_mut() {
            if user.eid == 0 {
                let mut eid = rng.gen_range(1..=0xffff);
                let mut retry = 0;
                while unsafe {
                    (&*users).iter().find(|(_, u)| u.eid != 0 && u.eid == eid).is_some()
                } {
                    eid = rng.gen_range(0..=0xffff);
                    retry += 1;
                    if retry > 50 {
                        panic!("can not assign an eid");
                    }
                }
                user.eid = eid;
            }
            if user.ident_time > 0 {
                user.ident_time -= 1;
            }
            let ship = &mut user.ship;
            ship.x += ship.vel_x * delta_time + ship.accel_x * delta_time_s;
            ship.y += ship.vel_y * delta_time + ship.accel_y * delta_time_s;
            ship.vel_x = (ship.vel_x + ship.accel_x * delta_time).clamp(-1024.0, 1024.0);
            ship.vel_y = (ship.vel_y + ship.accel_y * delta_time).clamp(-1024.0, 1024.0);
            ship.heading = (ship.heading + ship.spin * delta_time).fract();
            let (head_s, head_c) = (ship.heading * core::f32::consts::TAU).sin_cos();
            let engine_limited = ship.flight.engine & 15;
            let engine_on = engine_limited != 0;
            let gyro_abs = (ship.flight.engine & (16|32)) == (16|32);
            let gyro_rel = (ship.flight.engine & (16|32)) == 16;
            if gyro_abs && ship.flight.req_heading < 0x8000 {
                let req = (ship.flight.req_heading & 0x7fff) as f32 * 0.000030517578125;
                let delta = req - ship.heading;
                const NYOOM: f32 = 4.5;
                let spin = (delta * 8.0).clamp(-NYOOM, NYOOM);
                ship.spin = spin;
                //eprintln!("{:4x} {:1.7} {:1.7} {:1.7} {:1.7}",
                //    ship.flight.req_heading, req,
                //    ship.heading, delta, ship.spin);
            }
            // TODO try to pick the shorter of two spins about the rotation axis
            // instead of biasing to zero
            if gyro_rel {
                let spin = (ship.flight.req_heading as i16 as f32) * 0.00390625;
                ship.spin = spin;
            }
//       -1.0                     0x0000 (0.0, 1.0)
//       +Y         (-0.7, 0.7)      ^
//       /\                  0x7000  |  0x1000 (0.7, 0.7)
//      /  \1.0 (-1.0, 0.0)        \ | /
//   -X/    \+X          0x6000 <----*----> 0x2000 (1.0, 0.0)
//    /      \                     / | \
//    +-|--|-+  (-0.7, -0.7) 0x5000  |  0x3000 (0.7, -0.7)
//       -Y                          v
//                                0x4000 (0.0, -1.0)
            let (rel_vel_x, rel_vel_y) = (
                ship.vel_x * head_c + ship.vel_y * head_s,
                ship.vel_x * head_s + ship.vel_y * -head_c,
                );
            ship.flight.current_compass = ((ship.heading * 32768.0).round() as i32 & 0x7fff) as u16;
            ship.flight.current_vel_x = (rel_vel_x * 256.0).round().clamp(-32768.0, 32767.0) as i16 as u16;
            ship.flight.current_vel_y = (rel_vel_y * 256.0).round().clamp(-32768.0, 32767.0) as i16 as u16;
            if engine_on {
                let (delta_x, delta_y) = (
                    ((ship.flight.req_vel_x as i16 as f32)
                    - (ship.flight.current_vel_x as i16 as f32))
                    * 0.00390625,
                    ((ship.flight.req_vel_y as i16 as f32)
                    - (ship.flight.current_vel_y as i16 as f32))
                    * 0.00390625,
                );
                if 15 != engine_limited {
                    let delta_x = if delta_x < 0.0 {
                        // thrust towards -X
                        // aka the +X thruster
                        if 0 != engine_limited & 4 {
                            delta_x.clamp(-0.25, 0.0)
                        } else { 0.0 }
                    } else {
                        // thrust towards +X
                        // aka the -X thruster
                        if 0 != engine_limited & 8 {
                            delta_x.clamp(0.0, 0.25)
                        } else { 0.0 }
                    };
                    let delta_y = if delta_y < 0.0 {
                        // thrust towards -Y
                        // aka the +Y thruster
                        if 0 != engine_limited & 1 {
                            delta_y.clamp(-0.25, 0.0)
                        } else { 0.0 }
                    } else {
                        // thrust towards +Y
                        // aka the -Y thruster aka main engine
                        if 0 != engine_limited & 2 {
                            delta_y.clamp(0.0, 7.0)
                        } else { 0.0 }
                    };
                    ship.accel_x = delta_x * head_c + delta_y * head_s;
                    ship.accel_y = delta_x * head_s + delta_y * -head_c;
                } else {
                    let delta_x = delta_x * 0.25;
                    let delta_y = if delta_y < 0.0 { delta_y * 0.25 } else { delta_y * 7.0 };
                    ship.accel_x = delta_x * head_c + delta_y * head_s;
                    ship.accel_y = delta_x * head_s + delta_y * -head_c;
                }
            } else {
                ship.accel_x = 0.0;
                ship.accel_y = 0.0;
            }
            const MARGIN: f32 = 16.0;
            const RIGHT_EDGE: f32 = 1920.0 + MARGIN * 2.0;
            const BOTTOM_EDGE: f32 = 1080.0 + MARGIN * 2.0;
            if ship.x < 0.0 { ship.x += RIGHT_EDGE }
            if ship.x > RIGHT_EDGE { ship.x -= RIGHT_EDGE }
            if ship.y < 0.0 { ship.y += BOTTOM_EDGE }
            if ship.y > BOTTOM_EDGE { ship.y -= BOTTOM_EDGE }
            ship.nav.current_ship_x = ship.x.round().clamp(-32768.0, 32767.0) as i16 as u16;
            ship.nav.current_ship_y = ship.y.round().clamp(-32768.0, 32767.0) as i16 as u16;
            if ship.nav.target_select == 0 {
                // TODO the point should be selected relative to the
                // shortest path across wrapped screen space
                let target_point = (
                    (ship.nav.target_screen_x as i16 as f32) - ship.x,
                    (ship.nav.target_screen_y as i16 as f32) - ship.y);
                let rel_target_point = (
                    target_point.0 * head_c + target_point.1 * head_s,
                    target_point.0 * head_s + target_point.1 * -head_c,
                    );
                const NOT_QUITE_RADIANS_ANYMORE: f32 = 1.0 / core::f32::consts::TAU;
                let rel_heading = (rel_target_point.0).atan2(-rel_target_point.1) * NOT_QUITE_RADIANS_ANYMORE;
                let abs_heading = (target_point.0).atan2(target_point.1) * NOT_QUITE_RADIANS_ANYMORE;
                let i_rel_heading = ((rel_heading * 32768.0).round() as i32 & 0x7fff) as u16;
                let i_abs_heading = ((abs_heading * 32768.0).round() as i32 & 0x7fff) as u16;
                (
                    ship.nav.target_rel_dist_x,
                    ship.nav.target_rel_dist_y,
                    ship.nav.target_rel_vel_x,
                    ship.nav.target_rel_vel_y,
                    ship.nav.target_rel_heading_to,
                    ship.nav.target_rel_heading_fro,
                    ship.nav.target_abs_heading_to,
                    ship.nav.target_abs_heading_fro,
                ) = (
                    rel_target_point.0.round().clamp(-32768.0, 32767.0) as i16 as u16,
                    rel_target_point.1.round().clamp(-32768.0, 32767.0) as i16 as u16,
                    (-rel_vel_x * 256.0).round().clamp(-32768.0, 32767.0) as i16 as u16,
                    (-rel_vel_y * 256.0).round().clamp(-32768.0, 32767.0) as i16 as u16,
                    0x4000u16.wrapping_sub(i_rel_heading) & 0x7fff,
                    i_rel_heading.wrapping_neg() & 0x7fff,
                    0x4000u16.wrapping_sub(i_abs_heading) & 0x7fff,
                    i_abs_heading.wrapping_neg() & 0x7fff,
                );
            }
        }
        while ticks < tick_count {
            let mut proc_index = 0;
            while proc_index < queue_size {
                if let Some(user_proc) = self.processes.pop_front() {
                    let procs = unsafe {[
                        (*user_proc).proc.as_mut(),
                        (*user_proc).agent.as_mut(),
                    ]};
                    let mut still_running = false;
                    let mut ctx = Cycle {
                        vm: self,
                        user: user_proc,
                    };
                    for proc in procs {
                        if proc.is_running {
                            match proc.cycle(&mut ctx) {
                                Ok(()) => { still_running = true; }
                                Err(_) => { proc.is_running = false; }
                            }
                        }
                    }
                    if still_running {
                        // reschedule
                        self.processes.push_back(user_proc);
                    }
                    proc_index += 1;
                } else { break }
            }
            ticks += 1;
        }
    }

    pub fn delta_encode(&mut self) -> Arc<Vec<u8>> {
        let mut out: Vec<u8> = Vec::with_capacity(0x2000);
        let mut offset = 0u32;
        let mut runlength = 0u32;
        let mut runpos = 0usize;
        for (val, old) in self.memory.iter_mut() {
            const MEMSIZE: u32 = 1;
            if val == old { // the words are similar
                let newoffset = offset + MEMSIZE;
                if runlength > 0 {
                    // writes the length into the current
                    // header
                    out[runpos] = runlength as u8;
                    // close the current run
                    runlength = 0;
                }
                offset = newoffset;
            } else { // the words differ
                *old = *val;
                // add a header if we didn't write one yet
                if runlength == 0 {
                    if offset > u8::MAX as u32 {
                        out.push(3); // offset nn
                        out.push(offset as u8);
                        out.push((offset >> 8) as u8);
                        out.push(1); // copy n
                    } else if offset == 0 {
                        out.push(1); // copy n
                    } else {
                        // "offset n, copy n"
                        out.push(2);
                        out.push(offset as u8);
                    }
                    runpos = out.len();
                    out.push(0u8); // placeholder for runlength
                }
                // encode the word
                out.push(*val as u8);
                out.push((*val >> 8) as u8);
                runlength += MEMSIZE;
                offset = 0; // from last similar word
                if runlength == u8::MAX as u32 {
                    out[runpos] = runlength as u8;
                    runpos = !0;
                    runlength = 0;
                }
            }
        }
        if runlength > 0 {
            out[runpos] = runlength as u8;
        }
        out.reserve(1 + 11 * self.users.len());
        out.push(4); // prepare to update the ships
        for (_ , user) in self.users.iter() {
            let ship = &user.ship;
            let x = ship.x as i16 as u16;
            let y = ship.y as i16 as u16;
            let h = (ship.heading * 32768.0) as i16 as u16;
            let c = ship.flight.color;
            if user.ident_time > 0 {
                out.push(6); // with IDENT
            } else {
                out.push(5); // simple update
            }
            out.push(x as u8); out.push((x >> 8) as u8);
            out.push(y as u8); out.push((y >> 8) as u8);
            out.push(h as u8); out.push((h >> 8) as u8);
            out.push(c as u8); out.push((c >> 8) as u8);
            out.push((user.eid) as u8); out.push((user.eid >> 8) as u8);
            if (user.requested_fields & 1) != 0 {
                out.push(7); // with user_login
                out.push(user.user_login.len() as u8);
                out.extend(user.user_login.bytes());
                out.push(user.user_name.len() as u8);
                out.extend(user.user_name.bytes());
            }
        }
        Arc::new(out)
    }
}

pub fn vm_write(split: &mut std::str::SplitWhitespace, vmproc: &mut dyn VMAccessPriv, start_addr: u16) {
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
                    if val == 0 { val = 1; }
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
    fn how_to_fp_convert() {
        let v = -31024.0f32;
        let e = v as i16 as u16;
        assert_eq!(e, 0x86D0);
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
        assert_eq!(Opcode::parse(0x3206), Opcode::Load(RegIndex::C2, RegIndex::C3, 3));
        assert_eq!(Opcode::parse(0x32C6), Opcode::Load(RegIndex::C2, RegIndex::C3, 0));
        assert_eq!(Opcode::parse(0x3216), Opcode::LoadInc(RegIndex::C2, RegIndex::C3, 3));
        assert_eq!(Opcode::parse(0x3226), Opcode::Gather(RegIndex::C2, RegIndex::C3, 3));
        assert_eq!(Opcode::parse(0x3236), Opcode::GatherInc(RegIndex::C2, RegIndex::C3, 3));
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
    fn vm_wait_run(vm: &mut SimulationVM) -> (&[(u16, u16); MEM_SHARED_SIZE_U], &Box<WaveProc>) {
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
                    // r7 should be 0,1,1,1 after halt
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
        // r7 should be 0,1,1,1 pointing at the halt
        assert_eq!(reg[1], VMRegister{x:0, y:1, z:1, w:1});
        assert_eq!(reg[2], VMRegister{x:4, y:4, z:4, w:4});
        assert_eq!(reg[3], VMRegister{x:to_code[0], y:to_code[1], z:to_code[2], w:to_code[3]});
        assert_eq!(reg[4], VMRegister{x:to_shared[0], y:to_shared[1], z:to_shared[2], w:to_shared[3]});
        assert_eq!(    pc, VMRegister{x:0, y:1, z:1, w:1});
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

    // TODO test for load inc seq 1 or gather inc seq 1 not incrementing any extra address words
    #[test]
    fn test_load_seq_inc() {
        let to_write = &[
            0
        ];
        let to_code = &[
            0x800c, // 0x40 SetOne R0
            0x900c, // 0x41 SetOne R1
            0xa00c, // 0x42 SetOne R2
            0xb00c, // 0x43 SetOne R3
            0x8fd6, // 0x44 LoadInc1 R0, Ri;
            0xeeee, // 0x45 [Data]
            0x0f16, // 0x46 Skip4 C0, Ri // Inc4 Ri
            0x80fc, // 0x47 SetAll R0
            0x80fc, // 0x48 SetAll R0
            0x80fc, // 0x49 SetAll R0
            0x80fc, // 0x4a SetAll R0
            0x9f96, // 0x4b LoadInc2 R1, Ri;
            0xeeee, // 0x4c [Data]
            0xeeee, // 0x4d [Data]
            0x0f16, // 0x4e Skip4 C0, Ri // Inc4 Ri
            0x90fc, // 0x4f SetAll R1
            0x90fc, // 0x50 SetAll R1
            0x90fc, // 0x51 SetAll R1
            0x90fc, // 0x52 SetAll R1
            0xaf56, // 0x53 LoadInc3 R2, Ri;
            0xeeee, // 0x54 [Data]
            0xeeee, // 0x55 [Data]
            0xeeee, // 0x56 [Data]
            0x0f16, // 0x57 Skip4 C0, Ri // Inc4 Ri
            0xa0fc, // 0x58 SetAll R1
            0xa0fc, // 0x59 SetAll R1
            0xa0fc, // 0x5a SetAll R1
            0xa0fc, // 0x5b SetAll R1
            0x0000, // 0x5c Halt
        ];
        let mut vm = vm_setup(to_write, to_code);
        let (_, proc) = vm_wait_run(&mut vm);
        assert!(proc.ins_ptr.x + 1 >= MEM_PRIV_NV_START + to_code.len() as u16);
        assert_eq!(proc.ins_ptr, VMRegister{ x: proc.ins_ptr.x, y:0, z:0, w:0 });
        assert_eq!(proc.reg[0], VMRegister{ x: 0xeeee, y:1, z:1, w:1 });
        assert_eq!(proc.reg[1], VMRegister{ x: 0xeeee, y:0xeeee, z:1, w:1 });
        assert_eq!(proc.reg[2], VMRegister{ x: 0xeeee, y:0xeeee, z:0xeeee, w:1 });
        assert_eq!(proc.reg[3], VMRegister{ x: 1, y:1, z:1, w:1 });
    }
    #[test]
    fn test_gather_seq_inc() {
        let to_write = &[
            /* 0x00 */ 0,0x8,0x10,0x14,
            /* 0x04 */ 0,0,0,0,
            // Ri.y    0:   1:
            /* 0x08 */ 0x10,0x11,0x12,0x13,
            /* 0x0c */ 0x20,0x21,0x22,0x23,
            /* 0x10 */ 0x30,0x31,0x32,0x33,
            /* 0x14 */ 0x40,0x41,0x42,0x43,
            /* 0x18 */ 0,0,0,0,
            /* 0x1c */ 0,0,0,0,
        ];
        let to_code = &[
            0xf014, // Move.yzw Ri, C0
            0x8ff6, // GatherInc1 R0, Ri; // no inc YZW
            0xe100, // [Data]
            0x9fb6, // GatherInc2 R1, Ri; // load Y=0x10
            0xe200, // [Data]
            0xaf76, // GatherInc3 R2, Ri; // load Y=0x11, Z=0x30
            0xe300, // [Data]
            0xbf36, // GatherInc4 R3, Ri; // load Y=0x12, Z=0x31, W=0x40
            0xe400, // [Data]             // Ri points at 0x13,0x32,0x41
            0x0000, // Halt
        ];
        let mut vm = vm_setup(to_write, to_code);
        let (_, proc) = vm_wait_run(&mut vm);
        assert!(proc.ins_ptr.x + 1 >= MEM_PRIV_NV_START + to_code.len() as u16);
        assert_eq!(proc.ins_ptr, VMRegister{
            x: proc.ins_ptr.x, // wherever PC ended up
            y:to_write[1]+3,
            z:to_write[2]+2,
            w:to_write[3]+1
        });
        assert_eq!(proc.reg[0], VMRegister{ x: 0xe100, y:0, z:0, w:0 });
        assert_eq!(proc.reg[1], VMRegister{ x: 0xe200, y:0x10, z:0, w:0 });
        assert_eq!(proc.reg[2], VMRegister{ x: 0xe300, y:0x11, z:0x30, w:0 });
        assert_eq!(proc.reg[3], VMRegister{ x: 0xe400, y:0x12, z:0x31, w:0x40 });
    }
    #[test]
    fn test_shr() {
        let to_write = &[
            0xffff, 0,0,0,
            0xff,0xff,0xff,0xff,
        ];
        let to_code = &[
            0x8004, // Move.xyzw R0, C0
            0x810A, // shift
            0x8004, // Move.xyzw R0, C0
            0x811A, // shift
            0x8004, // Move.xyzw R0, C0
            0x812A, // shift
            0x8004, // Move.xyzw R0, C0
            0x813A, // rotate
            0x8004, // Move.xyzw R0, C0
            0x810b, // shift
            0x8004, // Move.xyzw R0, C0
            0x811b, // shift
            0x8004, // Move.xyzw R0, C0
            0x812b, // shift
            0x8004, // Move.xyzw R0, C0
            0x813b, // rotate
                    // use litterals
            0x8004, // Move.xyzw R0, C0
            0x8f8A, // shift
            0x8004, // Move.xyzw R0, C0
            0x8f9A, // shift
            0x8004, // Move.xyzw R0, C0
            0x8faA, // shift
            0x8004, // Move.xyzw R0, C0
            0x8fbA, // rotate
            0x8004, // Move.xyzw R0, C0
            0x8f8b, // shift
            0x8004, // Move.xyzw R0, C0
            0x8f9b, // shift
            0x8004, // Move.xyzw R0, C0
            0x8fab, // shift
            0x8004, // Move.xyzw R0, C0
            0x8fbb, // rotate
        ];
        let mut vm = vm_setup(to_write, to_code);
        let (_, _) = vm_wait_run(&mut vm);
    }
    #[test]
    fn test_nav_module_fixed_target() {
        let to_write = &[ 0 ];
        let to_code = &[ 0 ];
        let mut vm = vm_setup(to_write, to_code);
        let user = vm.make_user(0);
        user.ship.x = 90.0f32;
        user.ship.y = 90.0f32;
        user.ship.vel_x = 0.0;
        user.ship.vel_y = 0.0;
        user.ship.spin = 0.0;
        user.ship.heading = 0.0;
        user.ship.nav.target_select = 0;
        //user.ship.nav.target_screen_x = 130;
        //user.ship.nav.target_screen_y = 50;
        user.ship.nav.target_screen_x = 90 ;//+ 40;
        user.ship.nav.target_screen_y = 90 - 40;
        let points = [
            (0, -40), (40, -40), (40, 0), (40i16, 40i16),
            (0, 40), (-40, 40), (-40, 0), (-40, -40)
        ];
        let headings = [0.0, 0.125, 0.250, 0.375, 0.500, 0.625, 0.750, 0.875, ];
        let exp_rel_heading_to = [
            0x0000, 0x7000, 0x6000, 0x5000,
            0x4000, 0x3000, 0x2000, 0x1000];
        let exp_rel_heading_fro = [
            0x4000, 0x3000, 0x2000, 0x1000,
            0x0000, 0x7000, 0x6000, 0x5000];
        let exp_abs_heading_to = [
            0x0000, 0x1000, 0x2000, 0x3000,
            0x4000, 0x5000, 0x6000, 0x7000];
        let exp_abs_heading_fro = [
            0x4000, 0x5000, 0x6000, 0x7000,
            0x0000, 0x1000, 0x2000, 0x3000];
        let mut rel_heading_to = [0; 8];
        let mut rel_heading_fro = [0; 8];
        let mut abs_heading_to = [0; 8];
        let mut compass = [0; 8];
        let mut abs_heading_fro = [0; 8];
        let mut rel_points = [(0,0); 8];
        for (index, &heading) in headings.iter().enumerate() {
            let user = vm.make_user(0);
            user.ship.heading = heading;
            vm_wait_run(&mut vm);
            let user = vm.make_user(0);
            rel_heading_to[index] =
                user.ship.nav.target_rel_heading_to as i16;
            rel_heading_fro[index] =
                user.ship.nav.target_rel_heading_fro as i16;
            rel_points[index] = (
                user.ship.nav.target_rel_dist_x as i16,
                user.ship.nav.target_rel_dist_y as i16);
            compass[index] = user.ship.flight.current_compass as i16;
        }
        for (index, &(point_x, point_y)) in points.iter().enumerate() {
            let user = vm.make_user(0);
            user.ship.heading = 0.25;
            user.ship.nav.target_screen_x = (90 + point_x) as u16;
            user.ship.nav.target_screen_y = (90 + point_y) as u16;
            vm_wait_run(&mut vm);
            let user = vm.make_user(0);
            abs_heading_to[index] =
                user.ship.nav.target_abs_heading_to as i16;
            abs_heading_fro[index] =
                user.ship.nav.target_abs_heading_fro as i16;
        }
        let user = vm.make_user(0);
        assert_eq!(user.ship.nav.current_ship_x, 90);
        assert_eq!(user.ship.nav.current_ship_y, 90);
        assert_eq!(rel_heading_to, exp_rel_heading_to, "{:04x?}", rel_heading_to);
        assert_eq!(rel_heading_fro, exp_rel_heading_fro, "{:04x?}", rel_heading_fro);
        assert_eq!(abs_heading_to, exp_abs_heading_to, "{:04x?}", abs_heading_to);
        assert_eq!(compass, exp_abs_heading_to, "{:04x?}", abs_heading_to);
        assert_eq!(abs_heading_fro, exp_abs_heading_fro, "{:04x?}", abs_heading_fro);
    }

    struct TestVM {
        expectations: Vec<Option<u16>>,
    }
    impl VMAccessPriv for TestVM {
        fn read_priv(&self, _: u16) -> u16 { 0 }
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
        //let partial: serde_json::Value = read_persist(the_data.as_slice()).expect("partial deserialize");
        //let mut reflected_partial: SimulationVM = serde_json::from_value(partial).expect("deserialization without errors");
        reflected.memory_invalidate();
        for (k, lhs) in state.users.iter() {
            let rhs = reflected.users.get(k).expect("user must exist after deserialization");
            let WaveProc {
                cval,
                reg,
                ins_ptr,
                priv_mem,
                sleep_for,
                lval,
                rval,
                defer,
                core_id,
                current_ins_addr,
                is_running,
                save_ri,
                save_except,
                bank1_select,
                protect,
                mod_selects,
                con_store
            } = lhs.proc.as_ref();
            let rproc = rhs.proc.as_ref();
            assert_eq!(cval,             &rproc.cval);
            assert_eq!(reg,              &rproc.reg);
            assert_eq!(ins_ptr,          &rproc.ins_ptr);
            assert_eq!(priv_mem,         &rproc.priv_mem);
            assert_eq!(sleep_for,        &rproc.sleep_for);
            assert_eq!(lval,             &rproc.lval);
            assert_eq!(rval,             &rproc.rval);
            assert_eq!(defer,            &rproc.defer);
            assert_eq!(core_id,          &rproc.core_id);
            assert_eq!(current_ins_addr, &rproc.current_ins_addr);
            assert_eq!(is_running,       &rproc.is_running);
            assert_eq!(save_ri,          &rproc.save_ri);
            assert_eq!(save_except,      &rproc.save_except);
            assert_eq!(bank1_select,     &rproc.bank1_select);
            assert_eq!(protect,          &rproc.protect);
            assert_eq!(mod_selects,      &rproc.mod_selects);
            assert_eq!(con_store,        &rproc.con_store);
            assert_eq!(&lhs.agent, &rhs.agent);
            assert_eq!(&lhs.ship, &rhs.ship);
            assert_eq!(&lhs.user_login, &rhs.user_login);
            assert_eq!(&lhs.user_name, &rhs.user_name);
            assert_eq!(&lhs.user_color, &rhs.user_color);
            assert_eq!(&lhs.user_color_loaded, &rhs.user_color_loaded);
        }
        assert_eq!(&state.memory, &reflected.memory);
    }
    #[test]
    fn persist_vm() {
        let mut users = HashMap::new();
        users.insert(9230529035839, Box::new(VMUser{
            eid: 1,
            proc: Box::new(WaveProc {
                cval: [VMRegister{x:342, y: 92, z: 1000, w: 32905}; 8],
                reg: [VMRegister{x:2, y: 1, z: 3, w: 6}; 7],
                lval: VMRegister{x:342, y: 92, z: 1000, w: 32905},
                rval: VMRegister{x:493, y: 0, z: 0, w: 0x8000},
                ins_ptr: VMRegister{x: 1010, y: 230, z: 2333, w: 0xffff},
                sleep_for: 22,
                defer: Some(DeferredOp::DelayLoad(0x32, RegIndex::R4)),
                priv_mem: [0; MEM_PRIV_NV_SIZE],
                is_running: true,
                bank1_select: 1,
                current_ins_addr: 0,
                save_except: VMRegister{x:343, y: 93, z: 1002, w: 32906},
                save_ri: VMRegister{x: 1015, y: 235, z: 2338, w: 0xe3bc},
                core_id: 1,
                con_store: VMConsts::default(),
                mod_selects: default_module_selects(),
                protect: VMProtections {
                    core_control: true,
                    reg_saves: true,
                    exec_sleep: true,
                    exec_halt_inval: true,
                    in_pma: false,
                    in_shared: false,
                    in_nta: false,
                    pma_from_nta: MemoryProtect::ReadOnly,
                    pma_from_sma: MemoryProtect::AccessException,
                    nta_from_sma: MemoryProtect::AccessException,
                },
            }),
            agent: Box::new(WaveProc {
                cval: [VMRegister{x:342, y: 92, z: 1000, w: 32905}; 8],
                reg: [VMRegister{x:2, y: 1, z: 3, w: 6}; 7],
                lval: VMRegister{x:342, y: 92, z: 1000, w: 32905},
                rval: VMRegister{x:493, y: 0, z: 0, w: 0x8000},
                ins_ptr: VMRegister{x: 1010, y: 230, z: 2333, w: 0xffff},
                sleep_for: 22,
                defer: Some(DeferredOp::DelayLoad(0x32, RegIndex::R4)),
                priv_mem: [0; MEM_PRIV_NV_SIZE],
                is_running: true,
                bank1_select: 1,
                current_ins_addr: 0,
                save_except: VMRegister{x:344, y: 94, z: 1003, w: 32907},
                save_ri: VMRegister{x: 1016, y: 236, z: 2339, w: 0xe3bd},
                core_id: 2,
                con_store: VMConsts::default(),
                mod_selects: default_module_selects(),
                protect: VMProtections {
                    core_control: true,
                    reg_saves: true,
                    exec_sleep: true,
                    exec_halt_inval: true,
                    in_pma: false,
                    in_shared: false,
                    in_nta: false,
                    pma_from_nta: MemoryProtect::ReadOnly,
                    pma_from_sma: MemoryProtect::AccessException,
                    nta_from_sma: MemoryProtect::AccessException,
                },
            }),
            ship: VMShip::default(),
            user_color_loaded: true,
            user_color: 0xcccccc,
            user_name: String::from("Test"),
            user_login: String::from("test"),
            ..Default::default()
        }));
        let mut state = SimulationVM {
            users, processes: VecDeque::new(),
            memory: [(4, 4); MEM_SHARED_SIZE_U]
        };
        state.memory_invalidate();
        reflective_persist_test(&state);
    }
}

