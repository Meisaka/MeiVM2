
use core::fmt::Display;
use serde::{
    de::Visitor,
    Serialize, Deserialize,
};
use crate::inc_addr;

#[derive(Debug, Default, PartialEq, Eq, Clone, Copy)]
pub struct VMRegister {
    pub x: u16, pub y: u16, pub z: u16, pub w: u16,
}

impl Display for VMRegister {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:04x} {:04x} {:04x} {:04x}", self.x, self.y, self.z, self.w)
    }
}

impl VMRegister {
    pub fn inc_addr(&mut self) -> u16 {
        self.x = inc_addr(self.x);
        self.x
    }
    pub fn index_mut(&mut self, addr: u8) -> &mut u16 {
        match addr & 3 {
            0 => &mut self.x, 1 => &mut self.y, 2 => &mut self.z, 3 => &mut self.w,
            _ => unreachable!()
        }
    }
    pub fn index(&self, addr: u8) -> u16 {
        match addr & 3 {
            0 => self.x, 1 => self.y, 2 => self.z, 3 => self.w, _ => unreachable!()
        }
    }
    pub fn index8(&self, addr: u8) -> u8 {
        (match addr & 7 {
            0 => self.x, 1 => self.x >> 8,
            2 => self.y, 3 => self.y >> 8,
            4 => self.z, 5 => self.z >> 8,
            6 => self.w, 7 => self.w >> 8,
            _ => unreachable!()
        }) as u8
    }
}

impl Serialize for VMRegister {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer {
        let x = self.x.to_le_bytes();
        let y = self.y.to_le_bytes();
        let z = self.z.to_le_bytes();
        let w = self.w.to_le_bytes();
        let out = [x[0], x[1], y[0], y[1], z[0], z[1], w[0], w[1]];
        serializer.serialize_bytes(&out)
    }
}

impl<'de> Deserialize<'de> for VMRegister {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
        where
            D: serde::Deserializer<'de> {
        struct VMRegisterVisitor;
        impl<'de> Visitor<'de> for VMRegisterVisitor {
            type Value = VMRegister;
            fn expecting(&self, formatter: &mut std::fmt::Formatter) -> std::fmt::Result {
                write!(formatter, "a buffer at least 8 bytes in length")
            }
            fn visit_bytes<E>(self, v: &[u8]) -> Result<Self::Value, E>
                where E: serde::de::Error
            {
                if v.len() < 8 {
                    return Err(E::invalid_length(v.len(), &"at least 8 bytes"))
                }
                let x = u16::from_le_bytes(v[0..2].try_into().unwrap());
                let y = u16::from_le_bytes(v[2..4].try_into().unwrap());
                let z = u16::from_le_bytes(v[4..6].try_into().unwrap());
                let w = u16::from_le_bytes(v[6..8].try_into().unwrap());
                Ok(VMRegister{x,y,z,w})
            }
        }
        deserializer.deserialize_bytes(VMRegisterVisitor)
    }
}

