
use std::io::Write;
use std::fmt::Display;

use serde::de::{EnumAccess, MapAccess, SeqAccess, VariantAccess};
use serde::forward_to_deserialize_any;
use serde::ser::Error;
use serde::{
    de::Deserializer, ser::{SerializeMap, SerializeSeq, SerializeStruct, SerializeStructVariant, SerializeTuple, SerializeTupleStruct, SerializeTupleVariant, Serializer}, Deserialize, Serialize
};

#[derive(Debug)]
pub enum PersistError {
    EndOfInput(usize, PersistKind, usize, usize),
    MissingKind,
    MissingKindLength,
    Custom(String),
    InvalidInput,
    UnknownKind,
    NextMapValueCalledWithoutKey,
    ExpectingString(PersistKind),
    ExpectingEnumSequence(PersistKind),
    VariantWithoutValue,
    VariantUnexpectedValue,
    EnumSeqEmpty,
    EnumTooLong(usize),
    Meh,
}

impl Display for PersistError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Persist Error")
    }
}

impl serde::de::Error for PersistError {
    fn custom<T>(msg:T) -> Self where T:Display {
        PersistError::Custom(format!("{}", msg))
    }
}

impl serde::ser::StdError for PersistError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl serde::ser::Error for PersistError {
    fn custom<T>(msg:T) -> Self where T:Display {
        PersistError::Custom(format!("{}", msg))
    }
}

struct PersistThing {
    buf: Vec<u8>,
}

struct PersistSeq {
    buf: Vec<u8>,
}

impl SerializeSeq for PersistSeq {
    type Ok = Vec<u8>;
    type Error = PersistError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where T: ?Sized + serde::Serialize {
        let mut buf = Vec::new();
        core::mem::swap(&mut buf, &mut self.buf);
        self.buf = value.serialize(PersistThing{buf})?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.buf)
    }
}

impl SerializeTuple for PersistSeq {
    type Ok = Vec<u8>;
    type Error = PersistError;

    fn serialize_element<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where T: ?Sized + serde::Serialize { SerializeSeq::serialize_element(self, value) }

    fn end(self) -> Result<Self::Ok, Self::Error> { SerializeSeq::end(self) }
}

impl SerializeTupleVariant for PersistSeq {
    type Ok = Vec<u8>;
    type Error = PersistError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where T: ?Sized + serde::Serialize { SerializeSeq::serialize_element(self, value) }

    fn end(self) -> Result<Self::Ok, Self::Error> { SerializeSeq::end(self) }
}

impl SerializeTupleStruct for PersistSeq {
    type Ok = Vec<u8>;
    type Error = PersistError;

    fn serialize_field<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where T: ?Sized + serde::Serialize { SerializeSeq::serialize_element(self, value) }

    fn end(self) -> Result<Self::Ok, Self::Error> { SerializeSeq::end(self) }
}

struct PersistMap {
    buf: Vec<u8>,
    maybe_values: Option<(usize, Vec<u8>)>,
}

impl SerializeMap for PersistMap {
    type Ok = Vec<u8>;
    type Error = PersistError;

    fn serialize_key<T>(&mut self, key: &T) -> Result<(), Self::Error>
    where T: ?Sized + serde::Serialize {
        let mut buf = Vec::new();
        if let Some((count, tmpbuf)) = self.maybe_values.as_mut() {
            *count += 1;
            core::mem::swap(&mut buf, tmpbuf);
            *tmpbuf = key.serialize(PersistThing{buf})?;
        } else {
            core::mem::swap(&mut buf, &mut self.buf);
            self.buf = key.serialize(PersistThing{buf})?;
        }
        Ok(())
    }
    fn serialize_value<T>(&mut self, value: &T) -> Result<(), Self::Error>
    where T: ?Sized + serde::Serialize {
        let mut buf = Vec::new();
        if let Some((_, tmpbuf)) = self.maybe_values.as_mut() {
            core::mem::swap(&mut buf, tmpbuf);
            *tmpbuf = value.serialize(PersistThing{buf})?;
        } else {
            core::mem::swap(&mut buf, &mut self.buf);
            self.buf = value.serialize(PersistThing{buf})?;
        }
        Ok(())
    }
    fn serialize_entry<K, V>(&mut self, key: &K, value: &V) -> Result<(), Self::Error>
        where
            K: ?Sized + serde::Serialize,
            V: ?Sized + serde::Serialize, {
        let mut buf = Vec::new();
        if let Some((count, tmpbuf)) = self.maybe_values.as_mut() {
            *count += 1;
            core::mem::swap(&mut buf, tmpbuf);
            let buf = key.serialize(PersistThing{buf})?;
            *tmpbuf = value.serialize(PersistThing{buf})?;
        } else {
            core::mem::swap(&mut buf, &mut self.buf);
            let buf = key.serialize(PersistThing{buf})?;
            self.buf = value.serialize(PersistThing{buf})?;
        }
        Ok(())
    }
    fn end(mut self) -> Result<Self::Ok, Self::Error> {
        if let Some((count, tmpbuf)) = self.maybe_values.as_mut() {
            write_kind(&mut self.buf, PersistKind::Map(*count));
            self.buf.extend_from_slice(tmpbuf);
        }
        Ok(self.buf)
    }
}

impl SerializeStruct for PersistMap {
    type Ok = Vec<u8>;
    type Error = PersistError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where
        T: ?Sized + serde::Serialize {
        let mut buf = Vec::new();
        core::mem::swap(&mut buf, &mut self.buf);
        let buf = PersistThing{buf}.serialize_str(key)?;
        self.buf = value.serialize(PersistThing{buf})?;
        Ok(())
    }

    fn end(self) -> Result<Self::Ok, Self::Error> {
        Ok(self.buf)
    }
}

impl SerializeStructVariant for PersistMap {
    type Ok = Vec<u8>;
    type Error = PersistError;

    fn serialize_field<T>(&mut self, key: &'static str, value: &T) -> Result<(), Self::Error>
    where T: ?Sized + serde::Serialize { SerializeStruct::serialize_field(self, key, value) }

    fn end(self) -> Result<Self::Ok, Self::Error> { SerializeStruct::end(self) }
}

#[derive(Debug, Clone, Copy)]
pub enum PersistKind {
    PosInt(u64),
    NegInt(u64),
    Bytes(usize),
    String(usize),
    Sequence(usize),
    Map(usize),
    Meh,
    Float32(f32),
    Float64(f64),
    Bool(bool),
    None,
}

fn write_kind(buf: &mut Vec<u8>, kind: PersistKind) {
    /*
     * integer
     * l = n & 1f;
     *   0..=17 => l length
     *   18 => +1 bytes of length
     *   19 => +2 bytes of length
     *   1a => +4 bytes of length
     *   1b => +8 bytes of length
     * 0/1 +int
     * 2/3 -int
     * 4/5 bytes
     * 6/7 string
     * 8/9 sequence
     * a/b map
     * f4 false
     * f5 true
     * f6 none
     * fa f32
     * fb f64
     */
    let put_int = |buf: &mut Vec<u8>, kind: u8, value: u64| {
        let kind_val = kind << 5;
        match value {
            0..=0x17 => buf.push((value as u8) | kind_val),
            0x18..=0xff => {
                buf.push(kind_val | 0x18);
                buf.push(value as u8);
            }
            0x100..=0xffff => {
                buf.push(kind_val | 0x19);
                buf.push((value >> 8) as u8);
                buf.push((value) as u8);
            }
            0x1_0000..=0xffff_ffff => {
                buf.push(kind_val | 0x1a);
                buf.push((value >> 24) as u8);
                buf.push((value >> 16) as u8);
                buf.push((value >> 8) as u8);
                buf.push((value) as u8);
            }
            0x1_0000_0000..=0xffffffff_ffffffff => {
                buf.push(kind_val | 0x1b);
                buf.push((value >> 56) as u8);
                buf.push((value >> 48) as u8);
                buf.push((value >> 40) as u8);
                buf.push((value >> 32) as u8);
                buf.push((value >> 24) as u8);
                buf.push((value >> 16) as u8);
                buf.push((value >> 8) as u8);
                buf.push((value) as u8);
            }
        }
    };
    match kind {
        PersistKind::PosInt(v)   => put_int(buf, 0, v),
        PersistKind::NegInt(v)   => put_int(buf, 1, v),
        PersistKind::Bytes(v)    => put_int(buf, 2, v as _),
        PersistKind::String(v)   => put_int(buf, 3, v as _),
        PersistKind::Sequence(v) => put_int(buf, 4, v as _),
        PersistKind::Map(v)      => put_int(buf, 5, v as _),
        PersistKind::Meh   => put_int(buf, 6, 0),
        PersistKind::Bool(v)  => buf.push(if v { 0xf5 } else { 0xf4 }),
        PersistKind::None  => buf.push(0xf6),
        PersistKind::Float32(v) => { buf.push(0xfa); buf.extend(v.to_be_bytes().iter()); }
        PersistKind::Float64(v) => { buf.push(0xfb); buf.extend(v.to_be_bytes().iter()); }
    }
}


impl Serializer for PersistThing {
    type Ok = Vec<u8>;
    type Error = PersistError;
    type SerializeSeq = PersistSeq;
    type SerializeTuple = PersistSeq;
    type SerializeTupleStruct = PersistSeq;
    type SerializeTupleVariant = PersistSeq;
    type SerializeMap = PersistMap;
    type SerializeStruct = PersistMap;
    type SerializeStructVariant = PersistMap;

    fn serialize_bool(mut self, v: bool) -> Result<Self::Ok, Self::Error> {
        write_kind(&mut self.buf, PersistKind::Bool(v));
        Ok(self.buf)
    }
    fn serialize_i8(self, v: i8) -> Result<Self::Ok, Self::Error> { self.serialize_i64(v as _) }
    fn serialize_i16(self, v: i16) -> Result<Self::Ok, Self::Error> { self.serialize_i64(v as _) }
    fn serialize_i32(self, v: i32) -> Result<Self::Ok, Self::Error> { self.serialize_i64(v as _) }
    fn serialize_u8(self, v: u8) -> Result<Self::Ok, Self::Error> { self.serialize_u64(v as _) }
    fn serialize_u16(self, v: u16) -> Result<Self::Ok, Self::Error> { self.serialize_u64(v as _) }
    fn serialize_u32(self, v: u32) -> Result<Self::Ok, Self::Error> { self.serialize_u64(v as _) }

    fn serialize_i64(mut self, v: i64) -> Result<Self::Ok, Self::Error> {
        if v < 0 {
            write_kind(&mut self.buf, PersistKind::NegInt(-v as _));
        } else {
            write_kind(&mut self.buf, PersistKind::PosInt(v as _));
        }
        Ok(self.buf)
    }
    fn serialize_u64(mut self, v: u64) -> Result<Self::Ok, Self::Error> {
        write_kind(&mut self.buf, PersistKind::PosInt(v));
        Ok(self.buf)
    }

    fn serialize_f32(mut self, v: f32) -> Result<Self::Ok, Self::Error> {
        write_kind(&mut self.buf, PersistKind::Float32(v));
        Ok(self.buf)
    }
    fn serialize_f64(mut self, v: f64) -> Result<Self::Ok, Self::Error> {
        write_kind(&mut self.buf, PersistKind::Float64(v));
        Ok(self.buf)
    }

    fn serialize_char(self, c: char) -> Result<Self::Ok, Self::Error> {
        let mut buf = self.buf;
        let mut dst = [0; 4];
        let s = c.encode_utf8(&mut dst);
        write_kind(&mut buf, PersistKind::String(s.len()));
        buf.extend(s.as_bytes());
        Ok(buf)
    }

    fn serialize_str(self, v: &str) -> Result<Self::Ok, Self::Error> {
        let mut buf = self.buf;
        buf.reserve(v.len() + 9);
        write_kind(&mut buf, PersistKind::String(v.len()));
        buf.extend(v.as_bytes());
        Ok(buf)
    }

    fn serialize_bytes(self, v: &[u8]) -> Result<Self::Ok, Self::Error> {
        let mut buf = self.buf;
        buf.reserve(v.len() + 9);
        write_kind(&mut buf, PersistKind::Bytes(v.len()));
        buf.write_all(v).ok();
        Ok(buf)
    }

    fn serialize_none(self) -> Result<Self::Ok, Self::Error> {
        let mut buf = self.buf;
        write_kind(&mut buf, PersistKind::None);
        Ok(buf)
    }

    fn serialize_some<T>(self, value: &T) -> Result<Self::Ok, Self::Error>
    where T: ?Sized + serde::Serialize {
        value.serialize(self)
    }

    fn serialize_unit(self) -> Result<Self::Ok, Self::Error> {
        let mut buf = self.buf;
        write_kind(&mut buf, PersistKind::None);
        Ok(buf)
    }

    fn serialize_unit_struct(self, _name: &'static str) -> Result<Self::Ok, Self::Error> {
        self.serialize_unit()
    }

    fn serialize_unit_variant(
        mut self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
    ) -> Result<Self::Ok, Self::Error> {
        write_kind(&mut self.buf, PersistKind::Sequence(1));
        self.serialize_str(variant)
    }

    fn serialize_newtype_struct<T>(
        self,
        _name: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where T: ?Sized + serde::Serialize {
        value.serialize(self)
    }

    fn serialize_newtype_variant<T>(
        mut self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        value: &T,
    ) -> Result<Self::Ok, Self::Error>
    where T: ?Sized + serde::Serialize {
        write_kind(&mut self.buf, PersistKind::Sequence(2));
        let buf = self.serialize_str(variant)?;
        value.serialize(Self{buf})
    }

    fn serialize_seq(mut self, len: Option<usize>) -> Result<Self::SerializeSeq, Self::Error> {
        write_kind(&mut self.buf, PersistKind::Sequence(len.expect("a length is required for seqence")));
        Ok(Self::SerializeSeq{buf: self.buf})
    }

    fn serialize_tuple(mut self, len: usize) -> Result<Self::SerializeTuple, Self::Error> {
        write_kind(&mut self.buf, PersistKind::Sequence(len));
        Ok(Self::SerializeSeq{buf: self.buf})
    }

    fn serialize_tuple_struct(
        mut self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleStruct, Self::Error> {
        write_kind(&mut self.buf, PersistKind::Sequence(len));
        Ok(Self::SerializeSeq{buf: self.buf})
    }

    fn serialize_tuple_variant(
        mut self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeTupleVariant, Self::Error> {
        write_kind(&mut self.buf, PersistKind::Sequence(2));
        let mut buf = self.serialize_str(variant)?;
        write_kind(&mut buf, PersistKind::Sequence(len));
        Ok(PersistSeq{buf})
    }

    fn serialize_map(mut self, len: Option<usize>) -> Result<Self::SerializeMap, Self::Error> {
        if let Some(len) = len {
            write_kind(&mut self.buf, PersistKind::Map(len));
            Ok(Self::SerializeMap{
                buf: self.buf,
                maybe_values: None
            })
        } else {
            Ok(Self::SerializeMap{
                buf: self.buf,
                maybe_values: Some((0, Vec::new())),
            })
        }
    }

    fn serialize_struct(
        mut self,
        _name: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStruct, Self::Error> {
        write_kind(&mut self.buf, PersistKind::Map(len));
        Ok(Self::SerializeMap{buf:self.buf, maybe_values: None})
    }

    fn serialize_struct_variant(
        mut self,
        _name: &'static str,
        _variant_index: u32,
        variant: &'static str,
        len: usize,
    ) -> Result<Self::SerializeStructVariant, Self::Error> {
        write_kind(&mut self.buf, PersistKind::Sequence(2));
        let mut buf = self.serialize_str(variant)?;
        write_kind(&mut buf, PersistKind::Map(len));
        Ok(Self::SerializeMap{buf, maybe_values: None})
    }
}

struct UnpersistBuffer<'s> {
    buf: &'s [u8],
    pos: usize,
}
impl<'s> UnpersistBuffer<'s> {
    fn read_bytes(&mut self, length: usize, kind: PersistKind) -> Result<&'s [u8], PersistError> {
        if self.buf.len() < length { Err(PersistError::EndOfInput(self.pos, kind, self.buf.len(), length))? }
        let (pre, suf) = self.buf.split_at(length);
        self.pos += length;
        self.buf = suf;
        Ok(pre)
    }
    fn peek_kind(buf: &'s[u8]) -> Result<(PersistKind, &'s[u8], usize), PersistError> {
        let Some((kind_byte, suf)) = buf.split_first() else {
            Err(PersistError::MissingKind)?
        };
        let kind = *kind_byte >> 5;
        let val = *kind_byte & 0x1f;
        let decode_len = || match val {
            0..=0x17 => Some((val as u64, suf, 1)),
            0x18 => {
                suf.split_first_chunk::<1>()
                    .map(|([val], suf)| (*val as u64, suf, 2))
            }
            0x19 => {
                suf.split_first_chunk::<2>()
                    .map(|(pre, suf)| (u16::from_be_bytes(*pre) as u64, suf, 3))
            }
            0x1a => {
                suf.split_first_chunk::<4>()
                    .map(|(pre, suf)| (u32::from_be_bytes(*pre) as u64, suf, 5))
            }
            0x1b => {
                suf.split_first_chunk::<8>()
                    .map(|(pre, suf)| (u64::from_be_bytes(*pre), suf, 9))
            }
            _ => None,
        };
        match kind {
            0 => decode_len().map(|(value, suf, sz)| (PersistKind::PosInt(value), suf, sz)),
            1 => decode_len().map(|(value, suf, sz)| (PersistKind::NegInt(value), suf, sz)),
            2 => decode_len().map(|(value, suf, sz)| (PersistKind::Bytes(value as _), suf, sz)),
            3 => decode_len().map(|(value, suf, sz)| (PersistKind::String(value as _), suf, sz)),
            4 => decode_len().map(|(value, suf, sz)| (PersistKind::Sequence(value as _), suf, sz)),
            5 => decode_len().map(|(value, suf, sz)| (PersistKind::Map(value as _), suf, sz)),
            6 => Some((PersistKind::Meh, suf, 1)),
            7 => match val {
                0x14 => Some((PersistKind::Bool(false), suf, 1)),
                0x15 => Some((PersistKind::Bool(true), suf, 1)),
                0x16 => Some((PersistKind::None, suf, 1)),
                0x17 => Some((PersistKind::None, suf, 1)),
                0x1a => suf.split_first_chunk::<4>()
                    .map(|(pre, suf)| (PersistKind::Float32(f32::from_be_bytes(*pre)), suf, 5)),
                0x1b => suf.split_first_chunk::<8>()
                    .map(|(pre, suf)| (PersistKind::Float64(f64::from_be_bytes(*pre)), suf, 9)),
                _ => Err(PersistError::UnknownKind)?
            }
            _ => unreachable!("parse_kind")
        }.ok_or_else(|| PersistError::MissingKindLength)
    }

    fn read_kind(&mut self) -> Result<PersistKind, PersistError> {
        //let pos = self.pos;
        let (kind, suf, sz) = Self::peek_kind(self.buf)?;
        self.buf = suf;
        self.pos += sz;
        /*
        let rem_pos = self.pos;
        eprintln!("any: {pos}..{rem_pos} {kind:?} ({})", {
            if let PersistKind::String(length) = kind {
                if self.buf.len() >= length {
                    let (pre, _) = self.buf.split_at(length);
                    if let Ok(s) = core::str::from_utf8(pre) {
                        s
                    } else { "<!utf>" }
                } else {"<!len>"}
            } else {""}
        });
        */
        Ok(kind)
    }
}

struct UnpersistThing<'t, 's> {
    buf: &'t mut UnpersistBuffer<'s>,
}

struct UnpersistSeq<'t, 's> {
    buf: &'t mut UnpersistBuffer<'s>,
    pos: usize,
    length: usize,
}

impl<'de: 'a, 'a> SeqAccess<'de> for UnpersistSeq<'_, 'de> {
    type Error = PersistError;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
    where T: serde::de::DeserializeSeed<'de> {
        if self.pos < self.length {
            self.pos += 1;
            Ok(Some(seed.deserialize(UnpersistThing{buf: self.buf})?))
        } else {
            Ok(None)
        }
    }

    fn next_element<T>(&mut self) -> Result<Option<T>, Self::Error>
    where T: Deserialize<'de> {
        self.next_element_seed(std::marker::PhantomData)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.length)
    }
}

struct UnpersistMap<'t, 's> {
    buf: &'t mut UnpersistBuffer<'s>,
    pos: usize,
    have_key: bool,
    length: usize,
}

impl<'de: 's, 's, 't> MapAccess<'de> for UnpersistMap<'t, 'de> {
    type Error = PersistError;

    fn size_hint(&self) -> Option<usize> {
        Some(self.length)
    }
    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
    where K: serde::de::DeserializeSeed<'de> {
        if self.pos >= self.length { return Ok(None) }
        let key = seed.deserialize(UnpersistThing{buf: self.buf})?;
        self.have_key = true;
        Ok(Some(key))
    }

    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
    where V: serde::de::DeserializeSeed<'de> {
        if !self.have_key { return Err(PersistError::NextMapValueCalledWithoutKey) }
        let value = seed.deserialize(UnpersistThing{buf: self.buf})?;
        self.pos += 1;
        self.have_key = false;
        Ok(value)
    }
}

impl<'de: 's, 's, 't> Deserializer<'de> for UnpersistThing<'t, 'de> {
    type Error = PersistError;

    fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where V: serde::de::Visitor<'de> {
        let kind = self.buf.read_kind()?;
        match kind {
            PersistKind::PosInt(v) => {
                visitor.visit_u64(v)
            }
            PersistKind::NegInt(v) => {
                let n = -(v as i64);
                visitor.visit_i64(n)
            }
            PersistKind::Bytes(v) => {
                let bytes = self.buf.read_bytes(v, PersistKind::Bytes(v))?;
                visitor.visit_borrowed_bytes(bytes)
            }
            PersistKind::String(v) => {
                let bytes = self.buf.read_bytes(v, PersistKind::String(v))?;
                let s = core::str::from_utf8(bytes).map_err(|e| PersistError::custom(e))?;
                visitor.visit_borrowed_str(s)
            }
            PersistKind::Sequence(length) => {
                visitor.visit_seq(UnpersistSeq{buf: self.buf, length, pos: 0})
            }
            PersistKind::Map(length) => {
                visitor.visit_map(UnpersistMap{buf: self.buf, length, pos: 0, have_key: false})
            }
            PersistKind::Float32(v) => {
                visitor.visit_f32(v)
            }
            PersistKind::Float64(v) => {
                visitor.visit_f64(v)
            }
            PersistKind::Meh => visitor.visit_unit(),
            PersistKind::Bool(v) => visitor.visit_bool(v),
            PersistKind::None => visitor.visit_unit(),
        }
    }

    forward_to_deserialize_any! {
        bool i8 i16 i32 i64 u8 u16 u32 u64 f32 f64 bytes byte_buf
        tuple seq map unit unit_struct newtype_struct tuple_struct struct
    }
    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: serde::de::Visitor<'de> {
        let length;
        match self.buf.read_kind()? {
            PersistKind::String(l) => { length = l; }
            k @ _ => {
                return Err(PersistError::ExpectingString(k))
            }
        };
        let bytes = self.buf.read_bytes(length, PersistKind::String(length))?;
        let s = core::str::from_utf8(bytes).map_err(|e| PersistError::custom(e))?;
        visitor.visit_borrowed_str(s)
    }
    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value, Self::Error>
        where V: serde::de::Visitor<'de> {
        let length;
        match self.buf.read_kind()? {
            PersistKind::String(l) => { length = l; }
            k @ _ => {
                return Err(PersistError::ExpectingString(k))
            }
        };
        let bytes = self.buf.read_bytes(length, PersistKind::String(length))?;
        let s = core::str::from_utf8(bytes).map_err(|e| PersistError::custom(e))?;
        visitor.visit_string(String::from(s))
    }

    fn deserialize_char<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where V: serde::de::Visitor<'de> {
        let length;
        match self.buf.read_kind()? {
            PersistKind::String(l) => { length = l; }
            k @ _ => {
                return Err(PersistError::ExpectingString(k))
            }
        };
        let bytes = self.buf.read_bytes(length, PersistKind::String(length))?;
        let s = core::str::from_utf8(bytes).map_err(|e| PersistError::custom(e))?;
        visitor.visit_char(s.chars().next().ok_or_else(|| PersistError::InvalidInput)?)
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where V: serde::de::Visitor<'de> {
        let (kind, remain, sz) = UnpersistBuffer::peek_kind(self.buf.buf)?;
        if let PersistKind::None = kind {
            self.buf.buf = remain;
            self.buf.pos += sz;
            visitor.visit_none()
        } else {
            visitor.visit_some(self)
        }
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value, Self::Error>
    where V: serde::de::Visitor<'de> {
        struct Accessor<'de, 's> {
            buf: &'s mut UnpersistBuffer<'de>,
            have_value: bool,
        }
        impl<'de: 'a, 'a> VariantAccess<'de> for Accessor<'de, '_> {
            type Error = PersistError;

            fn unit_variant(self) -> Result<(), Self::Error> {
                if self.have_value { Err(PersistError::VariantUnexpectedValue) }
                else { Ok(()) }
            }

            fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
            where T: serde::de::DeserializeSeed<'de> {
                if !self.have_value { Err(PersistError::VariantWithoutValue)? }
                seed.deserialize(UnpersistThing{buf: self.buf})
            }

            fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
            where V: serde::de::Visitor<'de> {
                if !self.have_value { Err(PersistError::VariantWithoutValue)? }
                UnpersistThing{buf:self.buf}.deserialize_seq(visitor)
            }

            fn struct_variant<V>(
                self,
                _fields: &'static [&'static str],
                visitor: V,
            ) -> Result<V::Value, Self::Error>
            where V: serde::de::Visitor<'de> {
                if !self.have_value { Err(PersistError::VariantWithoutValue)? }
                UnpersistThing{buf: self.buf}.deserialize_map(visitor)
            }
        }
        impl<'de: 'a, 'a> EnumAccess<'de> for Accessor<'de, '_> {
            type Error = PersistError;
            type Variant = Self;

            fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
            where V: serde::de::DeserializeSeed<'de> {
                Ok((seed.deserialize(UnpersistThing{buf: self.buf})?, self))
            }
        }
        let seq_len;
        match self.buf.read_kind()? {
            PersistKind::Sequence(l) => {
                if l == 0 { return Err(PersistError::EnumSeqEmpty) }
                seq_len = l;
            }
            k @ _ => return Err(PersistError::ExpectingEnumSequence(k)),
        };
        if seq_len > 2 { return Err(PersistError::EnumTooLong(seq_len)) }
        visitor.visit_enum(Accessor{buf: self.buf, have_value: seq_len > 1 })
    }

    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where V: serde::de::Visitor<'de> {
        self.deserialize_str(visitor)
    }

    fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
    where V: serde::de::Visitor<'de> {
        self.deserialize_any(visitor)
    }
}

pub fn write_persist<V>(value: &V) -> Result<Vec<u8>, PersistError> where V: ?Sized + Serialize {
    value.serialize(PersistThing{buf: Vec::new()})
}

pub fn read_persist<'de, V>(value: &'de[u8]) -> Result<V, PersistError> where V: ?Sized + Deserialize<'de> {
    let mut buf = UnpersistBuffer{buf: value, pos: 0};
    V::deserialize(UnpersistThing{buf: &mut buf})
}

