use std::any::TypeId;
use std::cmp::min;
use std::fmt::Display;


use log::trace;
use nom::number::complete::{le_f32, le_f64, le_i32, le_u32, le_u8};
use serde::de::{self, DeserializeSeed, EnumAccess, MapAccess, SeqAccess, Visitor};

use crate::TypeReaderSpec;
use crate::{parse_7bit_int, ParseError, TypeRegistry, TypeSpec};

#[derive(Debug)]
pub struct Error {
    msg: String,
}

impl Error {
    pub fn new(msg: String) -> Self {
        Self { msg }
    }
}

type Result<T> = std::result::Result<T, Error>;

impl std::fmt::Display for Error {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(&self.msg)
    }
}

impl std::error::Error for Error {}

impl de::Error for Error {
    fn custom<T: Display>(msg: T) -> Self {
        Error {
            msg: format!("{msg}"),
        }
    }
}

impl From<anyhow::Error> for Error {
    fn from(value: anyhow::Error) -> Self {
        Self {
            msg: value.to_string(),
        }
    }
}

macro_rules! err {
    ($($args:expr),+) => {{
        Err($crate::de::Error::new(format!($($args),+)))
    }}
}

macro_rules! error {
    ($($args:expr),+) => {{
        $crate::de::Error::new(format!($($args),+))
    }}
}

pub struct Deserializer<'de> {
    // This string starts with the input data and characters are truncated off
    // the beginning as data is parsed.
    input: &'de [u8],
    registry: &'de TypeRegistry,
    reader_table: &'de Vec<TypeReaderSpec>,
    type_id: TypeId,
}

impl<'de> Deserializer<'de> {
    // By convention, `Deserializer` constructors are named like `from_xyz`.
    // That way basic use cases are satisfied by something like
    // `serde_json::from_str(...)` while advanced use cases that require a
    // deserializer can make one with `serde_json::Deserializer::from_str(...)`.
    pub fn from_bytes(
        registry: &'de TypeRegistry,
        reader_table: &'de Vec<TypeReaderSpec>,
        input: &'de [u8],
        type_id: TypeId,
    ) -> Self {
        Self {
            input,
            registry,
            reader_table,
            type_id,
        }
    }

    pub fn read_varint(&mut self) -> Result<u32> {
        let (data, val) =
            parse_7bit_int(self.input).map_err(|e| error!("error reading varint: {e}"))?;
        self.input = data;
        Ok(val)
    }

    pub fn read_bool(&mut self) -> Result<bool> {
        let (data, val) =
            le_u8::<_, ParseError>(self.input).map_err(|e| error!("error reading bool: {e}"))?;
        self.input = data;
        Ok(val != 0)
    }

    pub fn read_u32(&mut self) -> Result<u32> {
        let (data, val) =
            le_u32::<_, ParseError>(self.input).map_err(|e| error!("error reading u32: {e}"))?;
        self.input = data;
        Ok(val)
    }

    pub fn read_i32(&mut self) -> Result<i32> {
        let (data, val) =
            le_i32::<_, ParseError>(self.input).map_err(|e| error!("error reading i32: {e}"))?;
        self.input = data;
        Ok(val)
    }

    pub fn read_f32(&mut self) -> Result<f32> {
        let (data, val) =
            le_f32::<_, ParseError>(self.input).map_err(|e| error!("error reading f32: {e}"))?;
        self.input = data;
        Ok(val)
    }

    pub fn read_f64(&mut self) -> Result<f64> {
        let (data, val) =
            le_f64::<_, ParseError>(self.input).map_err(|e| error!("error reading f64: {e}"))?;
        self.input = data;
        Ok(val)
    }

    pub fn read_string(&mut self) -> Result<String> {
        trace!("reading str {:x?}", self.peek(64));
        self.read_and_validate_type(TypeId::of::<String>())?;
        let len = self.read_varint()? as usize;
        trace!("string length {len}");
        let str_data = &self.input[..len];
        trace!("string data {str_data:x?}");
        let val = String::from_utf8_lossy(str_data);
        trace!("string {val}");
        self.input = &self.input[len..];

        Ok(val.to_string())
    }

    fn peek(&self, max_len: usize) -> &[u8] {
        let len = min(max_len, self.input.len());
        &self.input[..len]
    }

    pub fn read_and_validate_type(&mut self, type_id: TypeId) -> Result<TypeSpec> {
        let spec = self
            .registry
            .get(self.type_id)
            .expect("type spec to exist")
            .iter()
            .next()
            .expect("type spec to exist");
        if !spec.tagged {
            return Ok(spec.clone());
        }

        let reader_index = self.read_varint()?;
        if reader_index == 0 {
            return err!("encountered null type index on non-nullable type (consider wrapping in `Option<T>`).");
        }
        let reader_spec = self
            .reader_table
            .get((reader_index - 1) as usize)
            .ok_or_else(|| error!("invalid reader index {reader_index}"))?;

        let type_specs = self.registry.get(type_id)?;

        type_specs
            .iter()
            .find(|&spec| {
                // Special case for ReflectiveReader on known types with no subtypes.
                if reader_spec.name == "Microsoft.Xna.Framework.Content.ReflectiveReader"
                    && spec.sub_types.is_empty()
                    && reader_spec.sub_types[0] == spec.name
                {
                    return true;
                }

                if spec.name != reader_spec.name {
                    return false;
                }

                if spec.sub_types.len() != reader_spec.sub_types.len() {
                    return false;
                }

                spec.sub_types
                    .iter()
                    .zip(reader_spec.sub_types.iter())
                    .fold(true, |acc, (type_id, name)| {
                        let Ok(specs) = self.registry.get(*type_id) else {
                            return false;
                        };
                        trace!("{specs:?}");
                        acc && specs
                            .iter()
                            .any(|spec| spec.name == *name && spec.sub_types.is_empty())
                    })
            })
            .cloned()
            .ok_or_else(|| {
                error!(
                    "encountered reader {:?} does not match any of the expected types: {type_specs:?}",
                    reader_spec
                )
            })
    }
}

impl<'de> Deserializer<'de> {}

impl<'de, 'a> de::Deserializer<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    fn deserialize_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_any not supported")
    }

    fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        trace!("parsing bool {:x?}", self.peek(1));
        let val = self.input[0];
        self.input = &self.input[1..];
        visitor.visit_bool(val != 0)
    }

    fn deserialize_i8<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_i8 not supported")
    }

    fn deserialize_i16<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_i16 not supported")
    }

    fn deserialize_i32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        trace!("parsing i32 {:x?}", self.peek(4));
        let val = self.read_i32()?;
        trace!("   -> {val}");
        visitor.visit_i32(val)
    }

    fn deserialize_i64<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_i64 not supported")
    }

    fn deserialize_u8<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_u8 not supported")
    }

    fn deserialize_u16<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_u16 not supported")
    }

    fn deserialize_u32<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_u32 not supported")
    }

    fn deserialize_u64<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_u64 not supported")
    }

    fn deserialize_f32<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        trace!("parsing f32 {:x?}", self.peek(8));
        let val = self.read_f32()?;
        trace!("   -> {val}");
        visitor.visit_f32(val)
    }

    fn deserialize_f64<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        trace!("parsing f64 {:x?}", self.peek(8));
        let val = self.read_f64()?;
        trace!("   -> {val}");
        visitor.visit_f64(val)
    }

    fn deserialize_char<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_char not supported")
    }

    fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        trace!("reading str {:x?}", self.peek(64));
        self.read_and_validate_type(TypeId::of::<String>())?;
        let len = self.read_varint()? as usize;
        trace!("string length {len}");
        let str_data = &self.input[..len];
        trace!("string data {str_data:x?}");
        let val = String::from_utf8_lossy(str_data);
        trace!("string {val}");
        self.input = &self.input[len..];

        visitor.visit_str(&val)
    }

    fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        trace!("reading string {:x?}", self.peek(64));
        self.read_and_validate_type(TypeId::of::<String>())?;
        let len = self.read_varint()? as usize;
        trace!("string length {len}");
        let str_data = &self.input[..len];
        trace!("string data {str_data:x?}");
        let val = String::from_utf8_lossy(str_data);
        trace!("string {val}");
        self.input = &self.input[len..];

        visitor.visit_string(val.to_string())
    }

    fn deserialize_bytes<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_bytes not supported")
    }

    fn deserialize_byte_buf<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_byte_buf not supported")
    }

    fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        trace!("deserializing option {:x?}", self.peek(1));
        let spec = self
            .registry
            .get(self.type_id)
            .expect("type spec to exist")
            .iter()
            .next()
            .expect("type spec to exist");
        trace!("option spec: {spec:?}");
        assert!(spec.sub_types.len() == 1);

        let value_type_id = spec.sub_types[0];
        let value_spec = self
            .registry
            .get(value_type_id)
            .expect("type spec to exist")
            .iter()
            .next()
            .expect("type spec to exist");
        trace!("value spec: {value_spec:?}");

        // Process the null cases seperatly based on if the type is nullable.
        if value_spec.nullable {
            // If the type is nullable, expect it to either be the reader_index 0 (null) or it's value.
            if self.input[0] == 0 {
                // Consume the null type byte.
                self.input = &self.input[1..];
                return visitor.visit_none();
            }
        } else {
            // If the type is not nullable, assume it's using the nullable reader.
            let is_some = self.read_bool()?;
            if !is_some {
                return visitor.visit_none();
            }
        }

        // Once we're in the non-null case, we treat the value the same
        self.type_id = value_type_id;
        visitor.visit_some(self)
    }

    // In Serde, unit means an anonymous value containing no data.
    fn deserialize_unit<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_unit not supported")
    }

    fn deserialize_unit_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_unit_struct not supported")
    }

    fn deserialize_newtype_struct<V>(self, _name: &'static str, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_newtype_struct not supported")
    }

    fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        trace!("parsing seq {:x?}", self.peek(64));
        let type_spec = self.read_and_validate_type(self.type_id)?;
        let num_entries = self.read_u32()?;

        assert!(type_spec.sub_types.len() == 1);

        visitor.visit_seq(List {
            de: self,
            type_id: type_spec.sub_types[0],
            num_fields_left: num_entries as usize,
        })
    }

    fn deserialize_tuple<V>(self, _len: usize, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_tuple not supported")
    }

    // Tuple structs look just like sequences in JSON.
    fn deserialize_tuple_struct<V>(
        self,
        _name: &'static str,
        _len: usize,
        _visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_tuple_struct not supported")
    }

    // Much like `deserialize_seq` but calls the visitors `visit_map` method
    // with a `MapAccess` implementation, rather than the visitor's `visit_seq`
    // method with a `SeqAccess` implementation.
    fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        trace!("parsing map {:x?}", self.peek(64));
        let spec = self.read_and_validate_type(self.type_id)?;
        let key_type_id = spec.sub_types[0];
        let value_type_id = spec.sub_types[1];
        let num_entries = self.read_u32()? as usize;

        visitor.visit_map(Dict {
            de: self,
            num_entries,
            key_type_id,
            value_type_id,
        })
    }

    // Structs look just like maps in JSON.
    //
    // Notice the `fields` parameter - a "struct" in the Serde data model means
    // that the `Deserialize` implementation is required to know what the fields
    // are before even looking at the input data. Any key-value pairing in which
    // the fields cannot be known ahead of time is probably a map.
    fn deserialize_struct<V>(
        self,
        _name: &'static str,
        _fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        trace!("parsing struct {:x?}", self.peek(64));
        let spec = self.read_and_validate_type(self.type_id)?;

        visitor.visit_seq(Struct {
            de: self,
            fields: spec.fields,
            cur_entry: 0,
        })
    }

    fn deserialize_enum<V>(
        self,
        _name: &'static str,
        _variants: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        visitor.visit_enum(self)
    }

    // An identifier in Serde is the type that identifies a field of a struct or
    // the variant of an enum. In JSON, struct fields and enum variants are
    // represented as strings. In other formats they may be represented as
    // numeric indices.
    fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        let type_specs = self.registry.get(self.type_id)?;
        let spec = type_specs
            .iter()
            .next()
            .expect("type spec set is never empty");
        match spec.name.as_str() {
            "Microsoft.Xna.Framework.Content.Int32Reader" => {
                let v = self.read_i32()?;
                visitor.visit_i32(v)
            }
            "Microsoft.Xna.Framework.Content.StringReader" => {
                let s = self.read_string()?;
                visitor.visit_str(&s)
            }
            val => Err(error!("Can't decode an enum with reader type {val}")),
        }
    }

    fn deserialize_ignored_any<V>(self, _visitor: V) -> Result<V::Value>
    where
        V: Visitor<'de>,
    {
        err!("deserialize_any not supported")
    }
}

impl<'de, 'a> EnumAccess<'de> for &'a mut Deserializer<'de> {
    type Error = Error;
    type Variant = Self;

    #[inline]
    fn variant_seed<V: DeserializeSeed<'de>>(self, seed: V) -> Result<(V::Value, Self)> {
        trace!("deserializing value {:x?}", self.peek(64));
        let val = seed.deserialize(&mut *self)?;
        //let val = DeserializeSeed::deserialize(seed, varint.into_deserializer())?;
        Ok((val, self))
    }
}

impl<'de, 'a> serde::de::VariantAccess<'de> for &'a mut Deserializer<'de> {
    type Error = Error;

    #[inline]
    fn unit_variant(self) -> Result<()> {
        Ok(())
    }

    #[inline]
    fn newtype_variant_seed<V: DeserializeSeed<'de>>(self, seed: V) -> Result<V::Value> {
        DeserializeSeed::deserialize(seed, self)
    }

    #[inline]
    fn tuple_variant<V: Visitor<'de>>(self, len: usize, visitor: V) -> Result<V::Value> {
        serde::de::Deserializer::deserialize_tuple(self, len, visitor)
    }

    #[inline]
    fn struct_variant<V: Visitor<'de>>(
        self,
        fields: &'static [&'static str],
        visitor: V,
    ) -> Result<V::Value> {
        serde::de::Deserializer::deserialize_tuple(self, fields.len(), visitor)
    }
}

struct Dict<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    key_type_id: TypeId,
    value_type_id: TypeId,
    num_entries: usize,
}

impl<'de, 'a> MapAccess<'de> for Dict<'a, 'de> {
    type Error = Error;

    fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>>
    where
        K: DeserializeSeed<'de>,
    {
        if self.num_entries == 0 {
            return Ok(None);
        }

        self.de.type_id = self.key_type_id;
        self.num_entries -= 1;
        seed.deserialize(&mut *self.de).map(Some)
    }
    fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value>
    where
        V: DeserializeSeed<'de>,
    {
        self.de.type_id = self.value_type_id;
        seed.deserialize(&mut *self.de)
    }
}

struct Struct<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    fields: Vec<(String, TypeId)>,
    cur_entry: usize,
}

impl<'a, 'de: 'a> SeqAccess<'de> for Struct<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        if self.cur_entry >= self.fields.len() {
            return Ok(None);
        }
        self.de.type_id = self.fields[self.cur_entry].1;
        trace!(
            "deserializing field[{}] {}: {:x?}",
            self.cur_entry,
            self.fields[self.cur_entry].0,
            self.de.peek(32),
        );
        self.cur_entry += 1;
        seed.deserialize(&mut *self.de).map(Some)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.fields.len() - self.cur_entry)
    }
}

struct List<'a, 'de: 'a> {
    de: &'a mut Deserializer<'de>,
    type_id: TypeId,
    num_fields_left: usize,
}

impl<'a, 'de: 'a> SeqAccess<'de> for List<'a, 'de> {
    type Error = Error;

    fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
    where
        T: DeserializeSeed<'de>,
    {
        if self.num_fields_left == 0 {
            return Ok(None);
        }
        self.de.type_id = self.type_id;
        self.num_fields_left -= 1;
        seed.deserialize(&mut *self.de).map(Some)
    }

    fn size_hint(&self) -> Option<usize> {
        Some(self.num_fields_left)
    }
}
