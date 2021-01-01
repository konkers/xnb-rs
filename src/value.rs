use super::parse_7bit_int;
use anyhow::{anyhow, Error, Result};
use indexmap::IndexMap;
use nom::{
    bytes::complete::{is_not, tag, take},
    character::complete::{char, one_of},
    combinator::{map_res, opt, recognize},
    multi::{count, many0, many1, separated_list1},
    number::complete::{le_i32, le_u32},
    sequence::terminated,
    IResult,
};
use std::{
    collections::HashMap,
    convert::{TryFrom, TryInto},
    hash::Hash,
    str::FromStr,
};

#[derive(Debug)]
pub struct TypeReaderSpec {
    pub name: String,
    pub subtypes: Vec<String>,
    pub version: u32,
}

impl TypeReaderSpec {
    pub fn new_reader(&self) -> Result<Box<dyn TypeReader>> {
        let reader: Box<dyn TypeReader> = match self.name.as_str() {
            "System.Int32" | "Microsoft.Xna.Framework.Content.Int32Reader" => {
                Box::new(I32Reader::new())
            }
            "System.String" | "Microsoft.Xna.Framework.Content.StringReader" => {
                Box::new(StringReader::new())
            }
            "Microsoft.Xna.Framework.Content.DictionaryReader" => {
                let key_reader = self.subtypes[0].parse::<TypeReaderSpec>()?.new_reader()?;
                let val_reader = self.subtypes[1].parse::<TypeReaderSpec>()?.new_reader()?;

                Box::new(DictReader::new(key_reader, val_reader))
            }
            "Microsoft.Xna.Framework.Content.ListReader" => {
                let reader = self.subtypes[0].parse::<TypeReaderSpec>()?.new_reader()?;
                Box::new(ListReader::new(reader))
            }
            _ => return Err(anyhow!("Unknown reader type {}", &self.name)),
        };

        Ok(reader)
    }

    pub fn with_version(mut self, version: u32) -> Self {
        self.version = version;
        self
    }
}

impl FromStr for TypeReaderSpec {
    type Err = Error;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (_, (name, subtypes)) = parse_type(s).map_err(|e| anyhow!("{}", e))?;

        Ok(TypeReaderSpec {
            name,
            subtypes,
            version: 0,
        })
    }
}

pub trait TypeReader {
    fn parse<'a>(&self, i: &'a [u8]) -> IResult<&'a [u8], Value>;

    // When a type is embedded in another object like a Dict, basic types do
    // not get tagged.
    fn parse_embedded<'a>(&self, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        if self.is_basic() {
            self.parse(i)
        } else {
            // Consume the tag.
            let (i, _) = parse_7bit_int(i)?;
            self.parse(i)
        }
    }

    // Basic types are untagged in Dicts
    fn is_basic(&self) -> bool;
}

struct I32Reader {}

impl I32Reader {
    pub fn new() -> I32Reader {
        return I32Reader {};
    }
}

impl TypeReader for I32Reader {
    fn parse<'a>(&self, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, val) = le_i32(i)?;

        Ok((i, Value::I32(val)))
    }

    fn is_basic(&self) -> bool {
        true
    }
}

struct StringReader {}

impl StringReader {
    pub fn new() -> StringReader {
        StringReader {}
    }
}

impl TypeReader for StringReader {
    fn parse<'a>(&self, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, len) = parse_7bit_int(i)?;
        let (i, raw_val) = take(len)(i)?;
        let val = String::from_utf8_lossy(raw_val);

        Ok((i, Value::String(val.to_string())))
    }

    fn is_basic(&self) -> bool {
        false
    }
}

struct DictReader {
    key_reader: Box<dyn TypeReader>,
    val_reader: Box<dyn TypeReader>,
}

impl DictReader {
    pub fn new(key_reader: Box<dyn TypeReader>, val_reader: Box<dyn TypeReader>) -> DictReader {
        DictReader {
            key_reader: key_reader,
            val_reader: val_reader,
        }
    }

    fn parse_element<'a>(&self, i: &'a [u8]) -> IResult<&'a [u8], (Value, Value)> {
        let (i, key) = self.key_reader.parse_embedded(i)?;
        let (i, val) = self.val_reader.parse_embedded(i)?;
        Ok((i, (key, val)))
    }
}

impl TypeReader for DictReader {
    fn parse<'a>(&self, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, num_entries) = le_u32(i)?;
        let (i, entries) = count(|i| self.parse_element(i), num_entries as usize)(i)?;

        Ok((i, Value::Dict(Dict { entries })))
    }

    fn is_basic(&self) -> bool {
        true
    }
}

struct ListReader {
    reader: Box<dyn TypeReader>,
}

impl ListReader {
    pub fn new(reader: Box<dyn TypeReader>) -> ListReader {
        ListReader { reader: reader }
    }
}

impl TypeReader for ListReader {
    fn parse<'a>(&self, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, num_entries) = le_u32(i)?;
        let (i, entries) = count(|i| self.reader.parse_embedded(i), num_entries as usize)(i)?;

        Ok((i, Value::List(entries)))
    }

    fn is_basic(&self) -> bool {
        true
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Dict {
    entries: Vec<(Value, Value)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    I32(i32),
    String(String),
    Dict(Dict),
    List(Vec<Value>),
}

impl TryFrom<Value> for i32 {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::I32(i32_val) = value {
            Ok(i32_val)
        } else {
            Err(anyhow!("Can't convert {:?} to i32", value))
        }
    }
}

impl TryFrom<Value> for String {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::String(str_val) = value {
            Ok(str_val)
        } else {
            Err(anyhow!("Can't convert {:?} to String", value))
        }
    }
}

impl<K: TryFrom<Value> + Eq + Hash, V: TryFrom<Value>> TryFrom<Value> for HashMap<K, V> {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::Dict(dict) = value {
            let mut map = HashMap::new();
            for e in dict.entries {
                let key: K = e.0.try_into().map_err(|_| anyhow!("Can't convert key"))?;
                let val: V = e.1.try_into().map_err(|_| anyhow!("Can't convert value"))?;
                map.insert(key, val);
            }
            Ok(map)
        } else {
            Err(anyhow!("Can't convert {:?} to HashMap", value))
        }
    }
}

impl<K: TryFrom<Value> + Eq + Hash, V: TryFrom<Value>> TryFrom<Value> for IndexMap<K, V> {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::Dict(dict) = value {
            let mut map = IndexMap::new();
            for e in dict.entries {
                let key: K = e.0.try_into().map_err(|_| anyhow!("Can't convert key"))?;
                let val: V = e.1.try_into().map_err(|_| anyhow!("Can't convert value"))?;
                map.insert(key, val);
            }
            Ok(map)
        } else {
            Err(anyhow!("Can't convert {:?} to HashMap", value))
        }
    }
}

fn decimal(input: &str) -> IResult<&str, i32> {
    map_res(
        recognize(many1(terminated(one_of("0123456789"), many0(char('_'))))),
        |out: &str| i32::from_str_radix(&str::replace(&out, "_", ""), 10),
    )(input)
}

pub fn parse_subtype(i: &str) -> IResult<&str, String> {
    let (i, _) = tag("[")(i)?;
    let (i, fields) = separated_list1(tag(","), is_not(",]"))(i)?;
    let (i, _) = tag("]")(i)?;
    Ok((i, fields[0].to_string()))
}

pub fn parse_subtypes(i: &str) -> IResult<&str, Vec<String>> {
    let (i, _) = tag("`")(i)?;
    let (i, num_subtypes) = decimal(i)?;

    let (i, _) = tag("[")(i)?;
    let (i, subtypes) = separated_list1(tag(","), parse_subtype)(i)?;
    let (i, _) = tag("]")(i)?;

    debug_assert_eq!(num_subtypes as usize, subtypes.len());

    Ok((i, subtypes))
}

pub fn parse_type(i: &str) -> IResult<&str, (String, Vec<String>)> {
    let (i, name) = is_not("`")(i)?;
    let (i, subtypes) = opt(parse_subtypes)(i)?;
    let subtypes = if let Some(subtypes) = subtypes {
        subtypes
    } else {
        Vec::new()
    };

    Ok((i, (name.to_string(), subtypes)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_subtypes() {
        assert_eq!(parse_subtypes("`2[[System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]").unwrap(),
        ("", vec!["System.String".to_string(), "System.String".to_string()]));
        assert_eq!(parse_subtypes("`2[[System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]"
            ).unwrap(),
        ("", vec!["System.Int32".to_string(), "System.String".to_string()]));
    }

    #[test]
    fn test_parse_type() {
        assert_eq!(parse_type("Microsoft.Xna.Framework.Content.DictionaryReader`2[[System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]").unwrap(),
        ("", ("Microsoft.Xna.Framework.Content.DictionaryReader".to_string(),vec!["System.Int32".to_string(), "System.String".to_string()])));
        assert_eq!(
            parse_type("Microsoft.Xna.Framework.Content.Int32Reader").unwrap(),
            (
                "",
                (
                    "Microsoft.Xna.Framework.Content.Int32Reader".to_string(),
                    vec![]
                )
            )
        );
    }

    #[test]
    fn test_parse_value() {
        assert_eq!(
            I32Reader::new().parse(&[0x00, 0x55, 0xaa, 0x7f]).unwrap(),
            (&[][..], Value::I32(0x7faa5500))
        );
        assert_eq!(
            StringReader::new().parse(&[0x3, 0x41, 0x42, 0x43]).unwrap(),
            (&[][..], Value::String("ABC".to_string()))
        );

        let reader = "Microsoft.Xna.Framework.Content.DictionaryReader`2[[System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]"
            .parse::<TypeReaderSpec>().unwrap().new_reader().unwrap();
        assert_eq!(
            reader
                .parse(&[
                    0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x03, 0x03, 0x41, 0x42, 0x43,
                    0x02, 0x00, 0x00, 0x00, 0x03, 0x02, 0x44, 0x45
                ])
                .unwrap(),
            (
                &[][..],
                Value::Dict(Dict {
                    entries: vec![
                        (Value::I32(1), Value::String("ABC".to_string())),
                        (Value::I32(2), Value::String("DE".to_string()))
                    ]
                })
            )
        );
    }

    #[test]
    fn test_value_convert() {
        let val: i32 = Value::I32(-1).try_into().unwrap();
        assert_eq!(val, -1);

        let val: String = Value::String("abc".to_string()).try_into().unwrap();
        assert_eq!(val, "abc".to_string(),);

        let val: HashMap<i32, String> = Value::Dict(Dict {
            entries: vec![
                (Value::I32(0), Value::String("zero".to_string())),
                (Value::I32(1), Value::String("one".to_string())),
            ],
        })
        .try_into()
        .unwrap();
        let mut expected = HashMap::new();
        expected.insert(0, "zero".to_string());
        expected.insert(1, "one".to_string());
        assert_eq!(val, expected);

        // We're not testing ordering here as we assume that IndexMap is
        // provides its ordering guarcaantee.
        let val: IndexMap<i32, String> = Value::Dict(Dict {
            entries: vec![
                (Value::I32(0), Value::String("zero".to_string())),
                (Value::I32(1), Value::String("one".to_string())),
            ],
        })
        .try_into()
        .unwrap();
        let mut expected = IndexMap::new();
        expected.insert(0, "zero".to_string());
        expected.insert(1, "one".to_string());
        assert_eq!(val, expected);
    }
}
