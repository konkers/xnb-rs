use std::{any::TypeId, str::FromStr};

use anyhow::anyhow;
use bitflags::bitflags;
use log::{debug, log_enabled, trace, Level};
use lzxd::{Lzxd, WindowSize};
use nom::{
    bytes::complete::{is_not, tag, take, take_while},
    character::complete::{char, one_of},
    combinator::{map_res, opt, peek, recognize},
    error::{ContextError, ErrorKind, FromExternalError},
    multi::{count, many0, separated_list0},
    number::complete::{be_u16, le_u32, u8},
    sequence::{terminated, tuple},
};
use pretty_hex::*;
use serde::de::DeserializeOwned;
use std::{cmp::min, io::Write};

pub use anyhow::Result;

mod de;
//pub mod value;
mod xnb_type;

pub mod xna;

//pub use value::Value;
pub use xnb_macro::{xnb_name, XnbType};
pub use xnb_type::{AnyType, FieldSpec, TypeRegistry, TypeSpec, XnbType};

pub use anyhow::Error;

#[derive(Clone, Debug)]
pub struct TypeReaderSpec {
    pub name: String,
    pub sub_types: Vec<String>,
    pub version: u32,
}

impl TypeReaderSpec {
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
            sub_types: subtypes,
            version: 0,
        })
    }
}

#[derive(Debug)]
pub struct ParseError {
    message: String,
}

impl ParseError {
    pub fn new(message: String) -> Self {
        Self { message }
    }
}

impl nom::error::ParseError<&[u8]> for ParseError {
    // on one line, we show the error code and the input that caused it
    fn from_error_kind(input: &[u8], kind: ErrorKind) -> Self {
        let len = min(input.len(), 64);
        let message = format!("{:?}:\t{:x?}\n", kind, &input[..len]);
        Self { message }
    }

    // if combining multiple errors, we show them one after the other
    fn append(input: &[u8], kind: ErrorKind, other: Self) -> Self {
        let len = min(input.len(), 64);
        let message = format!("{}{:?}:\t{:x?}\n", other.message, kind, &input[..len]);
        Self { message }
    }

    fn from_char(input: &[u8], c: char) -> Self {
        let len = min(input.len(), 64);
        let message = format!("'{}':\t{:x?}\n", c, &input[..len]);
        Self { message }
    }

    fn or(self, other: Self) -> Self {
        let message = format!("{}\tOR\n{}\n", self.message, other.message);
        Self { message }
    }
}

impl ContextError<&[u8]> for ParseError {
    fn add_context(input: &[u8], ctx: &'static str, other: Self) -> Self {
        let len = min(input.len(), 64);
        let message = format!("{}\"{}\":\t{:x?}\n", other.message, ctx, &input[..len]);
        Self { message }
    }
}

impl FromExternalError<&[u8], anyhow::Error> for ParseError {
    fn from_external_error(input: &[u8], kind: ErrorKind, e: anyhow::Error) -> Self {
        let len = min(input.len(), 64);
        let message = format!("{}{:?}:\t{:x?}\n", e, kind, &input[..len]);
        Self { message }
    }
}

type IResult<I, T> = nom::IResult<I, T, ParseError>;

#[derive(Debug)]
pub enum TargetPlatform {
    MicrosoftWindows,
    WindowsPhone7,
    Xbox360,
}

impl TargetPlatform {
    fn new(val: u8) -> Result<TargetPlatform> {
        match val {
            b'w' => Ok(TargetPlatform::MicrosoftWindows),
            b'm' => Ok(TargetPlatform::WindowsPhone7),
            b'x' => Ok(TargetPlatform::Xbox360),
            _ => Err(anyhow!("Invalid target platform {}", val)),
        }
    }
}

bitflags! {
    pub struct Flags: u8 {
        const HI_DEF = 0x01;
        const COMPRESSED = 0x80;
    }
}

#[derive(Debug)]
pub struct Header {
    pub target: TargetPlatform,
    pub version: u8,
    pub flags: Flags,
    pub file_size: u32,
    pub decompressed_size: Option<u32>,
}

pub fn from_bytes<T: XnbType + DeserializeOwned>(i: &[u8]) -> Result<T> {
    let data = decompressed_data(i)?;
    trace!("decompressed payload:");
    trace!("{:?}", data.hex_dump());

    // Parse and instantiate type readers.
    let (data, reader_specs) =
        parse_type_readers(&data).map_err(|e| anyhow!("Error decoding type readers: {}", e))?;

    if log_enabled!(Level::Debug) {
        for (i, spec) in reader_specs.iter().enumerate() {
            debug!("{i}: reader: {:?}", spec);
        }
    }

    // let mut context = ReaderContext::new();
    // for spec in reader_specs {
    //     let reader = spec.new_reader()?;
    //     context.add_reader(reader);
    // }

    // Number of shared resources.
    let (data, _) =
        parse_7bit_int(data).map_err(|e| anyhow!("Error reading shared resource count: {}", e))?;

    let registry = TypeRegistry::for_type::<T>()?;
    let mut deserializer =
        de::Deserializer::from_bytes(&registry, &reader_specs, data, TypeId::of::<T>());

    let t = T::deserialize(&mut deserializer)?;

    // let (_data, content) = context
    //     .parse_value(data)
    //     .map_err(|e| anyhow!("Error reading main object: {}", e))?;
    // Main object is tagged with its reader index.
    // let (data, reader_index) = parse_7bit_int(&data)
    //     .map_err(|e| anyhow!("Error reading main object reader index: {}", e))?;
    // let reader = &readers[(reader_index as usize) - 1];
    // let (_data, content) = reader
    //     .parse(&data)
    //     .map_err(|e| anyhow!("Error reading main object: {}", e))?;

    // Shared resources are not handled at the moment.

    Ok(t)
}

pub fn decompressed_data(i: &[u8]) -> Result<Vec<u8>> {
    let (i, header) = parse_header(i).map_err(|e| anyhow!("Error parsing xnb header: {}", e))?;

    // XNB files can be uncompressed or compressed with LZX or LZA.
    // Currently only LZX and uncompressed files are supported.
    let data = if header.flags.contains(Flags::COMPRESSED) {
        let (_, data) = parse_lzx_data(i)?;
        data
    } else {
        i.into()
    };

    Ok(data)
}

pub fn reader_info(i: &[u8]) -> Result<(usize, Vec<TypeReaderSpec>)> {
    let data = decompressed_data(i)?;
    trace!("decompressed payload:");
    trace!("{:?}", data.hex_dump());

    // Parse and instantiate type readers.
    let (data, reader_specs) =
        parse_type_readers(&data).map_err(|e| anyhow!("Error decoding type readers: {}", e))?;

    let (data, _) =
        parse_7bit_int(data).map_err(|e| anyhow!("Error reading shared resource count: {}", e))?;

    let (_data, root_reader_index) =
        parse_7bit_int(data).map_err(|e| anyhow!("Error reading root reader index: {}", e))?;

    Ok((root_reader_index as usize - 1, reader_specs))
}

fn parse_header(i: &[u8]) -> IResult<&[u8], Header> {
    let (i, _) = tag(b"XNB")(i)?;
    let (i, target) = map_res(u8, TargetPlatform::new)(i)?;
    let (i, version) = u8(i)?;
    let (i, flags_raw) = u8(i)?;
    let flags = Flags::from_bits_truncate(flags_raw);
    let (i, file_size) = le_u32(i)?;
    let (i, decompressed_size) = if flags.contains(Flags::COMPRESSED) {
        let (i, size) = le_u32(i)?;
        (i, Some(size))
    } else {
        (i, None)
    };

    Ok((
        i,
        Header {
            target,
            version,
            flags,
            file_size,
            decompressed_size,
        },
    ))
}

fn parse_lzx_data(i: &[u8]) -> Result<(&[u8], Vec<u8>)> {
    let mut i = i;
    let mut decoder = Lzxd::new(WindowSize::KB64);
    let mut decompressed = Vec::new();
    #[allow(clippy::while_let_loop)]
    loop {
        match parse_compressed_chunk(i) {
            Ok((rest, chunk)) => {
                let data = decoder.decompress_next(chunk)?;
                decompressed.write_all(data)?;
                i = rest;
            }
            Err(_) => break,
        }
    }

    Ok((i, decompressed))
}

fn parse_compressed_chunk(i: &[u8]) -> IResult<&[u8], &[u8]> {
    // Peek to see if the chunk starts if 0xff.  If it does, the frame size
    // is encoded with the chunk as well as chunk size
    let (i, marker) = peek(u8)(i)?;
    let (i, _frame_size, chunk_size) = if marker == 0xff {
        // Consume the peeked marker.
        let (i, _) = u8(i)?;
        let (i, frame_size) = be_u16(i)?;
        let (i, chunk_size) = be_u16(i)?;
        (i, frame_size as usize, chunk_size)
    } else {
        let (i, chunk_size) = be_u16(i)?;
        (i, 32 * 1024, chunk_size)
    };
    take(chunk_size)(i)
}

fn parse_type_readers(i: &[u8]) -> IResult<&[u8], Vec<TypeReaderSpec>> {
    let (i, readers_count) = parse_7bit_int(i)?;
    let (i, readers) = count(parse_type_reader, readers_count as usize)(i)?;
    Ok((i, readers))
}

fn parse_type_reader(i: &[u8]) -> IResult<&[u8], TypeReaderSpec> {
    let (i, spec) = map_res(parse_utf8_string, |type_str| {
        type_str.parse::<TypeReaderSpec>()
    })(i)?;
    let (i, version) = le_u32(i)?;

    Ok((i, spec.with_version(version)))
}

fn parse_type(i: &str) -> nom::IResult<&str, (String, Vec<String>)> {
    let (i, name) = is_not("`,")(i)?;
    let (i, subtypes) = opt(parse_subtypes)(i)?;

    Ok((i, (name.to_string(), subtypes.unwrap_or_default())))
}

fn decimal(input: &str) -> nom::IResult<&str, i32> {
    map_res(
        recognize(many0(terminated(one_of("0123456789"), many0(char('_'))))),
        |out: &str| str::replace(out, "_", "").parse::<i32>(),
    )(input)
}

pub fn subtype_field(i: &str) -> nom::IResult<&str, String> {
    let (i, ident) = recognize(tuple((take_while(|c| !",[]".contains(c)), opt(tag("[]")))))(i)?;

    Ok((i, ident.to_string()))
}

pub fn parse_subtype(i: &str) -> nom::IResult<&str, String> {
    let (i, _) = tag("[")(i)?;
    let (i, fields) = separated_list0(tag(","), subtype_field)(i)?;
    let (i, _) = tag("]")(i)?;
    Ok((i, fields[0].to_string()))
}

pub fn parse_subtypes(i: &str) -> nom::IResult<&str, Vec<String>> {
    let (i, _) = tag("`")(i)?;
    let (i, num_subtypes) = decimal(i)?;

    let (i, _) = tag("[")(i)?;
    let (i, subtypes) = separated_list0(tag(","), parse_subtype)(i)?;
    let (i, _) = tag("]")(i)?;

    debug_assert_eq!(num_subtypes as usize, subtypes.len());

    Ok((i, subtypes))
}

fn parse_utf8_string(i: &[u8]) -> IResult<&[u8], String> {
    let (i, len) = parse_7bit_int(i)?;

    let (i, string) = map_res(take(len), |raw: &[u8]| -> Result<String> {
        String::from_utf8(raw.to_vec()).map_err(|e| anyhow!("Error parsing string: {}", e))
    })(i)?;

    Ok((i, string))
}

// Parse a variable length encoded integer.
fn parse_7bit_int(i: &[u8]) -> IResult<&[u8], u32> {
    let mut res: u32 = 0;
    let mut shift = 0;

    let mut i = i;
    loop {
        let (rest, val) = u8(i)?;
        i = rest;
        res |= (val as u32 & 0x7f) << shift;
        shift += 7;

        if val & 0x80 == 0x0 {
            return Ok((i, res));
        }
    }
}

#[cfg(test)]
mod tests {
    use indexmap::IndexMap;

    use super::*;
    use std::fs::File;
    use std::io::{BufReader, Read};

    fn init() {
        let _ = env_logger::builder().is_test(true).try_init();
    }

    #[test]
    fn test_parse_7bit_int() {
        init();

        let data = [0x81u8, 0x01];
        assert_eq!(parse_7bit_int(&data).unwrap(), (&[][..], 0x81));
    }

    #[test]
    fn test_serde_simple_dict() -> Result<()> {
        init();

        let f = File::open("Fish.xnb")?;
        let mut r = BufReader::new(f);
        let mut data: Vec<u8> = Vec::new();

        r.read_to_end(&mut data)?;

        let fish: IndexMap<i32, String> =
            from_bytes(&data).map_err(|e| anyhow!("Error parsing xnb: {}", e))?;
        println!("{fish:#?}");

        Ok(())
    }
}
