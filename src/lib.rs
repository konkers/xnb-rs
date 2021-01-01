use {
    anyhow::{anyhow, Result},
    bitflags::bitflags,
    log::{debug, log_enabled, trace, Level},
    lzxd::{Lzxd, WindowSize},
    nom::{
        bytes::complete::{tag, take},
        combinator::{map_res, peek},
        multi::count,
        number::complete::{be_u16, le_u32, u8},
        IResult,
    },
    pretty_hex::*,
    std::io::{Read, Write},
};

mod value;

pub use value::Value;

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

#[derive(Debug)]
pub struct Xnb {
    pub header: Header,
    pub content: Value,
}

impl Xnb {
    pub fn new(r: &mut impl Read) -> Result<Xnb> {
        let mut data: Vec<u8> = Vec::new();

        r.read_to_end(&mut data)?;

        let xnb = Self::parse_xnb(&data).map_err(|e| anyhow!("Error parsing xnb: {}", e))?;

        Ok(xnb)
    }

    fn parse_xnb(i: &[u8]) -> Result<Xnb> {
        let (i, header) =
            Self::parse_header(&i).map_err(|e| anyhow!("Error parsing xnb header: {}", e))?;

        // XNB files can be uncompressed or compressed with LZX or LZA.
        // Currently only LZX and uncompressed files are supported.
        let data = if header.flags.contains(Flags::COMPRESSED) {
            let (_, data) = Self::parse_lzx_data(i)?;
            data
        } else {
            i.into()
        };
        trace!("decompressed payload:");
        trace!("{:?}", data.hex_dump());

        // Parse and instantiate type readers.
        let (data, reader_specs) = Self::parse_type_readers(&data)
            .map_err(|e| anyhow!("Error decoding type readers: {}", e))?;
        let mut readers = Vec::new();

        if log_enabled!(Level::Debug) {
            for spec in &reader_specs {
                debug!("reader: {:?}", spec);
            }
        }

        for spec in reader_specs {
            let reader = spec.new_reader()?;
            readers.push(reader);
        }

        // Number of shared resources.
        let (data, _) = parse_7bit_int(&data)
            .map_err(|e| anyhow!("Error reading shared resource count: {}", e))?;

        // Main object is tagged with its reader index.
        let (data, reader_index) = parse_7bit_int(&data)
            .map_err(|e| anyhow!("Error reading main object reader index: {}", e))?;
        let reader = &readers[(reader_index as usize) - 1];
        let (_data, content) = reader
            .parse(&data)
            .map_err(|e| anyhow!("Error reading main object: {}", e))?;

        // Shared resources are not handled at the moment.

        Ok(Xnb { header, content })
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
        loop {
            match Self::parse_compressed_chunk(i) {
                Ok((rest, chunk)) => {
                    let data = decoder.decompress_next(chunk)?;
                    decompressed.write(data)?;
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

    fn parse_type_readers(i: &[u8]) -> IResult<&[u8], Vec<value::TypeReaderSpec>> {
        let (i, readers_count) = parse_7bit_int(i)?;
        let (i, readers) = count(Self::parse_type_reader, readers_count as usize)(i)?;
        Ok((i, readers))
    }

    fn parse_type_reader(i: &[u8]) -> IResult<&[u8], value::TypeReaderSpec> {
        let (i, spec) = map_res(parse_utf8_string, |type_str| {
            type_str.parse::<value::TypeReaderSpec>()
        })(i)?;
        let (i, version) = le_u32(i)?;

        Ok((i, spec.with_version(version)))
    }
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
    use super::*;
    use std::fs::File;
    use std::io::BufReader;

    #[test]
    fn test_header() -> Result<()> {
        let f = File::open("Fish.xnb")?;
        let mut r = BufReader::new(f);
        let xnb = Xnb::new(&mut r);
        println!("{:#?}", &xnb);

        let f = File::open("ObjectInformation.xnb")?;
        let mut r = BufReader::new(f);
        let xnb = Xnb::new(&mut r);
        println!("{:#?}", &xnb);

        let f = File::open("Bundles.xnb")?;
        let mut r = BufReader::new(f);
        let xnb = Xnb::new(&mut r);
        println!("{:#?}", &xnb);

        Ok(())
    }

    #[test]
    fn test_parse_7bit_int() {
        let data = [0x81u8, 0x01];
        assert_eq!(parse_7bit_int(&data).unwrap(), (&[][..], 0x81));
    }
}
