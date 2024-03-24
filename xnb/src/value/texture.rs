use super::{TypeReader, Value};
use anyhow::anyhow;
use log::debug;
use nom::{bytes::complete::take, combinator::map_res, number::complete::le_i32};

use crate::{IResult, ReaderContext};

#[derive(Clone, Debug, PartialEq)]
pub struct Texture {
    pub width: i32,
    pub height: i32,
    pub data: Vec<u8>,
}

pub(super) struct TextureReader {}

impl TextureReader {
    pub fn new() -> TextureReader {
        TextureReader {}
    }
}

impl TypeReader for TextureReader {
    fn parse<'a>(&self, _context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, format) = map_res(le_i32, |value| match value {
            0 => Ok(value),
            _ => Err(anyhow!("Can't parse textures of format {value}.")),
        })(i)?;
        debug!("format {format}");
        let (i, width) = le_i32(i)?;
        debug!("width {width}");
        let (i, height) = le_i32(i)?;
        debug!("height {height}");
        let (i, mipmap_level_count) = map_res(le_i32, |value| match value {
            1 => Ok(value),
            _ => Err(anyhow!("Only textures with 1 mipmap level are supported.")),
        })(i)?;
        debug!("mipmap_level_count {mipmap_level_count}");
        let (i, data_len) = le_i32(i)?;
        debug!("data_len {data_len}");
        let (i, data) = take(data_len as usize)(i)?;

        Ok((
            i,
            Value::Texture(Texture {
                width,
                height,
                data: data.into(),
            }),
        ))
    }

    fn is_basic(&self) -> bool {
        false
    }
}
