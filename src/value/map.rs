use super::{TypeReader, Value};
use anyhow::{anyhow, Result};
use log::debug;
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::count,
    number::complete::{le_f32, le_i32, le_u32, u8},
    IResult,
};

#[derive(Clone, Debug, PartialEq)]
pub struct Size {
    w: i32,
    h: i32,
}

impl Size {
    fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        let (i, w) = le_i32(i)?;
        let (i, h) = le_i32(i)?;

        Ok((i, Size { w, h }))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Property {
    key: String,
    val: Value,
}

impl Property {
    fn bool(i: &[u8]) -> IResult<&[u8], Value> {
        let (i, _) = tag([0])(i)?;
        let (i, val) = parse_bool(i)?;
        Ok((i, Value::Bool(val)))
    }

    fn int(i: &[u8]) -> IResult<&[u8], Value> {
        let (i, _) = tag([1])(i)?;
        let (i, val) = le_i32(i)?;
        Ok((i, Value::I32(val)))
    }

    fn float(i: &[u8]) -> IResult<&[u8], Value> {
        let (i, _) = tag([2])(i)?;
        let (i, val) = le_f32(i)?;
        Ok((i, Value::F32(val)))
    }

    fn string(i: &[u8]) -> IResult<&[u8], Value> {
        let (i, _) = tag([3])(i)?;
        let (i, val) = parse_string(i)?;
        Ok((i, Value::String(val)))
    }

    fn parse(i: &[u8]) -> IResult<&[u8], Property> {
        let (i, key) = parse_string(i)?;
        let (i, val) = alt((Self::bool, Self::int, Self::float, Self::string))(i)?;
        Ok((i, Property { key, val }))
    }

    fn parse_properties(i: &[u8]) -> IResult<&[u8], Vec<Self>> {
        let (i, prop_count) = le_u32(i)?;
        let (i, props) = count(Self::parse, prop_count as usize)(i)?;
        Ok((i, props))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct TileSheet {
    pub id: String,
    pub desc: String,
    pub image_src: String,
    pub sheet_size: Size,
    pub tile_size: Size,
    pub margin: Size,
    pub spacing: Size,
    pub props: Vec<Property>,
}

impl TileSheet {
    fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        let (i, id) = parse_string(i)?;
        let (i, desc) = parse_string(i)?;
        let (i, image_src) = parse_string(i)?;
        let (i, sheet_size) = Size::parse(i)?;
        let (i, tile_size) = Size::parse(i)?;
        let (i, margin) = Size::parse(i)?;
        let (i, spacing) = Size::parse(i)?;
        let (i, props) = Property::parse_properties(i)?;
        Ok((
            i,
            TileSheet {
                id,
                desc,
                image_src,
                sheet_size,
                tile_size,
                margin,
                spacing,
                props,
            },
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct StaticTile {
    pub index: i32,
    pub tile_sheet: String,
    pub blend_mode: u8,
    pub properties: Vec<Property>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct AnimatedTile {
    pub interval: i32,
    pub count: i32,
    pub frames: Vec<StaticTile>,
    pub properties: Vec<Property>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Tile {
    Null,
    Static(StaticTile),
    Animated(AnimatedTile),
}

#[derive(Clone, Debug, PartialEq)]
enum TileOp {
    TileSheetId(String),
    Null(i32),
    Static {
        index: i32,
        blend_mode: u8,
        properties: Vec<Property>,
    },
    Animated {
        interval: i32,
        count: i32,
        frames: Vec<StaticTile>,
        properties: Vec<Property>,
    },
}

impl TileOp {
    fn parse_tile_sheet_id(i: &[u8]) -> IResult<&[u8], TileOp> {
        let (i, _) = tag("T")(i)?;
        let (i, id) = parse_string(i)?;
        Ok((i, Self::TileSheetId(id)))
    }

    fn parse_null(i: &[u8]) -> IResult<&[u8], TileOp> {
        let (i, _) = tag("N")(i)?;
        let (i, count) = le_i32(i)?;
        Ok((i, Self::Null(count)))
    }

    fn parse_static(i: &[u8]) -> IResult<&[u8], TileOp> {
        let (i, _) = tag("S")(i)?;
        let (i, index) = le_i32(i)?;
        let (i, blend_mode) = u8(i)?;
        let (i, properties) = Property::parse_properties(i)?;

        Ok((
            i,
            Self::Static {
                index,
                blend_mode,
                properties,
            },
        ))
    }

    fn parse_animated(i: &[u8]) -> IResult<&[u8], TileOp> {
        let (i, _) = tag("A")(i)?;
        let (i, interval) = le_i32(i)?;
        let (i, count) = le_i32(i)?;

        let mut frames_left = count;
        let mut tile_sheet = "".to_string();
        let mut frames = Vec::new();
        let mut i = i;
        while frames_left > 0 {
            let (next_i, op) = alt((Self::parse_tile_sheet_id, Self::parse_static))(i)?;
            match op {
                Self::TileSheetId(id) => tile_sheet = id,
                Self::Static {
                    index,
                    blend_mode,
                    properties,
                } => {
                    frames.push(StaticTile {
                        index,
                        tile_sheet: tile_sheet.clone(),
                        blend_mode,
                        properties,
                    });
                    frames_left -= 1;
                }
                _ => { /* TODO: add error */ }
            }
            i = next_i;
        }

        let (i, properties) = Property::parse_properties(i)?;

        Ok((
            i,
            TileOp::Animated {
                interval,
                count,
                frames,
                properties,
            },
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
struct Layer {
    id: String,
    visible: bool,
    desc: String,
    size: Size,
    tile_size: Size,
    props: Vec<Property>,
    tiles: Vec<Tile>,
}
impl Layer {
    fn parse(i: &[u8]) -> IResult<&[u8], Layer> {
        let (i, id) = parse_string(i)?;
        let (i, visible) = parse_bool(i)?;
        let (i, desc) = parse_string(i)?;
        let (i, size) = Size::parse(i)?;
        let (i, tile_size) = Size::parse(i)?;
        let (i, props) = Property::parse_properties(i)?;

        let mut i = i;
        let mut tile_sheet = "".to_string();
        let mut tiles = Vec::with_capacity((size.h * size.w) as usize);

        for _y in 0..size.h {
            let mut x = 0;
            while x < size.w {
                let (next_i, op) = alt((
                    TileOp::parse_tile_sheet_id,
                    TileOp::parse_null,
                    TileOp::parse_static,
                    TileOp::parse_animated,
                ))(i)?;
                match op {
                    TileOp::TileSheetId(id) => tile_sheet = id,
                    TileOp::Null(count) => {
                        for _ in 0..count {
                            tiles.push(Tile::Null);
                        }
                        x += count;
                    }
                    TileOp::Static {
                        index,
                        blend_mode,
                        properties,
                    } => {
                        tiles.push(Tile::Static(StaticTile {
                            index,
                            tile_sheet: tile_sheet.clone(),
                            blend_mode,
                            properties,
                        }));
                        x += 1;
                    }
                    TileOp::Animated {
                        interval,
                        count,
                        frames,
                        properties,
                    } => {
                        tiles.push(Tile::Animated(AnimatedTile {
                            interval,
                            count,
                            frames,
                            properties,
                        }));
                        x += 1;
                    }
                }
                i = next_i;
            }
        }
        Ok((
            i,
            Layer {
                id,
                visible,
                desc,
                size,
                tile_size,
                props,
                tiles,
            },
        ))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Map {}
impl Map {}

pub(super) struct MapReader {}

impl MapReader {
    pub fn new() -> MapReader {
        MapReader {}
    }

    fn parse_tile_sheets(i: &[u8]) -> IResult<&[u8], Vec<TileSheet>> {
        let (i, sheet_count) = le_u32(i)?;
        let (i, sheets) = count(TileSheet::parse, sheet_count as usize)(i)?;
        Ok((i, sheets))
    }

    fn parse_layers(i: &[u8]) -> IResult<&[u8], Vec<Layer>> {
        let (i, layer_count) = le_u32(i)?;
        let (i, layers) = count(Layer::parse, layer_count as usize)(i)?;
        Ok((i, layers))
    }
}

impl TypeReader for MapReader {
    fn parse<'a>(&self, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, _len) = le_u32(i)?;
        let (i, _) = tag("tBIN10")(i)?;
        let (i, id) = parse_string(i)?;
        debug!("id: {}", &id);
        let (i, desc) = parse_string(i)?;
        debug!("desc: {}", &desc);
        let (i, props) = Property::parse_properties(i)?;
        debug!("props: {:#?}", &props);
        let (i, sheets) = Self::parse_tile_sheets(i)?;
        debug!("sheets: {:#?}", &sheets);
        let (i, layers) = Self::parse_layers(i)?;
        debug!("layers: {:#?}", &layers);

        Ok((i, Value::Map(Map {})))
    }

    fn is_basic(&self) -> bool {
        false
    }
}

fn parse_bool(i: &[u8]) -> IResult<&[u8], bool> {
    let (i, val) = u8(i)?;
    Ok((i, val != 0))
}

fn parse_string(i: &[u8]) -> IResult<&[u8], String> {
    let (i, len) = le_u32(i)?;

    let (i, string) = map_res(take(len), |raw: &[u8]| -> Result<String> {
        String::from_utf8(raw.to_vec()).map_err(|e| anyhow!("Error parsing string: {}", e))
    })(i)?;

    Ok((i, string))
}
