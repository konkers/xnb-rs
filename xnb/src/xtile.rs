use std::collections::HashMap;

use anyhow::{anyhow, Result};
use nom::{
    branch::alt,
    bytes::complete::{tag, take},
    combinator::map_res,
    multi::count,
    number::complete::{le_f32, le_i32, le_u32, u8},
};

use crate::IResult;

#[derive(Clone, Debug, PartialEq)]
pub struct Size {
    pub w: i32,
    pub h: i32,
}

impl Size {
    fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        let (i, w) = le_i32(i)?;
        let (i, h) = le_i32(i)?;

        Ok((i, Size { w, h }))
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Bool(bool),
    I32(i32),
    F32(f32),
    String(String),
}

#[derive(Clone, Debug, PartialEq)]
pub struct Property {
    pub key: String,
    pub val: Value,
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
    tile_properties: HashMap<i32, HashMap<String, Value>>,
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

        let mut tile_properties: HashMap<i32, HashMap<String, Value>> = HashMap::new();
        for prop in &props {
            let parts: Vec<_> = prop.key.split('@').collect();
            if parts.len() != 4 || parts[1] != "TileIndex" {
                continue;
            }

            let Ok(index) = parts[2].parse::<i32>() else {
                continue;
            };

            tile_properties
                .entry(index)
                .or_default()
                .insert(parts[3].to_string(), prop.val.clone());
        }

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
                tile_properties,
            },
        ))
    }

    pub fn get_tile_property<'a>(&'a self, index: i32, name: &str) -> Option<&'a Value> {
        let props = self.tile_properties.get(&index)?;
        props.get(name)
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

impl Tile {
    pub fn get_property<'a>(&'a self, name: &str) -> Option<&'a Value> {
        let properties = match self {
            Tile::Static(tile) => &tile.properties,
            Tile::Animated(tile) => &tile.properties,
            Tile::Null => return None,
        };
        properties
            .iter()
            .find(|prop| prop.key == name)
            .map(|prop| &prop.val)
    }

    pub fn sheet_and_index(&self) -> Option<(i32, &str)> {
        match self {
            Tile::Static(tile) => Some((tile.index, &tile.tile_sheet)),
            Tile::Animated(tile) => Some((tile.frames[0].index, &tile.frames[0].tile_sheet)),
            Tile::Null => None,
        }
    }
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
pub struct Layer {
    pub id: String,
    pub visible: bool,
    pub desc: String,
    pub size: Size,
    pub tile_size: Size,
    pub props: Vec<Property>,
    pub tiles: Vec<Tile>,
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

    pub fn get_tile(&self, x: i32, y: i32) -> Option<&Tile> {
        let index = y * self.size.w + x;
        self.tiles.get(index as usize)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub struct Map {
    pub id: String,
    pub description: String,
    pub properties: Vec<Property>,
    pub tile_sheets: Vec<TileSheet>,
    pub layers: Vec<Layer>,
}

impl Map {
    pub(super) fn parse(i: &[u8]) -> IResult<&[u8], Self> {
        let (i, _len) = le_u32(i)?;
        let (i, _) = tag("tBIN10")(i)?;
        let (i, id) = parse_string(i)?;
        let (i, description) = parse_string(i)?;
        let (i, properties) = Property::parse_properties(i)?;
        let (i, tile_sheets) = Self::parse_tile_sheets(i)?;
        let (i, layers) = Self::parse_layers(i)?;

        Ok((
            i,
            Map {
                id,
                description,
                properties,
                tile_sheets,
                layers,
            },
        ))
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

    pub fn get_layer<'a>(&'a self, id: &str) -> Option<&'a Layer> {
        self.layers.iter().find(|x| x.id == id)
    }

    pub fn get_tile_property<'a>(
        &'a self,
        x: i32,
        y: i32,
        property_name: &str,
        layer_name: &str,
    ) -> Option<&'a Value> {
        // Ignores builds and furniture.
        let layer = self.get_layer(layer_name)?;
        let tile = layer.get_tile(x, y)?;
        if let Some(value) = tile.get_property(property_name) {
            return Some(value);
        }

        let (index, sheet_id) = tile.sheet_and_index()?;
        let sheet = self.tile_sheets.iter().find(|sheet| sheet.id == sheet_id)?;
        sheet.get_tile_property(index, property_name)
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
