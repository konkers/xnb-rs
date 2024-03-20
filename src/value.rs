use anyhow::{anyhow, Error, Result};
use indexmap::IndexMap;
use nom::{
    bytes::complete::{is_not, tag, take},
    character::complete::{char, one_of},
    combinator::{map_res, opt, recognize},
    multi::{count, many0, many1, separated_list1},
    number::complete::{le_f32, le_f64, le_i32, le_u32, le_u8},
    sequence::terminated,
};
use num_derive::FromPrimitive;
use num_traits::FromPrimitive;
use std::{
    collections::HashMap,
    convert::{TryFrom, TryInto},
    hash::Hash,
    str::FromStr,
};

pub mod map;
pub mod texture;

pub use map::Map;
pub use texture::Texture;

use crate::{parse_7bit_int, IResult, ParseError, ReaderContext};

#[derive(Debug)]
pub struct TypeReaderSpec {
    pub name: String,
    pub subtypes: Vec<String>,
    pub version: u32,
}

impl TypeReaderSpec {
    pub fn new_reader(&self) -> Result<Box<dyn TypeReader>> {
        let reader_type = self
            .name
            .split(',')
            .next()
            .ok_or_else(|| anyhow!("failed to parse type name: {}", self.name))?;
        let reader: Box<dyn TypeReader> = match reader_type {
            "System.Int32" | "Microsoft.Xna.Framework.Content.Int32Reader" => {
                Box::new(I32Reader::new())
            }
            "System.Single" => Box::new(F32Reader::new()),
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
            "xTile.Pipeline.TideReader" => Box::new(map::MapReader::new()),
            "Microsoft.Xna.Framework.Content.Texture2DReader" => {
                Box::new(texture::TextureReader::new())
            }

            "Microsoft.Xna.Framework.Content.ReflectiveReader" => {
                self.subtypes[0].parse::<TypeReaderSpec>()?.new_reader()?
            }
            "Microsoft.Xna.Framework.Content.BooleanReader" => {
                Box::new(BinReader::new("BooleanReader"))
            }
            "Microsoft.Xna.Framework.Content.SingleReader" => {
                Box::new(BinReader::new("SingleReader"))
            }
            "Microsoft.Xna.Framework.Content.NullableReader" => {
                Box::new(BinReader::new("NullableReader"))
            }
            "Microsoft.Xna.Framework.Content.EnumReader" => Box::new(BinReader::new("EnumReader")),
            "Microsoft.Xna.Framework.Content.DoubleReader" => {
                Box::new(BinReader::new("DoubleReader"))
            }

            "StardewValley.GameData.Buffs.BuffAttributesData" => {
                Box::new(BuffAttributesDataReader::new())
            }
            "StardewValley.GameData.Objects.ObjectBuffData" => {
                Box::new(ObjectBuffDataReader::new())
            }
            "StardewValley.GameData.Objects.ObjectData" => Box::new(ObjectDataReader::new()),
            "StardewValley.GameData.Objects.ObjectGeodeDropData" => {
                Box::new(ObjectGeodeDropDataReader::new())
            }

            "StardewValley.GameData.QuantityModifier" => Box::new(QuantityModifierReader::new()),
            "StardewValley.GameData.GenericSpawnItemDataWithCondition" => {
                Box::new(BinReader::new("QuantityModifier"))
            }
            "StardewValley.GameData.GenericSpawnItemData" => {
                Box::new(BinReader::new("QuantityModifier"))
            }

            _ => return Err(anyhow!("Unknown reader type {}", reader_type)),
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
    fn parse<'a>(&self, context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value>;

    // When a type is embedded in another object like a Dict, basic types do
    // not get tagged.
    fn parse_embedded<'a>(&self, readers: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        if self.is_basic() {
            self.parse(readers, i)
        } else {
            // Consume the tag.
            let (i, _) = parse_7bit_int(i)?;
            self.parse(readers, i)
        }
    }

    // Basic types are untagged in Dicts
    fn is_basic(&self) -> bool;
}

struct ObjectDataReader {}

impl ObjectDataReader {
    pub fn new() -> Self {
        Self {}
    }
}

impl TypeReader for ObjectDataReader {
    fn parse<'a>(&self, context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, name) = context.parse_into(i)?;
        let (i, display_name) = context.parse_into(i)?;
        let (i, description) = context.parse_into(i)?;
        let (i, ty) = context.parse_into(i)?;
        let (i, category) = le_i32(i)?;
        let (i, price) = le_i32(i)?;
        let (i, texture) = context.parse_into(i)?;
        let (i, sprite_index) = le_i32(i)?;
        let (i, edibility) = le_i32(i)?;
        let (i, is_drink) = le_u8(i)?;
        let (i, buffs) = context.parse_into(i)?;
        let (i, geode_drops_default_items) = le_u8(i)?;
        let (i, geode_drops) = context.parse_into(i)?;
        let (i, artifact_spot_chances) = context.parse_into(i)?;
        let (i, exclude_from_fishing_collection) = le_u8(i)?;
        let (i, exclude_from_shipping_collection) = le_u8(i)?;
        let (i, exclude_from_random_sale) = le_u8(i)?;
        let (i, context_tags) = context.parse_into(i)?;
        let (i, custom_fields) = context.parse_into(i)?;

        Ok((
            i,
            Value::ObjectData(Box::new(ObjectData {
                name,
                display_name,
                description,
                ty,
                category,
                price,
                texture,
                sprite_index,
                edibility,
                is_drink: is_drink != 0,
                buffs,
                geode_drops_default_items: geode_drops_default_items != 0,
                geode_drops,
                artifact_spot_chances,
                exclude_from_fishing_collection: exclude_from_fishing_collection != 0,
                exclude_from_shipping_collection: exclude_from_shipping_collection != 0,
                exclude_from_random_sale: exclude_from_random_sale != 0,
                context_tags,
                custom_fields,
            })),
        ))
    }

    fn is_basic(&self) -> bool {
        false
    }
}

struct ObjectBuffDataReader {}

impl ObjectBuffDataReader {
    pub fn new() -> Self {
        Self {}
    }
}

impl TypeReader for ObjectBuffDataReader {
    fn parse<'a>(&self, context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, id) = context.parse_into(i)?;
        let (i, buff_id) = context.parse_into(i)?;
        let (i, icon_texture) = context.parse_into(i)?;
        let (i, icon_sprite_index) = le_i32(i)?;
        let (i, duration) = le_i32(i)?;
        let (i, is_debuf) = le_u8(i)?;
        let (i, glow_color) = context.parse_into(i)?;
        let (i, custom_attributes) = context.parse_into(i)?;
        let (i, custom_fields) = context.parse_into(i)?;

        Ok((
            i,
            Value::ObjectBuffData(Box::new(ObjectBuffData {
                id,
                buff_id,
                icon_texture,
                icon_sprite_index,
                duration,
                is_debuf: is_debuf != 0,
                glow_color,
                custom_attributes,
                custom_fields,
            })),
        ))
    }

    fn is_basic(&self) -> bool {
        false
    }
}

struct BuffAttributesDataReader {}

impl BuffAttributesDataReader {
    pub fn new() -> Self {
        Self {}
    }
}

impl TypeReader for BuffAttributesDataReader {
    fn parse<'a>(&self, _context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, farming_level) = le_f32(i)?;
        let (i, fishing_level) = le_f32(i)?;
        let (i, mining_level) = le_f32(i)?;
        let (i, luck_level) = le_f32(i)?;
        let (i, foraging_level) = le_f32(i)?;
        let (i, max_stamina) = le_f32(i)?;
        let (i, magnetic_radius) = le_f32(i)?;
        let (i, speed) = le_f32(i)?;
        let (i, defense) = le_f32(i)?;
        let (i, attack) = le_f32(i)?;

        Ok((
            i,
            Value::BuffAttributesData(Box::new(BuffAttributesData {
                farming_level,
                fishing_level,
                mining_level,
                luck_level,
                foraging_level,
                max_stamina,
                magnetic_radius,
                speed,
                defense,
                attack,
            })),
        ))
    }

    fn is_basic(&self) -> bool {
        false
    }
}

struct ObjectGeodeDropDataReader {}

impl ObjectGeodeDropDataReader {
    pub fn new() -> Self {
        Self {}
    }
}

impl TypeReader for ObjectGeodeDropDataReader {
    fn parse<'a>(&self, context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        // The following is GenericSpawnItemData
        let (i, id) = context.parse_into(i)?;
        let (i, item_id) = context.parse_into(i)?;
        let (i, random_item_id) = context.parse_into(i)?;
        let (i, max_items) = le_u8(i)?;
        if max_items != 0 {
            panic!("max_items: {:x?}", &i[..64]);
        }
        let (i, min_stack) = le_i32(i)?;
        let (i, max_stack) = le_i32(i)?;
        let (i, quality) = le_i32(i)?;
        let (i, internal_name) = context.parse_into(i)?;
        let (i, display_name) = context.parse_into(i)?;
        let (i, tool_upgrade_level) = le_i32(i)?;

        let (i, is_recipe) = le_u8(i)?;

        let (i, stack_modifiers) = context.parse_into(i)?;

        // Assuming enums are i32s per https://learn.microsoft.com/en-us/dotnet/api/system.enum.getunderlyingtype?view=net-8.0
        let (i, stack_modifier_mode) = le_i32(i)?;
        let stack_modifier_mode =
            QuantityModifierMode::from_i32(stack_modifier_mode).ok_or_else(|| {
                nom::Err::Error(ParseError::new(format!(
                    "no quantity modifier mode {stack_modifier_mode}",
                )))
            })?;

        let (i, quality_modifiers) = context.parse_into(i)?;
        let (i, quality_modifier_mode) = le_i32(i)?;
        let quality_modifier_mode = QuantityModifierMode::from_i32(quality_modifier_mode)
            .ok_or_else(|| {
                nom::Err::Error(ParseError::new(format!(
                    "no quantity modifier mode {quality_modifier_mode}",
                )))
            })?;

        let (i, mod_data) = context.parse_into(i)?;
        let (i, per_item_condition) = context.parse_into(i)?;

        // The following is GenericSpawnItemDataWithCondition
        let (i, condition) = context.parse_into(i)?;

        // The rest is from ObjectGeodeDropData
        let (i, chance) = le_f64(i)?;
        let (i, set_flag_on_pickup) = context.parse_into(i)?;
        let (i, precedence) = le_i32(i)?;

        Ok((
            i,
            Value::ObjectGeodeDropData(Box::new(ObjectGeodeDropData {
                chance,
                set_flag_on_pickup,
                precedence,
                condition,
                id,
                item_id,
                random_item_id,
                max_items: None,
                min_stack,
                max_stack,
                quality,
                internal_name,
                display_name,
                tool_upgrade_level,
                is_recipe: is_recipe != 0,
                stack_modifiers,
                stack_modifier_mode,
                quality_modifiers,
                quality_modifier_mode,
                mod_data,
                per_item_condition,
            })),
        ))
    }

    fn is_basic(&self) -> bool {
        false
    }
}

struct QuantityModifierReader {}

impl QuantityModifierReader {
    pub fn new() -> Self {
        Self {}
    }
}

impl TypeReader for QuantityModifierReader {
    fn parse<'a>(&self, context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        // The following is GenericSpawnItemData
        let (i, id) = context.parse_string(i)?;
        let (i, condition) = context.parse_string(i)?;
        let (i, modification_val) = le_i32(i)?;
        let modification = ModificationType::from_i32(modification_val).ok_or_else(|| {
            nom::Err::Error(ParseError::new(format!(
                "no modification type {modification_val}",
            )))
        })?;
        let (i, amount) = le_f32(i)?;
        let (i, random_amount) = context.parse_value(i)?;
        let random_amount = random_amount
            .try_into()
            .map_err(|e| nom::Err::Error(ParseError::new(format!("{e}"))))?;

        Ok((
            i,
            Value::QuantityModifier(Box::new(QuantityModifier {
                id,
                condition,
                modification,
                amount,
                random_amount,
            })),
        ))
    }

    fn is_basic(&self) -> bool {
        false
    }
}

struct BinReader {
    ty: String,
}

impl BinReader {
    pub fn new(ty: &str) -> BinReader {
        BinReader { ty: ty.to_string() }
    }
}

impl TypeReader for BinReader {
    fn parse<'a>(&self, _context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        Err(nom::Err::Error(ParseError::new(format!(
            "parsing {} {:x?}",
            self.ty,
            &i[..64]
        ))))
    }

    fn is_basic(&self) -> bool {
        false
    }
}

struct I32Reader {}

impl I32Reader {
    pub fn new() -> Self {
        Self {}
    }
}

impl TypeReader for I32Reader {
    fn parse<'a>(&self, _context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, val) = le_i32(i)?;

        Ok((i, Value::I32(val)))
    }

    fn is_basic(&self) -> bool {
        true
    }
}

struct F32Reader {}

impl F32Reader {
    pub fn new() -> Self {
        Self {}
    }
}

impl TypeReader for F32Reader {
    fn parse<'a>(&self, _context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, val) = le_f32(i)?;

        Ok((i, Value::F32(val)))
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
    fn parse<'a>(&self, _context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
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
            key_reader,
            val_reader,
        }
    }

    fn parse_element<'a>(
        &self,
        context: &ReaderContext,
        i: &'a [u8],
    ) -> IResult<&'a [u8], (Value, Value)> {
        let (i, key) = self.key_reader.parse_embedded(context, i)?;
        let (i, val) = self.val_reader.parse_embedded(context, i)?;
        Ok((i, (key, val)))
    }
}

impl TypeReader for DictReader {
    fn parse<'a>(&self, context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, num_entries) = le_u32(i)?;
        let (i, entries) = count(|i| self.parse_element(context, i), num_entries as usize)(i)?;

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
        ListReader { reader }
    }
}

impl TypeReader for ListReader {
    fn parse<'a>(&self, context: &ReaderContext, i: &'a [u8]) -> IResult<&'a [u8], Value> {
        let (i, num_entries) = le_u32(i)?;
        let (i, entries) = count(
            |i| self.reader.parse_embedded(context, i),
            num_entries as usize,
        )(i)?;

        Ok((i, Value::List(entries)))
    }

    fn is_basic(&self) -> bool {
        true
    }
}

#[derive(Clone, Debug, FromPrimitive, PartialEq)]
pub enum ModificationType {
    Add,
    Subtract,
    Multiply,
    Divide,
    Set,
}

#[derive(Clone, Debug, PartialEq)]
pub struct QuantityModifier {
    pub id: String,
    pub condition: String,
    pub modification: ModificationType,
    pub amount: f32,
    pub random_amount: Vec<f32>,
}

#[derive(Clone, Debug, FromPrimitive, PartialEq)]
pub enum QuantityModifierMode {
    Stack,
    Minimum,
    Maximum,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectGeodeDropData {
    pub chance: f64,
    pub set_flag_on_pickup: String,
    pub precedence: i32,
    pub condition: String,
    pub id: String,
    pub item_id: String,
    pub random_item_id: Vec<String>,
    pub max_items: Option<i32>,
    pub min_stack: i32,
    pub max_stack: i32,
    pub quality: i32,
    pub internal_name: String,
    pub display_name: String,
    pub tool_upgrade_level: i32,
    pub is_recipe: bool,
    pub stack_modifiers: Vec<QuantityModifier>,
    pub stack_modifier_mode: QuantityModifierMode,
    pub quality_modifiers: Vec<QuantityModifier>,
    pub quality_modifier_mode: QuantityModifierMode,
    pub mod_data: IndexMap<String, String>,
    pub per_item_condition: String,
}

#[derive(Clone, Debug, PartialEq)]
pub struct BuffAttributesData {
    pub farming_level: f32,
    pub fishing_level: f32,
    pub mining_level: f32,
    pub luck_level: f32,
    pub foraging_level: f32,
    pub max_stamina: f32,
    pub magnetic_radius: f32,
    pub speed: f32,
    pub defense: f32,
    pub attack: f32,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectBuffData {
    pub id: String,
    pub buff_id: String,
    pub icon_texture: String,
    pub icon_sprite_index: i32,
    pub duration: i32,
    pub is_debuf: bool,
    pub glow_color: String,
    pub custom_attributes: Option<BuffAttributesData>,
    pub custom_fields: IndexMap<String, String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct ObjectData {
    pub name: String,
    pub display_name: String,
    pub description: String,
    pub ty: String,
    pub category: i32,
    pub price: i32,
    pub texture: String,
    pub sprite_index: i32,
    pub edibility: i32,
    pub is_drink: bool,
    pub buffs: Vec<ObjectBuffData>,
    pub geode_drops_default_items: bool,
    pub geode_drops: Vec<ObjectGeodeDropData>,
    pub artifact_spot_chances: IndexMap<String, f32>,
    pub exclude_from_fishing_collection: bool,
    pub exclude_from_shipping_collection: bool,
    pub exclude_from_random_sale: bool,
    pub context_tags: Vec<String>,
    pub custom_fields: IndexMap<String, String>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Dict {
    pub entries: Vec<(Value, Value)>,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    I32(i32),
    String(String),
    Dict(Dict),
    Map(Map),
    Texture(Texture),

    // Untested.
    List(Vec<Value>),

    // Types w/o readers
    Bool(bool),
    F32(f32),

    Null,

    // Stardew Types
    BuffAttributesData(Box<BuffAttributesData>),
    ObjectBuffData(Box<ObjectBuffData>),
    ObjectData(Box<ObjectData>),
    ObjectGeodeDropData(Box<ObjectGeodeDropData>),
    QuantityModifier(Box<QuantityModifier>),
}

impl TryFrom<Value> for i32 {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::I32(i32_val) => Ok(i32_val),
            Value::String(str_val) => str_val
                .parse()
                .map_err(|e| anyhow!("Can't convert {} to i32: {}", str_val, e)),
            _ => Err(anyhow!("Can't convert {:?} to i32", value)),
        }
    }
}

impl TryFrom<Value> for f32 {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::F32(f32_val) => Ok(f32_val),
            Value::String(str_val) => str_val
                .parse()
                .map_err(|e| anyhow!("Can't convert {} to f32: {}", str_val, e)),
            _ => Err(anyhow!("Can't convert {:?} to f32", value)),
        }
    }
}

impl TryFrom<Value> for String {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::String(val) => Ok(val),
            Value::Null => Ok(String::new()),
            _ => Err(anyhow!("Can't convert {:?} to String", value)),
        }
    }
}

impl TryFrom<Value> for QuantityModifier {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::QuantityModifier(val) = value {
            Ok(*val)
        } else {
            Err(anyhow!("Can't convert {:?} to QuantityModifier", value))
        }
    }
}

impl TryFrom<Value> for ObjectBuffData {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::ObjectBuffData(val) = value {
            Ok(*val)
        } else {
            Err(anyhow!("Can't convert {:?} to QuantityModifier", value))
        }
    }
}

impl TryFrom<Value> for ObjectGeodeDropData {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::ObjectGeodeDropData(val) = value {
            Ok(*val)
        } else {
            Err(anyhow!("Can't convert {:?} to QuantityModifier", value))
        }
    }
}

impl TryFrom<Value> for Option<BuffAttributesData> {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::BuffAttributesData(val) => Ok(Some(*val)),
            Value::Null => Ok(None),
            _ => Err(anyhow!("Can't convert {:?} to QuantityModifier", value)),
        }
    }
}

impl<K: TryFrom<Value> + Eq + Hash, V: TryFrom<Value>> TryFrom<Value> for HashMap<K, V> {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        if let Value::Dict(dict) = value {
            let mut map = HashMap::new();
            for e in dict.entries {
                let key: K =
                    e.0.clone()
                        .try_into()
                        .map_err(|_| anyhow!("Can't convert key {:?}", e.0))?;
                let val: V = e.1.try_into().map_err(|_| anyhow!("Can't convert value"))?;
                map.insert(key, val);
            }
            Ok(map)
        } else {
            Err(anyhow!("Can't convert {:?} to HashMap", value))
        }
    }
}

impl<K: TryFrom<Value> + Eq + Hash, V: TryFrom<Value>> TryFrom<Value> for IndexMap<K, V>
where
    <K as TryFrom<Value>>::Error: std::fmt::Display,
{
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::Dict(dict) => {
                let mut map = IndexMap::new();
                for e in dict.entries {
                    let key: K =
                        e.0.clone()
                            .try_into()
                            .map_err(|error| anyhow!("Can't convert key {e:?} {}", error))?;
                    let val: V = e.1.try_into().map_err(|_| anyhow!("Can't convert value"))?;
                    map.insert(key, val);
                }
                Ok(map)
            }
            Value::Null => Ok(IndexMap::new()),
            _ => Err(anyhow!("Can't convert {:?} to IndexMap", value)),
        }
    }
}

impl<V: TryFrom<Value>> TryFrom<Value> for Vec<V> {
    type Error = anyhow::Error;

    fn try_from(value: Value) -> Result<Self, Self::Error> {
        match value {
            Value::List(list) => {
                let mut vec = Vec::new();
                for item in list {
                    let val = item
                        .try_into()
                        .map_err(|_| anyhow!("Can't convert value"))?;
                    vec.push(val);
                }
                Ok(vec)
            }
            Value::Null => Ok(Vec::new()),
            _ => Err(anyhow!("Can't convert {:?} to Vec", value)),
        }
    }
}

fn decimal(input: &str) -> nom::IResult<&str, i32> {
    map_res(
        recognize(many1(terminated(one_of("0123456789"), many0(char('_'))))),
        |out: &str| str::replace(out, "_", "").parse::<i32>(),
    )(input)
}

pub fn parse_subtype(i: &str) -> nom::IResult<&str, String> {
    let (i, _) = tag("[")(i)?;
    let (i, fields) = separated_list1(tag(","), is_not(",]"))(i)?;
    let (i, _) = tag("]")(i)?;
    Ok((i, fields[0].to_string()))
}

pub fn parse_subtypes(i: &str) -> nom::IResult<&str, Vec<String>> {
    let (i, _) = tag("`")(i)?;
    let (i, num_subtypes) = decimal(i)?;

    let (i, _) = tag("[")(i)?;
    let (i, subtypes) = separated_list1(tag(","), parse_subtype)(i)?;
    let (i, _) = tag("]")(i)?;

    debug_assert_eq!(num_subtypes as usize, subtypes.len());

    Ok((i, subtypes))
}

pub fn parse_type(i: &str) -> nom::IResult<&str, (String, Vec<String>)> {
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
        let context = ReaderContext::new();
        assert_eq!(
            I32Reader::new()
                .parse(&context, &[0x00, 0x55, 0xaa, 0x7f])
                .unwrap(),
            (&[][..], Value::I32(0x7faa5500))
        );
        assert_eq!(
            StringReader::new()
                .parse(&context, &[0x3, 0x41, 0x42, 0x43])
                .unwrap(),
            (&[][..], Value::String("ABC".to_string()))
        );

        let reader = "Microsoft.Xna.Framework.Content.DictionaryReader`2[[System.Int32, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089],[System.String, mscorlib, Version=4.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]"
            .parse::<TypeReaderSpec>().unwrap().new_reader().unwrap();
        assert_eq!(
            reader
                .parse(
                    &context,
                    &[
                        0x02, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x03, 0x03, 0x41, 0x42,
                        0x43, 0x02, 0x00, 0x00, 0x00, 0x03, 0x02, 0x44, 0x45
                    ]
                )
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
