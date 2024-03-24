use std::{
    any::TypeId,
    collections::{HashMap, HashSet},
};

use anyhow::anyhow;
use indexmap::IndexMap;

use crate::Result;

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct TypeSpec {
    pub name: String,
    pub sub_types: Vec<TypeId>,
    pub fields: Vec<(String, TypeId)>,
}

impl TypeSpec {
    fn new(name: &str) -> Self {
        Self {
            name: name.into(),
            sub_types: Vec::new(),
            fields: Vec::new(),
        }
    }

    fn new_with_subtypes(name: &str, sub_types: &[TypeId]) -> Self {
        Self {
            name: name.into(),
            sub_types: sub_types.into(),
            fields: Vec::new(),
        }
    }

    // fn new_with_fields(name: &str, fields: &[TypeId]) -> Self {
    //     Self {
    //         name: name.into(),
    //         sub_types: Vec::new(),
    //         fields: fields.into(),
    //     }
    // }
}

#[derive(Debug, Default)]
pub struct TypeRegistry {
    types: HashMap<TypeId, HashSet<TypeSpec>>,
}

impl TypeRegistry {
    pub fn new() -> Self {
        Self {
            types: HashMap::new(),
        }
    }

    pub fn for_type<T: XnbType>() -> Result<Self> {
        let mut registry = Self::new();
        T::register(&mut registry)?;
        Ok(registry)
    }

    pub fn register_type(&mut self, spec: TypeSpec, type_id: TypeId) -> Result<()> {
        self.types.entry(type_id).or_default().insert(spec);
        Ok(())
    }

    pub fn get(&self, type_id: TypeId) -> Result<&HashSet<TypeSpec>> {
        self.types
            .get(&type_id)
            .ok_or_else(|| anyhow!("Can't find type spec for type id {type_id:?}"))
    }
}

pub trait XnbType
where
    Self: 'static,
{
    fn register(registry: &mut TypeRegistry) -> Result<()>;

    fn type_id() -> TypeId {
        TypeId::of::<Self>()
    }
}

impl XnbType for bool {
    fn register(registry: &mut TypeRegistry) -> Result<()> {
        registry.register_type(
            TypeSpec::new("Microsoft.Xna.Framework.Content.BooleanReader"),
            TypeId::of::<Self>(),
        )?;
        Ok(())
    }
}

impl XnbType for i32 {
    fn register(registry: &mut TypeRegistry) -> Result<()> {
        registry.register_type(TypeSpec::new("System.Int32"), TypeId::of::<Self>())?;
        registry.register_type(
            TypeSpec::new("Microsoft.Xna.Framework.Content.Int32Reader"),
            TypeId::of::<Self>(),
        )?;
        Ok(())
    }
}

impl XnbType for f32 {
    fn register(registry: &mut TypeRegistry) -> Result<()> {
        registry.register_type(TypeSpec::new("System.Single"), TypeId::of::<Self>())?;
        registry.register_type(
            TypeSpec::new("Microsoft.Xna.Framework.Content.SingleReader"),
            TypeId::of::<Self>(),
        )?;
        Ok(())
    }
}

impl XnbType for f64 {
    fn register(registry: &mut TypeRegistry) -> Result<()> {
        registry.register_type(
            TypeSpec::new("Microsoft.Xna.Framework.Content.DoubleReader"),
            TypeId::of::<Self>(),
        )?;
        Ok(())
    }
}

impl XnbType for String {
    fn register(registry: &mut TypeRegistry) -> Result<()> {
        registry.register_type(TypeSpec::new("System.String"), TypeId::of::<Self>())?;
        registry.register_type(
            TypeSpec::new("Microsoft.Xna.Framework.Content.StringReader"),
            TypeId::of::<Self>(),
        )?;
        Ok(())
    }
}

impl<T: 'static + XnbType> XnbType for Option<T> {
    fn register(registry: &mut TypeRegistry) -> Result<()> {
        <T as XnbType>::register(registry)?;
        registry.register_type(
            TypeSpec::new_with_subtypes("placeholder::Option", &[TypeId::of::<T>()]),
            TypeId::of::<Self>(),
        )
    }
}

impl<T: 'static + XnbType> XnbType for Vec<T> {
    fn register(registry: &mut TypeRegistry) -> Result<()> {
        <T as XnbType>::register(registry)?;
        registry.register_type(
            TypeSpec::new_with_subtypes(
                "Microsoft.Xna.Framework.Content.ListReader",
                &[TypeId::of::<T>()],
            ),
            TypeId::of::<Self>(),
        )
    }
}

impl<K: 'static + XnbType, V: 'static + XnbType> XnbType for IndexMap<K, V> {
    fn register(registry: &mut TypeRegistry) -> Result<()> {
        <K as XnbType>::register(registry)?;
        <V as XnbType>::register(registry)?;
        registry.register_type(
            TypeSpec::new_with_subtypes(
                "Microsoft.Xna.Framework.Content.DictionaryReader",
                &[TypeId::of::<K>(), TypeId::of::<V>()],
            ),
            TypeId::of::<Self>(),
        )
    }
}
