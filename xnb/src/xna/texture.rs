use serde::Deserialize;
use serde_repr::Deserialize_repr;

use crate as xnb;
use crate::{xnb_name, XnbType};

#[derive(Clone, Debug, Deserialize_repr, PartialEq, XnbType)]
#[repr(i32)]
pub enum SurfaceFormat {
    Color,
    Bgr656,
    Bgra5551,
    Bgra4444,
    Dxt1,
    Dxt3,
    Dxt5,
    NormalizedByte2,
    NormalizedByte4,
    Rgba1010102,
    Rg32,
    Rgba64,
    Alpha8,
    Single,
    Vector2,
    Vector4,
    HalfSingle,
    HalfVector2,
    HalfVector4,
    HdrBlendable,
    ColorBgraEXT,
    ColorSrgbEXT,
    Dxt5SrgbEXT,
    Bc7EXT,
    Bc7SrgbEXT,
}

#[derive(Clone, Debug, Deserialize, XnbType)]
#[xnb_name("Microsoft.Xna.Framework.Content.Texture2DReader")]
pub struct Texture2D {
    pub format: SurfaceFormat,
    pub width: i32,
    pub height: i32,
    pub mipmap_level: i32,
    #[xnb(untagged)]
    pub data: Vec<u8>,
}

#[cfg(feature = "image")]
impl TryFrom<Texture2D> for image::RgbaImage {
    type Error = crate::Error;

    fn try_from(texture: Texture2D) -> Result<Self, Self::Error> {
        if texture.format != SurfaceFormat::Color {
            return Err(anyhow::anyhow!(
                "can't convert texture format {:?} to RGBA image",
                texture.format
            ));
        }

        log::debug!(
            "Converting {}x{} texture to image with {} bytes of data",
            texture.width,
            texture.height,
            texture.data.len()
        );

        image::ImageBuffer::from_raw(
            texture.width as u32,
            texture.height as u32,
            texture.data.clone(),
        )
        .ok_or_else(|| anyhow::anyhow!("can't convert texture to RGBA image"))
    }
}
