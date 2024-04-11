use anyhow::{anyhow, Result};
use image::RgbaImage;
use pretty_hex::PrettyHex;
use std::path::PathBuf;
use structopt::StructOpt;
use xnb::xna::Texture2D;

#[derive(Debug, StructOpt)]
enum Opt {
    Dump {
        #[structopt(parse(from_os_str))]
        file: PathBuf,
    },
    ListReaders {
        #[structopt(parse(from_os_str))]
        file: PathBuf,
    },
    Texture {
        #[structopt(long, parse(from_os_str))]
        output: PathBuf,

        #[structopt(parse(from_os_str))]
        file: PathBuf,
    },
}

fn cmd_dump(file: &PathBuf) -> Result<()> {
    let data = std::fs::read(file)?;

    let decompressed_data = xnb::decompressed_data(&data)?;
    println!("{:?}", decompressed_data.hex_dump());

    Ok(())
}

fn cmd_list_readers(file: &PathBuf) -> Result<()> {
    let data = std::fs::read(file)?;
    let (root_reader_index, reader_specs) =
        xnb::reader_info(&data).map_err(|e| anyhow!("Error decoding type readers: {}", e))?;

    for (i, spec) in reader_specs.iter().enumerate() {
        println!("{i}: reader: {spec:?}");
    }

    println!("Root reader: {root_reader_index}");

    Ok(())
}

fn cmd_texture(output: &PathBuf, file: &PathBuf) -> Result<()> {
    let data = std::fs::read(file)?;
    let texture: Texture2D = xnb::from_bytes(&data)?;
    let image: RgbaImage = texture.try_into()?;

    image.save(output)?;

    Ok(())
}

fn main() -> Result<()> {
    env_logger::init();

    let opt = Opt::from_args();

    match opt {
        Opt::Dump { file } => cmd_dump(&file)?,
        Opt::ListReaders { file } => cmd_list_readers(&file)?,
        Opt::Texture { output, file } => cmd_texture(&output, &file)?,
    }

    Ok(())
}
