use anyhow::{anyhow, Result};
use pretty_hex::PrettyHex;
use std::path::PathBuf;
use structopt::StructOpt;

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

fn main() -> Result<()> {
    env_logger::init();

    let opt = Opt::from_args();

    match opt {
        Opt::Dump { file } => cmd_dump(&file)?,
        Opt::ListReaders { file } => cmd_list_readers(&file)?,
    }

    Ok(())
}
