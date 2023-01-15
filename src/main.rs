use clap::Parser;

mod parser;
use parser::*;

mod codegen;
use codegen::*;

#[derive(Parser, Debug)]
struct Args {
    filename: String,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    codegen();
    return Ok(());

    let args: Args = Args::parse();

    let contents = std::fs::read_to_string(args.filename)?;
    dbg!(parse_file_string(&contents));

    Ok(())
}
