use std::path::PathBuf;

use clap::{Parser, ValueEnum};

use struct_writer::generate::{self, Language};
use struct_writer::templating::merge;

#[derive(Debug, Clone, ValueEnum)]
enum LangArg {
    C,
    Rust,
    Scala,
}

impl From<LangArg> for Language {
    fn from(l: LangArg) -> Self {
        match l {
            LangArg::C => Language::C,
            LangArg::Rust => Language::Rust,
            LangArg::Scala => Language::Scala,
        }
    }
}

#[derive(Parser, Debug)]
#[command(author, version, about = "Generate C, Rust, or Scala binary-serialization code from API spec files")]
struct Cli {
    /// Input definition files (.toml, .json, .yaml)
    #[arg(short = 'i', long, required = true, num_args = 1..)]
    input_definitions: Vec<PathBuf>,

    /// Template override files
    #[arg(short = 't', long, num_args = 0..)]
    template_files: Vec<PathBuf>,

    /// Output file path
    #[arg(short = 'o', long, required = true)]
    output_file: PathBuf,

    /// Target language
    #[arg(short = 'l', long, default_value = "c")]
    language: LangArg,
}

fn main() {
    let cli = Cli::parse();

    let input_refs: Vec<&std::path::Path> = cli.input_definitions.iter().map(|p| p.as_path()).collect();
    let template_refs: Vec<&std::path::Path> = cli.template_files.iter().map(|p| p.as_path()).collect();
    let language: Language = cli.language.into();

    match generate::generate(&input_refs, &template_refs, language, &cli.output_file) {
        Ok(output) => {
            if let Some(parent) = cli.output_file.parent() {
                if !parent.as_os_str().is_empty() {
                    std::fs::create_dir_all(parent).expect("Failed to create output directory");
                }
            }
            std::fs::write(&cli.output_file, output).expect("Failed to write output file");
        }
        Err(e) => {
            eprintln!("Error: {}", e);
            std::process::exit(1);
        }
    }
}
