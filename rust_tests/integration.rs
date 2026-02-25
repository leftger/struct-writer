use std::path::PathBuf;

use struct_writer::generate::{generate, Language};

fn examples_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("examples")
}

#[test]
fn test_generate_c_from_example() {
    let input = examples_dir().join("structures.toml");
    let output = PathBuf::from("/tmp/struct_writer_test_out.h");
    let result = generate(&[input.as_path()], &[], Language::C, &output);
    assert!(result.is_ok(), "Generate C failed: {:?}", result.err());
    let s = result.unwrap();
    assert!(s.contains("temperature_units"), "Missing temperature_units enum");
    assert!(s.contains("commands"), "Missing group-derived content");
}

#[test]
fn test_generate_rust_from_example() {
    let input = examples_dir().join("structures.toml");
    let output = PathBuf::from("/tmp/struct_writer_test_out.rs");
    let result = generate(&[input.as_path()], &[], Language::Rust, &output);
    assert!(result.is_ok(), "Generate Rust failed: {:?}", result.err());
    let s = result.unwrap();
    assert!(s.contains("pub enum temperature_units"), "Missing temperature_units enum");
}

#[test]
fn test_generate_scala_from_example() {
    let input = examples_dir().join("structures.toml");
    let output = PathBuf::from("/tmp/struct_writer_test_out.scala");
    let result = generate(&[input.as_path()], &[], Language::Scala, &output);
    assert!(result.is_ok(), "Generate Scala failed: {:?}", result.err());
    let s = result.unwrap();
    assert!(s.contains("sealed trait temperature_units"), "Missing temperature_units trait");
}

#[test]
fn test_generate_with_template_override() {
    let input = examples_dir().join("structures.toml");
    let tmpl = examples_dir().join("template_c.toml");
    let output = PathBuf::from("/tmp/struct_writer_test_out_override.h");
    let result = generate(&[input.as_path()], &[tmpl.as_path()], Language::C, &output);
    assert!(result.is_ok(), "Generate C with override failed: {:?}", result.err());
}
