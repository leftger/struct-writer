use std::path::Path;

use serde_json::Value;

use crate::default_templates::{self, toml_to_json};
use crate::templating::merge;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Language {
    C,
    Rust,
    Scala,
}

impl Language {
    pub fn from_str(s: &str) -> Option<Self> {
        match s.to_lowercase().as_str() {
            "c" => Some(Self::C),
            "rust" => Some(Self::Rust),
            "scala" => Some(Self::Scala),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &'static str {
        match self {
            Self::C => "c",
            Self::Rust => "rust",
            Self::Scala => "scala",
        }
    }
}

/// Load a markup file (.toml, .json, .yaml/.yml) into a serde_json::Value.
pub fn load_markup_file(path: &Path) -> Result<Value, Box<dyn std::error::Error>> {
    let extension = path.extension().and_then(|e| e.to_str()).unwrap_or("");
    let content = std::fs::read(path)?;

    match extension {
        "toml" => {
            let s = String::from_utf8(content)?;
            let t: toml::Value = toml::from_str(&s)?;
            Ok(toml_to_json(t))
        }
        "json" => {
            let v: Value = serde_json::from_slice(&content)?;
            Ok(v)
        }
        "yml" | "yaml" => {
            let v: Value = serde_yaml::from_slice(&content)?;
            Ok(v)
        }
        other => Err(format!("Unsupported file extension: {}", other).into()),
    }
}

/// Render code for the given language from definitions and templates.
pub fn render(
    definitions: &Value,
    templates: &Value,
    language: Language,
    output_file: &Path,
) -> String {
    match language {
        Language::C => crate::render::c::render_file(definitions, templates, output_file),
        Language::Rust => crate::render::rust_lang::render_file(definitions, templates, output_file),
        Language::Scala => crate::render::scala::render_file(definitions, templates, output_file),
    }
}

/// Get the default template for a given language.
pub fn default_template(language: Language) -> Value {
    match language {
        Language::C => default_templates::c::default_template(),
        Language::Rust => default_templates::rust_lang::default_template(),
        Language::Scala => default_templates::scala::default_template(),
    }
}

/// Main generation function: loads inputs, merges templates, renders output.
pub fn generate(
    input_files: &[&Path],
    template_files: &[&Path],
    language: Language,
    output_file: &Path,
) -> Result<String, Box<dyn std::error::Error>> {
    // Load and merge all input definition files
    let mut definitions = Value::Object(Default::default());
    for input_file in input_files {
        let loaded = load_markup_file(input_file)?;
        definitions = merge(definitions, loaded);
    }

    // Start with the default template and overlay with user-provided templates
    let mut templates = default_template(language);
    for template_file in template_files {
        let loaded = load_markup_file(template_file)?;
        templates = merge(templates, loaded);
    }

    Ok(render(&definitions, &templates, language, output_file))
}
