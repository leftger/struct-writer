pub mod c;
pub mod rust_lang;
pub mod scala;

use std::path::Path;
use serde_json::Value;

/// Build a JSON context for the output file path.
/// Replicates Python's `Path` object attributes used in templates.
pub fn path_to_json(path: &Path) -> Value {
    let stem = path
        .file_stem()
        .and_then(|s| s.to_str())
        .unwrap_or("");
    let name = path
        .file_name()
        .and_then(|s| s.to_str())
        .unwrap_or("");
    let parent = path
        .parent()
        .and_then(|p| p.to_str())
        .unwrap_or("");
    serde_json::json!({
        "stem": stem,
        "name": name,
        "parent": parent,
        // convenience: suffix/extension
        "suffix": path.extension().and_then(|e| e.to_str()).unwrap_or(""),
    })
}
