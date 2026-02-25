/// Port of render_scala.py — Scala code generator using typed definitions.
use std::collections::HashSet;
use std::path::Path;

use serde_json::Value;

use crate::definitions::{BitField, DefinedType, Enumeration, Group, Structure, TypeDefinitions};
use crate::templating::Template;

use super::path_to_json;

const INT_MAX_VALUE: i64 = 2147483647;
const INT_MIN_VALUE: i64 = -2147483648;

const SCALA_KEYWORDS: &[&str] = &[
    "abstract", "case", "catch", "class", "def", "do", "else", "extends", "false", "final",
    "finally", "for", "forSome", "if", "implicit", "import", "lazy", "match", "new", "null",
    "object", "override", "package", "private", "protected", "return", "sealed", "super",
    "this", "throw", "trait", "true", "try", "type", "val", "var", "while", "with", "yield",
];

fn escape_scala_keyword(name: &str) -> String {
    if SCALA_KEYWORDS.contains(&name) {
        format!("`{}`", name)
    } else {
        name.to_string()
    }
}

fn format_large_int(value: i64) -> String {
    if value > INT_MAX_VALUE || value < INT_MIN_VALUE {
        format!("{}L", value)
    } else {
        value.to_string()
    }
}

fn extract_package_from_path(output_file: &Path) -> String {
    let parts: Vec<&str> = output_file
        .components()
        .map(|c| c.as_os_str().to_str().unwrap_or(""))
        .collect();

    // Look for src/main/scala pattern
    for i in 0..parts.len() {
        if parts[i] == "src"
            && i + 2 < parts.len()
            && parts[i + 1] == "main"
            && parts[i + 2] == "scala"
        {
            let package_parts: Vec<&str> = parts[i + 3..parts.len().saturating_sub(1)].to_vec();
            if !package_parts.is_empty() {
                return package_parts.join(".");
            }
        }
    }
    "generated".to_string()
}

pub fn render_file(
    definitions: &Value,
    templates: &Value,
    output_file: &Path,
) -> String {
    let mut rendered: HashSet<String> = HashSet::from(["file".to_string()]);
    let raw = definitions.as_object().unwrap().clone();
    let parsed = match TypeDefinitions::from_dict(raw) {
        Ok(d) => d,
        Err(e) => panic!("Failed to parse definitions: {}", e),
    };

    let out_file_json = path_to_json(output_file);
    let file_json = parsed.file_info.to_json();
    let package = extract_package_from_path(output_file);

    let ctx_desc = serde_json::json!({ "file": file_json });
    let ctx_head = serde_json::json!({
        "out_file": out_file_json,
        "package": package
    });

    let mut s = String::new();
    s += &Template::new(templates["file"]["description"].as_str().unwrap_or(""))
        .safe_render(&ctx_desc);
    s += &Template::new(templates["file"]["header"].as_str().unwrap_or(""))
        .safe_render(&ctx_head);
    s += &render_definitions(&parsed, templates, &mut rendered);
    s += &Template::new(templates["file"]["footer"].as_str().unwrap_or(""))
        .safe_render(&ctx_head);
    s
}

fn render_definitions(
    definitions: &TypeDefinitions,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    let mut s = String::new();
    let mut element_names: Vec<String> = definitions.definitions.keys().cloned().collect();
    element_names.sort();

    let group_names: Vec<String> = definitions
        .definitions
        .iter()
        .filter(|(_, v)| matches!(v, DefinedType::Group(_)))
        .map(|(k, _)| k.clone())
        .collect();

    for name in &group_names {
        s += &render_definition(name, definitions, templates, rendered);
    }
    for name in &element_names {
        s += &render_definition(name, definitions, templates, rendered);
    }
    s
}

fn render_definition(
    element_name: &str,
    definitions: &TypeDefinitions,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    if rendered.contains(element_name) {
        return String::new();
    }
    rendered.insert(element_name.to_string());

    match definitions.definitions.get(element_name) {
        Some(DefinedType::Structure(_)) => {
            render_structure(element_name, definitions, templates, rendered, None)
        }
        Some(DefinedType::Enumeration(_)) => {
            render_enum(element_name, definitions, templates)
        }
        Some(DefinedType::Group(_)) => {
            render_group(element_name, definitions, templates, rendered)
        }
        Some(DefinedType::BitField(_)) => {
            render_bit_field(element_name, definitions, templates, rendered)
        }
        None => String::new(),
    }
}

// ─── Enum ─────────────────────────────────────────────────────────────────────

fn render_enum(
    element_name: &str,
    definitions: &TypeDefinitions,
    templates: &Value,
) -> String {
    let enumeration = definitions.definitions[element_name].as_enumeration().unwrap();
    let enum_dict = enumeration.to_json();
    let eobj = enum_dict.as_object().unwrap();

    // Pre-compute template fragments
    let byte_matches = build_byte_matches(element_name, &enumeration.values);
    let to_byte_matches = build_to_byte_matches(element_name, &enumeration.values);
    let to_display_matches = build_to_display_matches(element_name, &enumeration.values);
    let from_display_matches = build_from_display_matches(element_name, &enumeration.values);

    let mut enum_ctx = eobj.clone();
    enum_ctx.insert("byte_matches".to_string(), Value::String(byte_matches));
    enum_ctx.insert("to_byte_matches".to_string(), Value::String(to_byte_matches));
    enum_ctx.insert("to_display_matches".to_string(), Value::String(to_display_matches));
    enum_ctx.insert("from_display_matches".to_string(), Value::String(from_display_matches));

    let enum_val = Value::Object(enum_ctx);
    let ctx = serde_json::json!({ "enumeration": enum_val });
    let mut s = String::new();
    s += &Template::new(templates["enum"]["header"].as_str().unwrap_or("")).safe_render(&ctx);

    for value in &enumeration.values {
        let escaped_label = escape_scala_keyword(&value.label);
        let val_ctx = serde_json::json!({
            "enumeration": enum_val,
            "value": {
                "label": value.label,
                "value": value.value,
                "display_name": value.display_name,
                "description": value.description,
                "escaped_label": escaped_label,
            }
        });
        s += &Template::new(templates["enum"]["valued"].as_str().unwrap_or(""))
            .safe_render(&val_ctx);
    }

    s += &Template::new(templates["enum"]["footer"].as_str().unwrap_or("")).safe_render(&ctx);
    s
}

fn build_byte_matches(enum_name: &str, values: &[crate::definitions::EnumValue]) -> String {
    values.iter().map(|v| {
        let escaped = escape_scala_keyword(&v.label);
        format!("    case {} => Some({}.{})", v.value, enum_name, escaped)
    }).collect::<Vec<_>>().join("\n")
}

fn build_to_byte_matches(enum_name: &str, values: &[crate::definitions::EnumValue]) -> String {
    values.iter().map(|v| {
        let escaped = escape_scala_keyword(&v.label);
        format!("    case {}.{} => {}.toByte", enum_name, escaped, v.value)
    }).collect::<Vec<_>>().join("\n")
}

fn build_to_display_matches(enum_name: &str, values: &[crate::definitions::EnumValue]) -> String {
    values.iter().map(|v| {
        let escaped = escape_scala_keyword(&v.label);
        format!("    case {}.{} => \"{}\"", enum_name, escaped, v.label)
    }).collect::<Vec<_>>().join("\n")
}

fn build_from_display_matches(enum_name: &str, values: &[crate::definitions::EnumValue]) -> String {
    values.iter().map(|v| {
        let escaped = escape_scala_keyword(&v.label);
        format!(
            "    if (s == \"{}\") return Some({}.{})",
            v.label, enum_name, escaped
        )
    }).collect::<Vec<_>>().join("\n")
}

// ─── Structure ────────────────────────────────────────────────────────────────

fn render_structure(
    structure_name: &str,
    definitions: &TypeDefinitions,
    templates: &Value,
    rendered: &mut HashSet<String>,
    extends_group: Option<&str>,
) -> String {
    if rendered.contains(structure_name) {
        return String::new();
    }
    rendered.insert(structure_name.to_string());

    let structure = definitions.definitions[structure_name].as_structure().unwrap();
    let structure_dict = structure.to_json();
    let sobj = structure_dict.as_object().unwrap();

    let mut s = String::new();
    // Render dependencies first
    for member in &structure.members {
        if definitions.definitions.contains_key(&member.type_) && !rendered.contains(&member.type_) {
            s += &render_definition(&member.type_, definitions, templates, rendered);
        }
    }

    let parent_trait = extends_group.unwrap_or("ByteSequence").to_string();
    let marker_def = if let Some(gname) = extends_group {
        // Find the tag value for this structure in the group
        if let Some(group) = definitions.definitions.get(gname).and_then(|d| d.as_group()) {
            if let Some(member) = group.members.iter().find(|m| m.type_ == structure_name) {
                format!("val TagValue: Int = {}", member.value)
            } else {
                String::new()
            }
        } else {
            String::new()
        }
    } else {
        String::new()
    };

    let deserialization = build_structure_deserialization(sobj, definitions, templates);
    let serialization = build_structure_serialization(sobj, definitions, templates);

    let mut struct_ctx = sobj.clone();
    struct_ctx.insert("parent_trait".to_string(), Value::String(parent_trait));
    struct_ctx.insert("marker_def".to_string(), Value::String(marker_def));
    struct_ctx.insert("deserialization".to_string(), Value::String(deserialization));
    struct_ctx.insert("serialization".to_string(), Value::String(serialization));

    let struct_val = Value::Object(struct_ctx);
    let ctx = serde_json::json!({ "structure": struct_val });

    s += &Template::new(templates["structure"]["header"].as_str().unwrap_or("")).safe_render(&ctx);
    s += &render_structure_member_defs(&structure.members, definitions, templates);
    s += &Template::new(templates["structure"]["footer"].as_str().unwrap_or("")).safe_render(&ctx);
    s
}

fn render_structure_member_defs(
    members: &[crate::definitions::StructureMember],
    definitions: &TypeDefinitions,
    templates: &Value,
) -> String {
    if members.is_empty() {
        return get_scala_member_template(templates, "empty", "definition").to_string();
    }
    let mut lines = Vec::new();
    let mut offset = 0;
    for member in members {
        let end = offset + member.size;
        let buf = serde_json::json!({ "start": offset, "end": end });
        let member_type = &member.type_;
        let scala_type = scala_type_for_member(member_type, member.size, definitions);
        let member_json = serde_json::json!({
            "name": member.name,
            "type": member.type_,
            "size": member.size,
            "description": member.description,
            "scala_type": scala_type,
        });
        let tmpl_str = get_scala_member_template(templates, member_type, "definition");
        let ctx = serde_json::json!({ "member": member_json, "buffer": buf });
        lines.push(Template::new(tmpl_str).safe_render(&ctx));
        offset = end;
    }
    lines.join("")
}

fn build_structure_deserialization(
    structure: &serde_json::Map<String, Value>,
    definitions: &TypeDefinitions,
    templates: &Value,
) -> String {
    let mut lines = Vec::new();
    let members = structure.get("members").and_then(Value::as_array).cloned().unwrap_or_default();
    let mut offset = 0;
    for member in &members {
        let mobj = member.as_object().unwrap();
        let size = mobj.get("size").and_then(Value::as_u64).unwrap_or(0) as usize;
        let end = offset + size;
        let buf = serde_json::json!({ "start": offset, "end": end });
        let member_type = mobj.get("type").and_then(Value::as_str).unwrap_or("default");
        let scala_type = scala_type_for_member(
            member_type,
            size,
            definitions,
        );
        let mut member_with_scala = mobj.clone();
        member_with_scala.insert("scala_type".to_string(), Value::String(scala_type));
        let tmpl_str = get_scala_member_template(templates, member_type, "deserialize");
        let ctx = serde_json::json!({ "member": Value::Object(member_with_scala), "buffer": buf });
        lines.push(Template::new(tmpl_str).safe_render(&ctx));
        offset = end;
    }
    lines.join("\n")
}

fn build_structure_serialization(
    structure: &serde_json::Map<String, Value>,
    definitions: &TypeDefinitions,
    templates: &Value,
) -> String {
    let mut lines = Vec::new();
    let members = structure.get("members").and_then(Value::as_array).cloned().unwrap_or_default();
    let mut offset = 0;
    for member in &members {
        let mobj = member.as_object().unwrap();
        let size = mobj.get("size").and_then(Value::as_u64).unwrap_or(0) as usize;
        let end = offset + size;
        let buf = serde_json::json!({ "start": offset, "end": end });
        let member_type = mobj.get("type").and_then(Value::as_str).unwrap_or("default");
        let scala_type = scala_type_for_member(member_type, size, definitions);
        let mut member_with_scala = mobj.clone();
        member_with_scala.insert("scala_type".to_string(), Value::String(scala_type));
        let tmpl_str = get_scala_member_template(templates, member_type, "serialize");
        let ctx = serde_json::json!({ "member": Value::Object(member_with_scala), "buffer": buf });
        lines.push(Template::new(tmpl_str).safe_render(&ctx));
        offset = end;
    }
    lines.join("\n")
}

fn scala_type_for_member(
    member_type: &str,
    size: usize,
    definitions: &TypeDefinitions,
) -> String {
    match member_type {
        "int" => match size {
            1 => "Byte", 2 => "Short", 4 => "Int", 8 => "Long", _ => "Int",
        }.to_string(),
        "uint" => match size {
            1 | 2 => "Int", 4 => "Long", 8 => "Long", _ => "Long",
        }.to_string(),
        "bool" => if size == 1 { "Boolean".to_string() } else { "Array[Byte]".to_string() },
        "bytes" | "reserved" => "Array[Byte]".to_string(),
        "str" => "String".to_string(),
        _ => {
            if definitions.definitions.contains_key(member_type) {
                member_type.to_string()
            } else {
                member_type.to_string()
            }
        }
    }
}

fn get_scala_member_template<'a>(templates: &'a Value, member_type: &str, sub_key: &str) -> &'a str {
    if let Some(mt) = templates["structure"]["members"].get(member_type) {
        if let Some(s) = mt.get(sub_key).and_then(Value::as_str) {
            return s;
        }
    }
    templates["structure"]["members"]["default"][sub_key].as_str().unwrap_or("")
}

// ─── Group ────────────────────────────────────────────────────────────────────

fn render_group(
    group_name: &str,
    definitions: &TypeDefinitions,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    let group = definitions.definitions[group_name].as_group().unwrap();

    if group.members.is_empty() {
        return String::new();
    }

    let common_fields = find_common_fields(group, definitions);
    let mut s = String::new();

    // Build trait with optional common fields
    let trait_str = build_group_trait(group_name, &common_fields, definitions);

    // Build tag read expression
    let tag_read_expression = match group.size {
        1 => "bytes(0) & 0xFF".to_string(),
        2 => "BinaryUtils.bytesToUint16LE(bytes, 0)".to_string(),
        4 => "BinaryUtils.bytesToUint32LE(bytes, 0).toInt".to_string(),
        _ => format!("bytes({}) & 0xFF", group.size - 1),
    };

    // Build decode matches
    let decode_matches: Vec<String> = {
        let mut members = group.members.clone();
        members.sort_by_key(|m| m.value);
        members.iter().map(|m| {
            let tag_val = format_large_int(m.value);
            format!(
                "      case {} => {}.decode(structureBytes, streamPositionHead).asInstanceOf[Try[{}]]",
                tag_val, m.type_, group_name
            )
        }).collect()
    };

    let mut group_ctx = group.to_json().as_object().unwrap().clone();
    group_ctx.insert("tag_read_expression".to_string(), Value::String(tag_read_expression));
    group_ctx.insert("decode_matches".to_string(), Value::String(decode_matches.join("\n")));
    let group_val = Value::Object(group_ctx);
    let ctx = serde_json::json!({ "group": group_val });

    s += &format!("// {}\n// {}\n", group.display_name, group.description);
    s += &trait_str;
    s += &Template::new(templates["group"]["header"].as_str().unwrap_or("")).safe_render(&ctx);
    s += &Template::new(templates["group"]["footer"].as_str().unwrap_or("")).safe_render(&ctx);

    // Render member structures
    for member in &group.members {
        if !rendered.contains(&member.type_) {
            s += &render_structure(&member.type_, definitions, templates, rendered, Some(group_name));
        }
    }

    s
}

fn find_common_fields(
    group: &Group,
    definitions: &TypeDefinitions,
) -> Vec<(String, String, usize)> {
    if group.members.is_empty() {
        return vec![];
    }

    let first_type = &group.members[0].type_;
    let first_struct = match definitions.definitions.get(first_type) {
        Some(DefinedType::Structure(s)) => s,
        _ => return vec![],
    };

    let mut candidates: std::collections::HashMap<String, (String, usize)> = first_struct
        .members
        .iter()
        .map(|m| (m.name.clone(), (m.type_.clone(), m.size)))
        .collect();

    for gm in &group.members[1..] {
        let s = match definitions.definitions.get(&gm.type_) {
            Some(DefinedType::Structure(s)) => s,
            _ => return vec![],
        };
        let member_map: std::collections::HashMap<String, (String, usize)> = s.members
            .iter()
            .map(|m| (m.name.clone(), (m.type_.clone(), m.size)))
            .collect();
        candidates.retain(|k, v| member_map.get(k) == Some(v));
    }

    let mut result: Vec<(String, String, usize)> = candidates
        .into_iter()
        .map(|(name, (type_, size))| (name, type_, size))
        .collect();
    result.sort_by(|a, b| a.0.cmp(&b.0));
    result
}

fn build_group_trait(
    group_name: &str,
    common_fields: &[(String, String, usize)],
    definitions: &TypeDefinitions,
) -> String {
    if common_fields.is_empty() {
        return format!("sealed trait {} extends ByteSequence\n\n", group_name);
    }

    let mut lines = vec![format!("sealed trait {} extends ByteSequence {{", group_name)];
    for (field_name, field_type, field_size) in common_fields {
        let scala_type = if definitions.definitions.contains_key(field_type.as_str()) {
            field_type.clone()
        } else {
            scala_type_for_member(field_type, *field_size, definitions)
        };
        lines.push(format!("  def {}: {}", field_name, scala_type));
    }
    lines.push("}\n".to_string());
    lines.join("\n") + "\n"
}

// ─── BitField ─────────────────────────────────────────────────────────────────

fn render_bit_field(
    bit_field_name: &str,
    definitions: &TypeDefinitions,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    let bit_field = definitions.definitions[bit_field_name].as_bit_field().unwrap();
    let bf_dict = bit_field.to_json();
    let bfobj = bf_dict.as_object().unwrap();

    let bits = bit_field.size * 8;
    let (read_func, write_func) = match bits {
        8 => ("bytesToUint8LE", "uint8LEtoBytes"),
        16 => ("bytesToUint16LE", "uint16LEtoBytes"),
        32 => ("bytesToUint32LE", "uint32LEtoBytes"),
        64 => ("bytesToUint64LE", "uint64LEtoBytes"),
        _ => ("bytesToUint32LE", "uint32LEtoBytes"),
    };
    let scala_raw_type = match bits {
        64 => "Long",
        _ => "Int",
    };

    let mut s = String::new();

    // Render dependencies
    for member in &bit_field.members {
        if definitions.definitions.contains_key(&member.type_) && !rendered.contains(&member.type_) {
            s += &render_definition(&member.type_, definitions, templates, rendered);
        }
    }

    // Build member definitions (excluding reserved)
    let member_defs: Vec<String> = bit_field.members.iter()
        .filter(|m| m.type_ != "reserved")
        .map(|m| {
            let scala_type = if m.type_ == "bool" && m.bits == 1 {
                "Boolean".to_string()
            } else {
                "Int".to_string()
            };
            format!("  {}: {},", m.name, scala_type)
        })
        .collect();

    // Build fromBytes deserialization
    let deser_lines: Vec<String> = bit_field.members.iter()
        .filter(|m| m.type_ != "reserved")
        .map(|m| {
            let mask = (1i64 << m.bits) - 1;
            let mask_str = format_large_int(mask);
            if m.type_ == "bool" && m.bits == 1 {
                format!("      {} = ((rawBits >> {}) & {}) != 0,", m.name, m.start, mask_str)
            } else {
                format!("      {} = ((rawBits >> {}) & {}).toInt,", m.name, m.start, mask_str)
            }
        })
        .collect();

    // Build toBytes serialization
    let ser_lines: Vec<String> = bit_field.members.iter()
        .filter(|m| m.type_ != "reserved")
        .map(|m| {
            let mask = (1i64 << m.bits) - 1;
            let mask_str = format_large_int(mask);
            if m.type_ == "bool" && m.bits == 1 {
                format!("    rawBits |= ((if (value.{}) 1 else 0) << {})", m.name, m.start)
            } else {
                format!("    rawBits |= ((value.{} & {}) << {})", m.name, mask_str, m.start)
            }
        })
        .collect();

    let raw_bits_read = format!("val rawBits = BinaryUtils.{}(bytes, 0)", read_func);

    let mut bf_ctx = bfobj.clone();
    bf_ctx.insert("raw_bits_read".to_string(), Value::String(raw_bits_read));
    bf_ctx.insert("deserialization".to_string(), Value::String(deser_lines.join("\n")));
    bf_ctx.insert("serialization".to_string(), Value::String(format!(
        "    var rawBits: {} = 0\n{}\n    BinaryUtils.{}(rawBits).toSeq",
        scala_raw_type,
        ser_lines.join("\n"),
        write_func
    )));

    let bf_val = Value::Object(bf_ctx);
    let ctx = serde_json::json!({ "bit_field": bf_val });

    s += &Template::new(templates["bit_field"]["header"].as_str().unwrap_or("")).safe_render(&ctx);

    for line in &member_defs {
        s += line;
        s += "\n";
    }

    s += &Template::new(templates["bit_field"]["footer"].as_str().unwrap_or("")).safe_render(&ctx);
    s
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::default_templates::scala::default_template;
    use std::path::PathBuf;

    #[test]
    fn test_render_empty_file() {
        let definitions = serde_json::json!({
            "file": {"brief": "A brief", "description": "Desc"}
        });
        let template = default_template();
        let result = render_file(&definitions, &template, &PathBuf::from("generated/Output.scala"));
        assert!(result.contains("package generated"));
        assert!(result.contains("A brief"));
    }

    #[test]
    fn test_render_enum() {
        let definitions = serde_json::json!({
            "temperature_units": {
                "description": "Temperature units",
                "display_name": "Temperature Units",
                "size": 1,
                "type": "enum",
                "values": [
                    {"label": "c", "value": 0, "display_name": "C", "description": "Celsius"},
                    {"label": "f", "value": 1, "display_name": "F", "description": "Fahrenheit"}
                ]
            }
        });
        let template = default_template();
        let result = render_file(&definitions, &template, &PathBuf::from("generated/Out.scala"));
        assert!(result.contains("sealed trait temperature_units"));
        assert!(result.contains("case object c extends temperature_units"));
        assert!(result.contains("case object f extends temperature_units"));
    }

    #[test]
    fn test_extract_package() {
        let path = Path::new("/my/project/src/main/scala/com/example/Output.scala");
        assert_eq!(extract_package_from_path(path), "com.example");

        let path2 = Path::new("generated/Output.scala");
        assert_eq!(extract_package_from_path(path2), "generated");
    }

    #[test]
    fn test_escape_scala_keyword() {
        assert_eq!(escape_scala_keyword("type"), "`type`");
        assert_eq!(escape_scala_keyword("myField"), "myField");
        assert_eq!(escape_scala_keyword("class"), "`class`");
    }
}
