/// Port of render_rust.py — Rust code generator using typed definitions.
use std::collections::HashSet;
use std::path::Path;

use serde_json::Value;

use crate::definitions::{BitField, DefinedType, Enumeration, Group, Structure, TypeDefinitions};
use crate::templating::Template;

use super::path_to_json;

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

    let ctx_desc = serde_json::json!({ "file": file_json });
    let ctx_head = serde_json::json!({ "out_file": out_file_json });

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
            render_structure(element_name, definitions, templates, rendered)
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

// ─── Structure ────────────────────────────────────────────────────────────────

fn render_structure(
    structure_name: &str,
    definitions: &TypeDefinitions,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    let structure = definitions.definitions[structure_name].as_structure().unwrap();
    let expected_size = structure.size;
    let mut measured_size = 0;
    let mut s = String::new();

    for member in &structure.members {
        measured_size += member.size;
        if definitions.definitions.contains_key(&member.type_) {
            s += &render_definition(&member.type_, definitions, templates, rendered);
        }
    }

    let mut structure_dict = structure.to_json();
    {
        let sobj = structure_dict.as_object_mut().unwrap();
        let serialization = render_structure_serialization(sobj, templates);
        let deserialization = render_structure_deserialization(sobj, templates);
        sobj.insert("serialization".to_string(), Value::String(serialization));
        sobj.insert("deserialization".to_string(), Value::String(deserialization));
    }

    let members_snapshot = structure_dict.as_object().unwrap().clone();
    let ctx = serde_json::json!({ "structure": structure_dict });
    s += &Template::new(templates["structure"]["header"].as_str().unwrap_or("")).safe_render(&ctx);
    s += &render_structure_members(&members_snapshot, templates);
    s += &Template::new(templates["structure"]["footer"].as_str().unwrap_or("")).safe_render(&ctx);

    s
}

fn render_structure_serialization(
    structure: &serde_json::Map<String, Value>,
    templates: &Value,
) -> String {
    let mut lines = Vec::new();
    let mut start = 0usize;
    let members = structure.get("members").and_then(Value::as_array);
    if let Some(members) = members {
        for member in members {
            let mobj = member.as_object().unwrap();
            let size = mobj.get("size").and_then(Value::as_u64).unwrap_or(0) as usize;
            let end = start + size;
            let buf = serde_json::json!({ "start": start, "end": end });
            let member_type = mobj.get("type").and_then(Value::as_str).unwrap_or("default");
            let tmpl_str = get_member_sub_template(templates, member_type, "serialize");
            let ctx = serde_json::json!({ "member": member, "buffer": buf });
            lines.push(Template::new(tmpl_str).safe_render(&ctx));
            start = end;
        }
    }
    lines.join("\n")
}

fn render_structure_deserialization(
    structure: &serde_json::Map<String, Value>,
    templates: &Value,
) -> String {
    let mut lines = Vec::new();
    let mut start = 0usize;
    let members = structure.get("members").and_then(Value::as_array);
    if let Some(members) = members {
        for member in members {
            let mobj = member.as_object().unwrap();
            let size = mobj.get("size").and_then(Value::as_u64).unwrap_or(0) as usize;
            let end = start + size;
            let buf = serde_json::json!({ "start": start, "end": end });
            let member_type = mobj.get("type").and_then(Value::as_str).unwrap_or("default");
            let tmpl_str = get_member_sub_template(templates, member_type, "deserialize");
            let ctx = serde_json::json!({ "member": member, "buffer": buf });
            lines.push(Template::new(tmpl_str).safe_render(&ctx));
            start = end;
        }
    }
    lines.join("\n")
}

fn render_structure_members(
    structure: &serde_json::Map<String, Value>,
    templates: &Value,
) -> String {
    let mut s = String::new();
    let members = structure.get("members").and_then(Value::as_array);
    if let Some(members) = members {
        if members.is_empty() {
            s += &get_member_sub_template(templates, "empty", "definition").to_string();
        } else {
            for member in members {
                let mobj = member.as_object().unwrap();
                let member_type = mobj.get("type").and_then(Value::as_str).unwrap_or("default");
                let tmpl_str = get_member_sub_template(templates, member_type, "definition");
                let ctx = serde_json::json!({ "member": member });
                s += &Template::new(tmpl_str).safe_render(&ctx);
            }
        }
    } else {
        s += &get_member_sub_template(templates, "empty", "definition").to_string();
    }
    s
}

fn get_member_sub_template<'a>(
    templates: &'a Value,
    member_type: &str,
    sub_key: &str,
) -> &'a str {
    if let Some(member_templates) = templates["structure"]["members"].get(member_type) {
        if let Some(s) = member_templates.get(sub_key).and_then(Value::as_str) {
            return s;
        }
    }
    templates["structure"]["members"]["default"][sub_key].as_str().unwrap_or("")
}

// ─── Enumeration ─────────────────────────────────────────────────────────────

fn render_enum(
    element_name: &str,
    definitions: &TypeDefinitions,
    templates: &Value,
) -> String {
    let enumeration = definitions.definitions[element_name].as_enumeration().unwrap();
    let mut enum_dict = enumeration.to_json();
    {
        let eobj = enum_dict.as_object_mut().unwrap();
        let repr_type = enum_repr_type(enumeration);
        eobj.insert("repr_type".to_string(), Value::String(repr_type));
        let matches = enum_matches(eobj);
        eobj.insert("matches".to_string(), Value::String(matches));
    }

    let ctx = serde_json::json!({ "enumeration": enum_dict });

    let mut s = String::new();
    s += &Template::new(templates["enum"]["header"].as_str().unwrap_or("")).safe_render(&ctx);

    if let Some(values) = enum_dict.as_object().and_then(|o| o.get("values")).and_then(Value::as_array) {
        let values = values.clone();
        for value in &values {
            let vctx = serde_json::json!({ "enumeration": enum_dict, "value": value });
            s += &Template::new(templates["enum"]["valued"].as_str().unwrap_or(""))
                .safe_render(&vctx);
        }
    }

    s += &Template::new(templates["enum"]["footer"].as_str().unwrap_or("")).safe_render(&ctx);
    s
}

fn enum_repr_type(enumeration: &Enumeration) -> String {
    let signed = if enumeration.values.iter().any(|v| v.value < 0) { "i" } else { "u" };
    let bits = enumeration.size * 8;
    format!("{}{}", signed, bits)
}

fn enum_matches(enum_dict: &serde_json::Map<String, Value>) -> String {
    let name = enum_dict.get("name").and_then(Value::as_str).unwrap_or("");
    let values = enum_dict.get("values").and_then(Value::as_array);
    if let Some(values) = values {
        values.iter()
            .filter_map(|v| {
                let vobj = v.as_object()?;
                let val = vobj.get("value")?.as_i64()?;
                let label = vobj.get("label")?.as_str()?;
                Some(format!("{} => Ok({}::{}),", val, name, label))
            })
            .collect::<Vec<_>>()
            .join("\n")
    } else {
        String::new()
    }
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

    let enum_size = group.size;
    let union_size: usize = group.members.iter()
        .filter_map(|m| definitions.definitions.get(&m.type_))
        .map(|d| d.size())
        .max()
        .unwrap_or(0);
    let type_size = enum_size + union_size;
    let repr_type = format!("u{}", enum_size * 8);

    let mut group_dict = group.to_json();
    let gobj = group_dict.as_object_mut().unwrap();
    gobj.insert("repr_type".to_string(), Value::String(repr_type.clone()));
    gobj.insert("max_size".to_string(), Value::Number(type_size.into()));

    let ctx = serde_json::json!({ "group": group_dict });
    let mut s = String::new();

    // Group header from template
    s += &Template::new(templates["group"]["header"].as_str().unwrap_or("")).safe_render(&ctx);

    // Enum variants
    for member in &group.members {
        let member_def = definitions.definitions.get(&member.type_);
        s += &format!("{}({}) = {},\n", member.name, member.type_, member.value);
    }
    s += "}\n";

    // impl size()
    s += &format!("impl {} {{\n", group_name);
    s += "pub fn size(&self) -> usize {\nmatch self{\n";
    for member in &group.members {
        let payload_size = definitions.definitions.get(&member.type_)
            .map(|d| d.size()).unwrap_or(0);
        s += &format!("{}::{}(_) => {},\n", group_name, member.name, enum_size + payload_size);
    }
    s += "}\n}\n";

    // size_from_tag
    s += &format!("pub fn size_from_tag(tag: {}) -> Option<usize> {{\nmatch tag {{\n", repr_type);
    for member in &group.members {
        let payload_size = definitions.definitions.get(&member.type_)
            .map(|d| d.size()).unwrap_or(0);
        s += &format!(
            "{:#04x} => Some({}), // {}::{}\n",
            member.value,
            enum_size + payload_size,
            group_name,
            member.name
        );
    }
    s += " _ => None,";
    s += "}\n}\n}\n";

    // From<Group> for Group_slice
    s += &format!(
        "impl From<{0}> for {0}_slice {{\nfn from(value: {0}) -> Self {{\n#[allow(unused_mut)]\nlet mut buf = [0_u8; {1}];\nmatch value {{\n",
        group_name, type_size
    );
    for member in &group.members {
        let payload_size = definitions.definitions.get(&member.type_)
            .map(|d| d.size()).unwrap_or(0);
        let end = enum_size + payload_size;
        s += &format!("{0}::{1}(inner) => {{\n", group_name, member.name);
        s += &format!("buf[0..{0}].copy_from_slice(&{1}_{2}.to_le_bytes());\n", enum_size, member.value, repr_type);
        s += &format!("let inner_buf: {0}_slice = inner.into();\n", member.type_);
        s += &format!("buf[{0}..{1}].copy_from_slice(&inner_buf);\n", enum_size, end);
        s += "}\n";
    }
    s += "}\nbuf\n}\n}\n";

    // TryFrom<&[u8]> for Group
    s += &format!(
        "impl TryFrom<&[u8]> for {0} {{\ntype Error = ();\n\nfn try_from(value: &[u8]) -> Result<Self, Self::Error> {{\nif !(value.len() >= {1}) {{return Err(());}}\nlet repr_int = {2}::from_le_bytes(value[0..{1}]\n.try_into()\n.unwrap());\nmatch repr_int {{\n",
        group_name, enum_size, repr_type
    );
    for member in &group.members {
        let payload_size = definitions.definitions.get(&member.type_)
            .map(|d| d.size()).unwrap_or(0);
        let end = enum_size + payload_size;
        s += &format!("{} => {{\n", member.value);
        s += &format!("let inner_buf: &[u8] = &value[{}..];\n", enum_size);
        s += "let inner = inner_buf.try_into()?;\n";
        s += &format!("Ok({0}::{1}(inner))\n", group_name, member.name);
        s += "}\n";
    }
    s += &format!("_ => Err(()),\n}}\n}}\n}}\n\nimpl TryFrom<{0}_slice> for {0} {{\ntype Error = ();\n\nfn try_from(value: {0}_slice) -> Result<Self, Self::Error> {{\nlet r: &[u8] = &value;\nr.try_into()\n}}\n}}\n", group_name);

    // Render member structures
    for member in &group.members {
        s += &render_definition(&member.type_, definitions, templates, rendered);
    }

    s
}

// ─── BitField ─────────────────────────────────────────────────────────────────

fn render_bit_field(
    bit_field_name: &str,
    definitions: &TypeDefinitions,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    let bit_field = definitions.definitions[bit_field_name].as_bit_field().unwrap();
    let mut s = String::new();

    for member in &bit_field.members {
        if definitions.definitions.contains_key(&member.type_) {
            s += &render_definition(&member.type_, definitions, templates, rendered);
        }
    }

    let mut bf_dict = bit_field.to_json();
    {
        let bfobj = bf_dict.as_object_mut().unwrap();
        let serialization = render_bit_field_serialization(bfobj);
        let deserialization = render_bit_field_deserialization(bfobj);
        bfobj.insert("serialization".to_string(), Value::String(serialization));
        bfobj.insert("deserialization".to_string(), Value::String(deserialization));
    }

    let members_snapshot = bf_dict.as_object().unwrap().clone();
    let ctx = serde_json::json!({ "bit_field": bf_dict });

    s += &Template::new(templates["bit_field"]["header"].as_str().unwrap_or("")).safe_render(&ctx);
    s += &render_bit_field_members(&members_snapshot, templates);
    s += &Template::new(templates["bit_field"]["footer"].as_str().unwrap_or("")).safe_render(&ctx);
    s
}

fn render_bit_field_members(
    bit_field: &serde_json::Map<String, Value>,
    templates: &Value,
) -> String {
    let mut s = String::new();
    let members = bit_field.get("members").and_then(Value::as_array).cloned().unwrap_or_default();
    let bf_val = Value::Object(bit_field.clone());

    for member in &members {
        let member_type = member.get("type").and_then(Value::as_str).unwrap_or("default");
        let ctx = serde_json::json!({ "bit_field": bf_val, "member": member });
        if let Some(tmpl) = templates["bit_field"]["members"].get(member_type) {
            s += &Template::new(tmpl.as_str().unwrap_or("")).safe_render(&ctx);
        } else {
            s += &Template::new(templates["bit_field"]["members"]["default"].as_str().unwrap_or(""))
                .safe_render(&ctx);
        }
    }
    s
}

fn render_bit_field_serialization(bit_field_dict: &serde_json::Map<String, Value>) -> String {
    let size = bit_field_dict.get("size").and_then(Value::as_u64).unwrap_or(1) as usize;
    let bit_field_type = format!("u{}", 8 * size);
    let mut lines = Vec::new();

    lines.push(format!("let mut raw_bits = 0_{};", bit_field_type));

    let members = bit_field_dict.get("members").and_then(Value::as_array).cloned().unwrap_or_default();
    for member in &members {
        let mobj = member.as_object().unwrap();
        let mtype = mobj.get("type").and_then(Value::as_str).unwrap_or("").to_lowercase();
        let bits = mobj.get("bits").and_then(Value::as_u64).unwrap_or(1) as u32;
        let shift = mobj.get("start").and_then(Value::as_u64).unwrap_or(0);
        let name = mobj.get("name").and_then(Value::as_str).unwrap_or("");
        let mask = format!("0b{}", "1".repeat(bits as usize));
        let msize = mobj.get("size").and_then(Value::as_u64).unwrap_or(1) as usize;

        match mtype.as_str() {
            "bool" => {
                lines.push(format!(
                    "raw_bits |=  (if input.{} {{1_{}}} else {{0_{}}} <<  {});",
                    name, bit_field_type, bit_field_type, shift
                ));
            }
            "int" => {}
            "uint" => {
                lines.push(format!(
                    "raw_bits |= ((input.{} as {}) & {}_{}) << {};",
                    name, bit_field_type, mask, bit_field_type, shift
                ));
            }
            "reserved" => continue,
            _ => {
                let member_repr = format!("u{}", msize * 8);
                lines.push(format!(
                    "let {0}: {1}_slice = input.{0}.into();",
                    name, mtype
                ));
                lines.push(format!(
                    "let {0} = {1}::from_le_bytes({0}) as {2};",
                    name, member_repr, bit_field_type
                ));
                lines.push(format!(
                    "raw_bits |= (({}) & {}_{}) << {};",
                    name, mask, bit_field_type, shift
                ));
            }
        }
    }

    lines.push("raw_bits.to_le_bytes()".to_string());
    lines.join("\n")
}

fn render_bit_field_deserialization(bit_field_dict: &serde_json::Map<String, Value>) -> String {
    let size = bit_field_dict.get("size").and_then(Value::as_u64).unwrap_or(1) as usize;
    let bits = 8 * size;
    let bit_field_type = format!("u{}", bits);
    let mut lines = Vec::new();

    lines.push(format!(
        "let raw_bits = {}::from_le_bytes(input[0..{}].try_into().unwrap());",
        bit_field_type, size
    ));
    lines.push("Ok(Self{".to_string());

    let members = bit_field_dict.get("members").and_then(Value::as_array).cloned().unwrap_or_default();
    for member in &members {
        let mobj = member.as_object().unwrap();
        let mtype = mobj.get("type").and_then(Value::as_str).unwrap_or("").to_lowercase();
        let mbits = mobj.get("bits").and_then(Value::as_u64).unwrap_or(1) as u32;
        let shift = mobj.get("start").and_then(Value::as_u64).unwrap_or(0);
        let name = mobj.get("name").and_then(Value::as_str).unwrap_or("");
        let msize = mobj.get("size").and_then(Value::as_u64).unwrap_or(1) as usize;
        let mask = format!("0b{}", "1".repeat(mbits as usize));
        let member_repr = format!("u{}", msize * 8);
        let to_uint = format!("(raw_bits >> {}) & {}_{}", shift, mask, bit_field_type);

        if mtype == "reserved" {
            continue;
        }

        let field_val = match mtype.as_str() {
            "bool" => format!("({}) != 0", to_uint),
            "int" => format!("({}) as i{}", to_uint, bits),
            "uint" => format!("({}) as {}", to_uint, member_repr),
            _ => format!(
                r#"{}::try_from(({}) as {}).map_err(|_| debug!("Failed to parse {}: {{}}", {}))?"#,
                mtype, to_uint, member_repr, mtype, to_uint
            ),
        };
        lines.push(format!("{}: {},", name, field_val));
    }

    lines.push("})".to_string());
    lines.join("\n")
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::default_templates::rust_lang::default_template;
    use std::path::PathBuf;

    #[test]
    fn test_render_empty_file() {
        let definitions = serde_json::json!({
            "file": {
                "brief": "A brief",
                "description": "A description"
            }
        });
        let template = default_template();
        let result = render_file(&definitions, &template, &PathBuf::from("my_file.rs"));
        assert!(result.contains("A brief"));
        assert!(result.contains("my_file"));
    }

    #[test]
    fn test_render_simple_enum() {
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
        let result = render_file(&definitions, &template, &PathBuf::from("out.rs"));
        assert!(result.contains("pub enum temperature_units"));
        assert!(result.contains("c = 0x0"));
        assert!(result.contains("f = 0x1"));
        assert!(result.contains("TryFrom<u8>"));
    }

    #[test]
    fn test_render_structure() {
        let definitions = serde_json::json!({
            "simple_struct": {
                "description": "A simple structure",
                "display_name": "Simple Structure",
                "size": 4,
                "type": "structure",
                "members": [
                    {"name": "x", "size": 4, "type": "uint", "description": "x value"}
                ]
            }
        });
        let template = default_template();
        let result = render_file(&definitions, &template, &PathBuf::from("out.rs"));
        assert!(result.contains("pub struct simple_struct"));
        assert!(result.contains("pub x: u32"));
    }
}
