/// Port of render_c.py — C code generator working with raw JSON definitions.
use std::collections::HashSet;
use std::path::Path;

use serde_json::Value;

use crate::struct_parse::complete_bit_field_member;
use crate::templating::Template;

use super::path_to_json;

pub fn render_file(
    definitions: &Value,
    templates: &Value,
    output_file: &Path,
) -> String {
    let mut rendered: HashSet<String> = HashSet::from(["file".to_string()]);
    // Work with a mutable clone so group rendering can inject synthetic definitions
    let mut defs = definitions.as_object().unwrap().clone();

    let file_val = defs.get("file").cloned().unwrap_or(Value::Object(Default::default()));
    let out_file_json = path_to_json(output_file);

    let ctx_desc = serde_json::json!({ "file": file_val });
    let ctx_head = serde_json::json!({ "out_file": out_file_json });

    let mut s = String::new();
    s += &Template::new(templates["file"]["description"].as_str().unwrap_or(""))
        .safe_render(&ctx_desc);
    s += &Template::new(templates["file"]["header"].as_str().unwrap_or(""))
        .safe_render(&ctx_head);
    s += &render_definitions(&mut defs, templates, &mut rendered);
    s += &Template::new(templates["file"]["footer"].as_str().unwrap_or(""))
        .safe_render(&ctx_head);
    s
}

fn render_definitions(
    definitions: &mut serde_json::Map<String, Value>,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    let mut s = String::new();
    let element_names: Vec<String> = definitions.keys().cloned().collect();
    let group_names: Vec<String> = element_names
        .iter()
        .filter(|k| {
            definitions.get(*k)
                .and_then(Value::as_object)
                .and_then(|o| o.get("type"))
                .and_then(Value::as_str)
                == Some("group")
        })
        .cloned()
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
    definitions: &mut serde_json::Map<String, Value>,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    if rendered.contains(element_name) {
        return String::new();
    }
    rendered.insert(element_name.to_string());

    let def_type = definitions.get(element_name)
        .and_then(Value::as_object)
        .and_then(|o| o.get("type"))
        .and_then(Value::as_str)
        .unwrap_or("")
        .to_string();

    match def_type.as_str() {
        "structure" => render_structure(element_name, definitions, templates, rendered),
        "enum" => render_enum(element_name, definitions, templates),
        "group" => render_group(element_name, definitions, templates, rendered),
        "bit_field" => render_bit_field(element_name, definitions, templates, rendered),
        _ => String::new(),
    }
}

fn render_structure(
    structure_name: &str,
    definitions: &mut serde_json::Map<String, Value>,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    let structure = definitions[structure_name].as_object().unwrap().clone();
    let mut structure_ctx = structure.clone();
    structure_ctx.insert("name".to_string(), Value::String(structure_name.to_string()));

    let expected_size = structure_ctx.get("size").and_then(Value::as_u64).unwrap_or(0);
    let mut measured_size = 0u64;
    let mut s = String::new();

    if let Some(members) = structure.get("members").and_then(Value::as_array) {
        for member in members {
            let mobj = member.as_object().unwrap();
            measured_size += mobj.get("size").and_then(Value::as_u64).unwrap_or(0);
            let member_type = mobj.get("type").and_then(Value::as_str).unwrap_or("");
            if definitions.contains_key(member_type) {
                s += &render_definition(member_type, definitions, templates, rendered);
            }
        }
    }

    let structure_val = Value::Object(structure_ctx);
    let ctx = serde_json::json!({ "structure": structure_val });

    s += &Template::new(templates["structure"]["header"].as_str().unwrap_or(""))
        .safe_render(&ctx);
    s += &render_structure_members(structure_name, definitions, templates);
    s += &Template::new(templates["structure"]["footer"].as_str().unwrap_or(""))
        .safe_render(&ctx);

    s
}

fn render_structure_members(
    structure_name: &str,
    definitions: &serde_json::Map<String, Value>,
    templates: &Value,
) -> String {
    let structure = definitions.get(structure_name).and_then(Value::as_object).unwrap();
    let mut s = String::new();
    if let Some(members) = structure.get("members").and_then(Value::as_array) {
        if members.is_empty() {
            s += templates["structure"]["members"]["empty"].as_str().unwrap_or("");
        } else {
            for member in members {
                s += &render_structure_member(member, templates);
            }
        }
    } else {
        s += templates["structure"]["members"]["empty"].as_str().unwrap_or("");
    }
    s
}

fn render_structure_member(member: &Value, templates: &Value) -> String {
    let mobj = member.as_object().unwrap();
    let member_type = mobj.get("type").and_then(Value::as_str).unwrap_or("");
    let ctx = serde_json::json!({ "member": member });

    if member_type == "union" {
        return render_structure_union(member, templates);
    }

    if let Some(tmpl) = templates["structure"]["members"].get(member_type) {
        Template::new(tmpl.as_str().unwrap_or("")).safe_render(&ctx)
    } else {
        Template::new(templates["structure"]["members"]["default"].as_str().unwrap_or(""))
            .safe_render(&ctx)
    }
}

fn render_structure_union(union: &Value, templates: &Value) -> String {
    let union_tmpl = &templates["structure"]["members"]["union"];
    let ctx = serde_json::json!({ "union": union });

    let mut s = String::new();
    s += &Template::new(union_tmpl["header"].as_str().unwrap_or("")).safe_render(&ctx);
    let union_obj = union.as_object().unwrap();
    if let Some(members) = union_obj.get("members").and_then(Value::as_array) {
        for m in members {
            s += &render_structure_member(m, templates);
        }
    }
    s += &Template::new(union_tmpl["footer"].as_str().unwrap_or("")).safe_render(&ctx);
    s
}

fn render_enum(
    element_name: &str,
    definitions: &serde_json::Map<String, Value>,
    templates: &Value,
) -> String {
    let enumeration = definitions[element_name].as_object().unwrap();
    let mut enum_ctx = enumeration.clone();
    enum_ctx.insert("name".to_string(), Value::String(element_name.to_string()));
    let enum_val = Value::Object(enum_ctx.clone());
    let ctx = serde_json::json!({ "enumeration": enum_val });

    let mut s = String::new();
    s += &Template::new(templates["enum"]["header"].as_str().unwrap_or("")).safe_render(&ctx);

    if let Some(values) = enum_ctx.get("values").and_then(Value::as_array) {
        let values = values.clone();
        for value in &values {
            s += &render_enum_value(value, &enum_val, templates);
        }
    }

    s += &Template::new(templates["enum"]["footer"].as_str().unwrap_or("")).safe_render(&ctx);
    s
}

fn render_enum_value(value: &Value, enumeration: &Value, templates: &Value) -> String {
    let ctx = serde_json::json!({ "enumeration": enumeration, "value": value });
    if value.get("value").is_some() {
        Template::new(templates["enum"]["valued"].as_str().unwrap_or("")).safe_render(&ctx)
    } else {
        Template::new(templates["enum"]["default"].as_str().unwrap_or("")).safe_render(&ctx)
    }
}

fn render_group(
    group_name: &str,
    definitions: &mut serde_json::Map<String, Value>,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    let group = definitions[group_name].as_object().unwrap().clone();

    // Collect group elements (all defs that reference this group)
    let mut group_elements: Vec<(String, serde_json::Map<String, Value>)> = definitions
        .iter()
        .filter_map(|(k, v)| {
            let obj = v.as_object()?;
            obj.get("groups")?.as_object()?.get(group_name)?;
            Some((k.clone(), obj.clone()))
        })
        .collect();

    if group_elements.is_empty() {
        return String::new();
    }

    // Sort by tag value
    group_elements.sort_by_key(|(_, obj)| {
        obj.get("groups")
            .and_then(Value::as_object)
            .and_then(|g| g.get(group_name))
            .and_then(Value::as_object)
            .and_then(|e| e.get("value"))
            .and_then(Value::as_i64)
            .unwrap_or(0)
    });

    let enum_size = group.get("size").and_then(Value::as_u64).unwrap_or(1) as usize;

    // Build synthetic enum for the tag
    let group_enum_name = format!("{}_tag", group_name);
    let mut group_enum_values = Vec::new();
    for (element_name, element) in &group_elements {
        let gentry = element["groups"][group_name].as_object().unwrap();
        let label = gentry["name"].as_str().unwrap_or("").to_string();
        let value = gentry["value"].as_i64().unwrap_or(0);
        let type_name = get_type_name_for_element(element_name, templates);
        let display = element.get("description").and_then(Value::as_str).unwrap_or("").to_string();
        let see_ref = format!("@see {}", type_name);
        group_enum_values.push(serde_json::json!({
            "label": label,
            "value": value,
            "display_name": display,
            "description": see_ref,
        }));
    }

    let group_enum = serde_json::json!({
        "name": group_enum_name,
        "display_name": format!("{} tag", group_name),
        "description": format!("Enumeration for {} tag", group_name),
        "type": "enum",
        "size": enum_size,
        "values": group_enum_values,
    });

    definitions.insert(group_enum_name.clone(), group_enum);
    let mut s = String::new();
    s += &render_definition(&group_enum_name, definitions, templates, rendered);

    for (element_name, _) in &group_elements {
        s += &render_definition(element_name, definitions, templates, rendered);
    }

    // Compute union size
    let union_size: usize = group_elements.iter().map(|(_, el)| {
        el.get("size").and_then(Value::as_u64).unwrap_or(0) as usize
    }).max().unwrap_or(0);

    // Build group struct with tag member + union member
    let mut group_struct_members = vec![
        serde_json::json!({
            "name": "tag",
            "size": enum_size,
            "type": group_enum_name,
            "description": format!("{} tag", group_name),
        })
    ];

    let mut union_members = Vec::new();
    for (element_name, element) in &group_elements {
        let gentry = element["groups"][group_name].as_object().unwrap();
        let member_name = gentry["name"].as_str().unwrap_or("").to_string();
        union_members.push(serde_json::json!({
            "name": member_name,
            "type": element_name,
            "display_name": element.get("display_name").and_then(Value::as_str).unwrap_or(""),
            "description": element.get("description").and_then(Value::as_str).unwrap_or(""),
            "size": element.get("size").and_then(Value::as_u64).unwrap_or(0),
        }));
    }

    let group_union = serde_json::json!({
        "name": "value",
        "type": "union",
        "description": "",
        "members": union_members,
        "size": union_size,
    });
    group_struct_members.push(group_union);

    let total_size = enum_size + union_size;
    let mut group_struct = serde_json::json!({
        "name": group_name,
        "display_name": group.get("display_name").and_then(Value::as_str).unwrap_or(""),
        "description": group.get("description").and_then(Value::as_str).unwrap_or(""),
        "type": "structure",
        "size": total_size,
        "members": group_struct_members,
    });

    definitions.insert(group_name.to_string(), group_struct);
    // Remove from rendered so it gets re-rendered as the struct
    rendered.remove(group_name);
    s += &render_definition(group_name, definitions, templates, rendered);

    s
}

fn get_type_name_for_element(element_name: &str, templates: &Value) -> String {
    // Try to evaluate templates["structure"]["type_name"] with element name
    if let Some(type_name_tmpl) = templates.get("structure").and_then(|s| s.get("type_name")) {
        let ctx = serde_json::json!({ "structure": { "name": element_name } });
        Template::new(type_name_tmpl.as_str().unwrap_or("")).safe_render(&ctx)
    } else {
        format!("{}_t", element_name)
    }
}

fn render_bit_field(
    bit_field_name: &str,
    definitions: &mut serde_json::Map<String, Value>,
    templates: &Value,
    rendered: &mut HashSet<String>,
) -> String {
    let bit_field = definitions[bit_field_name].as_object().unwrap().clone();
    let mut bf_ctx = bit_field.clone();
    bf_ctx.insert("name".to_string(), Value::String(bit_field_name.to_string()));

    let members = bit_field.get("members").and_then(Value::as_array).cloned().unwrap_or_default();
    let mut s = String::new();

    for member in &members {
        let member_type = member.get("type").and_then(Value::as_str).unwrap_or("");
        if definitions.contains_key(member_type) {
            s += &render_definition(member_type, definitions, templates, rendered);
        }
    }

    let bf_val = Value::Object(bf_ctx);
    let ctx = serde_json::json!({ "bit_field": bf_val });

    s += &Template::new(templates["bit_field"]["header"].as_str().unwrap_or("")).safe_render(&ctx);
    s += &render_bit_field_members(bit_field_name, definitions, templates);
    s += &Template::new(templates["bit_field"]["footer"].as_str().unwrap_or("")).safe_render(&ctx);
    s
}

fn render_bit_field_members(
    bit_field_name: &str,
    definitions: &serde_json::Map<String, Value>,
    templates: &Value,
) -> String {
    let bit_field = definitions.get(bit_field_name).and_then(Value::as_object).unwrap();
    let bf_size = bit_field.get("size").and_then(Value::as_u64).unwrap_or(1) as usize;
    let mut bf_ctx = bit_field.clone();
    bf_ctx.insert("name".to_string(), Value::String(bit_field_name.to_string()));
    let bf_val = Value::Object(bf_ctx);

    let members = bit_field.get("members").and_then(Value::as_array).cloned().unwrap_or_default();
    let mut s = String::new();
    let mut bit_position = 0u64;

    for member in &members {
        let mut mobj = member.as_object().unwrap().clone();
        let member_start = mobj.get("start").and_then(Value::as_u64).unwrap_or(0);

        complete_bit_field_member(&mut mobj, bf_size);
        let member_start_after = mobj.get("start").and_then(Value::as_u64).unwrap_or(0);

        if member_start_after == bit_position {
            s += &render_bit_field_member(&bf_val, &Value::Object(mobj.clone()), templates);
        } else {
            // Reserved gap
            let reserved = serde_json::json!({
                "name": "reserved",
                "start": bit_position,
                "last": member_start_after - 1,
                "type": "reserved",
                "bits": member_start_after - bit_position,
            });
            s += &render_bit_field_member(&bf_val, &reserved, templates);
            s += &render_bit_field_member(&bf_val, &Value::Object(mobj.clone()), templates);
        }

        bit_position = mobj.get("last").and_then(Value::as_u64).unwrap_or(0) + 1;
    }
    s
}

fn render_bit_field_member(
    bit_field: &Value,
    member: &Value,
    templates: &Value,
) -> String {
    let member_type = member.get("type").and_then(Value::as_str).unwrap_or("");
    let ctx = serde_json::json!({ "bit_field": bit_field, "member": member });

    if let Some(tmpl) = templates["bit_field"]["members"].get(member_type) {
        Template::new(tmpl.as_str().unwrap_or("")).safe_render(&ctx)
    } else {
        Template::new(templates["bit_field"]["members"]["default"].as_str().unwrap_or(""))
            .safe_render(&ctx)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::default_templates::c::default_template;
    use std::path::PathBuf;

    #[test]
    fn test_render_empty_file() {
        let definitions = serde_json::json!({
            "file": {
                "brief": "A brief file description",
                "description": "Longer prose describing what to find in the file"
            }
        });
        let template = default_template();
        let result = render_file(&definitions, &template, &PathBuf::from("my_file.h"));
        assert!(result.contains("A brief file description"));
        assert!(result.contains("MY_FILE_H_"));
        assert!(result.contains("#ifndef MY_FILE_H_"));
        assert!(result.contains("#define MY_FILE_H_"));
    }

    #[test]
    fn test_render_enum() {
        let definitions = serde_json::json!({
            "file": {
                "brief": "A brief file description",
                "description": "Longer prose describing what to find in the file"
            },
            "temperature_units": {
                "description": "The temperature units",
                "display_name": "Temperature units",
                "size": 1,
                "type": "enum",
                "values": [
                    {"label": "c", "value": 0, "display_name": "C", "description": "Degrees Celsius"},
                    {"label": "f", "display_name": "F", "description": "Degrees Fahrenheit"}
                ]
            }
        });
        let template = default_template();
        let result = render_file(&definitions, &template, &PathBuf::from("my_file.h"));
        assert!(result.contains("temperature_units_c"));
        assert!(result.contains("temperature_units_f"));
        assert!(result.contains("0x0"));
    }

    #[test]
    fn test_render_structure() {
        let definitions = serde_json::json!({
            "file": {"brief": "", "description": ""},
            "simple_struct": {
                "description": "A simple structure",
                "display_name": "Simple Structure",
                "size": 2,
                "type": "structure",
                "members": [
                    {"name": "a", "size": 1, "type": "int", "description": "field a"},
                    {"name": "b", "size": 1, "type": "uint", "description": "field b"}
                ]
            }
        });
        let template = default_template();
        let result = render_file(&definitions, &template, &PathBuf::from("out.h"));
        assert!(result.contains("simple_struct"));
        assert!(result.contains("int8_t a"));
        assert!(result.contains("uint8_t b"));
    }
}
