/// Port of struct_parse.py — binary codec for the defined types.
use serde_json::Value;
use std::collections::HashMap;

// ─── Bytes → String helper ────────────────────────────────────────────────────

pub fn bytes_to_str(data: &[u8]) -> String {
    let hex_parts: Vec<String> = data.iter().map(|b| format!("{:02X}", b)).collect();
    format!("{} (len={})", hex_parts.join(" "), data.len())
}

// ─── Complete helpers (for raw-dict processing) ───────────────────────────────

/// Fill in `bits` and `last` for a bit field member dict.
/// Returns a clone with the missing fields populated.
pub fn complete_bit_field_member(
    member: &mut serde_json::Map<String, Value>,
    bit_field_size: usize,
) {
    let start = member.get("start").and_then(|v| v.as_u64()).unwrap_or(0) as u32;

    let (end, bits) = match (
        member.get("end").and_then(|v| v.as_u64()),
        member.get("bits").and_then(|v| v.as_u64()),
    ) {
        (Some(e), Some(b)) => (e as u32, b as u32),
        (Some(e), None) => (e as u32, e as u32 - start),
        (None, Some(b)) => (start + b as u32, b as u32),
        (None, None) => (start + 1, 1),
    };

    member.insert("end".to_string(), Value::Number(end.into()));
    member.insert("bits".to_string(), Value::Number(bits.into()));
    member.insert("last".to_string(), Value::Number((end - 1).into()));
    member.insert(
        "size".to_string(),
        Value::Number(((bits as f64 / 8.0).ceil() as u64).into()),
    );
}

/// Fill in missing `value` fields for enum values with sequential integers.
pub fn complete_enums(mut enum_def: serde_json::Map<String, Value>) -> serde_json::Map<String, Value> {
    if enum_def.get("complete").and_then(Value::as_bool).unwrap_or(false) {
        return enum_def;
    }

    let values = enum_def.get_mut("values").and_then(Value::as_array_mut);
    if let Some(values) = values {
        let mut counter: i64 = 0;
        for v in values.iter_mut() {
            if let Some(obj) = v.as_object_mut() {
                if let Some(val) = obj.get("value").and_then(|v| v.as_i64()) {
                    counter = val;
                } else {
                    obj.insert("value".to_string(), Value::Number(counter.into()));
                }
                counter += 1;
            }
        }
    }

    enum_def.insert("complete".to_string(), Value::Bool(true));
    enum_def
}

// ─── element_into_bytes ───────────────────────────────────────────────────────

pub fn element_into_bytes(
    element: &Value,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
    size: Option<usize>,
) -> Vec<u8> {
    _element_into_bytes(element, definitions, endianness, size)
}

fn _element_into_bytes(
    element: &Value,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
    size: Option<usize>,
) -> Vec<u8> {
    let obj = element.as_object().expect("element must be a JSON object");
    let element_name = obj.keys().next().expect("element must have at least one key");
    let element_value = &obj[element_name];

    if let Some(definition) = definitions.get(element_name) {
        let def_obj = definition.as_object().unwrap();
        let def_type = def_obj.get("type").and_then(Value::as_str).unwrap_or("");
        match def_type {
            "group" => group_into_bytes(element, definitions, endianness),
            "structure" => structure_into_bytes(element, definitions, endianness),
            "enum" => enum_into_bytes(element, definitions, endianness),
            "bit_field" => bit_field_into_bytes(element, definitions, endianness),
            _ => vec![],
        }
    } else {
        let sz = size.expect("Size must be provided for primitive element");
        primitive_to_bytes(element, endianness, sz)
    }
}

fn group_into_bytes(
    element: &Value,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
) -> Vec<u8> {
    let obj = element.as_object().unwrap();
    let group_name = obj.keys().next().unwrap();
    let group_val = obj[group_name].as_object().unwrap();
    let group_member_name = group_val.keys().next().unwrap();

    let mut b = vec![];

    // Find tag value
    let tag = definitions.iter().find_map(|(k, v)| {
        let vobj = v.as_object()?;
        let groups = vobj.get("groups")?.as_object()?;
        let group_entry = groups.get(group_name)?.as_object()?;
        if k == group_member_name {
            group_entry.get("value")?.as_i64()
        } else {
            None
        }
    }).unwrap_or(0);

    b.push(tag as u8);

    for (name, val) in group_val.iter() {
        let sub = serde_json::json!({ name: val });
        b.extend(element_into_bytes(&sub, definitions, endianness, None));
    }

    b
}

fn structure_into_bytes(
    element: &Value,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
) -> Vec<u8> {
    let obj = element.as_object().unwrap();
    let struct_name = obj.keys().next().unwrap();
    let struct_members = obj[struct_name].as_object().unwrap();
    let struct_def = definitions[struct_name].as_object().unwrap();

    let mut b = vec![];
    let members = struct_def.get("members").and_then(Value::as_array);
    if let Some(members) = members {
        for member_def in members {
            let mobj = member_def.as_object().unwrap();
            let member_name = mobj["name"].as_str().unwrap();
            let member_type = mobj["type"].as_str().unwrap();
            let member_size = mobj["size"].as_u64().unwrap() as usize;
            let member_value = &struct_members[member_name];
            let sub = serde_json::json!({ member_type: member_value });
            b.extend(element_into_bytes(&sub, definitions, endianness, Some(member_size)));
        }
    }
    b
}

fn enum_into_bytes(
    element: &Value,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
) -> Vec<u8> {
    let obj = element.as_object().unwrap();
    let enum_name = obj.keys().next().unwrap();
    let enum_value = obj[enum_name].as_str().unwrap_or("");
    let enum_def = definitions[enum_name].as_object().unwrap();
    let enum_size = enum_def["size"].as_u64().unwrap() as usize;
    let is_signed = enum_def.get("signed").and_then(Value::as_bool).unwrap_or(false);

    let values = enum_def.get("values").and_then(Value::as_array).unwrap();
    let mut counter: i64 = 0;
    for v in values {
        let vobj = v.as_object().unwrap();
        if let Some(val) = vobj.get("value").and_then(|v| v.as_i64()) {
            counter = val;
        }
        if vobj["label"].as_str().unwrap_or("") == enum_value {
            return int_to_bytes(counter, enum_size, endianness, is_signed);
        }
        counter += 1;
    }
    vec![0; enum_size]
}

fn bit_field_into_bytes(
    element: &Value,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
) -> Vec<u8> {
    let obj = element.as_object().unwrap();
    let bf_name = obj.keys().next().unwrap();
    let bf_members = obj[bf_name].as_object().unwrap();
    let bf_def = definitions[bf_name].as_object().unwrap();
    let bf_size = bf_def["size"].as_u64().unwrap() as usize;

    let mut raw_value: u64 = 0;
    let members = bf_def.get("members").and_then(Value::as_array).unwrap();
    for member_def in members {
        let mut mobj = member_def.as_object().unwrap().clone();
        complete_bit_field_member(&mut mobj, bf_size);

        let member_name = mobj["name"].as_str().unwrap();
        let member_type = mobj["type"].as_str().unwrap();
        let bits = mobj["bits"].as_u64().unwrap() as u32;
        let start = mobj["start"].as_u64().unwrap() as u32;

        let member_value = match bf_members.get(member_name) {
            Some(v) => v.clone(),
            None => continue,
        };

        let sub = serde_json::json!({ member_type: &member_value });
        let byte_value = element_into_bytes(&sub, definitions, endianness, Some(bf_size));
        let v = bytes_to_u64(&byte_value, endianness);
        let mask = (1u64 << bits) - 1;
        let v = (v & mask) << start;
        raw_value |= v;
    }

    u64_to_bytes(raw_value, bf_size, endianness)
}

fn primitive_to_bytes(
    element: &Value,
    endianness: Endianness,
    size: usize,
) -> Vec<u8> {
    let obj = element.as_object().unwrap();
    let type_name = obj.keys().next().unwrap().as_str();
    let value = &obj[type_name];

    match type_name {
        "int" => {
            let v = value_to_i64(value);
            int_to_bytes(v, size, endianness, true)
        }
        "uint" => {
            let v = value_to_i64(value);
            int_to_bytes(v, size, endianness, false)
        }
        "bytes" => {
            if let Value::Array(arr) = value {
                arr.iter()
                    .map(|b| b.as_u64().unwrap_or(0) as u8)
                    .collect()
            } else {
                vec![0; size]
            }
        }
        "reserved" => vec![0xff; size],
        "str" => {
            let s = value.as_str().unwrap_or("");
            let mut bytes = s.as_bytes().to_vec();
            bytes.resize(size, 0);
            bytes
        }
        _ => vec![0; size],
    }
}

// ─── parse_bytes ──────────────────────────────────────────────────────────────

pub fn parse_bytes(
    byte_data: &[u8],
    type_name: &str,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
) -> Value {
    _parse_bytes(byte_data, type_name, definitions, endianness)
}

fn _parse_bytes(
    byte_data: &[u8],
    type_name: &str,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
) -> Value {
    if let Some(definition) = definitions.get(type_name) {
        let def_obj = definition.as_object().unwrap();
        let def_type = def_obj.get("type").and_then(Value::as_str).unwrap_or("");
        match def_type {
            "structure" => {
                return parse_struct(byte_data, type_name, definitions, endianness);
            }
            "enum" => {
                return Value::String(parse_enum(byte_data, type_name, definitions, endianness));
            }
            "group" => {
                return parse_group(byte_data, type_name, definitions, endianness);
            }
            "bit_field" => {
                return parse_bit_field(byte_data, type_name, definitions, endianness);
            }
            _ => {}
        }
    }
    Value::String(parse_primitive_to_str(byte_data, type_name, endianness))
}

fn parse_struct(
    byte_data: &[u8],
    struct_name: &str,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
) -> Value {
    let definition = definitions[struct_name].as_object().unwrap();
    let members = definition.get("members").and_then(Value::as_array);
    let mut parsed = serde_json::Map::new();
    let mut offset = 0;

    if let Some(members) = members {
        for member in members {
            let mobj = member.as_object().unwrap();
            let size = mobj["size"].as_u64().unwrap() as usize;
            let name = mobj["name"].as_str().unwrap();
            let type_ = mobj["type"].as_str().unwrap();
            let member_bytes = &byte_data[offset..offset + size];
            offset += size;
            parsed.insert(
                name.to_string(),
                parse_bytes(member_bytes, type_, definitions, endianness),
            );
        }
    }

    Value::Object(parsed)
}

fn parse_enum(
    byte_data: &[u8],
    enum_name: &str,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
) -> String {
    let definition = definitions[enum_name].as_object().unwrap();
    let values = definition.get("values").and_then(Value::as_array).unwrap();
    let is_signed = definition.get("signed").and_then(Value::as_bool).unwrap_or(false);

    let int_value = bytes_to_i64(byte_data, endianness, is_signed);
    let mut counter: i64 = 0;
    for v in values {
        let vobj = v.as_object().unwrap();
        if let Some(val) = vobj.get("value").and_then(|v| v.as_i64()) {
            counter = val;
        }
        if int_value == counter {
            return vobj["label"].as_str().unwrap_or("").to_string();
        }
        counter += 1;
    }
    format!("unknown({})", int_value)
}

fn parse_group(
    byte_data: &[u8],
    group_name: &str,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
) -> Value {
    let definition = definitions[group_name].as_object().unwrap();
    let tag_start = definition.get("offset").and_then(|v| v.as_u64()).unwrap_or(0) as usize;
    let tag_size = definition["size"].as_u64().unwrap() as usize;
    let tag_end = tag_start + tag_size;

    let group_tag_bytes = &byte_data[tag_start..tag_end];
    let group_tag = bytes_to_i64(group_tag_bytes, endianness, false);

    // Reconstruct byte_data without the tag bytes
    let mut remaining = Vec::new();
    remaining.extend_from_slice(&byte_data[..tag_start]);
    remaining.extend_from_slice(&byte_data[tag_end..]);

    for (element_name, element_def) in definitions.iter() {
        let eobj = element_def.as_object().unwrap();
        if let Some(groups) = eobj.get("groups").and_then(Value::as_object) {
            if let Some(group_entry) = groups.get(group_name).and_then(Value::as_object) {
                if group_entry.get("value").and_then(|v| v.as_i64()) == Some(group_tag) {
                    let parsed = parse_bytes(&remaining, element_name, definitions, endianness);
                    return serde_json::json!({ element_name: parsed });
                }
            }
        }
    }

    serde_json::json!({ "unknown": group_tag })
}

fn parse_bit_field(
    byte_data: &[u8],
    bf_name: &str,
    definitions: &serde_json::Map<String, Value>,
    endianness: Endianness,
) -> Value {
    let definition = definitions[bf_name].as_object().unwrap();
    let bf_size = definition["size"].as_u64().unwrap() as usize;
    let members = definition.get("members").and_then(Value::as_array).unwrap();

    let byte_value = bytes_to_u64(byte_data, endianness);
    let mut parsed = serde_json::Map::new();

    for member in members {
        let mut mobj = member.as_object().unwrap().clone();
        complete_bit_field_member(&mut mobj, bf_size);

        let member_name = mobj["name"].as_str().unwrap();
        let member_type = mobj["type"].as_str().unwrap();
        let bits = mobj["bits"].as_u64().unwrap() as u32;
        let start = mobj["start"].as_u64().unwrap() as u32;

        let is_signed = if let Some(member_def) = definitions.get(member_type) {
            member_def.as_object()
                .and_then(|o| o.get("signed"))
                .and_then(Value::as_bool)
                .unwrap_or(false)
        } else {
            member_type == "int"
        };

        let mask = (1u64 << bits) - 1;
        let mut bits_value = (byte_value >> start) & mask;

        if is_signed {
            let msb = 1u64 << (bits - 1);
            if bits_value & msb != 0 {
                bits_value |= !mask;
            }
        }

        let size = ((bits as f64) / 8.0).ceil() as usize;
        let masked_bytes = u64_to_bytes(bits_value, size, endianness);
        parsed.insert(
            member_name.to_string(),
            parse_bytes(&masked_bytes, member_type, definitions, endianness),
        );
    }

    Value::Object(parsed)
}

fn parse_primitive_to_str(
    byte_data: &[u8],
    type_name: &str,
    endianness: Endianness,
) -> String {
    match type_name {
        "int" => bytes_to_i64(byte_data, endianness, true).to_string(),
        "uint" => bytes_to_i64(byte_data, endianness, false).to_string(),
        "bool" => {
            let v = bytes_to_u64(byte_data, endianness);
            if v == 0 {
                "false".to_string()
            } else if v == 1 {
                "true".to_string()
            } else {
                format!("True(ish): 0x{:02X}", v)
            }
        }
        "bytes" | "reserved" => bytes_to_str(byte_data),
        "str" => {
            let s = String::from_utf8_lossy(byte_data);
            s.trim_end_matches('\0').to_string()
        }
        _ => bytes_to_str(byte_data),
    }
}

// ─── Endianness ───────────────────────────────────────────────────────────────

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Endianness {
    Little,
    Big,
}

impl Endianness {
    pub fn from_str(s: &str) -> Self {
        match s.to_lowercase().as_str() {
            "little" => Self::Little,
            _ => Self::Big,
        }
    }
}

// ─── Low-level byte conversion helpers ───────────────────────────────────────

fn bytes_to_u64(data: &[u8], endianness: Endianness) -> u64 {
    match endianness {
        Endianness::Little => {
            let mut result = 0u64;
            for (i, &b) in data.iter().enumerate() {
                result |= (b as u64) << (8 * i);
            }
            result
        }
        Endianness::Big => {
            let mut result = 0u64;
            for &b in data {
                result = (result << 8) | b as u64;
            }
            result
        }
    }
}

fn bytes_to_i64(data: &[u8], endianness: Endianness, signed: bool) -> i64 {
    let u = bytes_to_u64(data, endianness);
    if signed && !data.is_empty() {
        // Sign extend
        let bits = data.len() * 8;
        let sign_bit = 1u64 << (bits - 1);
        if u & sign_bit != 0 {
            // Negative
            let mask = (1u64 << bits) - 1;
            let neg = !u & mask;
            -(neg as i64 + 1)
        } else {
            u as i64
        }
    } else {
        u as i64
    }
}

fn int_to_bytes(value: i64, size: usize, endianness: Endianness, signed: bool) -> Vec<u8> {
    let mask = if size >= 8 { u64::MAX } else { (1u64 << (size * 8)) - 1 };
    let u = if signed && value < 0 {
        ((value as u64) & mask)
    } else {
        (value as u64) & mask
    };
    u64_to_bytes(u, size, endianness)
}

fn u64_to_bytes(value: u64, size: usize, endianness: Endianness) -> Vec<u8> {
    let mut bytes = vec![0u8; size];
    match endianness {
        Endianness::Little => {
            for i in 0..size {
                bytes[i] = ((value >> (8 * i)) & 0xFF) as u8;
            }
        }
        Endianness::Big => {
            for i in 0..size {
                bytes[size - 1 - i] = ((value >> (8 * i)) & 0xFF) as u8;
            }
        }
    }
    bytes
}

fn value_to_i64(v: &Value) -> i64 {
    match v {
        Value::Number(n) => n.as_i64().unwrap_or(0),
        Value::String(s) => s.parse().unwrap_or(0),
        Value::Bool(b) => *b as i64,
        _ => 0,
    }
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn example_definitions() -> serde_json::Map<String, Value> {
        json!({
            "commands": {
                "description": "Debug commands for thermostat",
                "display_name": "Thermostat command",
                "type": "group",
                "size": 1
            },
            "cmd_reset": {
                "description": "Request a software reset",
                "display_name": "reset request",
                "size": 0,
                "type": "structure",
                "groups": {
                    "commands": {"value": 1, "name": "reset"}
                }
            },
            "cmd_temperature_set": {
                "description": "Request a change in temperature",
                "display_name": "Request temperature change",
                "members": [
                    {"name": "temperature", "size": 2, "type": "int", "description": "Desired temperature"},
                    {"name": "units", "size": 1, "type": "temperature_units", "description": "Selected temperature unit"}
                ],
                "size": 3,
                "type": "structure",
                "groups": {
                    "commands": {"value": 2, "name": "temperature_set"}
                }
            },
            "temperature_units": {
                "description": "Units used for temperature",
                "display_name": "Temperature Units",
                "size": 1,
                "type": "enum",
                "values": [
                    {"label": "c", "value": 0, "display_name": "C", "description": "Degrees Celsius"},
                    {"label": "f", "display_name": "F", "description": "Degrees Fahrenheit"}
                ]
            }
        })
        .as_object()
        .unwrap()
        .clone()
    }

    #[test]
    fn test_parse_enum() {
        let defs = example_definitions();
        let data = &[0u8];
        let result = parse_bytes(data, "temperature_units", &defs, Endianness::Big);
        assert_eq!(result, Value::String("c".to_string()));

        let data = &[1u8];
        let result = parse_bytes(data, "temperature_units", &defs, Endianness::Big);
        assert_eq!(result, Value::String("f".to_string()));
    }

    #[test]
    fn test_enum_into_bytes() {
        let defs = example_definitions();
        let elem = json!({"temperature_units": "c"});
        let result = element_into_bytes(&elem, &defs, Endianness::Big, None);
        assert_eq!(result, vec![0]);

        let elem = json!({"temperature_units": "f"});
        let result = element_into_bytes(&elem, &defs, Endianness::Big, None);
        assert_eq!(result, vec![1]);
    }

    #[test]
    fn test_structure_roundtrip() {
        let defs = example_definitions();
        let elem = json!({
            "cmd_temperature_set": {
                "temperature": 0x1234i64,
                "units": "c"
            }
        });
        let bytes = element_into_bytes(&elem, &defs, Endianness::Big, None);
        assert_eq!(bytes.len(), 3);

        let parsed = parse_bytes(&bytes, "cmd_temperature_set", &defs, Endianness::Big);
        let obj = parsed.as_object().unwrap();
        assert_eq!(obj["units"], Value::String("c".to_string()));
    }

    #[test]
    fn test_bytes_to_str() {
        let data = &[0xABu8, 0xCD, 0xEF];
        let s = bytes_to_str(data);
        assert_eq!(s, "AB CD EF (len=3)");
    }

    #[test]
    fn test_complete_bit_field_member() {
        let mut m = json!({
            "name": "a_field",
            "start": 0,
            "bits": 4,
            "type": "uint"
        })
        .as_object()
        .unwrap()
        .clone();
        complete_bit_field_member(&mut m, 1);
        assert_eq!(m["end"].as_u64().unwrap(), 4);
        assert_eq!(m["last"].as_u64().unwrap(), 3);
        assert_eq!(m["bits"].as_u64().unwrap(), 4);
    }
}
