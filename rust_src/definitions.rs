use indexmap::IndexMap;
use serde_json::Value;
use std::collections::HashMap;

use crate::error::ParseError;

// ─── File Description ────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct FileDescription {
    pub brief: String,
    pub description: String,
}

impl FileDescription {
    pub fn empty() -> Self {
        Self {
            brief: String::new(),
            description: String::new(),
        }
    }

    pub fn from_dict(d: &serde_json::Map<String, Value>) -> Self {
        Self {
            brief: d.get("brief").and_then(Value::as_str).unwrap_or("").to_string(),
            description: d.get("description").and_then(Value::as_str).unwrap_or("").to_string(),
        }
    }

    pub fn to_json(&self) -> Value {
        serde_json::json!({
            "brief": self.brief,
            "description": self.description,
        })
    }
}

// ─── Structure ───────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct StructureMember {
    pub description: String,
    pub name: String,
    pub size: usize,
    pub type_: String,
}

impl StructureMember {
    pub fn from_dict(
        d: &serde_json::Map<String, Value>,
        parent_name: &str,
    ) -> Result<Self, ParseError> {
        let name = d
            .get("name")
            .and_then(Value::as_str)
            .ok_or_else(|| ParseError::required_field("StructureMember", parent_name, "name"))?
            .to_string();
        let size = d
            .get("size")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| {
                ParseError::required_field(
                    "StructureMember",
                    format!("{}::{}", parent_name, &name),
                    "size",
                )
            })? as usize;
        let type_ = d
            .get("type")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                ParseError::required_field(
                    "StructureMember",
                    format!("{}::{}", parent_name, &name),
                    "type",
                )
            })?
            .to_string();
        let description =
            d.get("description").and_then(Value::as_str).unwrap_or("").to_string();
        Ok(Self { description, name, size, type_ })
    }

    pub fn to_json(&self) -> Value {
        serde_json::json!({
            "description": self.description,
            "name": self.name,
            "size": self.size,
            "type": self.type_,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Structure {
    pub description: String,
    pub display_name: String,
    pub name: String,
    pub size: usize,
    pub members: Vec<StructureMember>,
}

impl Structure {
    pub fn from_named_dict(
        name: &str,
        d: &serde_json::Map<String, Value>,
    ) -> Result<Self, ParseError> {
        let members_raw = d.get("members").and_then(Value::as_array).cloned().unwrap_or_default();
        let mut members = Vec::new();
        for m in &members_raw {
            let mobj = m.as_object().ok_or_else(|| {
                ParseError::required_field("StructureMember", name, "name")
            })?;
            let member = StructureMember::from_dict(mobj, name).map_err(|e| {
                // Re-wrap with the parent::child format
                if let ParseError::RequiredFieldMissing { type_name, item_name, field } = &e {
                    ParseError::required_field(
                        type_name.clone(),
                        format!("{}::{}", name, item_name),
                        field.clone(),
                    )
                } else {
                    e
                }
            })?;
            members.push(member);
        }

        let measured_size: usize = members.iter().map(|m| m.size).sum();
        let size = d
            .get("size")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| ParseError::required_field("Structure", name, "size"))?
            as usize;
        let display_name = d
            .get("display_name")
            .and_then(Value::as_str)
            .ok_or_else(|| ParseError::required_field("Structure", name, "display_name"))?
            .to_string();
        let description =
            d.get("description").and_then(Value::as_str).unwrap_or("").to_string();

        if measured_size != size {
            return Err(ParseError::size_mismatch(name, size, measured_size as f64));
        }

        Ok(Self { description, display_name, name: name.to_string(), size, members })
    }

    pub fn to_json(&self) -> Value {
        serde_json::json!({
            "name": self.name,
            "description": self.description,
            "display_name": self.display_name,
            "size": self.size,
            "type": "structure",
            "members": self.members.iter().map(|m| m.to_json()).collect::<Vec<_>>(),
        })
    }
}

// ─── Enumeration ─────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct EnumValue {
    pub label: String,
    pub value: i64,
    pub display_name: String,
    pub description: String,
}

impl EnumValue {
    pub fn from_dict(
        d: &serde_json::Map<String, Value>,
        parent_name: &str,
        default_val: i64,
    ) -> Result<Self, ParseError> {
        let label = d
            .get("label")
            .and_then(Value::as_str)
            .ok_or_else(|| ParseError::required_field("EnumValue", parent_name, "label"))?
            .to_string();
        let value = d
            .get("value")
            .and_then(|v| v.as_i64())
            .unwrap_or(default_val);
        let display_name = d
            .get("display_name")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                ParseError::required_field("EnumValue", format!("{}::{}", parent_name, &label), "display_name")
            })?
            .to_string();
        let description =
            d.get("description").and_then(Value::as_str).unwrap_or("").to_string();
        Ok(Self { label, value, display_name, description })
    }

    pub fn to_json(&self) -> Value {
        serde_json::json!({
            "label": self.label,
            "value": self.value,
            "display_name": self.display_name,
            "description": self.description,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Enumeration {
    pub description: String,
    pub display_name: String,
    pub name: String,
    pub size: usize,
    pub values: Vec<EnumValue>,
}

impl Enumeration {
    pub fn from_named_dict(
        name: &str,
        d: &serde_json::Map<String, Value>,
    ) -> Result<Self, ParseError> {
        let values_raw = d.get("values").and_then(Value::as_array).cloned().unwrap_or_default();
        let mut values = Vec::new();
        let mut next_int: i64 = 0;
        for v in &values_raw {
            let vobj = v.as_object().ok_or_else(|| {
                ParseError::required_field("EnumValue", name, "label")
            })?;
            let ev = EnumValue::from_dict(vobj, name, next_int).map_err(|e| {
                if let ParseError::RequiredFieldMissing { type_name, item_name, field } = &e {
                    ParseError::required_field(
                        type_name.clone(),
                        format!("{}::{}", name, item_name),
                        field.clone(),
                    )
                } else {
                    e
                }
            })?;
            next_int = ev.value + 1;
            values.push(ev);
        }

        let bits = enum_bits(&values.iter().map(|v| v.value).collect::<Vec<_>>());
        let measured_size = (bits as f64 / 8.0).ceil() as usize;

        let size = d
            .get("size")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| ParseError::required_field("Enumeration", name, "size"))?
            as usize;
        let display_name = d
            .get("display_name")
            .and_then(Value::as_str)
            .ok_or_else(|| ParseError::required_field("Enumeration", name, "display_name"))?
            .to_string();
        let description =
            d.get("description").and_then(Value::as_str).unwrap_or("").to_string();

        if measured_size > size {
            return Err(ParseError::size_mismatch(name, size, measured_size as f64));
        }

        Ok(Self { description, display_name, name: name.to_string(), size, values })
    }

    pub fn to_json(&self) -> Value {
        serde_json::json!({
            "name": self.name,
            "description": self.description,
            "display_name": self.display_name,
            "size": self.size,
            "type": "enum",
            "values": self.values.iter().map(|v| v.to_json()).collect::<Vec<_>>(),
        })
    }
}

/// Compute the minimum number of bits required to represent all values.
pub fn enum_bits(values: &[i64]) -> u32 {
    if values.is_empty() {
        return 1;
    }
    let min_value = *values.iter().min().unwrap();
    let max_value = *values.iter().max().unwrap();

    if min_value == 0 && max_value == 0 {
        return 1;
    }

    let is_signed = min_value < 0;
    if !is_signed {
        if max_value == 0 {
            return 1;
        }
        ((max_value + 1) as f64).log2().ceil() as u32
    } else {
        let max_abs = std::cmp::max(min_value.unsigned_abs(), (max_value + 1).unsigned_abs());
        if max_abs == 0 {
            return 1;
        }
        (max_abs as f64).log2().ceil() as u32 + 1
    }
}

// ─── BitField ────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct BitFieldMember {
    pub name: String,
    pub start: u32,
    pub end: u32,
    pub bits: u32,
    pub type_: String,
    pub description: String,
}

impl BitFieldMember {
    pub fn from_dict(
        d: &serde_json::Map<String, Value>,
        parent_name: &str,
    ) -> Result<Self, ParseError> {
        let name = d
            .get("name")
            .and_then(Value::as_str)
            .ok_or_else(|| ParseError::required_field("BitFieldMember", parent_name, "name"))?
            .to_string();
        let start = d
            .get("start")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| {
                ParseError::required_field(
                    "BitFieldMember",
                    format!("{}::{}", parent_name, &name),
                    "start",
                )
            })? as u32;
        let type_ = d
            .get("type")
            .and_then(Value::as_str)
            .ok_or_else(|| {
                ParseError::required_field(
                    "BitFieldMember",
                    format!("{}::{}", parent_name, &name),
                    "type",
                )
            })?
            .to_string();
        let description =
            d.get("description").and_then(Value::as_str).unwrap_or("").to_string();

        // Determine end and bits, handling partial definitions
        let (end, bits) = match (d.get("end").and_then(|v| v.as_u64()), d.get("bits").and_then(|v| v.as_u64())) {
            (Some(e), Some(b)) => (e as u32, b as u32),
            (Some(e), None) => (e as u32, e as u32 - start),
            (None, Some(b)) => (start + b as u32, b as u32),
            (None, None) => (start + 1, 1),
        };

        Ok(Self { name, start, end, bits, type_, description })
    }

    pub fn reserved(start: u32, end: u32) -> Self {
        let bits = end - start;
        Self {
            name: format!("reserved_{}", start),
            start,
            end,
            bits,
            type_: "reserved".to_string(),
            description: "Reserved".to_string(),
        }
    }

    pub fn to_json(&self) -> Value {
        let size = ((self.bits as f64) / 8.0).ceil() as u64;
        serde_json::json!({
            "name": self.name,
            "start": self.start,
            "end": self.end,
            "bits": self.bits,
            "size": size,
            "type": self.type_,
            "description": self.description,
        })
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct BitField {
    pub description: String,
    pub display_name: String,
    pub name: String,
    pub size: usize,
    pub members: Vec<BitFieldMember>,
}

impl BitField {
    pub fn from_named_dict(
        name: &str,
        d: &serde_json::Map<String, Value>,
    ) -> Result<Self, ParseError> {
        let size = d
            .get("size")
            .and_then(|v| v.as_u64())
            .ok_or_else(|| ParseError::required_field("BitField", name, "size"))?
            as usize;
        let display_name = d
            .get("display_name")
            .and_then(Value::as_str)
            .ok_or_else(|| ParseError::required_field("BitField", name, "display_name"))?
            .to_string();
        let description =
            d.get("description").and_then(Value::as_str).unwrap_or("").to_string();

        let members_raw = d.get("members").and_then(Value::as_array).cloned().unwrap_or_default();
        let mut members: Vec<BitFieldMember> = Vec::new();
        let mut start_bit: u32 = 0;

        for m in &members_raw {
            let mobj = m.as_object().ok_or_else(|| {
                ParseError::required_field("BitFieldMember", name, "name")
            })?;
            // Check for gap and insert reserved member
            let m_start = mobj.get("start").and_then(|v| v.as_u64()).unwrap_or(start_bit as u64) as u32;
            if start_bit < m_start {
                members.push(BitFieldMember::reserved(start_bit, m_start));
            }
            let member = BitFieldMember::from_dict(mobj, name).map_err(|e| {
                if let ParseError::RequiredFieldMissing { type_name, item_name, field } = &e {
                    ParseError::required_field(
                        type_name.clone(),
                        format!("{}::{}", name, item_name),
                        field.clone(),
                    )
                } else {
                    e
                }
            })?;
            start_bit = member.end;
            members.push(member);
        }

        let expected_bits = size as u32 * 8;
        let measured_bits: u32 = members.iter().map(|m| m.bits).sum();

        if measured_bits > expected_bits {
            let measured = (measured_bits as f64 / 8.0).ceil();
            return Err(ParseError::size_mismatch(name, size, measured));
        }

        // Pad remaining bits with reserved
        if measured_bits < expected_bits {
            let remaining = expected_bits - measured_bits;
            members.push(BitFieldMember::reserved(start_bit, start_bit + remaining));
        }

        Ok(Self { description, display_name, name: name.to_string(), size, members })
    }

    pub fn to_json(&self) -> Value {
        serde_json::json!({
            "name": self.name,
            "description": self.description,
            "display_name": self.display_name,
            "size": self.size,
            "type": "bit_field",
            "members": self.members.iter().map(|m| m.to_json()).collect::<Vec<_>>(),
        })
    }
}

// ─── Group ───────────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct GroupMember {
    pub name: String,
    pub type_: String,
    pub value: i64,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Group {
    pub description: String,
    pub display_name: String,
    pub name: String,
    pub size: usize,
    pub members: Vec<GroupMember>,
}

impl Group {
    pub fn from_definitions(
        group_name: &str,
        definitions: &serde_json::Map<String, Value>,
    ) -> Self {
        let group_definition = definitions.get(group_name).and_then(Value::as_object).unwrap();
        let display_name = group_definition
            .get("display_name")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_string();
        let description = group_definition
            .get("description")
            .and_then(Value::as_str)
            .unwrap_or("")
            .to_string();
        let size = group_definition
            .get("size")
            .and_then(|v| v.as_u64())
            .unwrap_or(0) as usize;

        let mut members: Vec<GroupMember> = definitions
            .iter()
            .filter_map(|(k, v)| {
                let groups = v.as_object()?.get("groups")?.as_object()?;
                let group_entry = groups.get(group_name)?.as_object()?;
                let member_name = group_entry.get("name")?.as_str()?.to_string();
                let member_value = group_entry.get("value")?.as_i64().unwrap_or(0);
                Some(GroupMember {
                    name: member_name,
                    type_: k.clone(),
                    value: member_value,
                })
            })
            .collect();

        // Sort by value for deterministic output
        members.sort_by_key(|m| m.value);

        Self {
            description,
            display_name,
            name: group_name.to_string(),
            size,
            members,
        }
    }

    pub fn to_json(&self) -> Value {
        serde_json::json!({
            "name": self.name,
            "description": self.description,
            "display_name": self.display_name,
            "size": self.size,
            "type": "group",
        })
    }
}

// ─── DefinedType ─────────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub enum DefinedType {
    Structure(Structure),
    Enumeration(Enumeration),
    BitField(BitField),
    Group(Group),
}

impl DefinedType {
    pub fn as_structure(&self) -> Option<&Structure> {
        if let Self::Structure(s) = self { Some(s) } else { None }
    }

    pub fn as_enumeration(&self) -> Option<&Enumeration> {
        if let Self::Enumeration(e) = self { Some(e) } else { None }
    }

    pub fn as_bit_field(&self) -> Option<&BitField> {
        if let Self::BitField(b) = self { Some(b) } else { None }
    }

    pub fn as_group(&self) -> Option<&Group> {
        if let Self::Group(g) = self { Some(g) } else { None }
    }

    pub fn to_json(&self) -> Value {
        match self {
            Self::Structure(s) => s.to_json(),
            Self::Enumeration(e) => e.to_json(),
            Self::BitField(b) => b.to_json(),
            Self::Group(g) => g.to_json(),
        }
    }

    pub fn size(&self) -> usize {
        match self {
            Self::Structure(s) => s.size,
            Self::Enumeration(e) => e.size,
            Self::BitField(b) => b.size,
            Self::Group(g) => g.size,
        }
    }

    pub fn description(&self) -> &str {
        match self {
            Self::Structure(s) => &s.description,
            Self::Enumeration(e) => &e.description,
            Self::BitField(b) => &b.description,
            Self::Group(g) => &g.description,
        }
    }

    pub fn display_name(&self) -> &str {
        match self {
            Self::Structure(s) => &s.display_name,
            Self::Enumeration(e) => &e.display_name,
            Self::BitField(b) => &b.display_name,
            Self::Group(g) => &g.display_name,
        }
    }
}

// ─── TypeDefinitions ─────────────────────────────────────────────────────────

#[derive(Debug, Clone, PartialEq)]
pub struct TypeDefinitions {
    pub file_info: FileDescription,
    pub definitions: IndexMap<String, DefinedType>,
}

impl TypeDefinitions {
    pub fn from_dict(mut raw: serde_json::Map<String, Value>) -> Result<Self, ParseError> {
        let file_info = if let Some(file_val) = raw.remove("file") {
            if let Some(file_obj) = file_val.as_object() {
                FileDescription::from_dict(file_obj)
            } else {
                FileDescription::empty()
            }
        } else {
            FileDescription::empty()
        };

        let mut result: IndexMap<String, DefinedType> = IndexMap::new();

        for (k, v) in &raw {
            let obj = v.as_object().ok_or_else(|| {
                ParseError::required_field("MissingType", k.as_str(), "type")
            })?;
            let def_type = obj
                .get("type")
                .and_then(Value::as_str)
                .ok_or_else(|| ParseError::required_field("MissingType", k.as_str(), "type"))?;

            let defined = match def_type {
                "structure" => DefinedType::Structure(Structure::from_named_dict(k, obj)?),
                "enum" => DefinedType::Enumeration(Enumeration::from_named_dict(k, obj)?),
                "bit_field" => DefinedType::BitField(BitField::from_named_dict(k, obj)?),
                "group" => DefinedType::Group(Group::from_definitions(k, &raw)),
                other => {
                    return Err(ParseError::invalid_type(k.as_str(), other));
                }
            };
            result.insert(k.clone(), defined);
        }

        Ok(Self { file_info, definitions: result })
    }
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn empty_struct_dict() -> serde_json::Map<String, Value> {
        json!({
            "empty_struct": {
                "description": "An empty structure",
                "display_name": "Empty Structure",
                "type": "structure",
                "size": 0
            }
        })
        .as_object()
        .unwrap()
        .clone()
    }

    #[test]
    fn test_parse_empty_structure() {
        let result = TypeDefinitions::from_dict(empty_struct_dict()).unwrap();
        assert_eq!(result.file_info, FileDescription::empty());
        assert!(matches!(
            result.definitions.get("empty_struct"),
            Some(DefinedType::Structure(_))
        ));
        let s = result.definitions["empty_struct"].as_structure().unwrap();
        assert_eq!(s.name, "empty_struct");
        assert_eq!(s.size, 0);
        assert!(s.members.is_empty());
    }

    #[test]
    fn test_parse_simple_structure() {
        let dict = json!({
            "simple_struct": {
                "description": "A simple structure",
                "display_name": "Simple Structure",
                "type": "structure",
                "size": 1,
                "members": [
                    {"name": "number", "size": 1, "type": "int", "description": "A small number"}
                ]
            }
        });
        let result = TypeDefinitions::from_dict(dict.as_object().unwrap().clone()).unwrap();
        let s = result.definitions["simple_struct"].as_structure().unwrap();
        assert_eq!(s.size, 1);
        assert_eq!(s.members.len(), 1);
        assert_eq!(s.members[0].name, "number");
        assert_eq!(s.members[0].size, 1);
        assert_eq!(s.members[0].type_, "int");
    }

    #[test]
    fn test_parse_enumeration() {
        let dict = json!({
            "an_enum": {
                "description": "an example enum",
                "display_name": "An Enum",
                "type": "enum",
                "size": 1,
                "values": [
                    {"label": "a", "value": 0, "display_name": "A", "description": "The letter A"},
                    {"label": "b", "value": 1, "display_name": "B", "description": "The letter B"},
                ]
            }
        });
        let result = TypeDefinitions::from_dict(dict.as_object().unwrap().clone()).unwrap();
        let e = result.definitions["an_enum"].as_enumeration().unwrap();
        assert_eq!(e.size, 1);
        assert_eq!(e.values.len(), 2);
        assert_eq!(e.values[0].label, "a");
        assert_eq!(e.values[0].value, 0);
    }

    #[test]
    fn test_parse_bit_field() {
        let dict = json!({
            "a_bit_field": {
                "description": "An example bit field",
                "display_name": "A Bit Field",
                "type": "bit_field",
                "size": 1,
                "members": [
                    {"name": "a_number", "start": 0, "end": 4, "bits": 4, "type": "uint", "description": "A 4 bit number"},
                    {"name": "reserved_4", "start": 4, "end": 8, "bits": 4, "type": "reserved", "description": "Unused bits"},
                ]
            }
        });
        let result = TypeDefinitions::from_dict(dict.as_object().unwrap().clone()).unwrap();
        let b = result.definitions["a_bit_field"].as_bit_field().unwrap();
        assert_eq!(b.size, 1);
        assert_eq!(b.members.len(), 2);
        assert_eq!(b.members[0].name, "a_number");
        assert_eq!(b.members[0].bits, 4);
    }

    #[test]
    fn test_size_mismatch_structure() {
        let dict = json!({
            "simple_struct": {
                "description": "A simple structure",
                "display_name": "Simple Structure",
                "type": "structure",
                "size": 2,
                "members": [
                    {"name": "number", "size": 1, "type": "int", "description": "A small number"}
                ]
            }
        });
        let result = TypeDefinitions::from_dict(dict.as_object().unwrap().clone());
        assert!(matches!(result, Err(ParseError::SizeMismatch { .. })));
    }

    #[test]
    fn test_invalid_type() {
        let dict = json!({
            "empty_struct": {
                "description": "An empty structure",
                "display_name": "Empty Structure",
                "type": "ssstructure",
                "size": 0
            }
        });
        let result = TypeDefinitions::from_dict(dict.as_object().unwrap().clone());
        assert!(matches!(result, Err(ParseError::InvalidType { .. })));
        if let Err(e) = result {
            assert_eq!(e.to_string(), "Invalid type (`ssstructure`) given for empty_struct");
        }
    }

    #[test]
    fn test_missing_type_field() {
        let dict = json!({
            "empty_struct": {
                "description": "An empty structure",
                "display_name": "Empty Structure",
                "size": 0
            }
        });
        let result = TypeDefinitions::from_dict(dict.as_object().unwrap().clone());
        assert!(matches!(result, Err(ParseError::RequiredFieldMissing { .. })));
        if let Err(e) = result {
            assert_eq!(
                e.to_string(),
                "`MissingType` definition for `empty_struct` missing `type`"
            );
        }
    }

    #[test]
    fn test_group_parsing() {
        let dict = json!({
            "small_group": {
                "description": "A Small Example Group",
                "display_name": "Small Group",
                "type": "group",
                "size": 1
            },
            "simple_struct": {
                "description": "A simple structure",
                "display_name": "Simple Structure",
                "type": "structure",
                "size": 1,
                "members": [
                    {"name": "number", "size": 1, "type": "int", "description": "A small number"}
                ],
                "groups": {
                    "small_group": {"value": 1, "name": "simple"}
                }
            }
        });
        let result = TypeDefinitions::from_dict(dict.as_object().unwrap().clone()).unwrap();
        let g = result.definitions["small_group"].as_group().unwrap();
        assert_eq!(g.name, "small_group");
        assert_eq!(g.members.len(), 1);
        assert_eq!(g.members[0].name, "simple");
        assert_eq!(g.members[0].type_, "simple_struct");
        assert_eq!(g.members[0].value, 1);
    }

    #[test]
    fn test_enum_bits() {
        assert_eq!(enum_bits(&[0, 0]), 1);
        assert_eq!(enum_bits(&[0, 255]), 8);
        assert_eq!(enum_bits(&[0, 256]), 9);
        assert_eq!(enum_bits(&[-1, 127]), 8);
        assert_eq!(enum_bits(&[-128, 127]), 8);
        // Python: enum_bits([-129, 127]) = 9
        // max(abs(-129), abs(127+1)) = max(129, 128) = 129 → ceil(log2(129)) + 1 = 8 + 1 = 9
        assert_eq!(enum_bits(&[-129, 127]), 9);
        // Python: enum_bits([-128, 128]) = 9
        // max(abs(-128), abs(128+1)) = max(128, 129) = 129 → ceil(log2(129)) + 1 = 8 + 1 = 9
        assert_eq!(enum_bits(&[-128, 128]), 9);
    }
}
