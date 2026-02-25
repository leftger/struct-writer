pub mod c;
pub mod rust_lang;
pub mod scala;

use serde_json::Value;

/// Convert a toml::Value to serde_json::Value
pub fn toml_to_json(v: toml::Value) -> Value {
    match v {
        toml::Value::String(s) => Value::String(s),
        toml::Value::Integer(i) => Value::Number(i.into()),
        toml::Value::Float(f) => {
            serde_json::Number::from_f64(f)
                .map(Value::Number)
                .unwrap_or(Value::Null)
        }
        toml::Value::Boolean(b) => Value::Bool(b),
        toml::Value::Array(a) => Value::Array(a.into_iter().map(toml_to_json).collect()),
        toml::Value::Table(t) => {
            let map = t.into_iter().map(|(k, v)| (k, toml_to_json(v))).collect();
            Value::Object(map)
        }
        toml::Value::Datetime(d) => Value::String(d.to_string()),
    }
}

/// Parse a TOML string into a serde_json::Value
pub fn parse_toml_template(toml_str: &str) -> Value {
    let t: toml::Value = toml::from_str(toml_str).expect("Invalid default template TOML");
    toml_to_json(t)
}
