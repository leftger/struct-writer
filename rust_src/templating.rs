use regex::Regex;
use serde_json::Value;
use std::sync::OnceLock;

static TEMPLATE_RE: OnceLock<Regex> = OnceLock::new();

fn template_regex() -> &'static Regex {
    TEMPLATE_RE.get_or_init(|| {
        Regex::new(r"\$(?:(?P<escaped>\$)|\{(?P<braced>.+?)\}|(?P<invalid>))").unwrap()
    })
}

// ─── Arithmetic helper regex ──────────────────────────────────────────────────

static ARITH_RE: OnceLock<Regex> = OnceLock::new();

fn arith_regex() -> &'static Regex {
    ARITH_RE.get_or_init(|| {
        // Matches: `some.path.expr OP number_literal`
        // OP is *, +, -, /
        // Allows optional whitespace around operator
        Regex::new(r"^([A-Za-z_][A-Za-z0-9_.]*)\s*([*+\-/])\s*(\d+(?:\.\d+)?)$").unwrap()
    })
}

// ─── Public API ───────────────────────────────────────────────────────────────

pub struct Template {
    pub template: String,
}

impl Template {
    pub fn new(template: impl Into<String>) -> Self {
        Self { template: template.into() }
    }

    /// Render the template with the given context. Panics on unresolved expressions.
    pub fn render(&self, ctx: &Value) -> String {
        let re = template_regex();
        let mut prev = String::new();
        let mut current = self.template.clone();

        loop {
            let result = re.replace_all(&current, |caps: &regex::Captures| {
                if caps.name("escaped").is_some() {
                    return "$".to_string();
                }
                if let Some(expr) = caps.name("braced") {
                    if let Some(resolved) = resolve_expression(expr.as_str(), ctx) {
                        return resolved;
                    }
                    panic!("Failed to resolve template expression: {}", expr.as_str());
                }
                caps[0].to_string()
            });
            current = result.into_owned();
            if current == prev {
                break;
            }
            prev = current.clone();
            // Another pass for recursive resolution
            let result2 = re.replace_all(&current, |caps: &regex::Captures| {
                if caps.name("escaped").is_some() {
                    return "$".to_string();
                }
                if let Some(expr) = caps.name("braced") {
                    if let Some(resolved) = resolve_expression(expr.as_str(), ctx) {
                        return resolved;
                    }
                    panic!("Failed to resolve template expression: {}", expr.as_str());
                }
                caps[0].to_string()
            });
            let next = result2.into_owned();
            if next == current {
                break;
            }
            current = next;
        }
        current
    }

    /// Like `render`, but leaves unresolvable expressions unchanged instead of panicking.
    pub fn safe_render(&self, ctx: &Value) -> String {
        let re = template_regex();
        let mut prev = String::new();
        let mut current = self.template.clone();

        // Iteratively apply until stable (handles recursive templates)
        loop {
            let result = re.replace_all(&current, |caps: &regex::Captures| {
                if caps.name("escaped").is_some() {
                    return "$".to_string();
                }
                if let Some(expr) = caps.name("braced") {
                    if let Some(resolved) = resolve_expression(expr.as_str(), ctx) {
                        return resolved;
                    }
                    // Leave unchanged
                    return caps[0].to_string();
                }
                caps[0].to_string()
            });
            let next = result.into_owned();
            if next == current {
                break;
            }
            prev = current;
            current = next;
        }
        current
    }
}

// ─── Expression resolver ──────────────────────────────────────────────────────

/// Resolve a template expression like `member.size*8`, `out_file.stem.upper()`,
/// `value.value:#x`, or `bag_count:02X` against the given context.
pub fn resolve_expression(expr: &str, ctx: &Value) -> Option<String> {
    let expr = expr.trim();

    // Split on ':' for format spec. Only consider the first ':' that is not part
    // of a path segment (identifiers won't contain ':').
    let (path_expr, fmt_spec) = split_format_spec(expr);
    let path_expr = path_expr.trim();

    // Try arithmetic first
    let value = if let Some(v) = try_arithmetic(path_expr, ctx) {
        v
    } else {
        // Navigate dot-path (with optional trailing method call)
        let segments: Vec<&str> = path_expr.split('.').collect();
        resolve_path_chain(&segments, ctx)?
    };

    // Apply format spec if present
    if let Some(fmt) = fmt_spec {
        apply_format_spec(&value, fmt.trim())
    } else {
        Some(value_to_string(&value))
    }
}

/// Split `expr` into `(path, Some(format_spec))` if a `:` is found,
/// otherwise `(expr, None)`.
fn split_format_spec(expr: &str) -> (&str, Option<&str>) {
    if let Some(pos) = expr.find(':') {
        (&expr[..pos], Some(&expr[pos + 1..]))
    } else {
        (expr, None)
    }
}

/// Attempt to parse and evaluate arithmetic: `path.to.val OP literal`
fn try_arithmetic(expr: &str, ctx: &Value) -> Option<Value> {
    let re = arith_regex();
    let caps = re.captures(expr)?;

    let path = caps.get(1)?.as_str();
    let op = caps.get(2)?.as_str();
    let literal: f64 = caps.get(3)?.as_str().parse().ok()?;

    let segments: Vec<&str> = path.split('.').collect();
    let val = resolve_path_chain(&segments, ctx)?;
    let num = to_f64(&val)?;

    let result = match op {
        "*" => num * literal,
        "+" => num + literal,
        "-" => num - literal,
        "/" => num / literal,
        _ => return None,
    };

    if result.fract() == 0.0 {
        Some(Value::Number(serde_json::Number::from(result as i64)))
    } else {
        Some(Value::Number(serde_json::Number::from_f64(result)?))
    }
}

/// Navigate a dot-separated path chain through a JSON value.
/// The last segment may be a method call like `upper()` or `lower()`.
fn resolve_path_chain(segments: &[&str], ctx: &Value) -> Option<Value> {
    let mut current = ctx.clone();

    for &seg in segments {
        if seg.ends_with("()") {
            let method = &seg[..seg.len() - 2];
            current = match (method, &current) {
                ("upper", Value::String(s)) => Value::String(s.to_uppercase()),
                ("lower", Value::String(s)) => Value::String(s.to_lowercase()),
                ("strip", Value::String(s)) => Value::String(s.trim().to_string()),
                _ => return None,
            };
        } else {
            current = match current {
                Value::Object(ref map) => map.get(seg)?.clone(),
                _ => return None,
            };
        }
    }

    Some(current)
}

/// Convert a JSON value to its string representation for template output.
fn value_to_string(v: &Value) -> String {
    match v {
        Value::String(s) => s.clone(),
        Value::Number(n) => n.to_string(),
        Value::Bool(b) => b.to_string(),
        Value::Null => String::new(),
        Value::Array(a) => {
            let parts: Vec<String> = a.iter().map(value_to_string).collect();
            parts.join(", ")
        }
        Value::Object(_) => serde_json::to_string(v).unwrap_or_default(),
    }
}

/// Convert a JSON value to f64, if possible.
fn to_f64(v: &Value) -> Option<f64> {
    match v {
        Value::Number(n) => n.as_f64(),
        Value::String(s) => s.parse().ok(),
        _ => None,
    }
}

/// Apply a Python-style format spec to a value.
/// Supported: `#x`, `#X`, `02X`, `04x`, `08x`, etc.
fn apply_format_spec(v: &Value, fmt: &str) -> Option<String> {
    let num = to_f64(v)? as i64;

    // Parse format spec
    // Python format specs for integers: [fill][align][sign][#][0][width][type]
    // We handle the common subset:
    //   #x  →  0x{hex}
    //   #X  →  0x{HEX}  (Python uses 0X for #X, but we use 0x for consistency)
    //   0Nd →  zero-padded decimal
    //   0NX →  zero-padded uppercase hex
    //   0Nx →  zero-padded lowercase hex
    //   Nd  →  decimal width N

    let fmt = fmt.trim();

    // Handle `#x` and `#X`
    if fmt == "#x" {
        return Some(format!("{:#x}", num));
    }
    if fmt == "#X" {
        return Some(format!("{:#X}", num));
    }
    if fmt == "#o" {
        return Some(format!("{:#o}", num));
    }
    if fmt == "#b" {
        return Some(format!("{:#b}", num));
    }

    // Parse fill+width+type patterns like `02X`, `4d`, `08x`
    static PAD_RE: OnceLock<Regex> = OnceLock::new();
    let pad_re = PAD_RE.get_or_init(|| Regex::new(r"^(0?)(\d+)?([dxXobceEfgs]?)$").unwrap());

    if let Some(caps) = pad_re.captures(fmt) {
        let zero_fill = &caps[1] == "0";
        let width: usize = caps.get(2).map(|m| m.as_str().parse().unwrap_or(0)).unwrap_or(0);
        let type_char = caps.get(3).map(|m| m.as_str()).unwrap_or("d");

        return Some(match type_char {
            "x" if zero_fill && width > 0 => format!("{:0>width$x}", num, width = width),
            "X" if zero_fill && width > 0 => format!("{:0>width$X}", num, width = width),
            "x" if width > 0 => format!("{:>width$x}", num, width = width),
            "X" if width > 0 => format!("{:>width$X}", num, width = width),
            "d" | "" if zero_fill && width > 0 => format!("{:0>width$}", num, width = width),
            "d" | "" if width > 0 => format!("{:>width$}", num, width = width),
            "x" => format!("{:x}", num),
            "X" => format!("{:X}", num),
            _ => num.to_string(),
        });
    }

    // Fallback: just return the number as string
    Some(num.to_string())
}

// ─── merge() ─────────────────────────────────────────────────────────────────

/// Deep merge two JSON objects. Values from `b` overwrite `a` unless both are
/// objects, in which case they are merged recursively.
pub fn merge(a: Value, b: Value) -> Value {
    match (a, b) {
        (Value::Object(mut map_a), Value::Object(map_b)) => {
            for (k, vb) in map_b {
                let entry = map_a.entry(k).or_insert(Value::Null);
                let merged = merge(std::mem::replace(entry, Value::Null), vb);
                *entry = merged;
            }
            Value::Object(map_a)
        }
        (_, b) => b,
    }
}

// ─── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_simple_substitution() {
        let t = Template::new("I have ${bag_count} bags of coffee");
        let ctx = json!({"bag_count": 42});
        assert_eq!(t.render(&ctx), "I have 42 bags of coffee");
        assert_eq!(t.safe_render(&ctx), "I have 42 bags of coffee");
    }

    #[test]
    fn test_string_value() {
        let t = Template::new("I have ${bag_count} bags of coffee");
        let ctx = json!({"bag_count": "42"});
        assert_eq!(t.render(&ctx), "I have 42 bags of coffee");
    }

    #[test]
    fn test_hex_format() {
        let t = Template::new("I have 0x${bag_count:02X} bags of coffee");
        let ctx = json!({"bag_count": 42});
        assert_eq!(t.render(&ctx), "I have 0x2A bags of coffee");
    }

    #[test]
    fn test_escaped_dollar() {
        // $$$ means: $$ (escaped dollar → $) followed by ${cost} (template)
        let t = Template::new("I have ${bag_count} bags of coffee for $$${cost} each");
        let ctx = json!({"bag_count": 42, "cost": 12});
        assert_eq!(t.render(&ctx), "I have 42 bags of coffee for $12 each");
    }

    #[test]
    fn test_keyword_args_style() {
        let t = Template::new("${who} likes ${what}");
        let ctx = json!({"who": "tim", "what": "kung pao"});
        assert_eq!(t.render(&ctx), "tim likes kung pao");
    }

    #[test]
    fn test_dict_with_dollar() {
        let t = Template::new("Give ${who} $$200");
        let ctx = json!({"who": "tim"});
        assert_eq!(t.render(&ctx), "Give tim $200");
    }

    #[test]
    fn test_safe_render_leaves_unknown() {
        let t = Template::new("${who} likes ${what}");
        let ctx = json!({"who": "tim"});
        // `what` is not in context, safe_render leaves it
        assert_eq!(t.safe_render(&ctx), "tim likes ${what}");
    }

    #[test]
    fn test_nested_access() {
        let t = Template::new("Hello Mr. ${name.last}, ${name.first}");
        let ctx = json!({"name": {"first": "Charles", "last": "Dickens"}});
        assert_eq!(t.render(&ctx), "Hello Mr. Dickens, Charles");
    }

    #[test]
    fn test_method_call_upper() {
        let t = Template::new("${out_file.stem.upper()}_H_");
        let ctx = json!({"out_file": {"stem": "my_file"}});
        assert_eq!(t.render(&ctx), "MY_FILE_H_");
    }

    #[test]
    fn test_method_call_lower() {
        let t = Template::new("mod ${out_file.stem.lower()} {");
        let ctx = json!({"out_file": {"stem": "MyFile"}});
        assert_eq!(t.render(&ctx), "mod myfile {");
    }

    #[test]
    fn test_arithmetic_multiply() {
        let t = Template::new("int${member.size*8}_t");
        let ctx = json!({"member": {"size": 4}});
        assert_eq!(t.render(&ctx), "int32_t");
    }

    #[test]
    fn test_arithmetic_multiply_spaces() {
        let t = Template::new("u${member.size * 8}");
        let ctx = json!({"member": {"size": 2}});
        assert_eq!(t.render(&ctx), "u16");
    }

    #[test]
    fn test_arithmetic_subtract() {
        let t = Template::new("// ${member.start}...${member.end-1}");
        let ctx = json!({"member": {"start": 0, "end": 4}});
        assert_eq!(t.render(&ctx), "// 0...3");
    }

    #[test]
    fn test_hash_x_format() {
        let t = Template::new("${value.value:#x}");
        let ctx = json!({"value": {"value": 256}});
        assert_eq!(t.render(&ctx), "0x100");
    }

    #[test]
    fn test_recursive_resolution() {
        let ctx = json!({
            "person": {
                "name": {"first": "Charles", "last": "Dickens"},
                "title": "Mr."
            },
            "templates": {
                "full_name": "${person.name.first} ${person.name.last}",
                "last_first": "${person.name.last}, ${person.name.first}"
            }
        });
        let t = Template::new("Hello, ${person.title} ${templates.last_first}");
        assert_eq!(t.render(&ctx), "Hello, Mr. Dickens, Charles");
    }

    #[test]
    fn test_merge_simple() {
        let a = json!({"x": 1, "y": 2});
        let b = json!({"y": 3, "z": 4});
        let result = merge(a, b);
        assert_eq!(result, json!({"x": 1, "y": 3, "z": 4}));
    }

    #[test]
    fn test_merge_nested() {
        let a = json!({"name": {"first": "A", "last": "B"}});
        let b = json!({"name": {"last": "C"}});
        let result = merge(a, b);
        assert_eq!(result, json!({"name": {"first": "A", "last": "C"}}));
    }
}
