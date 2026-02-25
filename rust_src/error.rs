use thiserror::Error;

#[derive(Error, Debug)]
pub enum ParseError {
    #[error("Expected {name} to be size {expected}, but measured a size of {measured} bytes")]
    SizeMismatch {
        name: String,
        expected: usize,
        measured: f64,
    },

    #[error("Invalid type (`{def_type}`) given for {name}")]
    InvalidType { name: String, def_type: String },

    /// Note: `type_name` is the definition class name (e.g. "MissingType", "Structure"),
    /// `item_name` is the key/item name (e.g. "empty_struct").
    /// Message: "`{type_name}` definition for `{item_name}` missing `{field}`"
    #[error("`{type_name}` definition for `{item_name}` missing `{field}`")]
    RequiredFieldMissing {
        type_name: String,
        item_name: String,
        field: String,
    },
}

impl ParseError {
    pub fn size_mismatch(name: impl Into<String>, expected: usize, measured: f64) -> Self {
        Self::SizeMismatch {
            name: name.into(),
            expected,
            measured,
        }
    }

    pub fn invalid_type(name: impl Into<String>, def_type: impl Into<String>) -> Self {
        Self::InvalidType {
            name: name.into(),
            def_type: def_type.into(),
        }
    }

    /// Create a RequiredFieldMissing error.
    /// - `type_name`: the definition class (e.g. "Structure", "MissingType")
    /// - `item_name`: the key/item name (e.g. "empty_struct")
    /// - `field`: the missing field name
    pub fn required_field(
        type_name: impl Into<String>,
        item_name: impl Into<String>,
        field: impl Into<String>,
    ) -> Self {
        Self::RequiredFieldMissing {
            type_name: type_name.into(),
            item_name: item_name.into(),
            field: field.into(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_size_mismatch_message() {
        let e = ParseError::size_mismatch("simple_struct", 2, 1.0);
        assert_eq!(
            e.to_string(),
            "Expected simple_struct to be size 2, but measured a size of 1 bytes"
        );
    }

    #[test]
    fn test_invalid_type_message() {
        let e = ParseError::invalid_type("empty_struct", "ssstructure");
        assert_eq!(e.to_string(), "Invalid type (`ssstructure`) given for empty_struct");
    }

    #[test]
    fn test_required_field_missing_message() {
        let e = ParseError::required_field("MissingType", "empty_struct", "type");
        assert_eq!(
            e.to_string(),
            "`MissingType` definition for `empty_struct` missing `type`"
        );
    }

    #[test]
    fn test_required_field_structure() {
        let e = ParseError::required_field("Structure", "empty_struct", "size");
        assert_eq!(
            e.to_string(),
            "`Structure` definition for `empty_struct` missing `size`"
        );
    }
}
