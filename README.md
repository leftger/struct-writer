# Structured API

Define a bare structured API, useful for interfaces that don't need to be
portable across multiple platforms.  E.G. embedded applications targeting a
single architecture.

## Usage

### Basic Usage

Define the API in TOML or another markup language.

```toml
[file]
brief = 'Command set for a thermostat'
description = 'Provides basic debug commands for a thermostat.  Allows for both imperial and metric units.'

[commands]
description = "Debug commands for thermostat"
display_name = "Thermostat command"
type = "group"

[cmd_reset]
description = "Request a software reset"
display_name = "reset request"
size = 0
type = "structure"
groups.commands = { value = 1, name = "reset" }

[cmd_temperature_set]
description = "Request a change in temperature"
display_name = "Request temperature change"
members = [
  { name = "temperature", size = 2, type = 'int', description = 'Desired temperature' },
  { name = "units", size = 1, type = 'temperature_units', description = 'Selected temperature unit' },
]
size = 3
type = "structure"
groups.commands = { value = 2, name = "temperature_set" }
```

This will generate code that looks like this for c by default
```c
/**
 * @file
 * @brief Command set for a thermostat
 *
 * Provides basic debug commands for a thermostat.  Allows for both imperial and
 * metric units.
 *
 * @note This file is autogenerated using structured_api
 */
#ifndef STRUCTURES_H_
#define STRUCTURES_H_
#ifdef __cplusplus
extern "C" {
#endif

#include <static_assert.h>
#include <stdint.h>

/// commands tag
/// Enumeration for commands tag
typedef enum commands_tag_e {
    /// @see cmd_reset_t
    commands_tag_reset = 0x1,
    /// @see cmd_temperature_set_t
    commands_tag_temperature_set = 0x2,
} commands_tag_t;
STATIC_ASSERT_TYPE_SIZE(commands_tag_t, 1);

/// reset request
/// Request a software reset
typedef struct cmd_reset_s {
    /// Structure is intentionally empty (zero sized)
    uint8_t empty[0];
} cmd_reset_t;
STATIC_ASSERT_TYPE_SIZE(cmd_reset_t, 0);

/// Request temperature change
/// Request a change in temperature
typedef struct cmd_temperature_set_s {
    /// Desired temperature
    int16_t temperature;
    /// Selected temperature unit
    temperature_units_t units;
} cmd_temperature_set_t;
STATIC_ASSERT_TYPE_SIZE(cmd_temperature_set_t, 3);

/// Thermostat command
/// Debug commands for thermostat
typedef struct commands_u_s {
    /// commands tag
    commands_tag_t tag;
    union {
        /// Request a software reset
        cmd_reset_t reset;
        /// Request a change in temperature
        cmd_temperature_set_t temperature_set;
    } value;
} commands_u_t;
STATIC_ASSERT_TYPE_SIZE(commands_u_t, 4);

#ifdef __cplusplus
}
#endif
#endif // STRUCTURES_H_

```

An example is given in the examples folder:
```bash
struct-writer --input-definition examples/structures.toml --output-file examples/output/structures.h
```

### Customizable templates
Code generation is customizable using templates.  If no templates are provided, we use the default C template.

The default template is shown in the examples folder.

```bash
struct-writer --input-definition examples/structures.toml --output-file examples/output/structures.h --template-file examples/template.toml
```

### Multiple templates

Chain templates together to more tightly specify certain files in a project.  For example if a certain file needs specific includes.

```bash
struct-writer --input-definition examples/structures.toml --output-file examples/output/structures.h --template-file examples/template.toml --template-file examples/packed_templates.toml
```

## development
```
watchexec.exe --clear --restart  --debounce 500 --exts py,toml "isort . && black . && pytest && struct-writer --input-definition examples/structures.toml --template-files examples/template.toml --output-file examples/output/structures.h && pylint ."
```
