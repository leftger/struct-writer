import pytest

from struct_writer import struct_parse


def example_definitions():
    return {
        "file": {
            "brief": "Command set for a thermostat",
            "description": "Provides basic debug commands for a thermostat.  Allows for both imperial and metric units.",
        },
        "commands": {
            "description": "Debug commands for thermostat",
            "display_name": "Thermostat command",
            "type": "group",
        },
        "cmd_reset": {
            "description": "Request a software reset",
            "display_name": "reset request",
            "size": 0,
            "type": "structure",
            "groups": {
                "commands": {
                    "value": 1,
                    "name": "reset",
                },
            },
        },
        "cmd_temperature_set": {
            "description": "Request a change in temperature",
            "display_name": "Request temperature change",
            "members": [
                {
                    "name": "temperature",
                    "size": 2,
                    "type": "int",
                    "description": "Desired temperature",
                },
                {
                    "name": "units",
                    "size": 1,
                    "type": "temperature_units",
                    "description": "Selected temperature unit",
                },
            ],
            "size": 3,
            "type": "structure",
            "groups": {
                "commands": {
                    "value": 2,
                    "name": "temperature_set",
                },
            },
        },
        "cmd_label_thermostat": {
            "description": "Give the thermostat a name",
            "display_name": "Label thermostat",
            "members": [
                {
                    "name": "label",
                    "size": 20,
                    "type": "str",
                    "description": "Name for the thermostat",
                },
            ],
            "size": 20,
            "type": "structure",
            "groups": {
                "commands": {
                    "value": 3,
                    "name": "label",
                },
            },
        },
        "temperature_units": {
            "description": "Units used for temperature",
            "display_name": "Temperature Units",
            "size": 1,
            "type": "enum",
            "values": [
                {
                    "label": "c",
                    "value": 0,
                    "display_name": "C",
                    "description": "Degrees Celcius",
                },
                {
                    "label": "f",
                    "display_name": "F",
                    "description": "Degrees Fahrenheit",
                },
            ],
        },
    }


struct_into_bytes_params = [
    ({"commands": {"cmd_reset": {}}}, (b"\x01")),
    (
        {
            "commands": {
                "cmd_temperature_set": {
                    "temperature": 75,
                    "units": "f",
                }
            }
        },
        (
            b"\x02"
            + int(75).to_bytes(length=2, byteorder="big", signed=True)
            + b"\x01"
        ),
    ),
    (
        {
            "commands": {
                "cmd_label_thermostat": {
                    "label": "Living Room",
                }
            }
        },
        (
            b"\x03"
            + "Living Room".encode("utf-8")
            + b"\x00\x00\x00\x00\x00\x00\x00\x00\x00"
        ),
    ),
    (
        {
            "commands": {
                "cmd_label_thermostat": {
                    "label": "A very long room name that doesn't fit",
                }
            }
        },
        (b"\x03" + "A very long room nam".encode("utf-8")),
    ),
]


@pytest.mark.parametrize("command, expected", struct_into_bytes_params)
def test_element_into_bytes(command, expected):
    definitions = example_definitions()
    result = struct_parse.element_into_bytes(
        command, definitions, endianness="big"
    )
    assert expected == result