#!/usr/bin/env python3

"""Validate data/index.json using a JSON schema."""

import json

import requests
from jsonschema.validators import Draft202012Validator
from referencing import Registry, Resource


def retrieve_from_network(uri: str):
    """Read over network."""
    return Resource.from_contents(requests.get(uri).json())


def main(file_path: str) -> None:
    """Main entry point."""
    with open(file_path) as f:
        data = json.load(f)
    schema_url = data["$schema"]
    schema = requests.get(schema_url).json()

    registry = Registry(retrieve=retrieve_from_network)
    Draft202012Validator.check_schema(schema)
    validator = Draft202012Validator(schema=schema, registry=registry)
    validator.validate(data)

    print(f"Successfully validated {file_path}")


if __name__ == "__main__":
    import sys

    main(sys.argv[1])
