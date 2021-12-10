#!/usr/bin/env python
import argparse
import mdformat
from ruamel.yaml import YAML
from ruamel.yaml.scalarstring import LiteralScalarString, preserve_literal

TAB_LENGTH = 2

def should_reformat(value, line_length):
    """
    Do we want to reformat this string.
    
    Only bother if it's a string and longer than the current line length target
    or if it currently has a break
    """
    return isinstance(value, str) and ("\n" in value or len(value) > line_length)


def format_markdown(value, line_length):
    """
    Reformat the string assuming it's markdown which all text objects
    in this project are.
    """
    value = mdformat.text(value, options={"wrap": line_length})
    # need to escape certain things as R reader doesn't like them
    value = value.replace("°","&deg;")
    return preserve_literal(value)


def walk_tree(base, line_length):
    """
    Recursively walk the YAML tree looking for strings to reformat
    """
    # code inspired by https://stackoverflow.com/a/41357085
    if isinstance(base, dict):
        for key in base:
            value = base[key]
            if should_reformat(value, line_length):
                base[key] = format_markdown(value, line_length)
            else:
                walk_tree(value, line_length - TAB_LENGTH)
    elif isinstance(base, list):
        for i, value in enumerate(base):
            if should_reformat(value, line_length):
                base[i] = format_markdown(value, line_length)
            else:
                walk_tree(value, line_length - TAB_LENGTH)


def reformat_yaml(file_name, line_length):
    """
    Reformat all strings in a YAML file with a mdformat
    specifying a line width.
    """
    yaml = YAML()

    with open(file_name, "r", encoding='utf-8') as fi:
        data = yaml.load(fi)

    walk_tree(data, line_length - TAB_LENGTH)

    with open(file_name, "w", encoding='utf-8') as fo:
        yaml.dump(data, fo)        


def main():
    parser = argparse.ArgumentParser(description="Reformat markdown string elements in a YAML file to wrap text")
    parser.add_argument("file_name", help="Specify a YAML file to reformat")
    parser.add_argument("--line_length", "-l", type=int, default=80, help="Line length to target")

    args = parser.parse_args()
    reformat_yaml(args.file_name, args.line_length)


if __name__ == "__main__":
    main()
