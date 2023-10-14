"""
A script for modifying a TOML document.

python3 TOML_encode.py '[{"type": "set-key", "keys": ["key1"], "val": 123}]' < file.toml
"""

import sys
import json
import tomlkit

def main():
    updates = json.loads(sys.argv[1])
    doc = tomlkit.load(sys.stdin)

    for update in updates:
        update_type = update['type']
        if update_type == 'set-key':
            set_key(doc, update['keys'], update['val'])
        else:
            raise ValueError(f'Unknown type: {update_type}')

    tomlkit.dump(doc, sys.stdout)

def set_key(table, keys, val):
    if len(keys) == 0:
        return

    if len(keys) == 1:
        k = keys[0]
        table[k] = val
        return

    k, ks = keys[0], keys[1:]
    if k not in table:
        table[k] = {}
    if isinstance(table[k], dict):
        set_key(table[k], ks, val)

main()
