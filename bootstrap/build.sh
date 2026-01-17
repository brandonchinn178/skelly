#!/usr/bin/env bash

set -eux -o pipefail

here=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
venv="${here}/.venv"

if [[ ! -d "${venv}" ]]; then
    python=$(which python3)
    if ! "${python}" -c 'import sys; sys.exit(0 if sys.version_info >= (3,11) else 1)'; then
        echo >&2 'Python 3.11+ required'
        exit 1
    fi
    "${python}" -m venv "${venv}"
    "${venv}/bin/pip" install kdl-py
fi

exec "${venv}/bin/python" "${here}/build.py"
