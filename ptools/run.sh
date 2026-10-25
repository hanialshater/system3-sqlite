#!/bin/sh
# the engine (see ../run.sh)
exec "$(dirname "$(readlink -f "$0")")/../run.sh" "$@"
