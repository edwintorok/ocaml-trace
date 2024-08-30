#!/bin/sh
DUNE_OPTS="--profile=release --display=quiet"
exec dune exec $DUNE_OPTS bench/trace2.exe -- $@
