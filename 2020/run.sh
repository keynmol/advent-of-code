#!/usr/bin/env sh

DAY=$1
FLAGS=$2

echo "Running day $DAY with flags $FLAGS"

scala-cli $FLAGS day$DAY.scala _lib.scala -- inputs/day$DAY
