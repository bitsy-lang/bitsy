#!/bin/bash

set -ex

cargo run > out.mlir
cat out.mlir
firtool --format=mlir out.mlir
