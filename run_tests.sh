#!/usr/bin/env bash

cargo build --release
cd ../craftinginterpreters/ 
dart ./tool/bin/test.dart ${1:-clox} --interpreter=../rlox/target/release/rlox