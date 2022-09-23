#!/usr/bin/env bash

mkdir build
cp index.html build

elm make Main.elm --output build/blinkenwords.js
uglifyjs build/blinkenwords.js -o build/blinkenwords.min.js
