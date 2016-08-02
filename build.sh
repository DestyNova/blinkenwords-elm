#/bin/sh
mkdir build
cp index.html build

elm-make blinkenwords.elm --output build/blinkenwords.js
uglifyjs build/blinkenwords.js -o build/blinkenwords.min.js
