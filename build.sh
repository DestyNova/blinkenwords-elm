#/bin/sh
elm-make blinkenwords.elm --output blinkenwords.js
uglifyjs blinkenwords.js -o blinkenwords.min.js
