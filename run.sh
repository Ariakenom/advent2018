ghc -O -rtsopts "$1.hs" && cat "$1.in" | "./$1.exe" +RTS -s -A10m -RTS
