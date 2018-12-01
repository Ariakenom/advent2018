ghc -O "$1.hs" && cat "$1.in" | "$1.exe"
