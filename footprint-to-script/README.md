```
Usage: footprint-to-script [-x FLOAT] [-y FLOAT] [-r DEGREES]
                           INPUTFILE.kicad_mod OUTPUTFILE.py

Available options:
  -h,--help                Show this help text
  -v,--version             Print version
  -x FLOAT                 Amount to translate in X (default 0)
  -y FLOAT                 Amount to translate in Y (default 0)
  -r DEGREES               Clockwise rotation around origin, after applying
                           translation. Must be 0, 90, 180, or 270. (default 0)
```

`footprint-to-script` takes a [KiCad][2] footprint (a `.kicad_mod`
file), and produces a [Python][3] script which, when run, will
generate that footprint.

For each unique x or y coordinate, the Python script contains a
variable for that coordinate.  This makes it easier to move things
around by changing the values of the variables.

To run, the Python script requires the [KicadModTree][1] library.

Apologies for the long line length; this is a [known limitation][4] of
[language-python][5].

## Binary releases

[Binaries are available][9] for Linux, Mac OS X, and Windows.

## Building from source (with stack)

Install [stack][6] and then do:

```
git clone --recursive https://github.com/ppelleti/footprint-to-script.git
cd footprint-to-script
stack --install-ghc install
```

The binary will be installed in `~/.local/bin`.

## Building from source (with cabal)

Install the [Haskell Platform][7] and then do:

```
git clone --recursive https://github.com/ppelleti/footprint-to-script.git
cd footprint-to-script/footprint-to-script
cabal sandbox init
cabal sandbox add-source ../kicad-data
cabal install
```

The binary will be installed in `~/.cabal/bin`.

## License

`footprint-to-script` and most of its dependencies are licensed under
the 3-clause BSD license.  However, [one dependency][8] is GPL
licensed.  Therefore, the GPL applies to the program as a whole.

[1]: https://github.com/pointhi/kicad-footprint-generator
[2]: http://kicad-pcb.org/
[3]: https://www.python.org/
[4]: https://github.com/bjpop/language-python/issues/3
[5]: https://hackage.haskell.org/package/language-python
[6]: https://docs.haskellstack.org/en/stable/README/#how-to-install
[7]: https://www.haskell.org/platform/
[8]: https://hackage.haskell.org/package/pretty-compact
[9]: https://github.com/ppelleti/footprint-to-script/releases
