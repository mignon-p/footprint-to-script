Usage: `footprint-to-script inputfile.kicad_mod outputfile.py`

`footprint-to-script` takes a [KiCad][2] footprint (a `.kicad_mod`
file), and produces a [Python][3] script which, when run, will
generate that footprint.

For each unique x or y coordinate, the Python script contains a
variable for that coordinate.  This makes it easier to move things
around by changing the values of the variables.

To run, the Python script requires the [KicadModTree][1] library.

Apologies for the long line length; this is a [known limitation][4] of
[language-python][5].

[1]: https://github.com/pointhi/kicad-footprint-generator
[2]: http://kicad-pcb.org/
[3]: https://www.python.org/
[4]: https://github.com/bjpop/language-python/issues/3
[5]: https://hackage.haskell.org/package/language-python
