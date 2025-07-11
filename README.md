# PyGuymer3

![Documentation Status](https://readthedocs.org/projects/pyguymer3/badge/) !["coverage" GitHub Action Status](https://github.com/Guymer/PyGuymer3/actions/workflows/coverage.yaml/badge.svg) !["gmake" GitHub Action Status](https://github.com/Guymer/PyGuymer3/actions/workflows/gmake.yaml/badge.svg) !["mypy" GitHub Action Status](https://github.com/Guymer/PyGuymer3/actions/workflows/mypy.yaml/badge.svg) !["profile" GitHub Action Status](https://github.com/Guymer/PyGuymer3/actions/workflows/profile.yaml/badge.svg) !["publish" GitHub Action Status](https://github.com/Guymer/PyGuymer3/actions/workflows/publish.yaml/badge.svg) !["pylint" GitHub Action Status](https://github.com/Guymer/PyGuymer3/actions/workflows/pylint.yaml/badge.svg) !["unittest" GitHub Action Status](https://github.com/Guymer/PyGuymer3/actions/workflows/unittest.yaml/badge.svg)

This is a Python 3.X port of [my Python 2.X module](https://github.com/Guymer/PyGuymer) containing a bunch of random functions that I have written over the years. The [documentation is on Read The Docs](https://pyguymer3.readthedocs.io/) and the [module is on PyPI](https://pypi.org/project/PyGuymer3/).

You may [browse the coverage report](https://guymer.github.io/PyGuymer3/) for running [the unit tests](unitTests.py).

## Installation

To install PyGuymer3, run:

```sh
# To install the latest released version from PyPI:
pip install --user PyGuymer3

# To install the latest development version from GitHub:
pip install --user git+https://github.com/Guymer/PyGuymer3.git
```

... and everything should "just work".

## Dependencies

PyGuymer3 requires the following Python modules to be installed and available in your `PYTHONPATH`:

* [cartopy](https://pypi.org/project/Cartopy/)
* [exifread](https://pypi.org/project/ExifRead/)
* [geojson](https://pypi.org/project/geojson/)
* [lxml](https://pypi.org/project/lxml/)
* [matplotlib](https://pypi.org/project/matplotlib/)
* [numpy](https://pypi.org/project/numpy/)
* [PIL](https://pypi.org/project/Pillow/)
* [requests](https://pypi.org/project/requests/)
* [scipy](https://pypi.org/project/scipy/)
* [shapely](https://pypi.org/project/Shapely/)
* [sphinx](https://pypi.org/project/Sphinx/)
    * [sphinx_fortran](https://pypi.org/project/sphinx-fortran/)
    * [sphinx_rtd_theme](https://pypi.org/project/sphinx-rtd-theme/)

Additionally, due to the dependency on [my FORTRAN library](https://github.com/Guymer/fortranlib), if you want to run the FORTRAN-based functions in the [f90](pyguymer3/f90) sub-module then you will also require the following Python modules to be installed and available in your `PYTHONPATH`:

* [matplotlib](https://pypi.org/project/matplotlib/)
* [scipy](https://pypi.org/project/scipy/)
* [sphinx](https://pypi.org/project/Sphinx/)
    * [sphinx_fortran](https://pypi.org/project/sphinx-fortran/)
    * [sphinx_rtd_theme](https://pypi.org/project/sphinx-rtd-theme/)

Similarly, PyGuymer3 requires the following binaries to be installed and available in your `PATH`:

* `dot`
* `exiftool`
* `ffmpeg` and `ffprobe`
* `gifsicle`
* `git`
* `jpegtran`
* `lsdvd`
* `metaflac`
* `mp4file`
* `optipng`
* `tar`
* `xz`

## Notes

### The Zen of Python

```python
import this
```

## To Do

* Read this good [StackOverflow answer](https://stackoverflow.com/a/21384492) describing type-checking which also provides a quick summary on [PEP 484](https://www.python.org/dev/peps/pep-0484/) and [PEP 3107](https://www.python.org/dev/peps/pep-3107/).

## Bugs

* This module has been created from [my Python 2.X module](https://github.com/Guymer/PyGuymer) using the utility `2to3`. As such, there may be bugs in the code as most of it has not been tested since the port. If you find anything wrong then please [raise a bug report on GitHub](https://github.com/Guymer/PyGuymer3/issues).
* The function [pyguymer3.geo.area()](pyguymer3/geo/area.py) sometimes returns erroneous areas. This has been tracked down to the Shapely function [shapely.ops.voronoi_diagram()](https://shapely.readthedocs.io/en/stable/manual.html#shapely.ops.voronoi_diagram): for certain inputs (often when a point within the shape is exactly `0.0`) some of the parts of the Voronoi diagram are inverted. When it goes wrong it reliably goes wrong for those inputs. Below, the blue circle is the 1 km buffer around a point at `(0.0°, 0.0°)` and the orange wedges are the parts of the Voronoi diagram. Note that the orange wedges for `iVoroPoly = 10` and `iVoroPoly = 31` are inverted - they are the negative of what they are supposed to be. As a consequence, when the area of each orange wedge is calculated and summed the total is very wrong.
![bad Voronoi diagram](tests/area/bad.png).
