# PyGuymer3

[![Documentation Status](https://readthedocs.org/projects/pyguymer3/badge/?version=latest)](https://pyguymer3.readthedocs.io/en/latest/?badge=latest)

This is a Python 3.X port of [my Python 2.X module](https://github.com/Guymer/PyGuymer) containing a bunch of random functions that I have written over the years. The [documentation is on Read The Docs](https://pyguymer3.readthedocs.io/).

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
* [shapely](https://pypi.org/project/Shapely/)

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
