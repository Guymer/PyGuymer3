## Tests

This directory contains some basic tests to (hopefully):

* find any simple bugs that I might have introduced; and
* demonstrate some simple programming techniques.

To compare the images with a previous version, try something like:

```sh
ver1="/path/to/version-1/pyguymer3/tests"
ver2="/path/to/version-2/pyguymer3/tests"
for p1 in ${ver1}/*.png; do p0="$(basename "${p1}")"; p2="${ver2}/${p0}"; h1="$(md5 -q "${p1}")"; h2="$(md5 -q "${p2}")"; [[ $h1 == $h2 ]] && continue; echo "${p0} ..."; compare "${p1}" "${p2}" "${p0}"; done
```

The available tests are:

* the script [area.py](area.py) produces [these results](area.md);
* the script [buffer.py](buffer.py) produces [these results](buffer.md);
* the script [bufferPoint.py](bufferPoint.py) produces [these results](bufferPoint.md);
* the script [fillin.py](fillin.py) produces [these results](fillin.md);
* the script [greatCircle.py](greatCircle.py) produces [these results](greatCircle.md);
* the script [mapUnderlay.py](mapUnderlay.py) produces [these results](mapUnderlay.md); and
* the script [saveArrayAsImage.py](saveArrayAsImage.py) produces [these results](saveArrayAsImage.md).
