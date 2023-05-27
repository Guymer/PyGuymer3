Testing
-------
The following links contain some basic tests to (hopefully) find any simple bugs
that I might have introduced; and demonstrate some simple programming techniques.

.. toctree::
    test_area
    test_buffer
    test_bufferPoint
    test_fillin
    test_greatCircle
    test_mapUnderlay
    test_saveArrayAsImage

To compare the images with a previous version, try something like::

    ver1="/path/to/version-1/pyguymer3/tests"
    ver2="/path/to/version-2/pyguymer3/tests"
    for p1 in ${ver1}/*.png; do p0="$(basename "${p1}")"; p2="${ver2}/${p0}"; h1="$(md5 -q "${p1}")"; h2="$(md5 -q "${p2}")"; [[ $h1 == $h2 ]] && continue; echo "${p0} ..."; compare "${p1}" "${p2}" "${p0}"; done
