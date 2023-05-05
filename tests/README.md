This directory contains some basic tests to (hopefully):

* find any simple bugs that I might have introduced; and
* demonstrate some simple programming techniques.

To compare the images with a previous version, try something like:

```sh
ver1="/path/to/version-1/pyguymer3/tests"
ver2="/path/to/version-2/pyguymer3/tests"
for p1 in ${ver1}/*.png; do p0="$(basename "${p1}")"; p2="${ver2}/${p0}"; h1="$(md5 -q "${p1}")"; h2="$(md5 -q "${p2}")"; [[ $h1 == $h2 ]] && continue; echo "${p0} ..."; compare "${p1}" "${p2}" "${p0}"; done
```

### `add_map_underlay()`

The function [add_map_underlay](../pyguymer3/geo/add_map_underlay.py) is tested by the script [mapUnderlay.py](mapUnderlay.py) which produces the images below.

![mapUnderlay0 output image](mapUnderlay/mapUnderlay0.png)

![mapUnderlay1 output image](mapUnderlay/mapUnderlay1.png)

![mapUnderlay2 output image](mapUnderlay/mapUnderlay2.png)

![mapUnderlay3 output image](mapUnderlay/mapUnderlay3.png)

### `buffer()` (of points)

The function [buffer](../pyguymer3/geo/buffer.py) is tested by the script [bufferPoint.py](bufferPoint.py) which produces the images below.

![bufferPoint output image](bufferPoint/bufferPoint0.png)

![bufferPoint output image](bufferPoint/bufferPoint1.png)

![bufferPoint output image](bufferPoint/bufferPoint2.png)

![bufferPoint output image](bufferPoint/bufferPoint3.png)

![bufferPoint output image](bufferPoint/bufferPoint4.png)

![bufferPoint output image](bufferPoint/bufferPoint5.png)

![bufferPoint output image](bufferPoint/bufferPoint6.png)

![bufferPoint output image](bufferPoint/bufferPoint7.png)

![bufferPoint output image](bufferPoint/bufferPoint8.png)

![bufferPoint output image](bufferPoint/bufferPoint9.png)

### `buffer()` (of [Multi]Polygons)

The function [buffer](../pyguymer3/geo/buffer.py) is tested by the script [buffer.py](buffer.py) which produces the images below.

![buffer output image](buffer/buffer0.png)

![buffer output image](buffer/buffer1.png)

![buffer output image](buffer/buffer2.png)

![buffer output image](buffer/buffer3.png)

![buffer output image](buffer/buffer4.png)

![buffer output image](buffer/buffer5.png)

![buffer output image](buffer/buffer6.png)

![buffer output image](buffer/buffer7.png)

![buffer output image](buffer/buffer8.png)

![buffer output image](buffer/buffer9.png)

### `fillin()`

The function [fillin](../pyguymer3/geo/fillin.py) is tested by the script [fillin.py](fillin.py) which produces the images below.

![fillin output image](fillin/fillin0.png)

![fillin output image](fillin/fillin1.png)

![fillin output image](fillin/fillin2.png)

![fillin output image](fillin/fillin3.png)

### `great_circle()`

The function [great_circle](../pyguymer3/geo/great_circle.py) is tested by the script [greatCircle.py](greatCircle.py) which produces the image below.

![greatCircle output image](greatCircle/greatCircle0.png)

![greatCircle output image](greatCircle/greatCircle1.png)

![greatCircle output image](greatCircle/greatCircle2.png)

![greatCircle output image](greatCircle/greatCircle3.png)

### `save_array_as_image()`

The function [save_array_as_image](../pyguymer3/image/save_array_as_image.py) is tested by the script [saveArrayAsImage.py](saveArrayAsImage.py) which produces the images below.

![saveArrayAsImage output image](saveArrayAsImage/saveArrayAsImage0.png)

![saveArrayAsImage output image](saveArrayAsImage/saveArrayAsImage1.png)

![saveArrayAsImage output image](saveArrayAsImage/saveArrayAsImage2.png)

![saveArrayAsImage output image](saveArrayAsImage/saveArrayAsImage3.png)

![saveArrayAsImage output image](saveArrayAsImage/saveArrayAsImage4.png)
