## Conclusions

"32x16" is the largest tile size added to the repository, with "36x18" not
making it. This means that in the repository, there are only the following tile
sizes:

* GLOBE: 18x9
* GSHHG: 2x1, 4x2, 8x4, 16x8 and 32x16
* NE: 18x9
* [OS Terrain 50](https://www.ordnancesurvey.co.uk/products/os-terrain-50): 22x41

The threshold had to be at least "22x41" so that the [OS Terrain 50](https://www.ordnancesurvey.co.uk/products/os-terrain-50)
dataset could have at least one folder of tiles.

## Bugs

At time of writing (16/Aug/2025) there appears to be a bug in how Cartopy draws
images on an axis which means that they have small gaps between them.

You should have a look at the source code of Cartopy's `.imshow()` replacement:

* https://scitools.org.uk/cartopy/docs/latest/_modules/cartopy/mpl/geoaxes.html#GeoAxes.imshow

Cartopy regrids the image and transforms it before passing it to MatPlotLib. The
size of the gap between tiles changes as `regrid_shape` is changed. I cannot put
my finger on the exact source of the problem though.
