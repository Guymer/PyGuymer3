## Conclusions

In the repository there are only the following tile sizes:

* [Global Land One-km Base Elevation](https://www.ngdc.noaa.gov/mgg/topo/globe.html): 18x9
* [Global Land One-km Base Elevation](https://www.ngdc.noaa.gov/mgg/topo/globe.html)+[Global Self-consistent Hierarchical High-resolution Geography](https://www.ngdc.noaa.gov/mgg/shorelines/): 18x9
* [Global Land One-km Base Elevation](https://www.ngdc.noaa.gov/mgg/topo/globe.html)+[Natural Earth](https://www.naturalearthdata.com/): 18x9
* [Global Self-consistent Hierarchical High-resolution Geography](https://www.ngdc.noaa.gov/mgg/shorelines/): 2x1, 4x2, 8x4, 16x8, 32x16 and 64x32
* [Natural Earth](https://www.naturalearthdata.com/): 2x1, 4x2, 8x4, 16x8, 32x16 and 64x32
* [OS Terrain 50](https://www.ordnancesurvey.co.uk/products/os-terrain-50): 22x41

## Bugs

At time of writing (16/Aug/2025) there appears to be a bug in how Cartopy draws
images on an axis which means that they have small gaps between them.

You should have a look at the source code of Cartopy's `.imshow()` replacement:

* https://scitools.org.uk/cartopy/docs/latest/_modules/cartopy/mpl/geoaxes.html#GeoAxes.imshow

Cartopy regrids the image and transforms it before passing it to MatPlotLib. The
size of the gap between tiles changes as `regrid_shape` is changed. I cannot put
my finger on the exact source of the problem though.
