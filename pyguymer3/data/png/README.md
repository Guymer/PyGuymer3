This folder contains tiles of some datasets to allow quick and efficient plotting. Currently, the four datasets provided are:

* The [Global Land One-km Base Elevation](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset is a raster dataset at 43,200 px × 21,600 px (I wrote [a Python module to vectorise it](https://github.com/Guymer/vgd)).
* The [Global Self-consistent Hierarchical High-resolution Geography](https://www.ngdc.noaa.gov/mgg/shorelines/) datasets are vector datasets.
* The [Natural Earth](https://www.naturalearthdata.com/) datasets are vector datasets.
* The [OS Terrain 50](https://www.ordnancesurvey.co.uk/products/os-terrain-50) dataset is a raster dataset at 13,200 px × 24,600 px.

Using a very simple Python snippet of:

```python
n = [43200, 21600, 13200, 24600]
for i in range(100, 1000):
    if all(
        [
            n[0] % i == 0,
            n[1] % i == 0,
            n[2] % i == 0,
            n[3] % i == 0,
        ]
    ):
        print(f"{i:d} px/tile × {n[0] // i:3d} tiles = {n[0]:,d} px and {i:d} px/tile × {n[1] // i:3d} tiles = {n[1]:,d} px and {i:d} px/tile × {n[2] // i:3d} tiles = {n[2]:,d} px and {i:d} px/tile × {n[3] // i:3d} tiles = {n[3]:,d} px")
```

... it is possible to discover which tile sizes would fit for all the raster datasets simultaneously.

* For the [Global Land One-km Base Elevation](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset:
    * With a width of 43,200 px:
        * 100 px/tile × 432 tiles = 43,200 px
        * 120 px/tile × 360 tiles = 43,200 px
        * 150 px/tile × 288 tiles = 43,200 px
        * 200 px/tile × 216 tiles = 43,200 px
        * 300 px/tile × 144 tiles = 43,200 px
        * 600 px/tile ×  72 tiles = 43,200 px
    * With a height of 21,600 px:
        * 100 px/tile × 216 tiles = 21,600 px
        * 120 px/tile × 180 tiles = 21,600 px
        * 150 px/tile × 144 tiles = 21,600 px
        * 200 px/tile × 108 tiles = 21,600 px
        * 300 px/tile ×  72 tiles = 21,600 px
        * 600 px/tile ×  36 tiles = 21,600 px
* For the [OS Terrain 50](https://www.ordnancesurvey.co.uk/products/os-terrain-50) dataset:
    * With a width of 13,200 px:
        * 100 px/tile × 132 tiles = 13,200 px
        * 120 px/tile × 110 tiles = 13,200 px
        * 150 px/tile ×  88 tiles = 13,200 px
        * 200 px/tile ×  66 tiles = 13,200 px
        * 300 px/tile ×  44 tiles = 13,200 px
        * 600 px/tile ×  22 tiles = 13,200 px
    * With a height of 24,600 px:
        * 100 px/tile × 246 tiles = 24,600 px
        * 120 px/tile × 205 tiles = 24,600 px
        * 150 px/tile × 164 tiles = 24,600 px
        * 200 px/tile × 123 tiles = 24,600 px
        * 300 px/tile ×  82 tiles = 24,600 px
        * 600 px/tile ×  41 tiles = 24,600 px

For the time being, I choose to represent both raster datasets using 300 px × 300 px tiles.
