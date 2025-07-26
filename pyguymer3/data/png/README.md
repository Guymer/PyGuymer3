This folder contains tiles of some datasets to allow quick and efficient plotting. Currently, the four datasets provided are:

* The [Global Self-Consistent Hierarchical High-Resolution Geography](https://www.ngdc.noaa.gov/mgg/shorelines/) dataset is a vector dataset.
* The [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset is a raster dataset at 43,200 px × 21,600 px (I wrote [a Python module to vectorise it](https://github.com/Guymer/vgd)).
* The [Natural Earth](https://www.naturalearthdata.com/) datasets are vector datasets.
* The [OS Terrain 50](https://www.ordnancesurvey.co.uk/products/os-terrain-50) dataset is a raster dataset at 13,200 px × 24,600 px.

Using a very simple Python function of:

```python
for i in range(2, n):
    if n % i == 0:
        print(f"{i:d} × {n // i:d}")
```

... it is possible to discover which tile sizes would fit for each raster dataset. If I hide the solutions which have tiles larger than 1,024 px or smaller than 256 px, then:

* For the [GLOBE](https://www.ngdc.noaa.gov/mgg/topo/globe.html) dataset:
    * With a width of 43,200 px:
        * 45 × 960
        * 48 × 900 (would allow square tiles)
        * 50 × 864 (would allow square tiles)
        * 54 × 800 (would allow square tiles)
        * 60 × 720 (would allow square tiles)
        * 64 × 675 (would allow square tiles)
        * 72 × 600 (would allow square tiles)
        * 75 × 576
        * 80 × 540 (would allow square tiles)
        * 90 × 480 (would allow square tiles)
        * 96 × 450 (would allow square tiles)
        * 100 × 432 (would allow square tiles)
        * 108 × 400 (would allow square tiles)
        * 120 × 360 (would allow square tiles)
        * 135 × 320
        * 144 × 300 (would allow square tiles)
        * 150 × 288 (would allow square tiles)
        * 160 × 270 (would allow square tiles)
    * With a height of 21,600 px:
        * 24 × 900 (would allow square tiles)
        * 25 × 864 (would allow square tiles)
        * 27 × 800 (would allow square tiles)
        * 30 × 720 (would allow square tiles)
        * 32 × 675 (would allow square tiles)
        * 36 × 600 (would allow square tiles)
        * 40 × 540 (would allow square tiles)
        * 45 × 480 (would allow square tiles)
        * 48 × 450 (would allow square tiles)
        * 50 × 432 (would allow square tiles)
        * 54 × 400 (would allow square tiles)
        * 60 × 360 (would allow square tiles)
        * 72 × 300 (would allow square tiles)
        * 75 × 288 (would allow square tiles)
        * 80 × 270 (would allow square tiles)
* For the [OS Terrain 50](https://www.ordnancesurvey.co.uk/products/os-terrain-50) dataset:
    * With a width of 13,200 px:
        * 15 × 880
        * 16 × 825
        * 20 × 660
        * 22 × 600 (would allow square tiles)
        * 24 × 550
        * 25 × 528
        * 30 × 440
        * 33 × 400
        * 40 × 330
        * 44 × 300 (would allow square tiles)
        * 48 × 275
        * 50 × 264
    * With a height of 24,600 px:
        * 25 × 984
        * 30 × 820
        * 40 × 615
        * 41 × 600 (would allow square tiles)
        * 50 × 492
        * 60 × 410
        * 75 × 328
        * 82 × 300 (would allow square tiles)

It is possible to represent both raster datasets using 600 px × 600 px tiles.
