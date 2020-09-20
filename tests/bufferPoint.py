#!/usr/bin/env python3

# This is a test suite for “buffer_point()” with:
#     A) a point that span the whole numerical range;
#     B) a point that cross the equator;
#     C) a point that cross the anti-meridian;
#     D) a point that cross a pole;
#     E) a point that cross both the equator and the anti-meridian; and
#     F) a point that cross both a pole and the anti-meridian.
# Each point has a plot with both a top-down projection and a Robinson
# projection so that you can check it.

# Import special modules ...
try:
    import cartopy
except:
    raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"")
try:
    import matplotlib
    matplotlib.use("Agg")                                                       # NOTE: https://matplotlib.org/gallery/user_interfaces/canvasagg.html
    import matplotlib.pyplot
except:
    raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"")

# Import my modules ...
try:
    import pyguymer3
except:
    raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH")

print("Testing \"{:s}\" ...".format(pyguymer3.__path__[0]))

# Define points ...
pnts = [
    (-180.0, +90.0, 1000000.0), # Satisfies test A, C, D, F
    ( -90.0, +45.0, 1000000.0), # Satisfies test A
    (   0.0,   0.0, 1000000.0), # Satisfies test A, B
    ( +90.0, -45.0, 1000000.0), # Satisfies test A
    (+180.0, -90.0, 1000000.0), # Satisfies test A, C, D, F
    (+170.0, +10.0, 4000000.0), # Satisfies test B, C, E
    (+170.0, +80.0, 4000000.0), # Satisfies test C, D, F
]

# Loop over points ...
for pnt in pnts:
    # Determine file name ...
    fname = "bufferPoint{:d}.png".format(pnts.index(pnt))

    print(" > Making \"{:s}\" ...".format(fname))

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

    # Create first subplot ...
    ax1 = matplotlib.pyplot.subplot(1, 2, 1, projection = cartopy.crs.Robinson())
    ax1.set_global()
    pyguymer3.add_map_background(ax1)
    ax1.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Create second subplot ...
    ax2 = matplotlib.pyplot.subplot(1, 2, 2, projection = cartopy.crs.Orthographic(central_longitude = pnt[0], central_latitude = pnt[1]))
    ax2.set_global()
    pyguymer3.add_map_background(ax2)
    ax2.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

    # Buffer point and plot it twice ...
    poly = pyguymer3.buffer_point(pnt[0], pnt[1], pnt[2], nang = 91)
    ax1.add_geometries([poly], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)
    ax2.add_geometries([poly], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)

    # Save figure ...
    fg.suptitle("({:.1f},{:.1f}) buffered by {:,.1f}km".format(pnt[0], pnt[1], pnt[2] / 1000.0))
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    pyguymer3.optimize_image(fname, strip = True)
    matplotlib.pyplot.close("all")
