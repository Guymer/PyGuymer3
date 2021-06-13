#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.8/library/multiprocessing.html#multiprocessing-programming
if __name__ == "__main__":
    # This is a test suite for “buffer()” with:
    #     A) a polygon that span the whole numerical range;
    #     B) a polygon that cross the equator;
    #     C) a polygon that cross the anti-meridian;
    #     D) a polygon that cross a pole;
    #     E) a polygon that cross both the equator and the anti-meridian; and
    #     F) a polygon that cross both a pole and the anti-meridian.
    # Each polygon has a plot with both a top-down projection and a Robinson
    # projection so that you can check it.

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
        matplotlib.use("Agg")                                                   # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None

    # Import my modules ...
    try:
        import pyguymer3
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    print("Testing \"{:s}\" ...".format(pyguymer3.__path__[0]))

    # Define polygons ...
    polys = [
        (pyguymer3.geo.buffer_point(-180.0, +90.0, 1000000.0, debug = True, nang = 91, simp = -1.0), 1000000.0), # Satisfies test A, C, D, F
        (pyguymer3.geo.buffer_point( -90.0, +45.0, 1000000.0, debug = True, nang = 91, simp = -1.0), 1000000.0), # Satisfies test A
        (pyguymer3.geo.buffer_point(   0.0,   0.0, 1000000.0, debug = True, nang = 91, simp = -1.0), 1000000.0), # Satisfies test A, B
        (pyguymer3.geo.buffer_point( +90.0, -45.0, 1000000.0, debug = True, nang = 91, simp = -1.0), 1000000.0), # Satisfies test A
        (pyguymer3.geo.buffer_point(+180.0, -90.0, 1000000.0, debug = True, nang = 91, simp = -1.0), 1000000.0), # Satisfies test A, C, D, F
        (pyguymer3.geo.buffer_point(+170.0, +10.0, 1000000.0, debug = True, nang = 91, simp = -1.0), 4000000.0), # Satisfies test B, C, E
        (pyguymer3.geo.buffer_point(+170.0, +80.0, 1000000.0, debug = True, nang = 91, simp = -1.0), 4000000.0), # Satisfies test C, D, F
    ]

    # Loop over polygons ...
    for i, poly in enumerate(polys):
        # Determine file name ...
        fname = f"buffer{i:d}.png"

        print(" > Making \"{:s}\" ...".format(fname))

        # Create figure ...
        fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

        # Create first subplot ...
        ax1 = matplotlib.pyplot.subplot(1, 2, 1, projection = cartopy.crs.Robinson())
        ax1.set_global()
        pyguymer3.geo.add_map_background(ax1)
        ax1.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

        # Create second subplot ...
        ax2 = matplotlib.pyplot.subplot(1, 2, 2, projection = cartopy.crs.Orthographic(central_longitude = poly[0].centroid.x, central_latitude = poly[0].centroid.y))
        ax2.set_global()
        pyguymer3.geo.add_map_background(ax2)
        ax2.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

        # Buffer polygon and plot it twice ...
        buff = pyguymer3.geo.buffer(poly[0], poly[1], debug = True, nang = 91, simp = -1.0)
        ax1.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)
        ax2.add_geometries([buff], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = (1, 0.0, 0.0, 0.5), linewidth = 1.0)

        # Save figure ...
        fg.suptitle("({:.1f},{:.1f}) buffered by {:,.1f}km".format(poly[0].centroid.x, poly[0].centroid.y, 2.0 * poly[1] / 1000.0))
        fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
        pyguymer3.optimize_image(fname, strip = True)
        matplotlib.pyplot.close(fg)
