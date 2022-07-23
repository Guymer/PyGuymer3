#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.9/library/multiprocessing.html#multiprocessing-programming
if __name__ == "__main__":
    # This is a test suite for “geo.fillin()”.
    # Each ring has a plot with both a top-down projection and a Robinson
    # projection so that you can check it, along with an equirectangular plot.

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import geojson
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import matplotlib
        matplotlib.use("Agg")                                                   # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # **************************************************************************

    # Define rings ...
    rings = [
        [
            (  0.0,  44.0),
            ( 44.0,   0.0),
            (  0.0, -44.0),
            (-44.0,   0.0),
            (  0.0,  44.0),
        ],
        [
            (90.0 +   0.0,  44.0),
            (90.0 +  44.0,   0.0),
            (90.0 +   0.0, -44.0),
            (90.0 + -44.0,   0.0),
            (90.0 +   0.0,  44.0),
        ],
        [
            (  0.0, 45.0 +  44.0),
            ( 44.0, 45.0 +   0.0),
            (  0.0, 45.0 + -44.0),
            (-44.0, 45.0 +   0.0),
            (  0.0, 45.0 +  44.0),
        ],
        [
            (90.0 +   0.0, 45.0 +  44.0),
            (90.0 +  44.0, 45.0 +   0.0),
            (90.0 +   0.0, 45.0 + -44.0),
            (90.0 + -44.0, 45.0 +   0.0),
            (90.0 +   0.0, 45.0 +  44.0),
        ],
    ]

    # Define filling ...
    euclideanFill = 1.0                                                         # [°]
    geodesicFill = 10000.0                                                      # [m]

    # Loop over rings ...
    for i, ring in enumerate(rings):
        # Determine file names ...
        fname = f"fillin{i:d}.png"
        jname1 = f"fillin{i:d}Euclidean.geojson"
        jname2 = f"fillin{i:d}Geodesic.geojson"

        print(f" > Making \"{jname1}\", \"{jname2}\" and \"{fname}\" ...")

        # Create figure ...
        fg = matplotlib.pyplot.figure(figsize = (6, 6), dpi = 150)

        # Create first subplot ...
        ax1 = fg.add_subplot(2, 2, 1, projection = cartopy.crs.Robinson())
        ax1.set_global()
        pyguymer3.geo.add_map_background(ax1)
        pyguymer3.geo.add_horizontal_gridlines(ax1, [-180.0, +180.0, -90.0, +90.0], locs = range(-90, 135, 45))
        pyguymer3.geo.add_vertical_gridlines(ax1, [-180.0, +180.0, -90.0, +90.0], locs = range(-180, 225, 45))
        ax1.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

        # Create second subplot ...
        ax2 = fg.add_subplot(2, 2, 2, projection = cartopy.crs.Orthographic(central_longitude = ring[0][0], central_latitude = ring[1][1]))
        ax2.set_global()
        pyguymer3.geo.add_map_background(ax2)
        pyguymer3.geo.add_horizontal_gridlines(ax2, [-180.0, +180.0, -90.0, +90.0], locs = range(-90, 135, 45))
        pyguymer3.geo.add_vertical_gridlines(ax2, [-180.0, +180.0, -90.0, +90.0], locs = range(-180, 225, 45))
        ax2.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

        # Create third subplot ...
        ax3 = fg.add_subplot(2, 2, (3, 4))
        ax3.grid()
        ax3.set_aspect("equal")
        ax3.set_xlabel("Longitude [°]")
        ax3.set_xlim(-180.0, +180.0)
        ax3.set_xticks([-180.0, -135.0, -90.0, -45.0, 0.0, +45.0, +90.0, +135.0, +180.0])
        ax3.set_ylabel("Latitude [°]")
        ax3.set_ylim(-90.0, +90.0)
        ax3.set_yticks([-90.0, -45.0, 0.0, +45.0, +90.0])

        # Convert list of points to a LinearRing ...
        sparseRing = shapely.geometry.polygon.LinearRing(ring)

        # Fill in ring in Euclidean space and plot it thrice ...
        denseRing1 = pyguymer3.geo.fillin(sparseRing, euclideanFill, debug = True, fillSpace = "EuclideanSpace")
        ax1.add_geometries([denseRing1], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = "none", linewidth = 1.0)
        ax2.add_geometries([denseRing1], cartopy.crs.PlateCarree(), edgecolor = (1.0, 0.0, 0.0, 1.0), facecolor = "none", linewidth = 1.0)
        coords = numpy.array(denseRing1.coords)
        ax3.plot(coords[:, 0], coords[:, 1], color = (1.0, 0.0, 0.0, 1.0))
        del coords

        # Save GeoJSON ...
        with open(jname1, "wt", encoding = "utf-8") as fobj:
            geojson.dump(
                denseRing1,
                fobj,
                ensure_ascii = False,
                      indent = 4,
                   sort_keys = True,
            )

        # Clean up ...
        del denseRing1

        # Fill in ring in Geodesic space and plot it thrice ...
        denseRing2 = pyguymer3.geo.fillin(sparseRing,  geodesicFill, debug = True, fillSpace =  "GeodesicSpace")
        ax1.add_geometries([denseRing2], cartopy.crs.PlateCarree(), edgecolor = (0.0, 0.0, 1.0, 1.0), facecolor = "none", linewidth = 1.0)
        ax2.add_geometries([denseRing2], cartopy.crs.PlateCarree(), edgecolor = (0.0, 0.0, 1.0, 1.0), facecolor = "none", linewidth = 1.0)
        coords = numpy.array(denseRing2.coords)
        ax3.plot(coords[:, 0], coords[:, 1], color = (0.0, 0.0, 1.0, 1.0))
        del coords

        # Save GeoJSON ...
        with open(jname2, "wt", encoding = "utf-8") as fobj:
            geojson.dump(
                denseRing2,
                fobj,
                ensure_ascii = False,
                      indent = 4,
                   sort_keys = True,
            )

        # Clean up ...
        del denseRing2

        # Save figure ...
        fg.suptitle(f"A rhombus around ({ring[0][0]:.1f},{ring[1][1]:.1f}) filled in by {euclideanFill:,.0f}° & {0.001 * geodesicFill:,.0f}km\nred = Euclidean; blue = Geodesic")
        fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
        pyguymer3.image.optimize_image(fname, strip = True)
        matplotlib.pyplot.close(fg)
