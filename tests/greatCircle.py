#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.9/library/multiprocessing.html#multiprocessing-programming
if __name__ == "__main__":
    # This is a test suite for "geo.great_circle()”.

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

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # Define pairs of coordinates ...
    coords1 = [
        ( -90.0, +15.0),                                                        # [°], [°]
        (  +1.0, +50.7),                                                        # [°], [°]
        (-122.4, +37.6),                                                        # [°], [°]
        ( +91.0, +15.0),                                                        # [°], [°]
    ]
    coords2 = [
        ( +90.0, +15.0),                                                        # [°], [°]
        (-178.0, -88.0),                                                        # [°], [°]
        (+140.4, +35.8),                                                        # [°], [°]
        ( -91.0, +15.0),                                                        # [°], [°]
    ]

    # Define number of points ...
    npoints = [3, 4, 8, 16, 1000]                                               # [#]

    # Loop over tests ...
    for i, (coord1, coord2) in enumerate(zip(coords1, coords2)):
        # Determine file names ...
        fname = f"greatCircle{i:d}.png"
        jname = f"greatCircle{i:d}.geojson"

        print(f" > Making \"{jname}\" and \"{fname}\" ...")

        # Create figure ...
        fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

        # Create axis ...
        ax = fg.add_subplot(projection = cartopy.crs.Robinson())

        # Configure axis ...
        ax.set_global()
        pyguymer3.geo.add_map_background(ax)
        pyguymer3.geo.add_horizontal_gridlines(ax, [-180.0, +180.0, -90.0, +90.0], locs = [-90.0, -45.0, 0.0, +45.0, +90.0])
        pyguymer3.geo.add_vertical_gridlines(ax, [-180.0, +180.0, -90.0, +90.0], locs = [-180.0, -135.0, -90.0, -45.0, 0.0, +45.0, +90.0, +135.0, +180.0])
        ax.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

        # Loop over number of points ...
        for c, npoint in enumerate(npoints):
            # Find the great circle ...
            circle = pyguymer3.geo.great_circle(coord1[0], coord1[1], coord2[0], coord2[1], debug = True, npoint = npoint)

            # Save GeoJSON ...
            with open(jname, "wt", encoding = "utf-8") as fobj:
                geojson.dump(
                    circle,
                    fobj,
                    ensure_ascii = False,
                          indent = 4,
                       sort_keys = True,
                )

            # Loop over lines in the great circle ...
            for line in pyguymer3.geo.extract_lines(circle):
                # Extract the coordinates from this line ...
                coords = numpy.array(line.coords)                               # [°]

                # Transform coordinates ...
                # NOTE: See https://stackoverflow.com/a/52861074
                points = cartopy.crs.Robinson().transform_points(cartopy.crs.Geodetic(), coords[:, 0], coords[:, 1])

                # Plot great circle ...
                ax.plot(
                    points[:, 0],
                    points[:, 1],
                    transform = cartopy.crs.Robinson(),
                    linewidth = 1.0,
                        color = matplotlib.pyplot.cm.rainbow(float(c) / float(len(npoints) - 1)),
                )

        # Plot great circle ...
        # NOTE: This allows comparison between great_circle() and Cartopy.
        # ax.plot(
        #     [coord1[0], coord2[0]],
        #     [coord1[1], coord2[1]],
        #     transform = cartopy.crs.Geodetic(),
        #     linewidth = 1.0,
        #         color = "black"
        # )

        # Save figure ...
        fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
        matplotlib.pyplot.close(fg)

        # Optimize figure ...
        pyguymer3.image.optimize_image(fname, strip = True)
