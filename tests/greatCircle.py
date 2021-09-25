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
        import matplotlib
        matplotlib.use("Agg")                                                   # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None

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
    ]
    coords2 = [
        ( +90.0, +15.0),                                                        # [°], [°]
        (-178.0, -88.0),                                                        # [°], [°]
    ]

    # Define number of points ...
    npoints = [2, 3, 4, 8, 16, 1000]                                            # [#]

    # Loop over tests ...
    for i, (coord1, coord2) in enumerate(zip(coords1, coords2)):
        # Determine file names ...
        fname = f"greatCircle{i:d}.png"

        print(f" > Making \"{fname}\" ...")

        # Create figure ...
        fg = matplotlib.pyplot.figure(figsize = (6, 3), dpi = 150)

        # Create axis ...
        ax = fg.add_subplot(1, 1, 1, projection = cartopy.crs.Robinson())

        # Configure axis ...
        ax.set_global()
        pyguymer3.geo.add_map_background(ax)
        ax.coastlines(resolution = "110m", color = "black", linewidth = 0.1)

        # Loop over number of points ...
        for c, npoint in enumerate(npoints):
            # Find all the points on the great circle ...
            lon, lat = pyguymer3.geo.great_circle(coord1[0], coord1[1], coord2[0], coord2[1], npoint = npoint)  # [°], [°]

            # Loop over points ...
            for ipoint in range(npoint):
                # Transform point ...
                # NOTE: See https://stackoverflow.com/a/52861074
                lon[ipoint], lat[ipoint] = cartopy.crs.Robinson().transform_point(lon[ipoint], lat[ipoint], cartopy.crs.Geodetic())

            # Plot great circle ...
            ax.plot(lon, lat, transform = cartopy.crs.Robinson(), linewidth = 1.0, color = matplotlib.pyplot.cm.rainbow(float(c) / float(len(npoints) - 1)))

        # Save figure ...
        fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
        matplotlib.pyplot.close(fg)

        # Optimize figure ...
        pyguymer3.image.optimize_image(fname, strip = True)
