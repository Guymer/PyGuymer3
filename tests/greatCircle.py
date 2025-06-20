#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “geo.great_circle()”.

    # Import standard modules ...
    import argparse
    import os

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : os.path.expanduser("~/.local/share/cartopy_cache"),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import geojson
        geojson.geometry.Geometry.__init__.__defaults__ = (None, False, 12)     # NOTE: See https://github.com/jazzband/geojson/issues/135#issuecomment-596509669
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "backend" : "Agg",                                       # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                    "figure.dpi" : 300,
                "figure.figsize" : (9.6, 7.2),                                  # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                     "font.size" : 8,
            }
        )
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
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Demonstrate the convergence of some great circles.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

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

    # Make output directory ...
    if not os.path.exists("greatCircle"):
        os.mkdir("greatCircle")

    # **************************************************************************

    # Loop over tests ...
    for i, (coord1, coord2) in enumerate(zip(coords1, coords2, strict = True)):
        # Determine file name ...
        pname = f"greatCircle/greatCircle{i:d}.png"

        print(f" > Making \"{pname}\" ...")

        # Create figure ...
        fg = matplotlib.pyplot.figure(figsize = (12.8, 7.2))

        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
                   add_coastlines = True,
                    add_gridlines = True,
            coastlines_resolution = "c",
                            debug = args.debug,
                            nIter = None,
        )

        # Configure axis ...
        pyguymer3.geo.add_map_background(
            ax,
                 debug = args.debug,
            resolution = "large2048px",
        )

        # Loop over number of points ...
        for c, npoint in enumerate(npoints):
            # Determine file name ...
            jname = f"greatCircle/greatCircle{i:d}_{c:d}.geojson"

            print(f"   > Making \"{jname}\" ...")

            # Find the great circle ...
            circle = pyguymer3.geo.great_circle(
                coord1[0],
                coord1[1],
                coord2[0],
                coord2[1],
                  debug = args.debug,
                maxdist = None,
                  nIter = None,
                 npoint = npoint,
            )

            # Save GeoJSON ...
            with open(jname, "wt", encoding = "utf-8") as fObj:
                geojson.dump(
                    circle,
                    fObj,
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
                        color = matplotlib.colormaps["turbo"](float(c) / float(len(npoints) - 1)),
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

        # Configure figure ...
        fg.tight_layout()

        # Save figure ...
        fg.savefig(pname)
        matplotlib.pyplot.close(fg)

        # Optimize figure ...
        pyguymer3.image.optimize_image(
            pname,
              debug = args.debug,
              strip = True,
            timeout = args.timeout,
        )
