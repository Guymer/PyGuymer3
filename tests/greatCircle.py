#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “geo.great_circle()”.

    # Import standard modules ...
    import argparse
    import json
    import multiprocessing
    import os
    import pathlib

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy_cache").expanduser(),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import geojson
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
        "--absolute-path-to-repository",
        default = os.path.dirname(os.path.dirname(__file__)),
           dest = "absPathToRepo",
           help = "the absolute path to the PyGuymer3 repository",
           type = str,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--dont-make-plots",
        action = "store_true",
          dest = "dontMakePlots",
          help = "don't make the plots (only make the [Geo]JSONs)",
    )
    parser.add_argument(
        "--eps",
        default = 1.0e-12,
           dest = "eps",
           help = "the tolerance of the Vincenty formula iterations",
           type = float,
    )
    parser.add_argument(
        "--number-of-children",
        default = os.cpu_count(),       # TODO: Once I ditch Python 3.11 and
                                        #       Python 3.12 then I can use
                                        #       "os.process_cpu_count()" instead.
           dest = "nChild",
           help = "the number of child \"multiprocessing\" processes to use when making the tiles",
           type = int,
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    parser.add_argument(
        "--tolerance",
        default = 1.0e-10,
           dest = "tol",
           help = "the Euclidean distance that defines two points as being the same (in degrees)",
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

    # Create short-hand and make output directory ...
    dName = f'{args.absPathToRepo}/tests/{os.path.basename(__file__).removesuffix(".py")}'
    if not os.path.exists(dName):
        os.makedirs(dName)

    # **************************************************************************

    # Create a pool of workers ...
    with multiprocessing.Pool(args.nChild) as pObj:
        # Loop over tests ...
        for i, (coord1, coord2) in enumerate(zip(coords1, coords2, strict = True)):
            # Determine file name ...
            pname = f"{dName}/greatCircle{i:d}.png"

            print(f" > Making \"{pname}\" ...")

            # Check that the user wants to make plots ...
            if not args.dontMakePlots:
                # Create figure ...
                fg = matplotlib.pyplot.figure(
                        dpi = 100,                          # NOTE: Reduce DPI to make test quicker.
                    figsize = (12.8, 7.2),
                )

                # Create axis ...
                ax = pyguymer3.geo.add_axis(
                    fg,
                        add_coastlines = False,             # NOTE: Do not draw coastlines so that changes in GSHGG do not change the image.
                         add_gridlines = True,
                                 debug = args.debug,
                                   eps = args.eps,
                                 nIter = None,
                                   tol = args.tol,
                )

                # Configure axis ...
                pyguymer3.geo.add_map_background(
                    ax,
                         debug = args.debug,
                    resolution = "large1024px",             # NOTE: Reduce size to make test quicker.
                )

            # Loop over number of points ...
            for c, npoint in enumerate(npoints):
                # Determine file name ...
                jname = f"{dName}/greatCircle{i:d}_{c:d}.geojson"

                print(f"   > Making \"{jname}\" ...")

                # Find the great circle ...
                circle = pyguymer3.geo.great_circle(
                    coord1[0],
                    coord1[1],
                    coord2[0],
                    coord2[1],
                      debug = args.debug,
                        eps = args.eps,
                    maxdist = None,
                      nIter = None,
                     npoint = npoint,
                )

                # Save GeoJSON ...
                # NOTE: As of 4/Aug/2025, the Python module "geojson" just
                #       converts the object to a Python dictionary and then it
                #       just calls the standard "json.dump()" function to format
                #       the Python dictionary as text. There is no way to
                #       specify the precision of the written string.
                #       Fortunately, if you have no shame, then you can load and
                #       then dump the string again, see:
                #         * https://stackoverflow.com/a/29066406
                with open(jname, "wt", encoding = "utf-8") as fObj:
                    json.dump(
                        json.loads(
                            geojson.dumps(
                                circle,
                                ensure_ascii = False,
                                      indent = 4,
                                   sort_keys = True,
                            ),
                            parse_float = lambda x: round(float(x), 4),         # NOTE: 0.0001° is approximately 11.1 m.
                        ),
                        fObj,
                        ensure_ascii = False,
                              indent = 4,
                           sort_keys = True,
                    )

                # Check that the user wants to make plots ...
                if not args.dontMakePlots:
                    # Loop over lines in the great circle ...
                    for line in pyguymer3.geo.extract_lines(circle):
                        # Extract the coordinates from this line ...
                        coords = numpy.array(line.coords)                       # [°]

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

            # Check that the user wants to make plots ...
            if not args.dontMakePlots:
                # Configure figure ...
                fg.tight_layout()

                # Save figure ...
                fg.savefig(pname)
                matplotlib.pyplot.close(fg)

                # Optimise figure ...
                pyguymer3.image.optimise_image(
                    pname,
                      debug = args.debug,
                       pool = pObj,
                      strip = True,
                    timeout = args.timeout,
                )

        # Close the pool of worker processes and wait for all of the tasks to
        # finish ...
        # NOTE: The "__exit__()" call of the context manager for
        #       "multiprocessing.Pool()" calls "terminate()" instead of
        #       "join()", so I must manage the end of the pool of worker
        #       processes myself.
        pObj.close()
        pObj.join()
