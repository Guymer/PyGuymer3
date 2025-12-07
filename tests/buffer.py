#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “geo.buffer()” with:
    #     A) a polygon that span the whole numerical range;
    #     B) a polygon that cross the equator;
    #     C) a polygon that cross the anti-meridian;
    #     D) a polygon that cross a pole;
    #     E) a polygon that cross both the equator and the anti-meridian; and
    #     F) a polygon that cross both a pole and the anti-meridian.
    # Each polygon has a plot with both a top-down projection and a Robinson
    # projection so that you can check it, along with an equirectangular plot.

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
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy").expanduser(),
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
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",
                     "image.resample" : False,
            }
        )
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
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Buffer a point twice.",
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
        "--nAng",
        default = 361,
           dest = "nAng",
           help = "the number of angles around each circle",
           type = int,
    )
    parser.add_argument(
        "--nIter",
        default = 1000000,
           dest = "nIter",
           help = "the maximum number of iterations (particularly the Vincenty formula)",
           type = int,
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
        "--quiet",
        action = "store_true",
          help = "don't print most messages",
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

    # Configure functions ...
    fill = 1.0                                                                  # [°]
    fillSpace = "EuclideanSpace"
    simp = -1.0                                                                 # [°]

    # Define polygons ...
    polys = [
        (-180.0, +90.0,  1000000.0,  900000.0), # Satisfies test A, C, D, F
        ( -90.0, +45.0,  1000000.0,  900000.0), # Satisfies test A
        (   0.0,   0.0,  1000000.0,  900000.0), # Satisfies test A, B
        ( +90.0, -45.0,  1000000.0,  900000.0), # Satisfies test A
        (+180.0, -90.0,  1000000.0,  900000.0), # Satisfies test A, C, D, F
        (+170.0, +10.0,  1000000.0, 4000000.0), # Satisfies test B, C, E
        (+170.0, +80.0,  1000000.0, 4000000.0), # Satisfies test C, D, F
        (   0.0, +83.0,  1000000.0,  900000.0), # Satisfies test C, D, F
        ( -90.0, -83.0,  1000000.0,  900000.0), # Satisfies test C, D, F
        (   0.0,   0.0, 10000000.0,    1500.0), # Satisfies test A, B
    ]

    # Create short-hand and make output directory ...
    dName = f'{args.absPathToRepo}/tests/{os.path.basename(__file__).removesuffix(".py")}'
    if not os.path.exists(dName):
        os.makedirs(dName)

    # **************************************************************************

    # Create a pool of workers ...
    with multiprocessing.Pool(args.nChild) as pObj:
        # Loop over polygons ...
        for i, (lon, lat, dist1, dist2) in enumerate(polys):
            # Determine file names ...
            fname = f"{dName}/buffer{i:d}.png"
            jname = f"{dName}/buffer{i:d}.geojson"

            if not args.quiet:
                print(f" > Making \"{jname}\" and \"{fname}\" ...")

            # Check that the user wants to make plots ...
            if not args.dontMakePlots:
                # Create figure ...
                fg = matplotlib.pyplot.figure(
                        dpi = 100,                          # NOTE: Reduce DPI to make test quicker.
                    figsize = (9.6, 7.2),
                )

                # Create axis ...
                ax1 = pyguymer3.geo.add_axis(
                    fg,
                    add_coastlines = False,                 # NOTE: Do not draw coastlines so that changes in GSHGG do not change the image.
                     add_gridlines = True,
                             debug = args.debug,
                               eps = args.eps,
                             index = 1,
                             ncols = 2,
                             nIter = args.nIter,
                             nrows = 2,
                               tol = args.tol,
                )

                # Configure axis ...
                pyguymer3.geo.add_map_background(
                    ax1,
                         debug = args.debug,
                    resolution = "large1024px",             # NOTE: Reduce size to make test quicker.
                )

                # Create axis ...
                ax2 = pyguymer3.geo.add_axis(
                    fg,
                    add_coastlines = False,                 # NOTE: Do not draw coastlines so that changes in GSHGG do not change the image.
                     add_gridlines = True,
                             debug = args.debug,
                               eps = args.eps,
                             index = 2,
                               lat = lat,
                               lon = lon,
                             ncols = 2,
                             nIter = args.nIter,
                             nrows = 2,
                               tol = args.tol,
                )

                # Configure axis ...
                pyguymer3.geo.add_map_background(
                    ax2,
                         debug = args.debug,
                    resolution = "large1024px",             # NOTE: Reduce size to make test quicker.
                )

                # Create axis ...
                ax3 = fg.add_subplot(
                    2,
                    2,
                    (3, 4),
                )

                # Configure axis ...
                ax3.grid()
                ax3.set_aspect("equal")
                ax3.set_xlabel("Longitude [°]")
                ax3.set_xlim(-180.0, +180.0)
                ax3.set_xticks(range(-180, 225, 45))
                ax3.set_ylabel("Latitude [°]")
                ax3.set_ylim(-90.0, +90.0)
                ax3.set_yticks(range(-90, 135, 45))

            # Create point ...
            point = shapely.geometry.point.Point(lon, lat)

            # Buffer Point ...
            buff0 = pyguymer3.geo.buffer(
                point,
                dist1 + dist2,
                    debug = args.debug,
                      eps = args.eps,
                     fill = fill,
                fillSpace = fillSpace,
                     nAng = args.nAng,
                    nIter = args.nIter,
                     simp = simp,
                      tol = args.tol,
            )

            # Check that the user wants to make plots ...
            if not args.dontMakePlots:
                # Plot Point thrice ...
                ax1.add_geometries(
                    pyguymer3.geo.extract_polys(buff0),
                    cartopy.crs.PlateCarree(),
                    edgecolor = (1.0, 0.0, 0.0, 1.0),
                    facecolor = "none",
                    linewidth = 1.0,
                )
                ax2.add_geometries(
                    pyguymer3.geo.extract_polys(buff0),
                    cartopy.crs.PlateCarree(),
                    edgecolor = (1.0, 0.0, 0.0, 1.0),
                    facecolor = "none",
                    linewidth = 1.0,
                )
                for poly in pyguymer3.geo.extract_polys(buff0):
                    coords = numpy.array(poly.exterior.coords)                  # [°]
                    ax3.plot(
                        coords[:, 0],
                        coords[:, 1],
                        color = (1.0, 0.0, 0.0, 1.0),
                    )

            # Buffer Point ...
            buff1 = pyguymer3.geo.buffer(
                point,
                dist1,
                    debug = args.debug,
                      eps = args.eps,
                     fill = fill,
                fillSpace = fillSpace,
                     nAng = args.nAng,
                    nIter = args.nIter,
                     simp = simp,
                      tol = args.tol,
            )

            # Check that the user wants to make plots ...
            if not args.dontMakePlots:
                # Plot Point thrice ...
                ax1.add_geometries(
                    pyguymer3.geo.extract_polys(buff1),
                    cartopy.crs.PlateCarree(),
                    edgecolor = (0.0, 1.0, 0.0, 1.0),
                    facecolor = "none",
                    linewidth = 1.0,
                )
                ax2.add_geometries(
                    pyguymer3.geo.extract_polys(buff1),
                    cartopy.crs.PlateCarree(),
                    edgecolor = (0.0, 1.0, 0.0, 1.0),
                    facecolor = "none",
                    linewidth = 1.0,
                )
                for poly in pyguymer3.geo.extract_polys(buff1):
                    coords = numpy.array(poly.exterior.coords)                  # [°]
                    ax3.plot(
                        coords[:, 0],
                        coords[:, 1],
                        color = (0.0, 1.0, 0.0, 1.0),
                    )

            # Buffer Polygon ...
            buff2 = pyguymer3.geo.buffer(
                buff1,
                dist2,
                    debug = args.debug,
                      eps = args.eps,
                     fill = fill,
                fillSpace = fillSpace,
                     nAng = args.nAng,
                    nIter = args.nIter,
                     simp = simp,
                      tol = args.tol,
            )

            # Check that the user wants to make plots ...
            if not args.dontMakePlots:
                # Plot Point thrice ...
                ax1.add_geometries(
                    pyguymer3.geo.extract_polys(buff2),
                    cartopy.crs.PlateCarree(),
                    edgecolor = (0.0, 0.0, 1.0, 1.0),
                    facecolor = (0.0, 0.0, 1.0, 0.5),
                    linewidth = 1.0,
                )
                ax2.add_geometries(
                    pyguymer3.geo.extract_polys(buff2),
                    cartopy.crs.PlateCarree(),
                    edgecolor = (0.0, 0.0, 1.0, 1.0),
                    facecolor = (0.0, 0.0, 1.0, 0.5),
                    linewidth = 1.0,
                )
                for poly in pyguymer3.geo.extract_polys(buff2):
                    coords = numpy.array(poly.exterior.coords)                  # [°]
                    ax3.plot(
                        coords[:, 0],
                        coords[:, 1],
                        color = (0.0, 0.0, 1.0, 1.0),
                    )

            # Save GeoJSON ...
            # NOTE: As of 4/Aug/2025, the Python module "geojson" just converts
            #       the object to a Python dictionary and then it just calls the
            #       standard "json.dump()" function to format the Python
            #       dictionary as text. There is no way to specify the precision
            #       of the written string. Fortunately, if you have no shame,
            #       then you can load and then dump the string again, see:
            #         * https://stackoverflow.com/a/29066406
            with open(jname, "wt", encoding = "utf-8") as fObj:
                json.dump(
                    json.loads(
                        geojson.dumps(
                            buff2,
                            ensure_ascii = False,
                                  indent = 4,
                               sort_keys = True,
                        ),
                        parse_float = lambda x: round(float(x), 4),             # NOTE: 0.0001° is approximately 11.1 m.
                    ),
                    fObj,
                    ensure_ascii = False,
                          indent = 4,
                       sort_keys = True,
                )

            # Check that the user wants to make plots ...
            if not args.dontMakePlots:
                # Configure figure ...
                fg.suptitle(f"({lon:.1f},{lat:.1f}) buffered by {0.001 * dist1:,.1f}km & {0.001 * dist2:,.1f}km\nred = {0.001 * (dist1 + dist2):,.1f}km; green = {0.001 * dist1:,.1f}km; blue = {0.001 * dist1:,.1f}km & {0.001 * dist2:,.1f}km")
                fg.tight_layout()

                # Save figure ...
                fg.savefig(fname)
                matplotlib.pyplot.close(fg)

                # Optimise PNG ..
                pyguymer3.image.optimise_image(
                    fname,
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
