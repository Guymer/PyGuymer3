#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “geo.add_NE_map()” and “geo.add_NE_tiles()”.

    # Import standard modules ...
    import argparse
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
            description = "Demonstrate some Natural Earth map underlays.",
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
        "--eps",
        default = 1.0e-12,
           dest = "eps",
           help = "the tolerance of the Vincenty formula iterations",
           type = float,
    )
    parser.add_argument(
        "--nIter",
        default = 1000000,
           dest = "nIter",
           help = "the maximum number of iterations (particularly the Vincenty formula)",
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

    # Define resolutions and scales ...
    resolutions = [
         "10m",
         "50m",
        "110m",
    ]
    scales = {
         "10m" : "08km",
         "50m" : "16km",
        "110m" : "32km",
    }

    # Create short-hand and make output directory ...
    dName = f'{args.absPathToRepo}/tests/{os.path.basename(__file__).removesuffix(".py")}'
    if not os.path.exists(dName):
        os.makedirs(dName)

    # **************************************************************************

    # Loop over plots ...
    for iPlot, (dist, lat, lon, cultural, linewidth, maxElev, grid) in enumerate(
        [
            (   1.0e99,  None, None, False, 0.0, 8000,  "18x9" ),
            (1000.0e3 , +40.0,  0.0, False, 0.5, 8000,  "72x36"),
            (  25.0e3 , +51.5,  0.0,  True, 0.5, 8000, "144x72"),
            (  50.0e3 , +60.5, +7.5,  True, 0.5, 2000, "144x72"),
        ]
    ):
        # Determine file name ...
        pName = f"{dName}/mapNeUnderlay{iPlot}.png"

        print(f"Making \"{pName}\" ...")

        # Create short-hands ...
        if lon is not None and lat is not None:
            print("  Calculating field-of-view ...")
            fov = pyguymer3.geo.buffer(
                shapely.geometry.point.Point(lon, lat),
                dist,
                debug = args.debug,
                  eps = args.eps,
                 fill = -1.0,
                 nAng = 361,
                nIter = args.nIter,
                 simp = -1.0,
                  tol = args.tol,
            )
        else:
            print("  Defining planet Earth ...")
            fov = shapely.geometry.polygon.orient(
                shapely.geometry.polygon.Polygon(
                    shapely.geometry.polygon.LinearRing(
                        [
                            (-180.0,  90.0),
                            (+180.0,  90.0),
                            (+180.0, -90.0),
                            (-180.0, -90.0),
                            (-180.0,  90.0),
                        ]
                    )
                )
            )
        pyguymer3.geo.check(fov)

        # Create figure ...
        fg = matplotlib.pyplot.figure(
                dpi = 100,              # NOTE: Reduce DPI to make test quicker.
            figsize = (2 * 9.6, 3 * 7.2),
        )

        # Create short-hand ...
        regrid_shape = (
            round(2.0 * (fg.get_figwidth() / 2.0) * fg.get_dpi()),
            round(2.0 * (fg.get_figheight() / 3.0) * fg.get_dpi()),
        )                                                                       # [px], [px]

        # **********************************************************************

        # Loop over resolutions ...
        for iResolution, resolution in enumerate(resolutions):
            print(f"  Plotting \"{resolution}\" and \"{scales[resolution]}\" ...")

            # Create axis ...
            ax = pyguymer3.geo.add_axis(
                fg,
                add_coastlines = False,
                 add_gridlines = True,
                         debug = args.debug,
                          dist = dist,
                           eps = args.eps,
                           fov = fov,
                         index = 2 * iResolution + 1,
                           lat = lat,
                           lon = lon,
                         ncols = 2,
                         nIter = args.nIter,
                         nrows = 3,
                     onlyValid = True,
                        repair = True,
                           tol = args.tol,
            )

            # Configure axis ...
            ax.set_title(f"\"add_NE_map()\" at \"{resolution}\" and \"{scales[resolution]}\"")
            pyguymer3.geo.add_NE_map(
                ax,
                  cultural = cultural,
                     debug = args.debug,
                       fov = fov,
                 linewidth = linewidth,
                   maxElev = maxElev,
                 onlyValid = True,
                    repair = True,
                resolution = resolution,
                     scale = scales[resolution],
            )

            # ******************************************************************

            print(f"  Plotting \"{resolution}\" and \"{grid}\" (with \"regrid_shape = ({regrid_shape[0]:d},{regrid_shape[1]:d})\") ...")

            # Create axis ...
            ax = pyguymer3.geo.add_axis(
                fg,
                add_coastlines = False,
                 add_gridlines = True,
                         debug = args.debug,
                          dist = dist,
                           eps = args.eps,
                           fov = fov,
                         index = 2 * iResolution + 2,
                           lat = lat,
                           lon = lon,
                         ncols = 2,
                         nIter = args.nIter,
                         nrows = 3,
                     onlyValid = True,
                        repair = True,
                           tol = args.tol,
            )

            # Configure axis ...
            ax.set_title(f"\"add_NE_tiles()\" at \"{resolution}\" and \"{grid}\" (\"regrid_shape = ({regrid_shape[0]:d},{regrid_shape[1]:d})\")")
            pyguymer3.geo.add_NE_tiles(
                ax,
                fov,
                         debug = args.debug,
                          grid = grid,
                 interpolation = "gaussian",
                       maxElev = maxElev,
                mergedTileName = f'{pName.removesuffix(".png")}_{resolution}.png',
                  regrid_shape = regrid_shape,
                      resample = False,
                    resolution = resolution,
                       timeout = args.timeout,
            )

        # **********************************************************************

        # Configure figure ...
        fg.tight_layout()

        print("  Saving ...")

        # Save figure ...
        fg.savefig(pName)
        matplotlib.pyplot.close(fg)

        # Optimise PNG ...
        pyguymer3.image.optimise_image(
            pName,
              debug = args.debug,
              strip = True,
            timeout = args.timeout,
        )
