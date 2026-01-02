#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “geo.add_NE_map()”.

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

    # Define resolutions ...
    resolutions = [
         "10m",
         "50m",
        "110m",
    ]

    # Create short-hand and make output directory ...
    dName = f'{args.absPathToRepo}/tests/{os.path.basename(__file__).removesuffix(".py")}'
    if not os.path.exists(dName):
        os.makedirs(dName)

    # **************************************************************************

    # Determine file name ...
    fname = f"{dName}/mapUnderlay0.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(
            dpi = 100,                                      # NOTE: Reduce DPI to make test quicker.
        figsize = (12.8, 3 * 7.2),
    )

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
            add_coastlines = False,
             add_gridlines = True,
                     debug = args.debug,
                       eps = args.eps,
                     index = iresolution + 1,
                     ncols = 1,
                     nIter = None,
                     nrows = 3,
                       tol = args.tol,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_NE_map(
            ax,
              cultural = False,
                 debug = args.debug,
             linewidth = 0.0,
               maxElev = 8850,
              physical = True,
            resolution = resolution,
        )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimise PNG ...
    pyguymer3.image.optimise_image(
        fname,
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )

    # **************************************************************************

    # Determine file name ...
    fname = f"{dName}/mapUnderlay1.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(
            dpi = 100,                                      # NOTE: Reduce DPI to make test quicker.
        figsize = (7.2, 3 * 7.2),
    )

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
            add_coastlines = False,
             add_gridlines = True,
                     debug = args.debug,
                      dist = 1000.0e3,
                       eps = args.eps,
                     index = iresolution + 1,
                       lat = +40.0,
                       lon =   0.0,
                     ncols = 1,
                     nIter = 100,
                     nrows = 3,
                       tol = args.tol,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_NE_map(
            ax,
              cultural = False,
                 debug = args.debug,
             linewidth = 0.5,
               maxElev = 8850,
              physical = True,
            resolution = resolution,
        )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimise PNG ...
    pyguymer3.image.optimise_image(
        fname,
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )

    # **************************************************************************

    # Determine file name ...
    fname = f"{dName}/mapUnderlay2.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(
            dpi = 100,                                      # NOTE: Reduce DPI to make test quicker.
        figsize = (7.2, 3 * 7.2),
    )

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
            add_coastlines = False,
             add_gridlines = True,
                     debug = args.debug,
                      dist = 25.0e3,
                       eps = args.eps,
                     index = iresolution + 1,
                       lat = +51.5,
                       lon =   0.0,
                     ncols = 1,
                     nIter = 100,
                     nrows = 3,
                       tol = args.tol,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_NE_map(
            ax,
              cultural = True,
                 debug = args.debug,
             linewidth = 0.5,
               maxElev = 8850,
              physical = True,
            resolution = resolution,
        )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimise PNG ...
    pyguymer3.image.optimise_image(
        fname,
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )

    # **************************************************************************

    # Determine file name ...
    fname = f"{dName}/mapUnderlay3.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(
            dpi = 100,                                      # NOTE: Reduce DPI to make test quicker.
        figsize = (7.2, 3 * 7.2),
    )

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
            add_coastlines = False,
             add_gridlines = True,
                     debug = args.debug,
                      dist = 50.0e3,
                       eps = args.eps,
                     index = iresolution + 1,
                       lat = +60.5,
                       lon =  +7.5,
                     ncols = 1,
                     nIter = 100,
                     nrows = 3,
                       tol = args.tol,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_NE_map(
            ax,
              cultural = True,
                 debug = args.debug,
             linewidth = 0.5,
               maxElev = 2000,
              physical = True,
            resolution = resolution,
        )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimise PNG ...
    pyguymer3.image.optimise_image(
        fname,
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )
