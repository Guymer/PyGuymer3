#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “geo.add_NE_map_underlay()”.

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
            description = "Demonstrate some Natural Earth mapu underlays.",
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

    # Define resolutions ...
    resolutions = [
         "10m",
         "50m",
        "110m",
    ]

    # Make output directory ...
    if not os.path.exists("mapUnderlay"):
        os.mkdir("mapUnderlay")

    # **************************************************************************

    # Determine file name ...
    fname = "mapUnderlay/mapUnderlay0.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (4.8, 7.2))

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
            add_coastlines = False,
             add_gridlines = True,
                     debug = args.debug,
                     index = iresolution + 1,
                     ncols = 1,
                     nIter = None,
                     nrows = 3,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_NE_map_underlay(
            ax,
              cultural = False,
                 debug = args.debug,
             linewidth = 0.0,
               maxElev = 8850.0,
              physical = True,
            resolution = resolution,
        )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(
        fname,
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )

    # **************************************************************************

    # Determine file name ...
    fname = "mapUnderlay/mapUnderlay1.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (2.4, 7.2))

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
            add_coastlines = False,
             add_gridlines = True,
                     debug = args.debug,
                      dist = 1000.0e3,
                     index = iresolution + 1,
                       lat = +40.0,
                       lon =   0.0,
                     ncols = 1,
                     nIter = 100,
                     nrows = 3,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_NE_map_underlay(
            ax,
              cultural = False,
                 debug = args.debug,
             linewidth = 0.5,
               maxElev = 8850.0,
              physical = True,
            resolution = resolution,
        )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(
        fname,
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )

    # **************************************************************************

    # Determine file name ...
    fname = "mapUnderlay/mapUnderlay2.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (2.4, 7.2))

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
            add_coastlines = False,
             add_gridlines = True,
                     debug = args.debug,
                      dist = 25.0e3,
                     index = iresolution + 1,
                       lat = +51.5,
                       lon =   0.0,
                     ncols = 1,
                     nIter = 100,
                     nrows = 3,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_NE_map_underlay(
            ax,
              cultural = True,
                 debug = args.debug,
             linewidth = 0.5,
               maxElev = 8850.0,
              physical = True,
            resolution = resolution,
        )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(
        fname,
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )

    # **************************************************************************

    # Determine file name ...
    fname = "mapUnderlay/mapUnderlay3.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (2.4, 7.2))

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
            add_coastlines = False,
             add_gridlines = True,
                     debug = args.debug,
                      dist = 50.0e3,
                     index = iresolution + 1,
                       lat = +60.5,
                       lon =  +7.5,
                     ncols = 1,
                     nIter = 100,
                     nrows = 3,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_NE_map_underlay(
            ax,
              cultural = True,
                 debug = args.debug,
             linewidth = 0.5,
               maxElev = 2000.0,
              physical = True,
            resolution = resolution,
        )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(
        fname,
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )
