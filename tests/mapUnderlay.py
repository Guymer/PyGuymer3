#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.11/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “geo.add_map_underlay()”.

    # Import standard modules ...
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
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

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
                     index = iresolution + 1,
                     ncols = 1,
                     nrows = 3,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_map_underlay(ax, cultural = False, linewidth = 0.0, resolution = resolution)

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(fname, strip = True)

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
                      dist = 1000.0e3,
                     index = iresolution + 1,
                       lat = +40.0,
                       lon =   0.0,
                     ncols = 1,
                     nrows = 3,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_map_underlay(ax, cultural = False, resolution = resolution)

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(fname, strip = True)

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
                      dist = 25.0e3,
                     index = iresolution + 1,
                       lat = +51.5,
                       lon =   0.0,
                     ncols = 1,
                     nrows = 3,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_map_underlay(ax, resolution = resolution)

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(fname, strip = True)

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
                      dist = 50.0e3,
                     index = iresolution + 1,
                       lat = +60.5,
                       lon =  +7.5,
                     ncols = 1,
                     nrows = 3,
        )

        # Configure axis ...
        ax.set_title(resolution)
        pyguymer3.geo.add_map_underlay(ax, maxElev = 2000.0, resolution = resolution)

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(fname)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(fname, strip = True)
