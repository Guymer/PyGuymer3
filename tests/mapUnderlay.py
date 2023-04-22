#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.11/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “geo.add_map_underlay()”.

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                   "backend" : "Agg",                                           # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                "figure.dpi" : 300,
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

    # Define resolutions ...
    resolutions = [
         "10m",
         "50m",
        "110m",
    ]

    # **************************************************************************

    # Determine file name ...
    fname = "mapUnderlay0.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 9))

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = fg.add_subplot(3, 1, iresolution + 1, projection = cartopy.crs.Robinson())

        # Configure axis ...
        ax.set_global()
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
    fname = "mapUnderlay1.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 18))

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_top_down_axis(
            fg,
              0.0,
            +40.0,
            1000.0e3,
            nrows = 3,
            ncols = 1,
            index = iresolution + 1,
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
    fname = "mapUnderlay2.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 18))

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_top_down_axis(
            fg,
              0.0,
            +51.5,
            25.0e3,
            nrows = 3,
            ncols = 1,
            index = iresolution + 1,
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
    fname = "mapUnderlay3.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 18))

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = pyguymer3.geo.add_top_down_axis(
            fg,
             +7.5,
            +60.5,
            50.0e3,
            nrows = 3,
            ncols = 1,
            index = iresolution + 1,
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
