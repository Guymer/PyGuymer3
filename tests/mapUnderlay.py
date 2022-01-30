#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.9/library/multiprocessing.html#multiprocessing-programming
if __name__ == "__main__":
    # This is a test suite for "geo.add_map_underlay()”.

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
    fg = matplotlib.pyplot.figure(figsize = (6, 9), dpi = 150)

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = fg.add_subplot(
            3,
            1,
            iresolution + 1,
            projection = cartopy.crs.Robinson(),
        )

        # Configure axis ...
        ax.set_global()
        ax.set_title(resolution)
        pyguymer3.geo.add_map_underlay(ax, cultural = False, linewidth = 0.0, resolution = resolution)

    # Save figure ...
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(fname, strip = True)

    # **************************************************************************

    # Determine file name ...
    fname = "mapUnderlay1.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 18), dpi = 150)

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = fg.add_subplot(
            3,
            1,
            iresolution + 1,
            projection = cartopy.crs.Orthographic(
                central_longitude =  0.0,
                 central_latitude = 40.0,
            ),
        )

        # Configure axis ...
        ax.set_extent(
            [
                -15.0, # left
                 15.0, # right
                 30.0, # bottom
                 50.0, # top
            ]
        )
        ax.set_title(resolution)
        pyguymer3.geo.add_map_underlay(ax, cultural = False, resolution = resolution)

    # Save figure ...
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(fname, strip = True)

    # **************************************************************************

    # Determine file name ...
    fname = "mapUnderlay2.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 18), dpi = 150)

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = fg.add_subplot(
            3,
            1,
            iresolution + 1,
            projection = cartopy.crs.Orthographic(
                central_longitude =  0.0,
                 central_latitude = 51.5,
            ),
        )

        # Configure axis ...
        ax.set_extent(
            [
                -0.4, # left
                 0.4, # right
                51.3, # bottom
                51.7, # top
            ]
        )
        ax.set_title(resolution)
        pyguymer3.geo.add_map_underlay(ax, resolution = resolution)

    # Save figure ...
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(fname, strip = True)

    # **************************************************************************

    # Determine file name ...
    fname = "mapUnderlay3.png"

    print(f" > Making \"{fname}\" ...")

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (6, 18), dpi = 150)

    # Loop over resolutions ...
    for iresolution, resolution in enumerate(resolutions):
        # Create axis ...
        ax = fg.add_subplot(
            3,
            1,
            iresolution + 1,
            projection = cartopy.crs.Orthographic(
                central_longitude =  7.5,
                 central_latitude = 60.6,
            ),
        )

        # Configure axis ...
        ax.set_extent(
            [
                 6.5, # left
                 8.5, # right
                60.1, # bottom
                61.1, # top
            ]
        )
        ax.set_title(resolution)
        pyguymer3.geo.add_map_underlay(ax, resolution = resolution)

    # Save figure ...
    fg.savefig(fname, bbox_inches = "tight", dpi = 150, pad_inches = 0.1)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    pyguymer3.image.optimize_image(fname, strip = True)
