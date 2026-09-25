#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import math
    import os
    import shutil

    # Import special modules ...
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "axes.xmargin" : 0.01,
                       "axes.ymargin" : 0.01,
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",                                 # NOTE: See https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html
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
        import shapely.ops
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
            description = "Demonstrate a bug in \"shapely.ops.voronoi_diagram\".",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--chunksize",
        default = 1048576,
           help = "the size of the chunks of any files which are read in (in bytes)",
           type = int,
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
        "--exiftool-path",
        default = shutil.which("exiftool"),
           dest = "exiftoolPath",
           help = "the path to the \"exiftool\" binary",
           type = str,
    )
    parser.add_argument(
        "--gifsicle-path",
        default = shutil.which("gifsicle"),
           dest = "gifsiclePath",
           help = "the path to the \"gifsicle\" binary",
           type = str,
    )
    parser.add_argument(
        "--jpegtran-path",
        default = shutil.which("jpegtran"),
           dest = "jpegtranPath",
           help = "the path to the \"jpegtran\" binary",
           type = str,
    )
    parser.add_argument(
        "--nAng",
        default = 41,
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
        "--optipng-path",
        default = shutil.which("optipng"),
           dest = "optipngPath",
           help = "the path to the \"optipng\" binary",
           type = str,
    )
    parser.add_argument(
        "--RAM-limit",
        default = 1073741824,
           dest = "ramLimit",
           help = "the maximum RAM usage of each \"large\" array (in bytes)",
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

    # Define starting location ...
    lon = 0.0                                                                   # [°]
    lat = 0.0                                                                   # [°]

    # Create point ...
    pnt = shapely.geometry.point.Point(lon, lat)

    # Define buffering distance ...
    dist = 1000.0                                                               # [m]

    # **************************************************************************

    # Buffer Point ...
    buff = pyguymer3.geo.buffer(
        pnt,
        dist,
           debug = args.debug,
             eps = args.eps,
            fill = -1.0,
            nAng = args.nAng,
           nIter = args.nIter,
        ramLimit = args.ramLimit,
            simp = -1.0,
             tol = args.tol,
    )

    # Create short-hand ...
    pName = f'{__file__.removesuffix(".py")}.png'

    # Create figure and axes ...
    fg = matplotlib.pyplot.figure(figsize = (2 * 7.2, 2 * 7.2))
    ax = fg.subplots(7, 7).flatten()

    # Make the Voronoi diagram ...
    voro = shapely.ops.voronoi_diagram(buff)

    # Loop over Polygons in the Voronoi diagram ...
    for iVoroPoly, voroPoly in enumerate(
        pyguymer3.geo.extract_polys(
            voro,
            onlyValid = True,
               repair = False,
        )
    ):
        print(f"iVoroPoly = {iVoroPoly:2d} :: voroPoly area = {voroPoly.area:8.6f}°²")

        # Plot the current Polygon in the buffer of the Point ...
        coords = numpy.array(buff.exterior.coords)                              # [°]
        ax[iVoroPoly].plot(
            coords[:, 0],
            coords[:, 1],
            color = "C0",
        )

        # Plot the current Polygon in the Voronoi diagram ...
        coords = numpy.array(voroPoly.exterior.coords)                          # [°]
        ax[iVoroPoly].plot(
            coords[:, 0],
            coords[:, 1],
            color = "C1",
        )

        # Configure axis ...
        ax[iVoroPoly].grid()
        ax[iVoroPoly].set_aspect("equal")
        ax[iVoroPoly].set_title(f"iVoroPoly = {iVoroPoly:d}")
        ax[iVoroPoly].set_xlabel("Longitude [°]")
        ax[iVoroPoly].set_xlim(-0.03, +0.03)
        ax[iVoroPoly].set_xticks(
            [-0.03, -0.02, -0.01, 0.0, +0.01, +0.02, +0.03],
            labels = ["-0.03", "", "", "0.0", "", "", "+0.03"],
        )
        ax[iVoroPoly].set_ylabel("Latitude [°]")
        ax[iVoroPoly].set_ylim(-0.03, +0.03)
        ax[iVoroPoly].set_yticks(
            [-0.03, -0.02, -0.01, 0.0, +0.01, +0.02, +0.03],
            labels = ["-0.03", "", "", "0.0", "", "", "+0.03"],
        )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(pName)
    matplotlib.pyplot.close(fg)

    # Optimise PNG ...
    pyguymer3.image.optimise_image(
        pName,
           chunksize = args.chunksize,
               debug = args.debug,
        exiftoolPath = args.exiftoolPath,
        gifsiclePath = args.gifsiclePath,
        jpegtranPath = args.jpegtranPath,
         optipngPath = args.optipngPath,
               strip = True,
             timeout = args.timeout,
    )
