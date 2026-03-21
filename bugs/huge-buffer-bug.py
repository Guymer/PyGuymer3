#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import os

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
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Demonstrate a bug when buffering by huge distances.",
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

    # Configure functions ...
    fill = -1.0                                                                 # [°]
    fillSpace = "EuclideanSpace"
    simp = -1.0                                                                 # [°]

    # Create point ...
    pnt = shapely.geometry.point.Point(lon, lat)

    # **************************************************************************

    # Create short-hand ...
    pName = f'{__file__.removesuffix(".py")}.png'

    # Create figure ...
    fg = matplotlib.pyplot.figure()

    # Create axis ...
    ax = fg.add_subplot()

    # Loop over number of angles ...
    for i in range(3, 16):
        # Create short-hands ...
        nAng = pow(2, i) + 1                                                    # [#]
        cName = f'{__file__.removesuffix(".py")}_nAng={nAng:d}.csv'

        print(f"Processing {nAng:,d} angles ...")

        # **********************************************************************

        # Check if the CSV is missing ...
        if not os.path.exists(cName):
            print(f"  Making \"{cName}\" ...")

            # Open CSV ...
            with open(cName, "wt", encoding = "utf-8") as fObj:
                # Write header ...
                fObj.write("buffer distance [km],buffer Euclidean area [°2]\n")

                # Loop over distances ...
                for dist in range(9986 - 10, 10001 + 11, 1):
                    # Create short-hand ...
                    huge = bool(float(1000 * dist) > 0.25 * pyguymer3.CIRCUMFERENCE_OF_EARTH)

                    print(f"    Processing {dist:,d} km ({huge}) ...")

                    # Buffer Point and append values to list ...
                    buff = pyguymer3.geo.buffer(
                        pnt,
                        float(1000 * dist),
                            debug = args.debug,
                              eps = args.eps,
                             fill = fill,
                        fillSpace = fillSpace,
                             nAng = nAng,
                            nIter = args.nIter,
                         ramLimit = args.ramLimit,
                             simp = simp,
                              tol = args.tol,
                    )

                    # Write data ...
                    fObj.write(f"{dist:d},{buff.area:.15e}\n")

        # **********************************************************************

        print(f"  Loading \"{cName}\" ...")

        # Load data ...
        dists, areas = numpy.loadtxt(
            cName,
            delimiter = ",",
             skiprows = 1,
               unpack = True,
        )                                                                       # [km], [°2]

        # Plot data ...
        ax.plot(
            dists,
            areas,
            label = f"{nAng:,d} angles",
        )

    # Configure axis ...
    ax.grid()
    ax.legend(loc = "upper left")
    ax.set_xlabel("Buffer Distance [km]")
    ax.set_ylabel("Buffer Area [°2]")

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
