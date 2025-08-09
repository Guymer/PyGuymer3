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
            description = "Plot the limit of the Vincenty formula.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          dest = "debug",
          help = "print debug messages",
    )
    parser.add_argument(
        "--degree-interval",
        default = 45,
           dest = "degInt",
           help = "the longitude/latitude interval to survey (in degrees)",
           type = int,
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

    # Find the absolute path to the repository ...
    absPathToRepo = os.path.dirname(os.path.dirname(__file__))

    # Check input ...
    # NOTE: The possible values are:
    #         *  2 × 90
    #         *  3 × 60
    #         *  4 × 45
    #         *  5 × 36
    #         *  6 × 30
    #         *  9 × 20
    #         * 10 × 18
    #         * 12 × 15
    #         * 15 × 12
    #         * 18 × 10
    #         * 20 ×  9
    #         * 30 ×  6
    #         * 36 ×  5
    #         * 45 ×  4
    #         * 60 ×  3
    #         * 90 ×  2
    assert 180 % args.degInt == 0, "180 must be able to be divided by \"--degree-interval\" without a remainder"

    # **************************************************************************

    # Create longitude/latitude axes ...
    lats = numpy.linspace(
        -90.0,
        +90.0,
        num = 1 + 180 // args.degInt,
    )                                                                           # [°]
    lons = numpy.linspace(
        -180.0,
        +180.0,
        num = 1 + 360 // args.degInt,
    )                                                                           # [°]

    # Create maximum distance array ...
    maxDist = numpy.zeros(
        (lats.size - 1, lons.size - 1),
        dtype = numpy.uint16,
    )                                                                           # [km]

    # Loop over latitudes ...
    for iLat in range(lats.size - 1):
        # Loop over longitudes ...
        for iLon in range(lons.size - 1):
            # Create the initial starting Point ...
            ship = shapely.geometry.point.Point(
                0.5 * (lons[iLon] + lons[iLon + 1]),
                0.5 * (lats[iLat] + lats[iLat + 1]),
            )

            # Start an ~infinite loop ...
            for dist in range(20015 - 100, 20015 + 100):
                # Try to calculate the maximum distance the ship could have got
                # to ...
                try:
                    maxShip = pyguymer3.geo.buffer(
                        ship,
                        1000.0 * float(dist),
                        debug = args.debug,
                          eps = args.eps,
                         fill = -1.0,
                         nAng = 361,
                        nIter = args.nIter,
                         simp = -1.0,
                          tol = args.tol,
                    )
                except Exception:
                    break

                # Update maximum distance array ...
                maxDist[iLat, iLon] = dist                                      # [km]

    # **************************************************************************

    print(f"The minimum maximum distance which the Vincenty formula works is {maxDist.min():,d} km.")
    print(f"The maximum maximum distance which the Vincenty formula works is {maxDist.max():,d} km.")

    # **************************************************************************

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (12.8, 7.2))

    # Create axis ...
    ax = fg.add_subplot()

    # Plot data ...
    im = ax.pcolormesh(
        lons,
        lats,
        maxDist,
           cmap = "turbo",
        shading = "flat",
         zorder = 1.0,
    )
    ax.contour(
        0.5 * (lons[:-1] + lons[1:]),
        0.5 * (lats[:-1] + lats[1:]),
        maxDist,
        colors = "white",
        levels = numpy.linspace(
            maxDist.min() +  2,
            maxDist.min() + 20,
            num = 10,
        ),
        zorder = 1.5,
    )

    # Add colour bar ...
    cb = fg.colorbar(
        im,
                 ax = ax,
        orientation = "vertical",
    )

    # Configure colour bar ...
    cb.set_label("Geodesic Distance [km]")

    # Configure axis ...
    ax.grid()
    ax.set_aspect("equal")
    ax.set_title("How Far Can You Push Vincenty?")
    ax.set_xlabel("Longitude [°]")
    ax.set_xlim(-180.0, +180.0)
    ax.set_xticks(range(-180, +225, 45))
    ax.set_ylabel("Latitude [°]")
    ax.set_ylim(-90.0, +90.0)
    ax.set_yticks(range(-90, +135, 45))

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(f"{absPathToRepo}/scripts/plotLimitOfVincenty.png")
    matplotlib.pyplot.close(fg)

    # Optimise PNG ...
    pyguymer3.image.optimise_image(
        f"{absPathToRepo}/scripts/plotLimitOfVincenty.png",
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )
