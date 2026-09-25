#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import math
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
            description = "Demonstrate calculating the Geodesic area of a Polygon.",
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
        "--level",
        default = 1,
           dest = "level",
           help = "the number of levels to split shapes into when calculating their area",
           type = int,
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

    # Create short-hands ...
    dist = 1000.0                                                               # [m]
    area = math.pi * pow(dist, 2)                                               # [m²]

    # Create short-hand and make output directory ...
    dName = f'{args.absPathToRepo}/tests/{os.path.basename(__file__).removesuffix(".py")}'
    if not os.path.exists(dName):
        os.makedirs(dName)

    # **************************************************************************

    # Create short-hands ...
    lons = [float(lon) for lon in range(-179, 180)]                             # [°]
    lats = [float(lat) for lat in range(-89, 90)]                               # [°]

    # Check if the BIN file needs making ...
    if not os.path.exists(f"{dName}/fencePosts.bin"):
        print(f"Making \"{dName}/fencePosts.bin\" ...")

        # Initialize array to hold answer ...
        ratios = numpy.zeros(
            (len(lats), len(lons)),
            dtype = numpy.float64,
        )

        # Loop over longitudes ...
        for iLon, lon in enumerate(lons):
            # Loop over latitudes ...
            for iLat, lat in enumerate(lats):
                # Buffer Point ...
                buff = pyguymer3.geo.buffer(
                    shapely.geometry.point.Point(
                        lon,
                        lat,
                    ),
                    dist,
                    debug = args.debug,
                      eps = args.eps,
                     fill = -1.0,
                     nAng = args.nAng,
                    nIter = args.nIter,
                     simp = -1.0,
                      tol = args.tol,
                )

                # Populate array with the ratio of the estimated area to the
                # exact area ...
                ratios[iLat, iLon] = pyguymer3.geo.area(
                    buff,
                      eps = args.eps,
                    level = args.level,
                    nIter = args.nIter,
                ) / area

        # Save array as BIN ...
        ratios.tofile(f"{dName}/fencePosts.bin")

    # Load BIN ...
    ratios = numpy.fromfile(
        f"{dName}/fencePosts.bin",
        dtype = numpy.float64,
    ).reshape(len(lats), len(lons))

    # Save array as PNG (scaled from -1% to +1% of the correct answer) ...
    pyguymer3.image.save_array_as_image(
        255.0 * (ratios - 0.99) / 0.02,
        f"{dName}/fencePosts.png",
          debug = args.debug,
             ct = "turbo",
          scale = False,
        timeout = args.timeout,
    )

    # Print outliers ...
    for iLon, lon in enumerate(lons):
        for iLat, lat in enumerate(lats):
            if 0.99 <= ratios[iLat, iLon] <= 1.01:
                continue
            print(f"fencePosts  :: (lon = {lon:+6.1f}°, lat = {lat:+5.1f}°) has a ratio of {ratios[iLat, iLon]:.3f}.")

    # **************************************************************************

    # Create short-hands ...
    lons = [float(lon) + 0.5 for lon in range(-180, 180)]                       # [°]
    lats = [float(lat) + 0.5 for lat in range(-90, 90)]                         # [°]

    # Check if the BIN file needs making ...
    if not os.path.exists(f"{dName}/fencePanels.bin"):
        print(f"Making \"{dName}/fencePanels.bin\" ...")

        # Initialize array to hold answer ...
        ratios = numpy.zeros(
            (len(lats), len(lons)),
            dtype = numpy.float64,
        )

        # Loop over longitudes ...
        for iLon, lon in enumerate(lons):
            # Loop over latitudes ...
            for iLat, lat in enumerate(lats):
                # Buffer Point ...
                buff = pyguymer3.geo.buffer(
                    shapely.geometry.point.Point(
                        lon,
                        lat,
                    ),
                    dist,
                    debug = args.debug,
                      eps = args.eps,
                     fill = -1.0,
                     nAng = args.nAng,
                    nIter = args.nIter,
                     simp = -1.0,
                      tol = args.tol,
                )

                # Populate array with the ratio of the estimated area to the
                # exact area ...
                ratios[iLat, iLon] = pyguymer3.geo.area(
                    buff,
                      eps = args.eps,
                    level = args.level,
                    nIter = args.nIter,
                ) / area

        # Save array as BIN ...
        ratios.tofile(f"{dName}/fencePanels.bin")

    # Load BIN ...
    ratios = numpy.fromfile(
        f"{dName}/fencePanels.bin",
        dtype = numpy.float64,
    ).reshape(len(lats), len(lons))

    # Save array as PNG (scaled from -1% to +1% of the correct answer) ...
    pyguymer3.image.save_array_as_image(
        255.0 * (ratios - 0.99) / 0.02,
        f"{dName}/fencePanels.png",
          debug = args.debug,
             ct = "turbo",
          scale = False,
        timeout = args.timeout,
    )

    # Print outliers ...
    for iLon, lon in enumerate(lons):
        for iLat, lat in enumerate(lats):
            if 0.99 <= ratios[iLat, iLon] <= 1.01:
                continue
            print(f"fencePanels :: (lon = {lon:+6.1f}°, lat = {lat:+5.1f}°) has a ratio of {ratios[iLat, iLon]:.3f}.")
