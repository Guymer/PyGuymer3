#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import json
    import os
    import shutil

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Create a image of some points.",
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
        "--dont-attempt-FORTRAN",
        action = "store_true",
          dest = "dontAttemptFortran",
          help = "don't attempt to use FORTRAN",
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
        default = 361,
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

    # Create short-hand ...
    fName = f'{args.absPathToRepo}/tests/{os.path.basename(__file__).removesuffix(".py")}.png'

    # Load data and convert to NumPy array ...
    with open(
        f"{args.absPathToRepo}/tests/exampleLons.json",
        encoding = "utf-8",
            mode = "rt",
    ) as fObj:
        lons = json.load(fObj)                                                  # [°]
    lons = numpy.array(
        lons,
        dtype = numpy.float64,
    )                                                                           # [°]

    # Load data and convert to NumPy array ...
    with open(
        f"{args.absPathToRepo}/tests/exampleLats.json",
        encoding = "utf-8",
            mode = "rt",
    ) as fObj:
        lats = json.load(fObj)                                                  # [°]
    lats = numpy.array(
        lats,
        dtype = numpy.float64,
    )                                                                           # [°]

    # Start session ...
    with pyguymer3.start_session() as sess:
        # Create image of points ...
        pyguymer3.geo.create_image_of_points(
            lons,
            lats,
            6,
            sess,
            fName,
            attemptFortran = not args.dontAttemptFortran,
                 chunksize = args.chunksize,
                     debug = args.debug,
                       eps = args.eps,
              exiftoolPath = args.exiftoolPath,
              gifsiclePath = args.gifsiclePath,
              jpegtranPath = args.jpegtranPath,
                      nAng = args.nAng,
                     nIter = args.nIter,
                 onlyValid = True,
               optipngPath = args.optipngPath,
                    prefix = f".{__name__}.",
                    repair = False,
                   timeout = args.timeout,
                       tol = args.tol,
        )
