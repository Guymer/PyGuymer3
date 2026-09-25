#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import json
    import os

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
            description = "Create a map of some points.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--absolute-path-to-repository",
        default = os.path.dirname(os.path.dirname(__file__)),
           dest = "absPathToRepo",
           help = "the absolute path to the PyGuymer3 repository",
           type = str,
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

    # Create map of points ...
    pyguymer3.geo.create_map_of_points(
        lons,
        lats,
        fName,
    )
