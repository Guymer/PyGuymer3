#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.11/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import math

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # **************************************************************************

    # Define starting location ...
    lon = -1.0                                                                  # [°]
    lat = 50.5                                                                  # [°]

    # Define distance ...
    dist = 1000.0e3                                                             # [m]

    # Configure functions ...
    debug = False
    fill = -1.0                                                                 # [°]
    fillSpace = "EuclideanSpace"
    nang = 36001                                                                # [#]
    simp = -1.0                                                                 # [°]
    tol = 1.0e-10                                                               # [°]

    # Create point ...
    point = shapely.geometry.point.Point(lon, lat)

    # Buffer Point ...
    buff = pyguymer3.geo.buffer(
        point,
        dist,
            debug = debug,
             fill = fill,
        fillSpace = fillSpace,
             nang = nang,
             simp = simp,
              tol = tol,
    )

    # Create short-hand ...
    exactArea = math.pi * pow(dist, 2)                                          # [m2]

    # Loop over levels ...
    for level in range(4):
        # Find area ...
        estimArea = pyguymer3.geo.area(buff, level = level + 1)                 # [m2]

        print(f"level={level + 1:d} :: {estimArea / 1.0e6:,.1f} km2 ({100.0 * ((estimArea / exactArea) - 1.0):+,.2f}%)")
