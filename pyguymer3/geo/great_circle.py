#!/usr/bin/env python3

# Define function ...
def great_circle(lon1, lat1, lon2, lat2, kwArgCheck = None, debug = False, npoint = 5, prefix = ".", ramLimit = 1073741824):
    """
    This function reads in two starting coordinates (in degrees) and two
    finishing coordinates (in degrees) on the surface of a sphere and calculates
    the great circle that connects them.

    ramLimit : int, optional
        the maximum RAM usage of each "large" array, in bytes
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .check import check
    from .find_point_on_great_circle import find_point_on_great_circle
    from ..interpolate import interpolate

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check inputs ...
    if npoint < 3:
        raise Exception(f"the number of points is too small ({npoint:,d} < {3:,d})") from None

    # Check array size ...
    if npoint * 2 * 8 > ramLimit:
        raise Exception(f"\"circle\" is going to be {npoint * 2 * 8:,d} bytes, which is larger than {ramLimit:,d} bytes") from None

    # Initialize array ...
    circle = numpy.zeros((npoint, 2), dtype = numpy.float64)                    # [°]

    # Loop over points ...
    for ipoint in range(npoint):
        # Set point ...
        circle[ipoint, :] = find_point_on_great_circle(float(ipoint) / float(npoint - 1), lon1, lat1, lon2, lat2)   # [°]

    # Check if the great circle crosses the anti-meridean W to E ...
    if lon2 > lon1 and (circle[1, 0] < lon1 or circle[-2, 0] > lon2):
        if debug:
            print("INFO: The great circle crosses the anti-meridean (point #2 is E of point #1 but the great circle goes W).")

        # Find where it crosses the anti-meridean ...
        i = numpy.diff(circle[:, 0]).argmax()

        # Calculate the first intersection and convert to a LineString ...
        x = -180.0                                                              # [°]
        y = interpolate(circle[i, 0], circle[i + 1, 0] - 360.0, circle[i, 1], circle[i + 1, 1], x)  # [°]
        line1 = shapely.geometry.linestring.LineString(numpy.append(circle[:i + 1, :], [[x, y]], axis = 0))
        if debug:
            check(line1, prefix = prefix)

        # Calculate the second intersection and convert to a LineString ...
        x = +180.0                                                              # [°]
        y = interpolate(circle[i, 0] + 360.0, circle[i + 1, 0], circle[i, 1], circle[i + 1, 1], x)  # [°]
        line2 = shapely.geometry.linestring.LineString(numpy.append([[x, y]], circle[i + 1:, :], axis = 0))
        if debug:
            check(line2, prefix = prefix)

        # Clean up ...
        del circle

        # Convert to a MultiLineString ...
        multiline = shapely.geometry.multilinestring.MultiLineString([line1, line2])
        if debug:
            check(multiline, prefix = prefix)

        # Clean up ...
        del line1, line2

        # Return answer ...
        return multiline

    # Check if the great circle crosses the anti-meridean E to W ...
    if lon2 < lon1 and (circle[1, 0] > lon1 or circle[-2, 0] < lon2):
        if debug:
            print("INFO: The great circle crosses the anti-meridean (point #2 is W of point #1 but the great circle goes E).")

        # Find where it crosses the anti-meridean ...
        i = numpy.diff(circle[:, 0]).argmin()

        # Calculate the first intersection and convert to a LineString ...
        x = +180.0                                                              # [°]
        y = interpolate(circle[i, 0], circle[i + 1, 0] + 360.0, circle[i, 1], circle[i + 1, 1], x)  # [°]
        line1 = shapely.geometry.linestring.LineString(numpy.append(circle[:i + 1, :], [[x, y]], axis = 0))
        if debug:
            check(line1, prefix = prefix)

        # Calculate the second intersection and convert to a LineString ...
        x = -180.0                                                              # [°]
        y = interpolate(circle[i, 0] - 360.0, circle[i + 1, 0], circle[i, 1], circle[i + 1, 1], x)  # [°]
        line2 = shapely.geometry.linestring.LineString(numpy.append([[x, y]], circle[i + 1:, :], axis = 0))
        if debug:
            check(line2, prefix = prefix)

        # Clean up ...
        del circle

        # Convert to a MultiLineString ...
        multiline = shapely.geometry.multilinestring.MultiLineString([line1, line2])
        if debug:
            check(multiline, prefix = prefix)

        # Clean up ...
        del line1, line2

        # Return answer ...
        return multiline

    # Convert to a LineString ...
    line = shapely.geometry.linestring.LineString(circle)
    if debug:
        check(line, prefix = prefix)

    # Clean up ...
    del circle

    # Return answer ...
    return line
