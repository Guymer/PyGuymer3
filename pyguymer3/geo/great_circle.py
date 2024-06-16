#!/usr/bin/env python3

# Define function ...
def great_circle(lon1, lat1, lon2, lat2, /, *, debug = False, maxdist = None, npoint = None, prefix = ".", ramLimit = 1073741824):
    """Calculate the great circle that connects two coordinates.

    This function reads in two coordinates (in degrees) on the surface of the
    Earth and calculates the great circle that connects them, correctly handling
    crossing over the anti-meridian (should it occur).

    Parameters
    ----------
    lon1 : float
        the longitude of the first coordinate (in degrees)
    lat1 : float
        the latitude of the first coordinate (in degrees)
    lon2 : float
        the longitude of the second coordinate (in degrees)
    lat2 : float
        the latitude of the second coordinate (in degrees)
    debug : bool, optional
        print debug messages
    maxdist : float, optional
        the maximum distance between points along the great circle
    npoint : int, optional
        the number of points along the great circle
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)

    Returns
    -------
    line : shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString
        the great circle

    Notes
    -----
    If neither maxdist nor npoint are provided then npoint will default to 5.
    For large great circles, prettier results are obtained by using maxdist
    rather than npoint.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
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
    from .calc_dist_between_two_locs import calc_dist_between_two_locs
    from .check import check
    from .find_middle_of_great_circle import find_middle_of_great_circle
    from .find_point_on_great_circle import find_point_on_great_circle
    from ..interpolate import interpolate
    from ..consts import CIRCUMFERENCE_OF_EARTH

    # **************************************************************************

    # Set default value ...
    if maxdist is None and npoint is None:
        npoint = 5                                                              # [#]

    # Check inputs ...
    if isinstance(maxdist, float):
        if maxdist < 10.0:
            raise Exception(f"the maximum distance is too small ({maxdist:,.1f}m < {10.0:,.1f}m)") from None
        if maxdist > 0.5 * CIRCUMFERENCE_OF_EARTH:
            raise Exception(f"the maximum distance is too large ({maxdist:,.1f}m > {0.5 * CIRCUMFERENCE_OF_EARTH:,.1f}m)") from None
    elif isinstance(npoint, int):
        if npoint < 3:
            raise Exception(f"the number of points is too small ({npoint:,d} < {3:,d})") from None
    else:
        raise Exception("\"maxdist\" is not a float and \"npoint\" is not an integer") from None

    # **************************************************************************

    # Check if the user wants to make the great circle using either "maxdist" or
    # "npoints" ...
    if isinstance(maxdist, float):
        # Create a poorly resolved great circle ...
        points = [
            (lon1, lat1),
            find_point_on_great_circle(0.2, lon1, lat1, lon2, lat2),
            find_point_on_great_circle(0.4, lon1, lat1, lon2, lat2),
            find_point_on_great_circle(0.6, lon1, lat1, lon2, lat2),
            (lon2, lat2),
        ]                                                                       # [°], [°]

        # Initialize flag ...
        addedPoint = True

        # Commence infinite loop ...
        while addedPoint:
            # Reset the flag ..
            addedPoint = False

            # Loop over points ...
            for ipoint in range(1, len(points)):
                # Calculate the distance between this point and the previous one ...
                dist, _, _ = calc_dist_between_two_locs(
                    points[ipoint - 1][0],
                    points[ipoint - 1][1],
                    points[ipoint][0],
                    points[ipoint][1],
                )                                                               # [m]

                # Check if the distance is too large ...
                if dist > maxdist:
                    # Replace the list of points with one which has a new
                    # intermediate point inserted ...
                    points = points[:ipoint] + [
                        find_middle_of_great_circle(
                            points[ipoint - 1][0],
                            points[ipoint - 1][1],
                            points[ipoint][0],
                            points[ipoint][1],
                        )
                    ] + points[ipoint:]                                         # [°], [°]

                    # Set the flag ...
                    addedPoint = True

                    # Stop looping over points and start the survey again ...
                    break

        # Convert list to array and clean up ...
        circle = numpy.array(points, dtype = numpy.float64)                     # [°]
        del points
    elif isinstance(npoint, int):
        # Check array size ...
        if npoint * 2 * 8 > ramLimit:
            raise Exception(f"\"circle\" is going to be {npoint * 2 * 8:,d} bytes, which is larger than {ramLimit:,d} bytes") from None

        # Initialize array ...
        circle = numpy.zeros((npoint, 2), dtype = numpy.float64)                # [°]

        # Loop over points ...
        for ipoint in range(npoint):
            # Set point ...
            circle[ipoint, :] = find_point_on_great_circle(float(ipoint) / float(npoint - 1), lon1, lat1, lon2, lat2)   # [°]
    else:
        raise Exception("\"maxdist\" is not a float and \"npoint\" is not an integer") from None

    # **************************************************************************

    # Check if the great circle crosses the anti-meridian W to E ...
    if lon2 > lon1 and (circle[1, 0] < lon1 or circle[-2, 0] > lon2):
        if debug:
            print("INFO: The great circle crosses the anti-meridian (point #2 is E of point #1 but the great circle goes W).")

        # Find where it crosses the anti-meridian ...
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

        # Convert to a MultiLineString ...
        multiline = shapely.geometry.multilinestring.MultiLineString([line1, line2])
        if debug:
            check(multiline, prefix = prefix)

        # Return answer ...
        return multiline

    # Check if the great circle crosses the anti-meridian E to W ...
    if lon2 < lon1 and (circle[1, 0] > lon1 or circle[-2, 0] < lon2):
        if debug:
            print("INFO: The great circle crosses the anti-meridian (point #2 is W of point #1 but the great circle goes E).")

        # Find where it crosses the anti-meridian ...
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

        # Convert to a MultiLineString ...
        multiline = shapely.geometry.multilinestring.MultiLineString([line1, line2])
        if debug:
            check(multiline, prefix = prefix)

        # Return answer ...
        return multiline

    # **************************************************************************

    # Convert to a LineString ...
    line = shapely.geometry.linestring.LineString(circle)
    if debug:
        check(line, prefix = prefix)

    # Return answer ...
    return line
