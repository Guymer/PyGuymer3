def great_circle(lon1, lat1, lon2, lat2, kwArgCheck = None, debug = False, npoint = 5):
    """
    This function reads in two starting coordinates (in degrees) and two
    finishing coordinates (in degrees) on the surface of a sphere and calculates
    the great circle that connects them.
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._debug import _debug
    from .find_point_on_great_circle import find_point_on_great_circle
    from ..interpolate import interpolate

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check inputs ...
    if npoint < 3:
        raise Exception(f"the number of points is too small ({npoint:,d} < {3:,d})") from None

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

        # Calculate the second intersection and convert to a LineString ...
        x = +180.0                                                              # [°]
        y = interpolate(circle[i, 0] + 360.0, circle[i + 1, 0], circle[i, 1], circle[i + 1, 1], x)  # [°]
        line2 = shapely.geometry.linestring.LineString(numpy.append([[x, y]], circle[i + 1:, :], axis = 0))

        # Convert to a MultiLineString ...
        multiline = shapely.geometry.multilinestring.MultiLineString([line1, line2])

        # Check MultiLineString ...
        if not isinstance(multiline, shapely.geometry.multilinestring.MultiLineString):
            raise TypeError("\"multiline\" is not a MultiLineString") from None
        if not multiline.is_valid:
            _debug(multiline)
            raise Exception(f"\"multiline\" is not a valid MultiLineString ({shapely.validation.explain_validity(multiline)})") from None
        if multiline.is_empty:
            raise Exception("\"multiline\" is an empty MultiLineString") from None

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

        # Calculate the second intersection and convert to a LineString ...
        x = -180.0                                                              # [°]
        y = interpolate(circle[i, 0] - 360.0, circle[i + 1, 0], circle[i, 1], circle[i + 1, 1], x)  # [°]
        line2 = shapely.geometry.linestring.LineString(numpy.append([[x, y]], circle[i + 1:, :], axis = 0))

        # Convert to a MultiLineString ...
        multiline = shapely.geometry.multilinestring.MultiLineString([line1, line2])

        # Check MultiLineString ...
        if not isinstance(multiline, shapely.geometry.multilinestring.MultiLineString):
            raise TypeError("\"multiline\" is not a MultiLineString") from None
        if not multiline.is_valid:
            _debug(multiline)
            raise Exception(f"\"multiline\" is not a valid MultiLineString ({shapely.validation.explain_validity(multiline)})") from None
        if multiline.is_empty:
            raise Exception("\"multiline\" is an empty MultiLineString") from None

        # Return answer ...
        return multiline

    # Convert to a LineString ...
    line = shapely.geometry.linestring.LineString(circle)

    # Check LineString ...
    if not isinstance(line, shapely.geometry.linestring.LineString):
        raise TypeError("\"line\" is not a LineString") from None
    if not line.is_valid:
        _debug(line)
        raise Exception(f"\"line\" is not a valid LineString ({shapely.validation.explain_validity(line)})") from None
    if line.is_empty:
        raise Exception("\"line\" is an empty LineString") from None

    # Return answer ...
    return line
