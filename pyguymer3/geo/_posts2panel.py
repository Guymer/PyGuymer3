def _posts2panel(pointA, pointB, pointsA, pointsB, polyA, polyB, kwArgCheck = None, debug = False, tol = 1.0e-10):
    """Convert two buffered points to a Polygon

    This function reads in two coordinates that exists on the surface of the
    Earth, two arrays of coordinates that are the rings around the coordinates
    buffered by a constant distance and two Polygons that are the rings around
    the coordinates buffered by a constant distance, and returns a Polygon which
    describes the buffer of the points and the line that connects them.

    Parameters
    ----------
    pointA : numpy.ndarray
            the first (2) array of (lon,lat) coordinate (in degrees)
    pointB : numpy.ndarray
            the second (2) array of (lon,lat) coordinate (in degrees)
    pointsA : numpy.ndarray
            the (nang, 2) array of (lon,lat) coordinates around the first (lon,lat) coordinate (in degrees)
    pointsB : numpy.ndarray
            the (nang, 2) array of (lon,lat) coordinates around the second (lon,lat) coordinate (in degrees)
    polyA : shapely.geometry.polygon.Polygon
            the Polygon around the first (lon,lat) coordinate
    polyB : shapely.geometry.polygon.Polygon
            the Polygon around the second (lon,lat) coordinate
    debug : bool, optional
            print debug messages
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    finalPoly : shapely.geometry.polygon.Polygon
            the buffer around the two points and the line that connects them
    """

    # Import special modules ...
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

    # Import sub-functions ...
    from .check import check
    from .clean import clean

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(pointA, numpy.ndarray):
        raise TypeError("\"pointA\" is not a NumPy array") from None
    if not isinstance(pointB, numpy.ndarray):
        raise TypeError("\"pointB\" is not a NumPy array") from None
    if not isinstance(pointsA, numpy.ndarray):
        raise TypeError("\"pointsA\" is not a NumPy array") from None
    if not isinstance(pointsB, numpy.ndarray):
        raise TypeError("\"pointsB\" is not a NumPy array") from None
    if not isinstance(polyA, shapely.geometry.polygon.Polygon):
        raise TypeError("\"polyA\" is not a Polygon") from None
    check(polyA)
    if not isinstance(polyB, shapely.geometry.polygon.Polygon):
        raise TypeError("\"polyB\" is not a Polygon") from None
    check(polyB)

    # Check if the two points are the same ...
    if numpy.all(pointA == pointB):
        # Find the correctly oriented Polygon ...
        finalPoly = shapely.geometry.polygon.orient(polyA.simplify(tol))
        check(finalPoly)

        # Return answer ...
        return finalPoly

    # Create a line connecting the two original points ...
    line = shapely.geometry.linestring.LineString([pointA, pointB])
    check(line)

    # Find the minimum distance from an original point to any point on its ring ...
    minDist = min(
        numpy.hypot(pointsA[:, 0] - pointA[0], pointsA[:, 1] - pointA[1]).min(),
        numpy.hypot(pointsB[:, 0] - pointB[0], pointsB[:, 1] - pointB[1]).min(),
    )                                                                           # [°]

    # Add conservatism ...
    minDist *= 0.1                                                              # [°]

    # Buffer (in Euclidean space) the line connecting the two original points ...
    line = shapely.geometry.polygon.orient(line.buffer(minDist))
    check(line)

    # Convert the two Polygons and the buffered line that connects them to a
    # correctly oriented (unified) Polygon ...
    finalPoly = shapely.geometry.polygon.orient(shapely.ops.unary_union([polyA, line, polyB]).simplify(tol))
    check(finalPoly)

    # Clean up ...
    del line

    # Find the correctly oriented convex hull of the Polygon ...
    finalPoly = shapely.geometry.polygon.orient(finalPoly.convex_hull.simplify(tol))
    check(finalPoly)

    # Return answer ...
    return clean(finalPoly, debug = debug, tol = tol)
