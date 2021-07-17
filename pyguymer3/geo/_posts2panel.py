def _posts2panel(pointA, pointB, pointsA, pointsB, polyA, polyB, kwArgCheck = None, tol = 1.0e-10):
    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.geometry
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

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
    if not polyA.is_valid:
        raise Exception(f"\"polyA\" is not a valid Polygon ({shapely.validation.explain_validity(polyA)})") from None
    if polyA.is_empty:
        raise Exception("\"polyA\" is an empty Polygon") from None
    if not isinstance(polyB, shapely.geometry.polygon.Polygon):
        raise TypeError("\"polyB\" is not a Polygon") from None
    if not polyB.is_valid:
        raise Exception(f"\"polyB\" is not a valid Polygon ({shapely.validation.explain_validity(polyB)})") from None
    if polyB.is_empty:
        raise Exception("\"polyB\" is an empty Polygon") from None

    # Create a line connecting the two original points ...
    line = shapely.geometry.linestring.LineString([pointA, pointB])
    if not isinstance(line, shapely.geometry.linestring.LineString):
        raise Exception("\"line\" is not a LineString") from None
    if not line.is_valid:
        raise Exception(f"\"line\" is not a valid LineString ({shapely.validation.explain_validity(line)})") from None
    if line.is_empty:
        raise Exception("\"line\" is an empty LineString") from None

    # Find the minimum distance from an original point to any point on its ring ...
    minDist = min(
        numpy.hypot(pointsA[:, 0] - pointA[0], pointsA[:, 1] - pointA[1]).min(),
        numpy.hypot(pointsB[:, 0] - pointB[0], pointsB[:, 1] - pointB[1]).min(),
    )                                                                           # [°]

    # Add conservatism ...
    minDist *= 0.1                                                              # [°]

    # Buffer (in Euclidean space) the line connecting the two original points...
    line = line.buffer(minDist)
    if not isinstance(line, shapely.geometry.polygon.Polygon):
        raise Exception("\"line\" is not a Polygon") from None
    if not line.is_valid:
        raise Exception(f"\"line\" is not a valid Polygon ({shapely.validation.explain_validity(line)})") from None
    if line.is_empty:
        raise Exception("\"line\" is an empty Polygon") from None

    # Find the correctly oriented convex hull of the unification of the two
    # Polygons and the buffered line that connects them ...
    finalPoly = shapely.geometry.polygon.orient(shapely.ops.unary_union([polyA, line, polyB]).simplify(tol).convex_hull)

    # Clean up ...
    del line

    # Check Polygon ...
    if not isinstance(finalPoly, shapely.geometry.polygon.Polygon):
        raise Exception("\"finalPoly\" is not a Polygon") from None
    if not finalPoly.is_valid:
        raise Exception(f"\"finalPoly\" is not a valid Polygon ({shapely.validation.explain_validity(finalPoly)})") from None
    if finalPoly.is_empty:
        raise Exception("\"finalPoly\" is an empty Polygon") from None

    # Return answer ...
    return finalPoly
