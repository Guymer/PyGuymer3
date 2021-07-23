def _points2poly(point, points, kwArgCheck = None, tol = 1.0e-10):
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

    # Import sub-functions ...
    from ._debug import _debug

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(point, numpy.ndarray):
        raise TypeError("\"point\" is not a NumPy array") from None
    if not isinstance(points, numpy.ndarray):
        raise TypeError("\"points\" is not a NumPy array") from None

    # Create short-hand ...
    nang = points.shape[0]

    # Initialize list ...
    wedges = []

    # Check that the ring encompasses the original point ...
    if points[:, 0].min() > point[0]:
        raise Exception(f"the W-edge of the ring does not encompass the original point ({points[:, 0].min():.6f}° > {point[0]:.6f}°)")
    if points[:, 0].max() < point[0]:
        raise Exception(f"the E-edge of the ring does not encompass the original point ({points[:, 0].max():.6f}° < {point[0]:.6f}°)")
    if points[:, 1].min() > point[1]:
        # Create a correctly oriented Polygon from the lower extent of the ring
        # down to the South Pole ...
        wedge = shapely.geometry.polygon.Polygon(
            [
                (points[:, 0].min(), -90.0),
                (points[:, 0].max(), -90.0),
                (points[:, 0].max(), points[:, 1].min()),
                (points[:, 0].min(), points[:, 1].min()),
                (points[:, 0].min(), -90.0),
            ]
        )
        if not isinstance(wedge, shapely.geometry.polygon.Polygon):
            raise Exception("\"wedge\" is not a Polygon") from None
        if not wedge.is_valid:
            _debug(wedge)
            raise Exception(f"\"wedge\" is not a valid Polygon ({shapely.validation.explain_validity(wedge)})") from None
        if wedge.is_empty:
            raise Exception("\"wedge\" is an empty Polygon") from None

        # Append Polygon to list ...
        wedges.append(wedge)

        # Clean up ...
        del wedge
    if points[:, 1].max() < point[1]:
        # Create a correctly oriented Polygon from the upper extent of the ring
        # up to the North Pole ...
        wedge = shapely.geometry.polygon.Polygon(
            [
                (points[:, 0].min(), points[:, 1].max()),
                (points[:, 0].max(), points[:, 1].max()),
                (points[:, 0].max(), 90.0),
                (points[:, 0].min(), 90.0),
                (points[:, 0].min(), points[:, 1].max()),
            ]
        )
        if not isinstance(wedge, shapely.geometry.polygon.Polygon):
            raise Exception("\"wedge\" is not a Polygon") from None
        if not wedge.is_valid:
            _debug(wedge)
            raise Exception(f"\"wedge\" is not a valid Polygon ({shapely.validation.explain_validity(wedge)})") from None
        if wedge.is_empty:
            raise Exception("\"wedge\" is an empty Polygon") from None

        # Append Polygon to list ...
        wedges.append(wedge)

        # Clean up ...
        del wedge

    # Loop over angles ...
    for iang in range(nang - 1):
        # Create a correctly oriented Polygon from the original point to this
        # segment of the ring ...
        wedge = shapely.geometry.polygon.orient(
            shapely.geometry.polygon.Polygon(
                [
                    (point[           0], point[           1]),
                    (points[iang    , 0], points[iang    , 1]),
                    (points[iang + 1, 0], points[iang + 1, 1]),
                    (point[           0], point[           1]),
                ]
            )
        )
        if not isinstance(wedge, shapely.geometry.polygon.Polygon):
            raise Exception("\"wedge\" is not a Polygon") from None
        if not wedge.is_valid:
            _debug(wedge)
            raise Exception(f"\"wedge\" is not a valid Polygon ({shapely.validation.explain_validity(wedge)})") from None
        if wedge.is_empty:
            raise Exception("\"wedge\" is an empty Polygon") from None

        # Append Polygon to list ...
        wedges.append(wedge)

        # Clean up ...
        del wedge

    # Convert list of Polygons to a correctly oriented (unified) Polygon ...
    wedges = shapely.geometry.polygon.orient(shapely.ops.unary_union(wedges).simplify(tol))
    if not isinstance(wedges, shapely.geometry.polygon.Polygon):
        raise Exception("\"wedges\" is not a Polygon") from None
    if not wedges.is_valid:
        _debug(wedges)
        raise Exception(f"\"wedges\" is not a valid Polygon ({shapely.validation.explain_validity(wedges)})") from None
    if wedges.is_empty:
        raise Exception("\"wedges\" is an empty Polygon") from None

    # Return answer ...
    return wedges
