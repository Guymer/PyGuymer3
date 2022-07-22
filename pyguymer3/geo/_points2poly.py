def _points2poly(point, points, kwArgCheck = None, debug = False, tol = 1.0e-10):
    """Convert a buffered point to a Polygon

    This function reads in a coordinate that exists on the surface of the Earth,
    and an array of coordinates that are the ring around the coordinate buffered
    by a constant distance, and returns a Polygon which describes the buffer.

    Parameters
    ----------
    point : numpy.ndarray
            the (2) array of (lon,lat) coordinate (in degrees)
    points : numpy.ndarray
            the (nang, 2) array of (lon,lat) coordinates around the (lon,lat) coordinate (in degrees)
    debug : bool, optional
            print debug messages
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    wedges : shapely.geometry.polygon.Polygon
            the buffered (lon,lat) coordinate
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
        # Create a correctly oriented Polygon from the western limit of the ring
        # left to the anti-meridian ...
        wedge = shapely.geometry.polygon.Polygon(
            [
                (            -180.0, points[:, 1].max()),
                (            -180.0, points[:, 1].min()),
                (points[:, 0].min(), points[:, 1].min()),
                (points[:, 0].min(), points[:, 1].max()),
                (            -180.0, points[:, 1].max()),
            ]
        )
        check(wedge)

        # Append Polygon to list ...
        wedges.append(wedge)

        # Clean up ...
        del wedge
    if points[:, 0].max() < point[0]:
        # Create a correctly oriented Polygon from the eastern limit of the ring
        # right to the anti-meridian ...
        wedge = shapely.geometry.polygon.Polygon(
            [
                (            +180.0, points[:, 1].min()),
                (            +180.0, points[:, 1].max()),
                (points[:, 0].max(), points[:, 1].max()),
                (points[:, 0].max(), points[:, 1].min()),
                (            +180.0, points[:, 1].min()),
            ]
        )
        check(wedge)

        # Append Polygon to list ...
        wedges.append(wedge)

        # Clean up ...
        del wedge
    if points[:, 1].min() > point[1]:
        # Create a correctly oriented Polygon from the southern limit of the
        # ring down to the South Pole ...
        wedge = shapely.geometry.polygon.Polygon(
            [
                (points[:, 0].min(),              -90.0),
                (points[:, 0].max(),              -90.0),
                (points[:, 0].max(), points[:, 1].min()),
                (points[:, 0].min(), points[:, 1].min()),
                (points[:, 0].min(),              -90.0),
            ]
        )
        check(wedge)

        # Append Polygon to list ...
        wedges.append(wedge)

        # Clean up ...
        del wedge
    if points[:, 1].max() < point[1]:
        # Create a correctly oriented Polygon from the northern limit of the
        # ring up to the North Pole ...
        wedge = shapely.geometry.polygon.Polygon(
            [
                (points[:, 0].min(), points[:, 1].max()),
                (points[:, 0].max(), points[:, 1].max()),
                (points[:, 0].max(),               90.0),
                (points[:, 0].min(),               90.0),
                (points[:, 0].min(), points[:, 1].max()),
            ]
        )
        check(wedge)

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
        check(wedge)

        # Append Polygon to list ...
        wedges.append(wedge)

        # Clean up ...
        del wedge

    # Check if the first and the last points are far apart in Euclidean space ...
    if numpy.hypot(points[-1, 0] - points[0, 0], points[-1, 1] - points[0, 1]) > tol:
        # Create a correctly oriented Polygon from the original point to this
        # segment of the ring ...
        wedge = shapely.geometry.polygon.orient(
            shapely.geometry.polygon.Polygon(
                [
                    (point[     0], point[     1]),
                    (points[-1, 0], points[-1, 1]),
                    (points[ 0, 0], points[ 0, 1]),
                    (point[     0], point[     1]),
                ]
            )
        )
        check(wedge)

        # Append Polygon to list ...
        wedges.append(wedge)

        # Clean up ...
        del wedge

    # Convert list of Polygons to a correctly oriented (unified) Polygon ...
    wedges = shapely.geometry.polygon.orient(shapely.ops.unary_union(wedges).simplify(tol))
    check(wedges)

    # Remake the Polygon to remove any interiors ...
    # HACK: Sometimes "wedges" has zero-area interiors.
    wedges = shapely.geometry.polygon.orient(shapely.geometry.polygon.Polygon(wedges.exterior))
    check(wedges)

    # Return answer ...
    return clean(wedges, debug = debug, tol = tol)
