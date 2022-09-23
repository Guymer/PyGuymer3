def _points2polys(point, points, kwArgCheck = None, debug = False, fill = 1.0, fillSpace = "EuclideanSpace", ramLimit = 1073741824, tol = 1.0e-10):
    """Convert a buffered point to a list of Polygons

    This function reads in a coordinate that exists on the surface of the Earth,
    and an array of coordinates that are the ring around the coordinate buffered
    by a constant distance, and returns a list of Polygons which describes the
    buffer.

    Parameters
    ----------
    point : numpy.ndarray
        the (2) array of (lon,lat) coordinate (in degrees)
    points : numpy.ndarray
        the (nang, 2) array of (lon,lat) coordinates around the (lon,lat)
        coordinate (in degrees)
    debug : bool, optional
        print debug messages
    fill : float, optional
        the Euclidean or Geodetic distance to fill in between each point within
        the shapes by (in degrees or metres)
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    ramLimit : int, optional
        the maximum RAM usage of each "large" array, in bytes
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    polys : list of shapely.geometry.polygon.Polygon
        the buffered (lon,lat) coordinate

    Notes
    -----
    A LinearRing may not cross itself and may not touch itself at a single point [1]_

    Copyright 2017 Thomas Guymer [2]_

    References
    ----------
    .. [1] Shapely LinearRing Documentation, https://shapely.readthedocs.io/en/stable/manual.html#linearrings
    .. [2] PyGuymer3, https://github.com/Guymer/PyGuymer3
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
    from .fillin import fillin
    from .wrapLongitude import wrapLongitude
    from ..interpolate import interpolate

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(point, numpy.ndarray):
        raise TypeError("\"point\" is not a NumPy array") from None
    if not isinstance(points, numpy.ndarray):
        raise TypeError("\"points\" is not a NumPy array") from None

    # **************************************************************************

    # Create short-hands ...
    nang = points.shape[0]
    mang = (nang - 1) // 2

    # Determine if the ring crosses the North Pole ...
    if (wrapLongitude(point[0]) - tol) < wrapLongitude(points[0, 0]) < (wrapLongitude(point[0]) + tol):
        crossNorthPole = False
    else:
        crossNorthPole = True

    # Determine if the ring crosses the South Pole ...
    if (wrapLongitude(point[0]) - tol) < wrapLongitude(points[mang, 0]) < (wrapLongitude(point[0]) + tol):
        crossSouthPole = False
    else:
        crossSouthPole = True

    # **************************************************************************

    # Calculate the Euclidean distances between each point on the ring ...
    dists = numpy.hypot(numpy.diff(points[:, 0]), numpy.diff(points[:, 1]))     # [°]

    # Initialize list which will hold the two rings and set the index to point
    # to the first ring ...
    rings = [[], []]
    iring = 0

    # Populate the start of the ring (taking care not to add duplicate points) ...
    if crossNorthPole:
        point = (points[0, 0], +90.0)
        if point not in rings[iring]:
            rings[iring].append(point)
    if crossSouthPole:
        point = (points[0, 0], -90.0)
        if point not in rings[iring]:
            rings[iring].append(point)
    point = (points[0, 0], points[0, 1])
    if point not in rings[iring]:
        rings[iring].append(point)

    # Loop over angles ...
    for iang in range(nang - 1):
        # Check if the ring has crossed the [anti-]meridian ...
        if dists[iang] >= 180.0:
            # Check if it was the anti-meridian or the meridian ...
            if points[iang + 1, 0] < points[iang, 0]:
                # Set the longitude and find the latitude of the crossing ...
                lonCross = +180.0                                               # [°]
                latCross = interpolate(points[iang, 0], points[iang + 1, 0] + 360.0, points[iang, 1], points[iang + 1, 1], lonCross)    # [°]
            else:
                # Set the longitude and find the latitude of the crossing ...
                lonCross = -180.0                                               # [°]
                latCross = interpolate(points[iang, 0], points[iang + 1, 0] - 360.0, points[iang, 1], points[iang + 1, 1], lonCross)    # [°]

            # Add points before the crossing (taking care not to add duplicate
            # points) ...
            point = (lonCross, latCross)
            if point not in rings[iring]:
                rings[iring].append(point)
            if crossNorthPole:
                point = (lonCross, +90.0)
                if point not in rings[iring]:
                    rings[iring].append(point)
            if crossSouthPole:
                point = (lonCross, -90.0)
                if point not in rings[iring]:
                    rings[iring].append(point)

            # Switch to the other ring ...
            iring = 1 - iring

            # Add points after the crossing (taking care not to add duplicate
            # points) ...
            if crossNorthPole:
                point = (-lonCross, +90.0)
                if point not in rings[iring]:
                    rings[iring].append(point)
            if crossSouthPole:
                point = (-lonCross, -90.0)
                if point not in rings[iring]:
                    rings[iring].append(point)
            point = (-lonCross, latCross)
            if point not in rings[iring]:
                rings[iring].append(point)

        # Add point (taking care not to add duplicate points) ...
        point = (points[iang + 1, 0], points[iang + 1, 1])
        if point not in rings[iring]:
            rings[iring].append(point)

    # Populate the end of the ring (taking care not to add duplicate points) ...
    if crossNorthPole:
        point = (points[-1, 0], +90.0)
        if point not in rings[iring]:
            rings[iring].append(point)
    if crossSouthPole:
        point = (points[-1, 0], -90.0)
        if point not in rings[iring]:
            rings[iring].append(point)

    # Clean up ...
    del dists

    # **************************************************************************

    # Initialize list which will hold the two Polygons ...
    polys = []

    # Loop over rings ...
    for ring in rings:
        # Skip this ring if it does not have any points ...
        if len(ring) == 0:
            continue

        # Make a NumPy array of this ring ...
        tmpRing = numpy.array(ring)                                             # [°]

        # Skip this ring if it has zero width or zero height ...
        if abs(tmpRing[:, 0].max() - tmpRing[:, 0].min()) < tol:
            continue
        if abs(tmpRing[:, 1].max() - tmpRing[:, 1].min()) < tol:
            continue

        # Find the Euclidean distance between each point on the ring ...
        tmpDist = numpy.hypot(
            numpy.diff(tmpRing[:, 0]),
            numpy.diff(tmpRing[:, 1]),
        )                                                                       # [°]

        # Clean up ...
        del tmpRing

        # Make a cleaned copy of the ring (the above if-tests for duplicate
        # points may fail due to the floating-point precision being better than
        # the floating-point accuracy in Vincenty's formulae) ...
        cleanedRing = []
        cleanedRing.append(ring[0])
        for i in range(tmpDist.size):
            if tmpDist[i] < tol:
                continue
            cleanedRing.append(ring[i + 1])

        # Clean up ...
        del tmpDist

        # Do one final check for if the start and end points are very close
        # (because if they are not numerically identical then Shapely will add
        # the first point to the end of the list to close it, which may tangle
        # the subsequent LinearRing) ...
        tmpDist = numpy.hypot(
            cleanedRing[0][0] - cleanedRing[-1][0],
            cleanedRing[0][1] - cleanedRing[-1][1],
        )                                                                       # [°]
        if tmpDist < tol:
            cleanedRing.pop()

        # Make a LinearRing out of this (cleaned) ring ...
        line = shapely.geometry.polygon.LinearRing(cleanedRing)
        check(line)

        # Clean up ...
        del cleanedRing

        # Make a Polygon out of this LinearRing ...
        poly = shapely.geometry.polygon.orient(shapely.geometry.polygon.Polygon(line))
        check(poly)

        # Clean up ...
        del line

        # Check if the user wants to fill in the Polygon ...
        if fill > 0.0:
            # Fill in Polygon ...
            poly = fillin(poly, fill, debug = debug, fillSpace = fillSpace, ramLimit = ramLimit, tol = tol)
            check(poly)

        # Append Polygon to the list ...
        polys.append(poly)

    # Return answer ...
    return polys
