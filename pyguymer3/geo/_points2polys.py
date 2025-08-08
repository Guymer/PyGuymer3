#!/usr/bin/env python3

# Define function ...
def _points2polys(
    point,
    points,
    /,
    *,
     debug = __debug__,
      huge = False,
    prefix = ".",
       tol = 1.0e-10,
):
    """Convert a buffered point to a list of Polygons

    This function reads in a coordinate that exists on the surface of the Earth,
    and an array of coordinates that are the counter-clockwise ring around the
    coordinate buffered by a constant distance, and returns a list of Polygons
    which describes the buffer.

    Parameters
    ----------
    point : numpy.ndarray
        the (2) array of (lon,lat) coordinate (in degrees)
    points : numpy.ndarray
        the (nAng, 2) array of (lon,lat) coordinates around the (lon,lat)
        coordinate (in degrees)
    debug : bool, optional
        print debug messages
    huge : bool, optional
        if the buffering distance was huge then the points can be turned into a
        Polygon very easily (as they will definitely cross the [anti-]meridian
        and a Pole)
    prefix : str, optional
        change the name of the output debugging CSVs
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    polys : list of shapely.geometry.polygon.Polygon
        the buffered (lon,lat) coordinate

    Notes
    -----
    According to the `Shapely documentation for the LinearRings
    <https://shapely.readthedocs.io/en/stable/manual.html#linearrings>`_ , a
    LinearRing may not cross itself and may not touch itself at a single point.

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
    from .check import check
    from .wrapLongitude import wrapLongitude
    from ..interpolate import interpolate

    # **************************************************************************

    # Check arguments ...
    assert isinstance(point, numpy.ndarray), "\"point\" is not a NumPy array"
    assert isinstance(points, numpy.ndarray), "\"points\" is not a NumPy array"
    if huge:
        if debug:
            print("DEBUG: overruling \"huge\" flag because the feature is currently broken (as of 8/Aug/2025).")
        huge = False

    # **************************************************************************

    # Create short-hands ...
    nAng = points.shape[0]
    mang = (nAng - 1) // 2

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

    # Determine if the ring crosses both poles ...
    crossBothPoles = crossNorthPole and crossSouthPole

    # **************************************************************************

    # Check if the user promises that the ring is huge ...
    if huge and not crossBothPoles:
        # Find the keys that index the points from West to East (taking care not
        # to add a duplicate point) ...
        keys = points[:-1, 0].argsort()

        # Find the latitude of the [anti-]meridian crossing ...
        latCross = interpolate(
            points[keys[0], 0],
            points[keys[-1], 0] - 360.0,
            points[keys[0], 1],
            points[keys[-1], 1],
            -180.0,
        )                                                                       # [°]

        # Initialize list which will hold the ring ...
        ring = []

        # Add point before the crossing ...
        ring.append((-180.0, latCross))

        # Loop over points ...
        for iang in range(nAng - 1):
            # Add point ...
            ring.append((points[keys[iang], 0], points[keys[iang], 1]))

        # Add point after the crossing ...
        ring.append((+180.0, latCross))

        # Clean up ...
        del keys

        # Determine if the ring is around a point in the Northern hemisphere ...
        if point[1] > 0.0:
            # Add points around the North Pole ...
            ring.append((+180.0, +90.0))
            ring.append((-180.0, +90.0))
        else:
            # Add points around the South Pole ...
            ring.append((+180.0, -90.0))
            ring.append((-180.0, -90.0))

        # Make a LinearRing out of the ring ...
        line = shapely.geometry.polygon.LinearRing(ring)
        if debug:
            check(line, prefix = prefix)

        # Clean up ...
        del ring

        # Make a correctly oriented Polygon out of this LinearRing ...
        poly = shapely.geometry.polygon.orient(shapely.geometry.polygon.Polygon(line))
        if debug:
            check(poly, prefix = prefix)

        # Clean up ...
        del line

        # NOTE: Do not call "fillin()" on the Polygon. If the user is calling
        #       this function themselves, then they can also call "fillin()"
        #       themselves. If this function is being called by
        #       "buffer_CoordinateSequence()" then the result of
        #       "shapely.ops.unary_union().simplify()" can be filled in instead
        #       in that function.

        # Return answer ...
        return [poly]

    # **************************************************************************

    # Calculate the Euclidean distances between each point on the ring ...
    dists = numpy.hypot(numpy.diff(points[:, 0]), numpy.diff(points[:, 1]))     # [°]

    # Initialize list which will hold the two rings and set the index to point
    # to the first ring ...
    rings = [[], []]
    iring = 0

    # Populate the start of the ring (taking care not to add duplicate points) ...
    if crossNorthPole and not crossSouthPole:
        point = (points[0, 0], +90.0)
        if point not in rings[iring]:
            rings[iring].append(point)
    if crossSouthPole and not crossNorthPole:
        point = (points[0, 0], -90.0)
        if point not in rings[iring]:
            rings[iring].append(point)
    point = (points[0, 0], points[0, 1])
    if point not in rings[iring]:
        rings[iring].append(point)

    # Loop over angles ...
    for iang in range(nAng - 1):
        # Check if the ring has crossed the [anti-]meridian ...
        if dists[iang] >= 180.0:
            # Check if it was the anti-meridian or the meridian ...
            if points[iang + 1, 0] < points[iang, 0]:
                # Set the longitude and find the latitude of the crossing ...
                lonCross = +180.0                                               # [°]
                latCross = interpolate(
                    points[iang, 0],
                    points[iang + 1, 0] + 360.0,
                    points[iang, 1],
                    points[iang + 1, 1],
                    lonCross,
                )                                                               # [°]
            else:
                # Set the longitude and find the latitude of the crossing ...
                lonCross = -180.0                                               # [°]
                latCross = interpolate(
                    points[iang, 0],
                    points[iang + 1, 0] - 360.0,
                    points[iang, 1],
                    points[iang + 1, 1],
                    lonCross,
                )                                                               # [°]

            # Add points before the crossing (taking care not to add duplicate
            # points) ...
            point = (lonCross, latCross)
            if point not in rings[iring]:
                rings[iring].append(point)
            if crossNorthPole and not crossSouthPole:
                point = (lonCross, +90.0)
                if point not in rings[iring]:
                    rings[iring].append(point)
            if crossSouthPole and not crossNorthPole:
                point = (lonCross, -90.0)
                if point not in rings[iring]:
                    rings[iring].append(point)

            # Switch to the other ring ...
            iring = 1 - iring

            # Add points after the crossing (taking care not to add duplicate
            # points) ...
            if crossNorthPole and not crossSouthPole:
                point = (-lonCross, +90.0)
                if point not in rings[iring]:
                    rings[iring].append(point)
            if crossSouthPole and not crossNorthPole:
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
    if crossNorthPole and not crossSouthPole:
        point = (points[-1, 0], +90.0)
        if point not in rings[iring]:
            rings[iring].append(point)
    if crossSouthPole and not crossNorthPole:
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
        if debug:
            check(line, prefix = prefix)

        # Clean up ...
        del cleanedRing

        # Make a correctly oriented Polygon out of this LinearRing ...
        poly = shapely.geometry.polygon.orient(shapely.geometry.polygon.Polygon(line))
        if debug:
            check(poly, prefix = prefix)

        # Clean up ...
        del line

        # NOTE: Do not call "fillin()" on the Polygon. If the user is calling
        #       this function themselves, then they can also call "fillin()"
        #       themselves. If this function is being called by
        #       "buffer_CoordinateSequence()" then the result of
        #       "shapely.ops.unary_union().simplify()" can be filled in instead
        #       in that function.

        # Append Polygon to the list ...
        polys.append(poly)

    # **************************************************************************

    # Check if the two Polygons are holes in a larger Polygon ...
    if crossBothPoles:
        # Make a correctly oriented Polygon of the planet ...
        earth = shapely.geometry.polygon.orient(
            shapely.geometry.polygon.Polygon(
                shapely.geometry.polygon.LinearRing(
                    [
                        (-180.0,  90.0),
                        (+180.0,  90.0),
                        (+180.0, -90.0),
                        (-180.0, -90.0),
                        (-180.0,  90.0),
                    ]
                )
            )
        )
        if debug:
            check(earth, prefix = prefix)

        # Loop over Polygons ...
        for poly in polys:
            # Subtract this Polygon from the planet ...
            earth = earth.difference(poly)

        # Clean up ...
        del polys

        # Return answer ...
        return [earth]

    # Return answer ...
    return polys
