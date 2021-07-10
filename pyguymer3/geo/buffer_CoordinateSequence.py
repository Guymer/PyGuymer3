def buffer_CoordinateSequence(coords, dist, kwArgCheck = None, debug = False, fill = 1.0, nang = 19, simp = 0.1, tol = 1.0e-10):
    """Buffer a CoordinateSequence

    This function reads in a CoordinateSequence that exists on the surface of
    the Earth and returns a [Multi]Polygon of the same CoordinateSequence
    buffered by a constant distance (in metres).

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
            the CoordinateSequence
    dist : float
            the Geodesic distance to buffer each point within the CoordinateSequence by (in metres)
    debug : bool, optional
            print debug messages
    fill : float, optional
            the Euclidean distance to fill in between each point within the [Multi]Polygon by; negative values disable filling in (in degrees)
    nang : int, optional
            the number of angles around each point within the CoordinateSequence that are calculated when buffering
    simp : float, optional
            how much the final [Multi]Polygons is simplified by; negative values disable simplification (in degrees)
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered CoordinateSequence

    Notes
    -----
    According to the Shapely documentation for the method object.buffer():

        "Passed a distance of 0, buffer() can sometimes be used to “clean” self-touching or self-crossing polygons such as the classic “bowtie”. Users have reported that very small distance values sometimes produce cleaner results than 0. Your mileage may vary when cleaning surfaces."

    According to the Shapely documentation for the function shapely.geometry.polygon.orient():

        "A sign of 1.0 means that the coordinates of the product’s exterior ring will be oriented counter-clockwise."
    """

    # Import standard modules ...
    import math

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

    # Load sub-functions ...
    from ._buffer_points_crudely import _buffer_points_crudely
    from ._earthA import _earthA
    from ._earthB import _earthB
    from ._earthC import _earthC
    from ._earthD import _earthD
    from ._earthE import _earthE
    from ._earthF import _earthF
    from ._earthG import _earthG
    from .fillin import fillin
    try:
        from ..f90 import f90
        if debug:
            print("INFO: Will find the rings using FORTRAN.")
        fortran = True
    except:
        if debug:
            print("INFO: Will find the rings using Python.")
        fortran = False

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(coords, shapely.coords.CoordinateSequence):
        raise TypeError("\"coords\" is not a CoordinateSequence") from None

    # Correct inputs ...
    # NOTE: Limit the buffering distance to be between 10m and a quarter of
    #       Earth's circumference.
    # NOTE: Must do at least 9 points around the compass.
    dist = max(10.0, min(0.5 * math.pi * 6371008.8, dist))                      # [m]
    nang = max(9, nang)

    # **************************************************************************
    # Step 1: Convert the CoordinateSequence to a NumPy array of the original  #
    #         points                                                           #
    # **************************************************************************

    # Convert the CoordinateSequence to a NumPy array ...
    points1 = numpy.array(coords)                                               # [°]

    # Check inputs ...
    if points1[:, 0].min() < -180.0:
        raise Exception(f"a point exists a long way off the W-edge of Earth ({points1[:, 0].min():.6f}° < -180°)") from None
    if points1[:, 0].max() > +180.0:
        raise Exception(f"a point exists a long way off the E-edge of Earth ({points1[:, 0].max():.6f}° > +180°)") from None
    if points1[:, 1].min() < -90.0:
        raise Exception(f"a point exists a long way off the S-edge of Earth ({points1[:, 1].min():.6f}° < -90°)") from None
    if points1[:, 1].max() > +90.0:
        raise Exception(f"a point exists a long way off the N-edge of Earth ({points1[:, 1].max():.6f}° > +90°)") from None

    # **************************************************************************
    # Step 2: Buffer the NumPy array of the original points to get a NumPy     #
    #         array of the rings around them                                   #
    # **************************************************************************

    # Buffer (in Geodesic space) the CoordinateSequence ...
    if fortran:
        points2 = f90.buffer_points_crudely(points1, dist, nang)                # [°]
    else:
        points2 = _buffer_points_crudely(points1, dist, nang)                   # [°]

    # **************************************************************************
    # Step 3: Convert the NumPy array of the rings around the original points  #
    #         to a list of Polygons of the buffered original points            #
    # **************************************************************************

    # Initialize list ...
    polys = []

    # Loop over points ...
    for ipoint in range(points1.shape[0]):
        # Initialize list ...
        wedges = []

        # Check that the ring encompasses the original point ...
        if points2[ipoint, :, 0].min() > points1[ipoint, 0]:
            raise Exception(f"the W-edge of the ring does not encompass the original point ({points2[ipoint, :, 0].min():.6f}° > {points1[ipoint, 0]:.6f}°)")
        if points2[ipoint, :, 0].max() < points1[ipoint, 0]:
            raise Exception(f"the E-edge of the ring does not encompass the original point ({points2[ipoint, :, 0].max():.6f}° < {points1[ipoint, 0]:.6f}°)")
        if points2[ipoint, :, 1].min() > points1[ipoint, 1]:
            # Create a correctly oriented Polygon from the lower extent of the
            # ring down to the South Pole ...
            wedge = shapely.geometry.polygon.Polygon(
                [
                    (-360.0, -90.0),
                    (+360.0, -90.0),
                    (+360.0, points2[ipoint, :, 1].min()),
                    (-360.0, points2[ipoint, :, 1].min()),
                    (-360.0, -90.0),
                ]
            )
            if not isinstance(wedge, shapely.geometry.polygon.Polygon):
                raise Exception("\"wedge\" is not a Polygon") from None
            if not wedge.is_valid:
                raise Exception(f"\"wedge\" is not a valid Polygon ({shapely.validation.explain_validity(wedge)})") from None
            if wedge.is_empty:
                raise Exception("\"wedge\" is an empty Polygon") from None

            # Append Polygon to list ...
            wedges.append(wedge)
        if points2[ipoint, :, 1].max() < points1[ipoint, 1]:
            # Create a correctly oriented Polygon from the upper extent of the
            # ring up to the North Pole ...
            wedge = shapely.geometry.polygon.Polygon(
                [
                    (-360.0, points2[ipoint, :, 1].max()),
                    (+360.0, points2[ipoint, :, 1].max()),
                    (+360.0, 90.0),
                    (-360.0, 90.0),
                    (-360.0, points2[ipoint, :, 1].max()),
                ]
            )
            if not isinstance(wedge, shapely.geometry.polygon.Polygon):
                raise Exception("\"wedge\" is not a Polygon") from None
            if not wedge.is_valid:
                raise Exception(f"\"wedge\" is not a valid Polygon ({shapely.validation.explain_validity(wedge)})") from None
            if wedge.is_empty:
                raise Exception("\"wedge\" is an empty Polygon") from None

            # Append Polygon to list ...
            wedges.append(wedge)

        # Loop over angles ...
        for iang in range(nang - 1):
            # Create a correctly oriented Polygon from the original point to
            # this segment of the ring ...
            wedge = shapely.geometry.polygon.orient(
                shapely.geometry.polygon.Polygon(
                    [
                        (points1[ipoint,           0], points1[ipoint,           1]),
                        (points2[ipoint, iang    , 0], points2[ipoint, iang    , 1]),
                        (points2[ipoint, iang + 1, 0], points2[ipoint, iang + 1, 1]),
                        (points1[ipoint,           0], points1[ipoint,           1]),
                    ]
                )
            )
            if not isinstance(wedge, shapely.geometry.polygon.Polygon):
                raise Exception("\"wedge\" is not a Polygon") from None
            if not wedge.is_valid:
                raise Exception(f"\"wedge\" is not a valid Polygon ({shapely.validation.explain_validity(wedge)})") from None
            if wedge.is_empty:
                raise Exception("\"wedge\" is an empty Polygon") from None

            # Append Polygon to list ...
            wedges.append(wedge)

        # Convert list of Polygons to a correctly oriented (unified) Polygon ...
        wedges = shapely.geometry.polygon.orient(shapely.ops.unary_union(wedges).simplify(tol))
        if not isinstance(wedges, shapely.geometry.polygon.Polygon):
            raise Exception("\"wedges\" is not a Polygon") from None
        if not wedges.is_valid:
            raise Exception(f"\"wedges\" is not a valid Polygon ({shapely.validation.explain_validity(wedges)})") from None
        if wedges.is_empty:
            raise Exception("\"wedges\" is an empty Polygon") from None

        # Append (unified) Polygon to list ...
        polys.append(wedges)

    # **************************************************************************
    # Step 4: Append Polygons of the convex hulls of adjacent buffered         #
    #         original points and the lines that connects them                 #
    # **************************************************************************

    # Initialize list ...
    finalPolys = []

    # Check if there are some connections ...
    if points1.shape[0] == 1:
        # Append Polygon to list ...
        finalPolys.append(polys[0])
    else:
        # Loop over points ...
        for ipoint in range(points1.shape[0] - 1):
            # Create a line connecting the two original points ...
            line = shapely.geometry.linestring.LineString([points1[ipoint, :], points1[ipoint + 1, :]])
            if not isinstance(line, shapely.geometry.linestring.LineString):
                raise Exception("\"line\" is not a LineString") from None
            if not line.is_valid:
                raise Exception(f"\"line\" is not a valid LineString ({shapely.validation.explain_validity(line)})") from None
            if line.is_empty:
                raise Exception("\"line\" is an empty LineString") from None

            # Find the minimum distance from an original point to any point on
            # its ring ...
            minDist = min(
                numpy.hypot(points2[ipoint    , :, 0] - points1[ipoint    , 0], points2[ipoint    , :, 1] - points1[ipoint    , 1]).min(),
                numpy.hypot(points2[ipoint + 1, :, 0] - points1[ipoint + 1, 0], points2[ipoint + 1, :, 1] - points1[ipoint + 1, 1]).min(),
            )                                                                   # [°]

            # Add conservatism ...
            minDist *= 0.1                                                      # [°]

            # Buffer (in Euclidean space) the line connecting the two original
            # points...
            line = line.buffer(minDist)
            if not isinstance(line, shapely.geometry.polygon.Polygon):
                raise Exception("\"line\" is not a Polygon") from None
            if not line.is_valid:
                raise Exception(f"\"line\" is not a valid Polygon ({shapely.validation.explain_validity(line)})") from None
            if line.is_empty:
                raise Exception("\"line\" is an empty Polygon") from None

            # Find the correctly oriented convex hull of the unification of the
            # two Polygons and the buffered line that connects them ...
            finalPoly = shapely.geometry.polygon.orient(
                shapely.ops.unary_union(
                    [
                        polys[ipoint],
                        line,
                        polys[ipoint + 1]
                    ]
                ).simplify(tol).convex_hull
            )
            if not isinstance(finalPoly, shapely.geometry.polygon.Polygon):
                raise Exception("\"finalPoly\" is not a Polygon") from None
            if not finalPoly.is_valid:
                raise Exception(f"\"finalPoly\" is not a valid Polygon ({shapely.validation.explain_validity(finalPoly)})") from None
            if finalPoly.is_empty:
                raise Exception("\"finalPoly\" is an empty Polygon") from None

            # Append the convex hull of the two Polygons and the buffered line
            # that connects them to list ...
            finalPolys.append(finalPoly)

    # **************************************************************************
    # Step 5: Create a single [Multi]Polygon that is the union of all of the   #
    #         Polygons and re-map it so that it does not extend off the edge   #
    #         of Earth                                                         #
    # **************************************************************************

    # Convert list of Polygons to a correctly oriented (unified) Polygon ...
    finalPolys = shapely.geometry.polygon.orient(shapely.ops.unary_union(finalPolys).simplify(tol))
    if not isinstance(finalPolys, shapely.geometry.polygon.Polygon):
        raise Exception("\"finalPolys\" is not a Polygon") from None
    if not finalPolys.is_valid:
        raise Exception(f"\"finalPolys\" is not a valid Polygon ({shapely.validation.explain_validity(finalPolys)})") from None
    if finalPolys.is_empty:
        raise Exception("\"finalPolys\" is an empty Polygon") from None

    # Initialize list ...
    buffs = []

    # Append the Polygons, which are the subset of the (unified) Polygon that
    # intersects with Earth-A that has been re-mapped on to Earth-D, to list ...
    buffs += _earthA(finalPolys)

    # Append the Polygons, which are the subset of the (unified) Polygon that
    # intersects with Earth-B that has been re-mapped on to Earth-D, to list ...
    buffs += _earthB(finalPolys)

    # Append the Polygons, which are the subset of the (unified) Polygon that
    # intersects with Earth-C that has been re-mapped on to Earth-D, to list ...
    buffs += _earthC(finalPolys)

    # Append the Polygons, which are the subset of the (unified) Polygon that
    # intersects with Earth-D that has been re-mapped on to Earth-D, to list ...
    buffs += _earthD(finalPolys)

    # Append the Polygons, which are the subset of the (unified) Polygon that
    # intersects with Earth-E that has been re-mapped on to Earth-D, to list ...
    buffs += _earthE(finalPolys)

    # Append the Polygons, which are the subset of the (unified) Polygon that
    # intersects with Earth-F that has been re-mapped on to Earth-D, to list ...
    buffs += _earthF(finalPolys)

    # Append the Polygons, which are the subset of the (unified) Polygon that
    # intersects with Earth-G that has been re-mapped on to Earth-D, to list ...
    buffs += _earthG(finalPolys)

    # Convert list of Polygons to (unified) [Multi]Polygon ...
    buffs = shapely.ops.unary_union(buffs).simplify(tol)
    if not buffs.is_valid:
        raise Exception(f"\"buffs\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffs)})") from None
    if buffs.is_empty:
        raise Exception("\"buffs\" is an empty [Multi]Polygon") from None

    # Check if the user wants to fill in the [Multi]Polygon ...
    if fill > 0.0:
        # Fill in [Multi]Polygon ...
        buffs = fillin(buffs, fill, debug = debug, tol = tol)

    # Check if the user wants to simplify the [Multi]Polygon ...
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffsSimp = buffs.simplify(simp)

        # Check simplified [Multi]Polygon ...
        if buffsSimp.is_valid and not buffsSimp.is_empty:
            # Return simplified answer ...
            return buffsSimp

        if debug:
            print(f"WARNING: \"buffsSimp\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffsSimp)}), will return \"buffs\" instead")

    # Return answer ...
    return buffs
