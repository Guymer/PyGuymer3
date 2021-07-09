def buffer_CoordinateSequence(coords, dist, kwArgCheck = None, debug = False, nang = 19, simp = 0.1):
    """Buffer a CoordinateSequence

    This function reads in a CoordinateSequence that exists on the surface of
    the Earth and returns a [Multi]Polygon of the same CoordinateSequence
    buffered by a constant distance (in metres).

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
            the CoordinateSequence
    dist : float
            the distance to buffer each point within the CoordinateSequence by (in metres)
    debug : bool, optional
            print debug messages
    nang : int, optional
            the number of angles around each point within the CoordinateSequence that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered CoordinateSequence
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
        raise Exception(f"a point exists off the W-edge of Earth ({points1[:, 0].min():.1f}° < -180°)") from None
    if points1[:, 0].max() > +180.0:
        raise Exception(f"a point exists off the E-edge of Earth ({points1[:, 0].max():.1f}° > +180°)") from None
    if points1[:, 1].min() < -90.0:
        raise Exception(f"a point exists off the S-edge of Earth ({points1[:, 1].min():.1f}° < -90°)") from None
    if points1[:, 1].max() > +90.0:
        raise Exception(f"a point exists off the N-edge of Earth ({points1[:, 1].max():.1f}° > +90°)") from None

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
            raise Exception(f"the W-edge of the ring does not encompass the original point ({points2[ipoint, :, 0].min():.1f}° > {points1[ipoint, 0]:.1f}°)")
        if points2[ipoint, :, 0].max() < points1[ipoint, 0]:
            raise Exception(f"the E-edge of the ring does not encompass the original point ({points2[ipoint, :, 0].max():.1f}° < {points1[ipoint, 0]:.1f}°)")
        if points2[ipoint, :, 1].min() > points1[ipoint, 1]:
            # Create a Polygon from the lower extent of the ring down to the
            # South Pole ...
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
            # Create a Polygon from the upper extent of the ring up to the
            # North Pole ...
            wedge = shapely.geometry.polygon.Polygon(
                [
                    (-360.0, 90.0),
                    (+360.0, 90.0),
                    (+360.0, points2[ipoint, :, 1].max()),
                    (-360.0, points2[ipoint, :, 1].max()),
                    (-360.0, 90.0),
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
            # Create a Polygon from the original point to this segment of the
            # ring ...
            wedge = shapely.geometry.polygon.Polygon(
                [
                    (points1[ipoint,           0], points1[ipoint,           1]),
                    (points2[ipoint, iang    , 0], points2[ipoint, iang    , 1]),
                    (points2[ipoint, iang + 1, 0], points2[ipoint, iang + 1, 1]),
                    (points1[ipoint,           0], points1[ipoint,           1]),
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

        # Convert list of Polygons to (unified) Polygon ...
        wedges = shapely.ops.unary_union(wedges)
        if not isinstance(wedges, shapely.geometry.polygon.Polygon):
            raise Exception("\"wedges\" is not a Polygon") from None
        if not wedges.is_valid:
            raise Exception(f"\"wedges\" is not a valid Polygon ({shapely.validation.explain_validity(wedges)})") from None
        if wedges.is_empty:
            raise Exception("\"wedges\" is an empty Polygon") from None

        # Append (unified) Polygon to list ...
        polys.append(wedges)

    # **************************************************************************
    # Step 4: Append Polygons of the buffered connections between the original #
    #         points to the list of Polygons of the buffered original points   #
    # **************************************************************************

    # Check that there are some connections ...
    if points1.shape[0] > 1:
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

            # Append Polygon to list ...
            polys.append(line)

    # **************************************************************************
    # Step 5: Create a single [Multi]Polygon that is the union of all of the   #
    #         Polygons and re-map it so that it does not extend off the edge   #
    #         of Earth                                                         #
    # **************************************************************************

    # Convert list of Polygons to (unified) Polygon ...
    polys = shapely.ops.unary_union(polys)
    if not isinstance(polys, shapely.geometry.polygon.Polygon):
        raise Exception("\"polys\" is not a Polygon") from None
    if not polys.is_valid:
        raise Exception(f"\"polys\" is not a valid Polygon ({shapely.validation.explain_validity(polys)})") from None
    if polys.is_empty:
        raise Exception("\"polys\" is an empty Polygon") from None

    # Initialize list ...
    buffs = []

    # Append the Polygon, which is the subset of the (unified) Polygon that
    # intersects with Earth-A that has been re-mapped on to Earth-D, to list ...
    buffs += _earthA(polys)

    print(buffs)
    exit()

    print(type(earthA))
    print(type(polys))
    print(type(inter))
    exit()



    print(polys)
    exit()

    # Convert list of [Multi]Polygons to (unified) [Multi]Polygon ...
    buffs = shapely.ops.unary_union(buffs)

    # Check [Multi]Polygon ...
    if not buffs.is_valid:
        raise Exception(f"\"buffs\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffs)})") from None

    # Check [Multi]Polygon ...
    if buffs.is_empty:
        raise Exception("\"buffs\" is an empty [Multi]Polygon") from None

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
