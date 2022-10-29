def buffer_CoordinateSequence(coords, dist, kwArgCheck = None, debug = False, fill = 1.0, fillSpace = "EuclideanSpace", nang = 9, ramLimit = 1073741824, simp = 0.1, tol = 1.0e-10):
    """Buffer a CoordinateSequence

    This function reads in a CoordinateSequence that exists on the surface of
    the Earth and returns a [Multi]Polygon of the same CoordinateSequence
    buffered by a constant distance (in metres).

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
        the CoordinateSequence
    dist : float
        the Geodetic distance to buffer each point within the CoordinateSequence
        by (in metres)
    debug : bool, optional
        print debug messages
    fill : float, optional
        the Euclidean or Geodetic distance to fill in between each point within
        the shapes by (in degrees or metres)
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    nang : int, optional
        the number of angles around each point within the CoordinateSequence
        that are calculated when buffering
    ramLimit : int, optional
        the maximum RAM usage of each "large" array, in bytes
    simp : float, optional
        how much the final [Multi]Polygons is simplified by; negative values
        disable simplification (in degrees)
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the buffered CoordinateSequence

    Notes
    -----
    According to the Shapely documentation for the method object.buffer():

        "Passed a distance of 0, buffer() can sometimes be used to “clean”
        self-touching or self-crossing polygons such as the classic “bowtie”.
        Users have reported that very small distance values sometimes produce
        cleaner results than 0. Your mileage may vary when cleaning surfaces."

    According to the Shapely documentation for the function shapely.geometry.polygon.orient():

        "A sign of 1.0 means that the coordinates of the product’s exterior ring
        will be oriented counter-clockwise."

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
        import shapely.ops
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .._buffer_points_crudely import _buffer_points_crudely
    from .._points2polys import _points2polys
    from ..check import check
    from ..clean import clean
    from ..fillin import fillin
    try:
        from ...f90 import funcs
        if debug:
            print("INFO: Will find the rings using FORTRAN.")
        fortran = True
    except ModuleNotFoundError:
        if debug:
            print("INFO: Will find the rings using Python.")
        fortran = False

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(coords, shapely.coords.CoordinateSequence):
        raise TypeError("\"coords\" is not a CoordinateSequence") from None
    if debug:
        check(coords)

    # Check inputs ...
    # NOTE: Using WGS84, the semi-major axis of Earth is 6,378,137.0 m and the
    #       semi-minor axis of Earth is approximately 6,356,752.3 m. Making a
    #       sphere using just the semi-minor axis means that the largest
    #       distance that can be buffered in one go is 19,970,326.3 m.
    # NOTE: See https://en.wikipedia.org/wiki/World_Geodetic_System#1984_version
    if dist < 10.0:
        raise Exception(f"the buffering distance is too small ({dist:,.1f}m < {10.0:,.1f}m)") from None
    if dist > 19970326.3:
        raise Exception(f"the buffering distance is too large ({dist:,.1f}m > {19970326.3:,.1f}m)") from None
    if nang < 9:
        raise Exception(f"the number of angles is too small ({nang:,d} < {9:,d})") from None
    if nang % 2 == 0:
        raise Exception(f"the number of angles is even ({nang:,d})") from None
    if (nang - 1) % 4 != 0:
        raise Exception(f"the number of angles is not 4n+1 ({nang:,d})") from None

    # **************************************************************************
    # Step 1: Convert the CoordinateSequence to a NumPy array of the original  #
    #         points                                                           #
    # **************************************************************************

    # Clean the input ...
    coords = clean(
        coords,
        debug = debug,
          tol = tol,
    ).coords

    # Check if the user wants to fill in the CoordinateSequence ...
    if fill > 0.0 and len(coords) > 1:
        # Convert the filled in CoordinateSequence to a NumPy array ...
        points1 = numpy.array(
            fillin(
                coords,
                fill,
                    debug = debug,
                fillSpace = fillSpace,
                 ramLimit = ramLimit,
                      tol = tol,
            ).coords
        )                                                                       # [°]
    else:
        # Convert the CoordinateSequence to a NumPy array ...
        points1 = numpy.array(coords)                                           # [°]

    # Create short-hand ...
    npoint = points1.shape[0]

    # **************************************************************************
    # Step 2: Buffer the NumPy array of the original points to get a NumPy     #
    #         array of the rings around them                                   #
    # **************************************************************************

    # Buffer (in Geodesic space) the CoordinateSequence ...
    if fortran:
        points2 = funcs.buffer_points_crudely(points1, dist, nang)              # [°]
    else:
        points2 = _buffer_points_crudely(points1, dist, nang, ramLimit = ramLimit)  # [°]

    # **************************************************************************
    # Step 3: Convert the NumPy array of the rings around the original points  #
    #         to a list of Polygons of the buffered original points            #
    # **************************************************************************

    # Initialize list of Polygons ...
    buffs = []

    # Loop over points ...
    for ipoint in range(npoint):
        # Add list of Polygons to list of Polygons ...
        buffs += _points2polys(
            points1[ipoint, :],
            points2[ipoint, :, :],
            debug = debug,
             huge = dist > 10001500.0,
              tol = tol,
        )

    # Clean up ...
    del points1, points2

    # **************************************************************************
    # Step 4: Create a single [Multi]Polygon that is the union of all of the   #
    #         Polygons                                                         #
    # **************************************************************************

    # Convert list of Polygons to a (unified) [Multi]Polygon ...
    buffs = shapely.ops.unary_union(buffs).simplify(tol)
    if debug:
        check(buffs)

    # Check if the user wants to fill in the [Multi]Polygon ...
    # NOTE: This is only needed because the "shapely.ops.unary_union()" call
    #       above includes a "simplify()".
    if simp < 0.0 < fill:
        # Fill in [Multi]Polygon ...
        buffs = fillin(
            buffs,
            fill,
                debug = debug,
            fillSpace = fillSpace,
             ramLimit = ramLimit,
                  tol = tol,
        )
        if debug:
            check(buffs)

    # Check if the user wants to simplify the [Multi]Polygon ...
    # NOTE: This is only needed because the "shapely.ops.unary_union()" call
    #       above might allow more simplification.
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffsSimp = buffs.simplify(simp)
        if debug:
            check(buffsSimp)

        # Return simplified answer ...
        return buffsSimp

    # Return answer ...
    return buffs
