#!/usr/bin/env python3

# Define function ...
def buffer_CoordinateSequence(
    coords,
    dist,
    /,
    *,
        debug = __debug__,
          eps = 1.0e-12,
         fill = 1.0,
    fillSpace = "EuclideanSpace",
         nAng = 9,
        nIter = 100,
       prefix = ".",
     ramLimit = 1073741824,
         simp = 0.1,
          tol = 1.0e-10,
):
    """Buffer a CoordinateSequence

    This function reads in a CoordinateSequence that exists on the surface of
    the Earth and returns a [Multi]Polygon of the same CoordinateSequence
    buffered by a constant distance (in metres).

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
        the CoordinateSequence
    dist : float
        the Geodesic distance to buffer each point within the CoordinateSequence
        by (in metres)
    debug : bool, optional
        print debug messages
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    fill : float, optional
        the Euclidean or Geodesic distance to fill in between each point within
        the shapes by (in degrees or metres)
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    nAng : int, optional
        the number of angles around each point within the CoordinateSequence
        that are calculated when buffering
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)
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
    According to the `Shapely documentation for the method object.buffer()
    <https://shapely.readthedocs.io/en/stable/manual.html#object.buffer>`_ :

        "Passed a distance of 0, buffer() can sometimes be used to "clean"
        self-touching or self-crossing polygons such as the classic "bowtie".
        Users have reported that very small distance values sometimes produce
        cleaner results than 0. Your mileage may vary when cleaning surfaces."

    According to the `Shapely documentation for the function
    shapely.geometry.polygon.orient()
    <https://shapely.readthedocs.io/en/stable/manual.html#shapely.geometry.polygon.orient>`_ :

        "A sign of 1.0 means that the coordinates of the product's exterior ring
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
    from ..._consts import CIRCUMFERENCE_OF_EARTH, MAXIMUM_VINCENTY
    try:
        from ...f90 import funcs
        if debug:
            print("INFO: Will find the rings using FORTRAN.")
        fortran = True
    except ModuleNotFoundError:
        if debug:
            print("INFO: Will find the rings using Python (did not find FORTRAN module).")
        fortran = False
    except ImportError:
        if debug:
            print("INFO: Will find the rings using Python (error when attempting to import found FORTRAN).")
        fortran = False

    # **************************************************************************

    # Check argument ...
    assert isinstance(coords, shapely.coords.CoordinateSequence), "\"coords\" is not a CoordinateSequence"
    if debug:
        check(coords, prefix = prefix)

    # Check inputs ...
    assert dist >= 10.0, f"the buffering distance is too small ({dist:,.1f}m < {10.0:,.1f}m)"
    assert dist <= MAXIMUM_VINCENTY, f"the buffering distance is too large ({dist:,.1f}m > {MAXIMUM_VINCENTY:,.1f}m)"
    assert nAng >= 9, f"the number of angles is too small ({nAng:,d} < {9:,d})"
    assert nAng % 2 == 1, f"the number of angles is even ({nAng:,d})"
    assert (nAng - 1) % 4 == 0, f"the number of angles is not 4n+1 ({nAng:,d})"

    # **************************************************************************
    # Step 1: Convert the CoordinateSequence to a NumPy array of the original  #
    #         points                                                           #
    # **************************************************************************

    # Clean the input ...
    coords = clean(
        coords,
         debug = debug,
        prefix = prefix,
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
                      eps = eps,
                fillSpace = fillSpace,
                    nIter = nIter,
                   prefix = prefix,
                 ramLimit = ramLimit,
                      tol = tol,
            ).coords
        )                                                                       # [°]
    else:
        # Convert the CoordinateSequence to a NumPy array ...
        points1 = numpy.array(coords)                                           # [°]

    # Create short-hand ...
    npoint = int(points1.shape[0])                                              # [#]

    # **************************************************************************
    # Step 2: Buffer the NumPy array of the original points to get a NumPy     #
    #         array of the rings around them                                   #
    # **************************************************************************

    # Buffer (in Geodesic space) the CoordinateSequence ...
    if fortran:
        points2 = funcs.buffer_points_crudely(
            points1,
            dist,
            nAng,
        )                                                                       # [°]
    else:
        points2 = _buffer_points_crudely(
            points1,
            dist,
            nAng,
                 eps = eps,
               nIter = nIter,
            ramLimit = ramLimit,
        )                                                                       # [°]

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
              huge = bool(dist > 0.25 * CIRCUMFERENCE_OF_EARTH),
            prefix = prefix,
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
        check(buffs, prefix = prefix)

    # Check if the user wants to fill in the [Multi]Polygon ...
    # NOTE: This is only needed because the "shapely.ops.unary_union()" call
    #       above includes a "simplify()".
    if simp < 0.0 < fill:
        # Fill in [Multi]Polygon ...
        buffs = fillin(
            buffs,
            fill,
                debug = debug,
                  eps = eps,
            fillSpace = fillSpace,
                nIter = nIter,
               prefix = prefix,
             ramLimit = ramLimit,
                  tol = tol,
        )
        if debug:
            check(buffs, prefix = prefix)

    # Check if the user wants to simplify the [Multi]Polygon ...
    # NOTE: This is only needed because the "shapely.ops.unary_union()" call
    #       above might allow more simplification.
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffsSimp = buffs.simplify(simp)
        if debug:
            check(buffsSimp, prefix = prefix)

        # Return simplified answer ...
        return buffsSimp

    # Return answer ...
    return buffs
