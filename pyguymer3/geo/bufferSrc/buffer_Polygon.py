#!/usr/bin/env python3

# Define function ...
def buffer_Polygon(
    poly,
    dist,
    /,
    *,
            debug = __debug__,
              eps = 1.0e-12,
             fill = 1.0,
        fillSpace = "EuclideanSpace",
    keepInteriors = True,
             nAng = 9,
            nIter = 100,
           prefix = ".",
         ramLimit = 1073741824,
             simp = 0.1,
              tol = 1.0e-10,
):
    """Buffer a Polygon

    This function reads in a Polygon (with an exterior and any number of
    interiors) that exists on the surface of the Earth and returns a
    [Multi]Polygon of the same Polygon buffered by a constant distance (in
    metres).

    Parameters
    ----------
    poly : shapely.geometry.polygon.Polygon
        the Polygon
    dist : float
        the Geodesic distance to buffer each point within the Polygon by (in
        metres)
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
    keepInteriors : bool, optional
        keep the interiors of the Polygon
    nAng : int, optional
        the number of angles around each point within the Polygon that are
        calculated when buffering
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)
    simp : float, optional
        how much intermediary [Multi]Polygons are simplified by; negative values
        disable simplification (in degrees)
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the buffered Polygon

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
        import shapely
        import shapely.geometry
        import shapely.ops
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ..check import check
    from ..fillin import fillin
    from .buffer_LinearRing import buffer_LinearRing

    # **************************************************************************

    # Check argument ...
    assert isinstance(poly, shapely.geometry.polygon.Polygon), "\"poly\" is not a Polygon"
    if debug:
        check(poly, prefix = prefix)

    # Initialize list ...
    buffs = []

    # Check if the user wants to keep interiors ...
    if keepInteriors:
        # Append Polygon to list ...
        buffs.append(poly)
    else:
        # Append a correctly oriented Polygon made up of just the exterior
        # LinearRing to list ...
        buffs.append(shapely.geometry.polygon.orient(shapely.geometry.polygon.Polygon(poly.exterior)))

    # Append buffer of exterior LinearRing to list ...
    # TODO: Think about finding the bounding box of the exterior ring and not
    #       bothering buffering it if is the whole Earth, i.e., the Polygon is
    #       the Earth with an interior ring. Make sure to not just use the
    #       bounding box, i.e., the exterior ring should not be more complicated
    #       than four corners too.
    buffs.append(
        buffer_LinearRing(
            poly.exterior,
            dist,
                debug = debug,
                  eps = eps,
                 fill = fill,
            fillSpace = fillSpace,
                 nAng = nAng,
                nIter = nIter,
               prefix = prefix,
             ramLimit = ramLimit,
                 simp = simp,
                  tol = tol,
        )
    )

    # Check if the user wants to keep interiors ...
    if keepInteriors:
        # Loop over interior LinearRings ...
        for interior in poly.interiors:
            # Skip if it doesn't contain any length ...
            if interior.length < tol:
                if debug:
                    print(f"INFO: Removing a tiny-length interior ring at ({interior.centroid.x:+.6f}°,{interior.centroid.y:+.6f}°).")
                continue

            # Append buffer of interior LinearRing to list ...
            buffs.append(
                buffer_LinearRing(
                    interior,
                    dist,
                        debug = debug,
                          eps = eps,
                         fill = fill,
                    fillSpace = fillSpace,
                         nAng = nAng,
                        nIter = nIter,
                       prefix = prefix,
                     ramLimit = ramLimit,
                         simp = simp,
                          tol = tol,
                )
            )

    # Convert list of [Multi]Polygons to a (unified) [Multi]Polygon ...
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
