#!/usr/bin/env python3

# Define function ...
def buffer_GeometryCollection(
    geometrycollection,
    dist,
    /,
    *,
    attemptFortran = True,
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
    """Buffer a GeometryCollection

    This function reads in a GeometryCollection that exists on the surface of
    the Earth and returns a [Multi]Polygon of the same GeometryCollection
    buffered by a constant distance (in metres).

    Parameters
    ----------
    geometrycollection : shapely.geometry.collection.GeometryCollection
        the GeometryCollection
    dist : float
        the Geodesic distance to buffer each geometry within the
        GeometryCollection by (in metres)
    attemptFortran : bool, optional
        attempt to use a f2py implementation
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
        the number of angles around each point within the GeometryCollection
        that are calculated when buffering
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
        the buffered GeometryCollection

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
    from .buffer_CoordinateSequence import buffer_CoordinateSequence
    from .buffer_LineString import buffer_LineString
    from .buffer_LinearRing import buffer_LinearRing
    from .buffer_MultiLineString import buffer_MultiLineString
    from .buffer_MultiPoint import buffer_MultiPoint
    from .buffer_MultiPolygon import buffer_MultiPolygon
    from .buffer_Point import buffer_Point
    from .buffer_Polygon import buffer_Polygon
    from ..check import check
    from ..fillin import fillin

    # **************************************************************************

    # Check argument ...
    assert isinstance(geometrycollection, shapely.geometry.collection.GeometryCollection), "\"geometrycollection\" is not a GeometryCollection"
    if debug:
        check(geometrycollection, prefix = prefix)

    # Initialize list ...
    buffs = []

    # Loop over geometries ...
    for geom in geometrycollection.geoms:
        # Append buffer of geometry to list ...
        match geom:
            case shapely.coords.CoordinateSequence():
                buffs.append(
                    buffer_CoordinateSequence(
                        geom,
                        dist,
                        attemptFortran = attemptFortran,
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
            case shapely.geometry.point.Point():
                buffs.append(
                    buffer_Point(
                        geom,
                        dist,
                        attemptFortran = attemptFortran,
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
            case shapely.geometry.multipoint.MultiPoint():
                buffs.append(
                    buffer_MultiPoint(
                        geom,
                        dist,
                        attemptFortran = attemptFortran,
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
            case shapely.geometry.polygon.LinearRing():
                buffs.append(
                    buffer_LinearRing(
                        geom,
                        dist,
                        attemptFortran = attemptFortran,
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
            case shapely.geometry.linestring.LineString():
                buffs.append(
                    buffer_LineString(
                        geom,
                        dist,
                        attemptFortran = attemptFortran,
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
            case shapely.geometry.multilinestring.MultiLineString():
                buffs.append(
                    buffer_MultiLineString(
                        geom,
                        dist,
                        attemptFortran = attemptFortran,
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
            case shapely.geometry.polygon.Polygon():
                buffs.append(
                    buffer_Polygon(
                        geom,
                        dist,
                        attemptFortran = attemptFortran,
                                 debug = debug,
                                   eps = eps,
                                  fill = fill,
                             fillSpace = fillSpace,
                         keepInteriors = keepInteriors,
                                  nAng = nAng,
                                 nIter = nIter,
                                prefix = prefix,
                              ramLimit = ramLimit,
                                  simp = simp,
                                   tol = tol,
                    )
                )
            case shapely.geometry.multipolygon.MultiPolygon():
                buffs.append(
                    buffer_MultiPolygon(
                        geom,
                        dist,
                        attemptFortran = attemptFortran,
                                 debug = debug,
                                   eps = eps,
                                  fill = fill,
                             fillSpace = fillSpace,
                         keepInteriors = keepInteriors,
                                  nAng = nAng,
                                 nIter = nIter,
                                prefix = prefix,
                              ramLimit = ramLimit,
                                  simp = simp,
                                   tol = tol,
                    )
                )
            case _:
                raise TypeError(f"\"geometrycollection\" is an unexpected type ({repr(type(geometrycollection))})") from None

    # Convert list of [Multi]Polygons to a (unified) [Multi]Polygon ...
    buffs = shapely.geometry.polygon.orient(shapely.ops.unary_union(buffs)).simplify(tol)
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
