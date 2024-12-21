#!/usr/bin/env python3

# Define function ...
def buffer(
    shape,
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
    """Buffer a shape

    This function reads in a shape that exists on the surface of the Earth and
    returns the same shape buffered by a constant distance (in metres).

    Parameters
    ----------
    shape : shapely.coords.CoordinateSequence, shapely.geometry.point.Point, shapely.geometry.multipoint.MultiPoint, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the shape
    dist : float
        the Geodesic distance to buffer each point within the shape by (in
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
        the number of angles around each point within the shape that are
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
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the buffered shape

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
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .bufferSrc import buffer_CoordinateSequence
    from .bufferSrc import buffer_LinearRing
    from .bufferSrc import buffer_LineString
    from .bufferSrc import buffer_MultiLineString
    from .bufferSrc import buffer_MultiPoint
    from .bufferSrc import buffer_MultiPolygon
    from .bufferSrc import buffer_Point
    from .bufferSrc import buffer_Polygon

    # **************************************************************************

    # Check what type it is ...
    match shape:
        case shapely.coords.CoordinateSequence():
            # Return it buffered ...
            return buffer_CoordinateSequence(
                shape,
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
        case shapely.geometry.point.Point():
            # Return it buffered ...
            return buffer_Point(
                shape,
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
        case shapely.geometry.multipoint.MultiPoint():
            # Return it buffered ...
            return buffer_MultiPoint(
                shape,
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
        case shapely.geometry.polygon.LinearRing():
            # Return it buffered ...
            return buffer_LinearRing(
                shape,
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
        case shapely.geometry.linestring.LineString():
            # Return it buffered ...
            return buffer_LineString(
                shape,
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
        case shapely.geometry.multilinestring.MultiLineString():
            # Return it buffered ...
            return buffer_MultiLineString(
                shape,
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
        case shapely.geometry.polygon.Polygon():
            # Return it buffered ...
            return buffer_Polygon(
                shape,
                dist,
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
        case shapely.geometry.multipolygon.MultiPolygon():
            # Return it buffered ...
            return buffer_MultiPolygon(
                shape,
                dist,
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
        case _:
            # Cry ...
            raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
