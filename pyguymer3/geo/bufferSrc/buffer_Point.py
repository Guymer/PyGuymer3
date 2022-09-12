def buffer_Point(point, dist, kwArgCheck = None, debug = False, fill = 1.0, fillSpace = "EuclideanSpace", nang = 9, simp = 0.1, tol = 1.0e-10):
    """Buffer a Point

    This function reads in a Point that exists on the surface of the Earth and
    returns a [Multi]Polygon of the same Point buffered by a constant distance
    (in metres).

    Parameters
    ----------
    point : shapely.geometry.point.Point
        the Point
    dist : float
        the distance to buffer the Point by (in metres)
    debug : bool, optional
        print debug messages
    fill : float, optional
        the Euclidean or Geodetic distance to fill in between each point within
        the shapes by (in degrees or metres)
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    nang : int, optional
        the number of angles around the Point that are calculated when buffering
    simp : float, optional
        how much intermediary [Multi]Polygons are simplified by; negative values
        disable simplification (in degrees)
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the buffered Point

    Notes
    -----
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
    from ..check import check
    from .buffer_CoordinateSequence import buffer_CoordinateSequence

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(point, shapely.geometry.point.Point):
        raise TypeError("\"point\" is not a Point") from None
    check(point)

    # Return buffered Point ...
    return buffer_CoordinateSequence(point.coords, dist, debug = debug, fill = fill, fillSpace = fillSpace, nang = nang, simp = simp, tol = tol)
