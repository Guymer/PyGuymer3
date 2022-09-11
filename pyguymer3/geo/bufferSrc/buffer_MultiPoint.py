def buffer_MultiPoint(multipoint, dist, kwArgCheck = None, debug = False, fill = 1.0, fillSpace = "EuclideanSpace", nang = 19, simp = 0.1, tol = 1.0e-10):
    """Buffer a MultiPoint

    This function reads in a MultiPoint that exists on the surface of the Earth
    and returns a [Multi]Polygon of the same MultiPoint buffered by a constant
    distance (in metres).

    Parameters
    ----------
    multipoint : shapely.geometry.multipoint.MultiPoint
        the MultiPoint
    dist : float
        the Geodetic distance to buffer each point within the MultiPoint by (in
        metres)
    debug : bool, optional
        print debug messages
    fill : float, optional
        the Euclidean or Geodetic distance to fill in between each point within
        the shapes by (in degrees or metres)
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    nang : int, optional
        the number of angles around each point within the MultiPoint that are
        calculated when buffering
    simp : float, optional
        how much intermediary [Multi]Polygons are simplified by; negative values
        disable simplification (in degrees)
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the buffered MultiPoint

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
    from ..fillin import fillin
    from .buffer_Point import buffer_Point

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(multipoint, shapely.geometry.multipoint.MultiPoint):
        raise TypeError("\"multipoint\" is not a MultiPoint") from None
    check(multipoint)

    # Initialize list ...
    buffs = []

    # Loop over Point ...
    for point in multipoint.geoms:
        # Append buffer of Point to list ...
        buffs.append(buffer_Point(point, dist, debug = debug, fill = fill, fillSpace = fillSpace, nang = nang, simp = simp, tol = tol))

    # Convert list of [Multi]Polygons to a (unified) [Multi]Polygon ...
    buffs = shapely.ops.unary_union(buffs).simplify(tol)
    check(buffs)

    # Check if the user wants to fill in the [Multi]Polygon ...
    if fill > 0.0:
        # Fill in [Multi]Polygon ...
        buffs = fillin(buffs, fill, debug = debug, fillSpace = fillSpace)

    # Check if the user wants to simplify the [Multi]Polygon ...
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffsSimp = buffs.simplify(simp)
        check(buffsSimp)

        # Return simplified answer ...
        return buffsSimp

    # Return answer ...
    return buffs
