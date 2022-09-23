def buffer_MultiPolygon(multipoly, dist, kwArgCheck = None, debug = False, fill = 1.0, fillSpace = "EuclideanSpace", nang = 9, ramLimit = 1073741824, simp = 0.1, tol = 1.0e-10):
    """Buffer a MultiPolygon

    This function reads in a MultiPolygon, made up of Polygons (with an exterior
    and any number of interiors), that exists on the surface of the Earth and
    returns a [Multi]Polygon of the same MultiPolygon buffered by a constant
    distance (in metres).

    Parameters
    ----------
    multipoly : shapely.geometry.multipolygon.MultiPolygon
        the MultiPolygon
    dist : float
        the Geodetic distance to buffer each point within the MultiPolygon by
        (in metres)
    debug : bool, optional
        print debug messages
    fill : float, optional
        the Euclidean or Geodetic distance to fill in between each point within
        the shapes by (in degrees or metres)
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    nang : int, optional
        the number of angles around each point within the MultiPolygon that are
        calculated when buffering
    ramLimit : int, optional
        the maximum RAM usage of each "large" array, in bytes
    simp : float, optional
        how much intermediary [Multi]Polygons are simplified by; negative values
        disable simplification (in degrees)
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the buffered MultiPolygon

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
        import shapely.ops
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ..check import check
    from ..fillin import fillin
    from .buffer_Polygon import buffer_Polygon

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(multipoly, shapely.geometry.multipolygon.MultiPolygon):
        raise TypeError("\"multipoly\" is not a MultiPolygon") from None
    if debug:
        check(multipoly)

    # Initialize list ...
    buffs = []

    # Loop over Polygons ...
    for poly in multipoly.geoms:
        # Append buffer of Polygon to list ...
        buffs.append(buffer_Polygon(poly, dist, debug = debug, fill = fill, fillSpace = fillSpace, nang = nang, ramLimit = ramLimit, simp = simp, tol = tol))

    # Convert list of [Multi]Polygons to a (unified) [Multi]Polygon ...
    buffs = shapely.ops.unary_union(buffs).simplify(tol)
    if debug:
        check(buffs)

    # Check if the user wants to fill in the [Multi]Polygon ...
    if fill > 0.0:
        # Fill in [Multi]Polygon ...
        buffs = fillin(buffs, fill, debug = debug, fillSpace = fillSpace, ramLimit = ramLimit, tol = tol)
        if debug:
            check(buffs)

    # Check if the user wants to simplify the [Multi]Polygon ...
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffsSimp = buffs.simplify(simp)
        if debug:
            check(buffsSimp)

        # Return simplified answer ...
        return buffsSimp

    # Return answer ...
    return buffs
