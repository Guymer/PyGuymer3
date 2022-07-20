def fillin_MultiPolygon(multipoly, fill, kwArgCheck = None, debug = False, fillSpace = "EuclideanSpace", tol = 1.0e-10):
    """Fill in a MultiPolygon

    This function reads in a MultiPolygon, made up of Polygons (with an exterior
    and any number of interiors), that exists on the surface of the Earth and
    returns a MultiPolygon of the same MultiPolygon filled in by a constant
    distance: either in degrees in Euclidean space; or in metres in Geodesic
    space.

    Parameters
    ----------
    multipoly : shapely.geometry.multipolygon.MultiPolygon
            the MultiPolygon
    fill : float
            the Euclidean or Geodesic distance to fill in between each point within the shape by (in degrees or metres)
    debug : bool, optional
            print debug messages
    fillSpace : str, optional
            the geometric space to perform the filling in (either "EuclideanSpace" or "GeodesicSpace")
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    fills : shapely.geometry.multipolygon.MultiPolygon
            the filled in MultiPolygon
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._debug import _debug
    from .fillin_Polygon import fillin_Polygon

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(multipoly, shapely.geometry.multipolygon.MultiPolygon):
        raise TypeError("\"multipoly\" is not a MultiPolygon") from None
    if not multipoly.is_valid:
        _debug(multipoly)
        raise Exception(f"\"multipoly\" is not a valid MultiPolygon ({shapely.validation.explain_validity(multipoly)})") from None
    if multipoly.is_empty:
        raise Exception("\"multipoly\" is an empty MultiPolygon") from None

    # Initialize list ...
    polys = []

    # Loop over Polygons ...
    for poly in multipoly.geoms:
        # Append filled in Polygon to list ...
        polys.append(fillin_Polygon(poly, fill, debug = debug, fillSpace = fillSpace, tol = tol))

    # Convert list of Polygons to a (unified) MultiPolygon ...
    fills = shapely.ops.unary_union(polys)

    # Clean up ...
    del polys

    # Check MultiPolygon ...
    if not isinstance(fills, shapely.geometry.multipolygon.MultiPolygon):
        raise TypeError("\"fills\" is not a MultiPolygon") from None
    if not fills.is_valid:
        _debug(fills)
        raise Exception(f"\"fills\" is not a valid MultiPolygon ({shapely.validation.explain_validity(fills)})") from None
    if fills.is_empty:
        raise Exception("\"fills\" is an empty MultiPolygon") from None

    # Return answer ...
    return fills
