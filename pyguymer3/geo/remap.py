def remap(poly, kwArgCheck = None, tol = 1.0e-10):
    """Re-map a Polygon

    This function reads in a Polygon that should exist on the surface of
    the Earth and returns a [Multi]Polygon of the same Polygon existing on the
    surface of the Earth.

    Parameters
    ----------
    poly : shapely.geometry.polygon.Polygon
            the Polygon
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    polys : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the re-mapped Polygon
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
    from .remapSrc import earthA
    from .remapSrc import earthB
    from .remapSrc import earthC
    from .remapSrc import earthD
    from .remapSrc import earthE
    from .remapSrc import earthF
    from .remapSrc import earthG

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(poly, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly\" is not a Polygon") from None
    if not poly.is_valid:
        _debug(poly)
        raise Exception(f"\"poly\" is not a valid Polygon ({shapely.validation.explain_validity(poly)})") from None
    if poly.is_empty:
        raise Exception("\"poly\" is an empty Polygon") from None

    # Initialize list ...
    polys = []

    # Append the Polygons, which are the subset of the Polygon that intersects
    # with Earth-A that has been re-mapped on to Earth-D, to list ...
    polys += earthA(poly)

    # Append the Polygons, which are the subset of the Polygon that intersects
    # with Earth-B that has been re-mapped on to Earth-D, to list ...
    polys += earthB(poly)

    # Append the Polygons, which are the subset of the Polygon that intersects
    # with Earth-C that has been re-mapped on to Earth-D, to list ...
    polys += earthC(poly)

    # Append the Polygons, which are the subset of the Polygon that intersects
    # with Earth-D that has been re-mapped on to Earth-D, to list ...
    polys += earthD(poly)

    # Append the Polygons, which are the subset of the Polygon that intersects
    # with Earth-E that has been re-mapped on to Earth-D, to list ...
    polys += earthE(poly)

    # Append the Polygons, which are the subset of the Polygon that intersects
    # with Earth-F that has been re-mapped on to Earth-D, to list ...
    polys += earthF(poly)

    # Append the Polygons, which are the subset of the Polygon that intersects
    # with Earth-G that has been re-mapped on to Earth-D, to list ...
    polys += earthG(poly)

    # Convert list of Polygons to (unified) [Multi]Polygon ...
    polys = shapely.ops.unary_union(polys).simplify(tol)
    if not polys.is_valid:
        _debug(polys)
        raise Exception(f"\"polys\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(polys)})") from None
    if polys.is_empty:
        raise Exception("\"polys\" is an empty [Multi]Polygon") from None

    # Return answer ...
    return polys
