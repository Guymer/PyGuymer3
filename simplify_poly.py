def simplify_poly(poly1, simp = 0.1):
    """Simplify a [Multi]Polygon

    This function accepts either a Polygon or a MultiPolygon and creates a
    Polygon or a MultiPolygon from the simplified (member) Polygon(s).

    Parameters
    ----------
    poly1 : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the [Multi]Polygon to simplify
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification

    Returns
    -------
    poly2 : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the simplified [Multi]Polygon
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Create empty list ...
    poly2 = []

    # Check what the argument is ...
    if isinstance(poly1, shapely.geometry.multipolygon.MultiPolygon):
        # Loop over Polygons and add simplified copys to the list if valid ...
        for geom in poly1.geoms:
            if simp > 0.0:
                geomSimp = geom.simplify(simp)
                if geomSimp.is_valid:
                    poly2.append(geomSimp)
                else:
                    poly2.append(geom)
            else:
                poly2.append(geom)
    elif isinstance(poly1, shapely.geometry.polygon.Polygon):
        # Add simplified copy to the list if valid ...
        if simp > 0.0:
            poly1Simp = poly1.simplify(simp)
            if poly1Simp.is_valid:
                poly2.append(poly1Simp)
            else:
                poly2.append(poly1)
        else:
            poly2.append(poly1)
    else:
        raise TypeError("\"poly1\" is an unexpected type") from None

    # Convert list of Polygons to (unified) MultiPolygon ...
    poly2 = shapely.ops.unary_union(poly2)

    # Check MultiPolygon ...
    if not poly2.is_valid:
        raise Exception("\"poly2\" is not a valid [Multi]Polygon ({0:s})".format(shapely.validation.explain_validity(poly2))) from None

    # Check if the user wants to simplify the MultiPolygon ...
    if simp > 0.0:
        # Simplify MultiPolygon ...
        poly2Simp = poly2.simplify(simp)

        # Check simplified MultiPolygon ...
        if poly2Simp.is_valid:
            # Return simplified answer ...
            return poly2Simp

    # Return answer ...
    return poly2
