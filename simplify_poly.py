# -*- coding: utf-8 -*-

def simplify_poly(poly1, simp = 0.1):
    """
    This function accepts either a Polygon or a MultiPolygon and creates a
    Polygon or a MultiPolygon from the simplified (member) Polygon(s).
    """

    # Import modules ...
    import shapely
    import shapely.geometry
    import shapely.validation

    # Create empty list ...
    poly2 = []

    # Check what the argument is ...
    if isinstance(poly1, shapely.geometry.multipolygon.MultiPolygon):
        # Loop over Polygons and add simplified copys to the list ...
        for tmp1 in poly1.geoms:
            poly2.append(tmp1.simplify(simp))
    elif isinstance(poly1, shapely.geometry.polygon.Polygon):
        # Add simplified copy to the list ...
        poly2.append(poly1.simplify(simp))
    else:
        raise TypeError("\"poly1\" is an unexpected type")

    # Convert list to MultiPolygon ...
    poly2 = shapely.geometry.multipolygon.MultiPolygon(poly2)
    if not poly2.is_valid:
        raise Exception("\"poly2\" is not a valid [Multi]Polygon ({0:s})".format(shapely.validation.explain_validity(poly2)))

    # Return answer ...
    return poly2
