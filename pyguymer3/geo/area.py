#!/usr/bin/env python3

# Define function ...
def area(shape, /, *, eps = 1.0e-12, level = 1, nmax = 100, onlyValid = False, repair = False):
    """Find the area of a shape.

    Parameters
    ----------
    shape : shapely.coords.CoordinateSequence, shapely.geometry.point.Point, shapely.geometry.multipoint.MultiPoint, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the shape
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    level : int, optional
        the number of levels to split the shape into
    nmax : int, optional
        the maximum number of the Vincenty formula iterations
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons

    Returns
    -------
    area : float
        the area (in metres-squared)

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
        import shapely.ops
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._area import _area
    from .extract_polys import extract_polys

    # **************************************************************************

    # Initialize total ...
    tot = 0.0                                                                   # [m2]

    # Loop over Polygons in the Voronoi diagram of the shape ...
    for voronoi in extract_polys(shapely.ops.voronoi_diagram(shape), onlyValid = onlyValid, repair = repair):
        # Loop over parts of the Polygons which intersect the shape ...
        for voronoiPart in extract_polys(shape.intersection(voronoi), onlyValid = onlyValid, repair = repair):
            # Loop over triangles within the Polygon ...
            for triangle1 in extract_polys(shapely.ops.triangulate(voronoiPart), onlyValid = onlyValid, repair = repair):
                # Check if the user wants this level of refinement ...
                if level == 1:
                    # Increment the total and move on to the next one ...
                    tot += _area(triangle1, eps = eps, nmax = nmax)             # [m2]
                    continue

                # Loop over triangles within the triangle ...
                for triangle2 in extract_polys(shapely.ops.triangulate(triangle1), onlyValid = onlyValid, repair = repair):
                    # Check if the user wants this level of refinement ...
                    if level == 2:
                        # Increment the total and move on to the next one ...
                        tot += _area(triangle2, eps = eps, nmax = nmax)         # [m2]
                        continue

                    # Loop over triangles within the triangle ...
                    for triangle3 in extract_polys(shapely.ops.triangulate(triangle2), onlyValid = onlyValid, repair = repair):
                        # Check if the user wants this level of refinement ...
                        if level == 3:
                            # Increment the total and move on to the next one ...
                            tot += _area(triangle3, eps = eps, nmax = nmax)     # [m2]
                            continue

                        # Loop over triangles within the triangle ...
                        for triangle4 in extract_polys(shapely.ops.triangulate(triangle3), onlyValid = onlyValid, repair = repair):
                            # Check if the user wants this level of refinement ...
                            if level == 4:
                                # Increment the total and move on to the next
                                # one ...
                                tot += _area(triangle4, eps = eps, nmax = nmax) # [m2]
                                continue

                            # Cry ...
                            raise Exception(f"\"level\" is too large (f{level:,d})") from None

    # Return total ...
    return tot
