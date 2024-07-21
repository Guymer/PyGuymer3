#!/usr/bin/env python3

# Define function ...
def fillin_Polygon(poly, fill, /, *, debug = False, eps = 1.0e-12, fillSpace = "EuclideanSpace", nmax = 100, prefix = ".", ramLimit = 1073741824, tol = 1.0e-10):
    """Fill in a Polygon

    This function reads in a Polygon (with an exterior and any number of
    interiors) that exists on the surface of the Earth and returns a
    Polygon of the same Polygon filled in by a constant distance: either in
    degrees in Euclidean space; or in metres in Geodesic space.

    Parameters
    ----------
    poly : shapely.geometry.polygon.Polygon
        the Polygon
    fill : float
        the Euclidean or Geodesic distance to fill in between each point within
        the shape by (in degrees or metres)
    debug : bool, optional
        print debug messages
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    nmax : int, optional
        the maximum number of the Vincenty formula iterations
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    fills : shapely.geometry.polygon.Polygon
        the filled in Polygon

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
    from .fillin_LinearRing import fillin_LinearRing

    # Check argument ...
    if not isinstance(poly, shapely.geometry.polygon.Polygon):
        raise TypeError("\"poly\" is not a Polygon") from None
    if debug:
        check(poly, prefix = prefix)

    # Fill in exterior LinearRing ...
    exterior = fillin_LinearRing(
        poly.exterior,
        fill,
            debug = debug,
              eps = eps,
        fillSpace = fillSpace,
             nmax = nmax,
           prefix = prefix,
         ramLimit = ramLimit,
    )

    # Initialize list ...
    interiors = []

    # Loop over interior LinearRings ...
    for interior in poly.interiors:
        # Skip if it doesn't contain any length ...
        if interior.length < tol:
            if debug:
                print(f"INFO: Removing a tiny-length interior ring at ({interior.centroid.x:+.6f}°,{interior.centroid.y:+.6f}°).")
            continue

        # Append filled in interior LinearRing to list ...
        interiors.append(
            fillin_LinearRing(
                interior,
                fill,
                    debug = debug,
                      eps = eps,
                fillSpace = fillSpace,
                     nmax = nmax,
                   prefix = prefix,
                 ramLimit = ramLimit,
            )
        )

    # Convert exterior LinearRing and list of interior LinearRings to a
    # correctly oriented Polygon ...
    fills = shapely.geometry.polygon.orient(shapely.geometry.polygon.Polygon(exterior, interiors))
    if debug:
        check(fills, prefix = prefix)

    # Return answer ...
    return fills
