#!/usr/bin/env python3

# Define function ...
def _area(triangle, /, *, eps = 1.0e-12, nMax = 100):
    """Find the area of a triangle.

    Parameters
    ----------
    triangle : shapely.geometry.polygon.Polygon
        the triangle
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    nMax : int, optional
        the maximum number of Vincenty formula iterations

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

    # Import standard modules ...
    import math

    # Import sub-functions ...
    from .calc_dist_between_two_locs import calc_dist_between_two_locs

    # Find the distance from the second point to the first point, and the
    # bearing of the first point as viewed from the second point ...
    a, bearing1, _ = calc_dist_between_two_locs(
        triangle.exterior.coords[1][0],
        triangle.exterior.coords[1][1],
        triangle.exterior.coords[0][0],
        triangle.exterior.coords[0][1],
         eps = eps,
        nMax = nMax,
    )                                                                           # [m], [°]

    # Find the distance from the second point to the third point, and the
    # bearing of the third point as viewed from the second point ...
    b, bearing2, _ = calc_dist_between_two_locs(
        triangle.exterior.coords[1][0],
        triangle.exterior.coords[1][1],
        triangle.exterior.coords[2][0],
        triangle.exterior.coords[2][1],
         eps = eps,
        nMax = nMax,
    )                                                                           # [m], [°]

    # Use the two bearings to find the interior angle between the first and
    # third points ...
    C = (bearing2 - bearing1) % 180.0                                           # [°]

    # Return answer ...
    return 0.5 * a * b * math.sin(math.radians(C))
