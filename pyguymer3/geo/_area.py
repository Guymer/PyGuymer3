def _area(triangle, kwArgCheck = None, eps = 1.0e-12, nmax = 100):
    # Import standard modules ...
    import math

    # Import sub-functions ...
    from .calc_dist_between_two_locs import calc_dist_between_two_locs

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Find the distance from the second point to the first point, and the
    # bearing of the first point as viewed from the second point ...
    a, bearing1, _ = calc_dist_between_two_locs(
        triangle.exterior.coords[1][0],
        triangle.exterior.coords[1][1],
        triangle.exterior.coords[0][0],
        triangle.exterior.coords[0][1],
         eps = eps,
        nmax = nmax,
    )                                                                           # [m], [°]

    # Find the distance from the second point to the third point, and the
    # bearing of the third point as viewed from the second point ...
    b, bearing2, _ = calc_dist_between_two_locs(
        triangle.exterior.coords[1][0],
        triangle.exterior.coords[1][1],
        triangle.exterior.coords[2][0],
        triangle.exterior.coords[2][1],
         eps = eps,
        nmax = nmax,
    )                                                                           # [m], [°]

    # Use the two bearings to find the interior angle between the first and
    # third points ...
    C = (bearing2 - bearing1) % 180.0                                           # [°]

    # Return answer ...
    return 0.5 * a * b * math.sin(math.radians(C))
