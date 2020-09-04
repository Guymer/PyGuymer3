def buffer_point_crudely(lon1, lat1, dist, nang):
    """
    This function reads in coordinates (in degrees) that exist on the surface of
    the Earth and returns a list of coordinates that are a ring around the input
    coordinates buffered by a constant distance (in metres).
    """

    # Load sub-functions ...
    from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist

    # Create empty list ...
    ring = []

    # Loop over angles ...
    # NOTE: The first and last angles will *always* be exactly North.
    # NOTE: The most two subsequent points can be apart is ~45 degrees (with
    #       nang >= 9).
    for iang in range(nang):
        # Calculate initial angle, then the ring coordinates and add them to the
        # list ...
        ang1 = 360.0 * float(iang) / float(nang - 1)
        lon2, lat2, ang2 = calc_loc_from_loc_and_bearing_and_dist(lon1, lat1, ang1, dist)
        ring.append((lon2, lat2))

    # Return answer ...
    return ring
