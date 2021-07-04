def buffer_CoordinateSequence(coords, dist, kwArgCheck = None, debug = False, fill = -1.0, nang = 19, simp = 0.1):
    """Buffer a CoordinateSequence

    This function reads in a CoordinateSequence that exists on the surface of
    the Earth and returns a [Multi]Polygon of the same CoordinateSequence
    buffered by a constant distance (in metres).

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
            the CoordinateSequence
    dist : float
            the distance to buffer each point within the CoordinateSequence by (in metres)
    debug : bool, optional
            print debug messages
    fill : float, optional
            how many intermediary points are added to fill in the straight lines which connect the points; negative values disable filling
    nang : int, optional
            the number of angles around each point within the CoordinateSequence that are calculated when buffering
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered CoordinateSequence
    """

    # Import standard modules ...
    import os

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from ._buffer_points import _buffer_points
    from .calc_dist_between_two_locs import calc_dist_between_two_locs
    from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
    # from .find_point_on_great_circle import find_point_on_great_circle

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(coords, shapely.coords.CoordinateSequence):
        raise TypeError("\"coords\" is not a CoordinateSequence") from None

    # Convert the CoordinateSequence to a NumPy array ...
    raw = numpy.array(coords)                                                   # [°], [°]

    # Buffer the CoordinateSequence ...
    poly = _buffer_points(raw, dist, debug = debug, nang = nang, simp = simp)

    # Check if the user doesn't want to fill in the straight lines which connect
    # the raw points ...
    if fill <= 0.0:
        # Return buffered CoordinateSequence ...
        return poly

    # Initialize list ...
    buffs = []
    buffs.append(poly)

    # Loop over raw points ...
    for iraw in range(raw.shape[0] - 1):
        # Create short-hands ...
        a = raw[iraw, :]                                                        # [°], [°]
        b = raw[iraw + 1, :]                                                    # [°], [°]

        # Find the Geodesic distance between A and B, as well as the bearing
        # from A to B and from B to A ...
        geodesicDist, a2b, b2a = calc_dist_between_two_locs(a[0], a[1], b[0], b[1]) # [m], [°], [°]

        # # Find the number of filled segments required between A and B ...
        # # NOTE: This is the number of fence panels, not the number of fence
        # #       posts.
        # ns = max(2, round(fill * geodesicDist / dist))                          # [#]

        # Calculate the two points port and starboard (C and D) when going from
        # A to B ...
        c = calc_loc_from_loc_and_bearing_and_dist(a[0], a[1], (a2b - 90.0) % 360.0, dist)[:2]  # [°], [°]
        d = calc_loc_from_loc_and_bearing_and_dist(a[0], a[1], (a2b + 90.0) % 360.0, dist)[:2]  # [°], [°]

        # Calculate the two points port and starboard (E and F) when going from
        # B to A ...
        e = calc_loc_from_loc_and_bearing_and_dist(b[0], b[1], (b2a - 90.0) % 360.0, dist)[:2]  # [°], [°]
        f = calc_loc_from_loc_and_bearing_and_dist(b[0], b[1], (b2a + 90.0) % 360.0, dist)[:2]  # [°], [°]

        poly = shapely.geometry.MultiPoint([a, b, c, d, e, f]).convex_hull

        # # Swap E and F around if the Polygon will be a bowtie ...
        # tmp1 = calc_dist_between_two_locs(c[0], c[1], e[0], e[1])[1]
        # tmp2 = calc_dist_between_two_locs(c[0], c[1], f[0], f[1])[1]
        # if tmp1 > tmp2:
        #     e, f = f, e
        #
        # # Find the fractions along the route from A to B of each filled point ...
        # fracs = numpy.linspace(0.0, 1.0, endpoint = False, num = ns)[1:]
        #
        # # Initialize arrays ...
        # c2e = numpy.zeros((fracs.size, 2), dtype = numpy.float64)               # [°], [°]
        # f2d = numpy.zeros((fracs.size, 2), dtype = numpy.float64)               # [°], [°]
        #
        # # Loop over filled points ...
        # for ifill in range(ns - 1):
        #     # Calculate the filled point from C to E for this fraction ...
        #     c2e[ifill, 0], c2e[ifill, 1] = find_point_on_great_circle(fracs[ifill], c[0], c[1], e[0], e[1]) # [°], [°]
        #
        #     # Calculate the filled point from F to D for this fraction ...
        #     f2d[ifill, 0], f2d[ifill, 1] = find_point_on_great_circle(fracs[ifill], f[0], f[1], d[0], d[1]) # [°], [°]
        #
        # # Create a ring that "goes from A to C to E to B to F to D to A" ...
        # ring = []                                                               # [°], [°]
        # ring.append(a)                                                          # [°], [°]
        # ring.append(c)                                                          # [°], [°]
        # for ifill in range(ns - 1):
        #     ring.append(c2e[ifill, :])                                          # [°], [°]
        # ring.append(e)                                                          # [°], [°]
        # ring.append(b)                                                          # [°], [°]
        # ring.append(f)                                                          # [°], [°]
        # for ifill in range(ns - 1):
        #     ring.append(f2d[ifill, :])                                          # [°], [°]
        # ring.append(d)                                                          # [°], [°]
        # ring.append(a)                                                          # [°], [°]
        #
        # # Create a Polygon from the ring and append it to the list ...
        # poly = shapely.geometry.polygon.Polygon(ring)

        # Check [Multi]Polygon ...
        if not poly.is_valid:
            with open(f"{os.path.dirname(__file__)}/buffer_CoordinateSequence.debug.ring.csv", "wt") as fobj:
                fobj.write("lon [°],lat [°]\n")
                for x, y in poly.exterior:
                    fobj.write(f"{x:.15e},{y:.15e}\n")
            raise Exception(f"\"poly\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(poly)})") from None

        # Check [Multi]Polygon ...
        if poly.is_empty:
            raise Exception("\"poly\" is an empty [Multi]Polygon") from None

        # Append the Polygon to the list ...
        buffs.append(poly)

    # Convert list of [Multi]Polygons to (unified) [Multi]Polygon ...
    buffs = shapely.ops.unary_union(buffs)

    # Check [Multi]Polygon ...
    if not buffs.is_valid:
        raise Exception(f"\"buffs\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffs)})") from None

    # Check [Multi]Polygon ...
    if buffs.is_empty:
        raise Exception("\"buffs\" is an empty [Multi]Polygon") from None

    # Check if the user wants to simplify the [Multi]Polygon ...
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffsSimp = buffs.simplify(simp)

        # Check simplified [Multi]Polygon ...
        if buffsSimp.is_valid and not buffsSimp.is_empty:
            # Return simplified answer ...
            return buffsSimp

        if debug:
            print(f"WARNING: \"buffsSimp\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buffsSimp)}), will return \"buffs\" instead")

    # Return answer ...
    return buffs
