def buffer_point(lon1, lat1, dist, kwArgCheck = None, nang = 19, simp = 0.1, debug = False):
    """Buffer a point

    This function reads in coordinates (in degrees) that exist on the surface of
    the Earth and returns a [Multi]Polygon of it buffered by a constant distance
    (in metres).

    Parameters
    ----------
    lon1 : float
            the longitude of the point (in degrees)
    lat1 : float
            the latitude of the point (in degrees)
    dist : float
            the distance to buffer the point by (in metres)
    nang : int, optional
            the number of angles around the point that are calculated
    simp : float, optional
            how much intermediary [Multi]Polygons are simplified by; negative values disable simplification (in degrees)
    debug : bool, optional
            print debug messages

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the buffered point
    """

    # Import standard modules ...
    import copy
    import math

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from .buffer_point_crudely import buffer_point_crudely
    from ..interpolate import interpolate

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Correct inputs ...
    lon1 = max(-180.0, min(180.0, lon1))                                        # NOTE: Limit longitude to -180 <--> +180
    lat1 = max(-90.0, min(90.0, lat1))                                          # NOTE: Limit latitude to -90 <--> +90
    dist = max(1.0, min(math.pi * 6371009.0, dist))                             # NOTE: Limit distance to 1m <--> (half-circumference)
    nang = max(9, nang)                                                         # NOTE: Must do at least 9 points around the compass

    # Buffer the point crudely ...
    try:
        from ..f90 import f90
        if debug:
            print("DEBUG: finding the ring using FORTRAN")
        ring = f90.buffer_point_crudely(lon1, lat1, dist, nang)
    except:
        if debug:
            print("DEBUG: finding the ring using Python")
        ring = buffer_point_crudely(lon1, lat1, dist, nang)

    # Initialise flag, create empty lists and append first point ...
    flag = True
    part1 = []
    part2 = []
    part1.append((ring[0, 0], ring[0, 1]))

    # Loop over angles ...
    # NOTE: Start at 1 (not 0) because the first one will be checked when the
    #       last one is (as they are the same).
    for iang in range(1, nang):
        # Check if the anti-meridian has been crossed between this and the last
        # point ...
        if min(ring[iang - 1, 0], ring[iang, 0]) < -90.0 and max(ring[iang - 1, 0], ring[iang, 0]) > +90.0:
            # NOTE: There is no way with, at most, ~45 degree jumps and only, at
            #       most, half the Earth's circumference travelled that two
            #       neighbouring points can straddle *both* -90 longitude and
            #       +90 longitude.
            # NOTE: The negative one is East of the anti-meridian.
            # NOTE: The positive one is West of the anti-meridian.

            # Create short-hand variables for interpolation and then interpolate ...
            if ring[iang - 1, 0] > ring[iang, 0]:
                x1 = ring[iang - 1, 0]
                x2 = +180.0
                x3 = ring[iang, 0] + 360.0
                y1 = ring[iang - 1, 1]
                y3 = ring[iang, 1]
            else:
                x1 = ring[iang - 1, 0]
                x2 = -180.0
                x3 = ring[iang, 0] - 360.0
                y1 = ring[iang, 1]
                y3 = ring[iang - 1, 1]
            y2 = interpolate(x1, x3, y1, y3, x2)

            # Decide which list needs appending ...
            if flag:
                # Append interpolated points on the anti-meridian ...
                part1.append(( x2, y2))
                flag = not flag
                part2.append((-x2, y2))
            else:
                # Append interpolated points on the anti-meridian ...
                part2.append(( x2, y2))
                flag = not flag
                part1.append((-x2, y2))

        # Decide which list needs appending and append point ...
        if flag:
            part1.append((ring[iang, 0], ring[iang, 1]))
        else:
            part2.append((ring[iang, 0], ring[iang, 1]))

    # Check flag ...
    # NOTE: The only way a continuous ring can cross the anti-meridian an odd
    #       number of times is if it also crosses one of the poles.
    if not flag:
        # Check if it is in the northern hemi-sphere ...
        north = True
        for coord in part1:
            if coord[1] < 0.0:
                north = False
                break
        for coord in part2:
            if coord[1] < 0.0:
                north = False
                break

        # Check if it is in the southern hemi-sphere ...
        south = True
        for coord in part1:
            if coord[1] > 0.0:
                south = False
                break
        for coord in part2:
            if coord[1] > 0.0:
                south = False
                break

        # Check hemi-spheres ...
        if not north and not south:
            raise Exception("the ring crossed the anti-meridian an odd number of times and exists in both hemi-spheres") from None

        # Add vertical lines to the pole at both ends and over-write lists ...
        tmp = []
        tmp += part1
        if north:
            tmp += [(part1[-1][0], +90.0), (part2[0][0], +90.0)]
        if south:
            tmp += [(part1[-1][0], -90.0), (part2[0][0], -90.0)]
        tmp += part2
        part1 = copy.copy(tmp)
        part2 = []

    # HACK: Sometimes the iteration returns not exactly identical locations for
    #       0 degrees and 360 degrees (call it rounding error; if only shapely
    #       had a "tolerance" parameter).
    if part1[0] != part1[-1]:
        part1[-1] = part1[0]
    if len(part2) > 0:
        if part2[0] != part2[-1]:
            part2[-1] = part2[0]

    # Create a list of Polygons if there are enough unique points to make a
    # Polygon ...
    buff = []
    if len(set(part1)) >= 3:
        tmp = shapely.geometry.polygon.Polygon(part1)
        if not tmp.is_valid:
            if debug:
                print("DEBUG: \"tmp\" is not a valid Polygon ({0:s})".format(shapely.validation.explain_validity(tmp)))
        else:
            buff.append(tmp)
    if len(set(part2)) >= 3:
        tmp = shapely.geometry.polygon.Polygon(part2)
        if not tmp.is_valid:
            if debug:
                print("DEBUG: \"tmp\" is not a valid Polygon ({0:s})".format(shapely.validation.explain_validity(tmp)))
        else:
            buff.append(tmp)

    # Convert list of Polygons to (unified) MultiPolygon ...
    buff = shapely.ops.unary_union(buff)

    # Check MultiPolygon ...
    if not buff.is_valid:
        raise Exception("\"buff\" is not a valid [Multi]Polygon ({0:s})".format(shapely.validation.explain_validity(buff))) from None

    # Check if the user wants to simplify the MultiPolygon ...
    if simp > 0.0:
        # Simplify MultiPolygon ...
        buffSimp = buff.simplify(simp)

        # Check simplified MultiPolygon ...
        if buffSimp.is_valid:
            # Return simplified answer ...
            return buffSimp

    # Return answer ...
    return buff
