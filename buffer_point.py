def buffer_point(lon1, lat1, dist, nang = 19, debug = False):
    """
    This function reads in coordinates (in degrees) that exist on the surface of
    the Earth and returns a [Multi]Polygon of it buffered by a constant distance
    (in metres).
    """

    # Import modules ...
    import copy
    import math
    import shapely
    import shapely.geometry
    import shapely.validation

    # Load sub-functions ...
    from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
    from .interpolate import interpolate

    # Correct inputs ...
    lon1 = (lon1 + 180.0) % 360.0 - 180.0                                       # NOTE: Normalize longitude to -180 <--> +180
    lat1 = max(-90.0, min(90.0, lat1))                                          # NOTE: Limit latitude to -90 <--> +90
    dist = max(1.0, min(math.pi * 6371009.0, dist))                             # NOTE: Limit distance to 1m <--> (half-circumference)
    nang = max(9, nang)                                                         # NOTE: Must do at least 9 points around the compass

    # Create empty list ...
    ring = []

    # Loop over angles ...
    # NOTE: The first and last angles will *always* be exactly North.
    # NOTE: The most two subsequent points can be apart is ~45 degrees (with
    #       nang = 9).
    for i in range(nang):
        # Calculate initial angle, then the ring coordinates and add them to the
        # list ...
        ang1 = 360.0 * float(i) / float(nang - 1)
        lon2, lat2, ang2 = calc_loc_from_loc_and_bearing_and_dist(lon1, lat1, ang1, dist)
        ring.append((lon2, lat2))

    # Initialise flag, create empty lists and append first point ...
    flag = True
    part1 = []
    part2 = []
    part1.append((ring[0][0], ring[0][1]))

    # Loop over angles ...
    # NOTE: Start at 1 (not 0) because the first one will be checked when the
    #       last one is (as they are the same).
    for i in range(1, nang):
        # Check if the anti-meridian has been crossed between this and the last
        # point ...
        if min(ring[i - 1][0], ring[i][0]) < -90.0 and max(ring[i - 1][0], ring[i][0]) > +90.0:
            # NOTE: There is no way with, at most, ~45 degree jumps and only, at
            #       most, half the Earth's circumference travelled that two
            #       neighbouring points can straddle *both* -90 longitude and
            #       +90 longitude.
            # NOTE: The negative one is East of the anti-meridian.
            # NOTE: The positive one is West of the anti-meridian.

            # Create short-hand variables for interpolation and then interpolate ...
            if ring[i - 1][0] > ring[i][0]:
                x1 = ring[i - 1][0]
                x2 = +180.0
                x3 = ring[i][0] + 360.0
                y1 = ring[i - 1][1]
                y3 = ring[i][1]
            else:
                x1 = ring[i - 1][0]
                x2 = -180.0
                x3 = ring[i][0] - 360.0
                y1 = ring[i][1]
                y3 = ring[i - 1][1]
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
            part1.append((ring[i][0], ring[i][1]))
        else:
            part2.append((ring[i][0], ring[i][1]))

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
            raise Exception("the ring crossed the anti-meridian an odd number of times and exists in both hemi-spheres")

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
    ans = []
    if len(set(part1)) >= 3:
        tmp = shapely.geometry.polygon.Polygon(part1)
        if not tmp.is_valid:
            if debug:
                print("DEBUG: \"tmp\" is not a valid Polygon ({0:s})".format(shapely.validation.explain_validity(tmp)))
        else:
            ans.append(tmp)
    if len(set(part2)) >= 3:
        tmp = shapely.geometry.polygon.Polygon(part2)
        if not tmp.is_valid:
            if debug:
                print("DEBUG: \"tmp\" is not a valid Polygon ({0:s})".format(shapely.validation.explain_validity(tmp)))
        else:
            ans.append(tmp)

    # Convert list of Polygons to MultiPolygon ...
    ans = shapely.geometry.multipolygon.MultiPolygon(ans)

    # Check MultiPolygon ...
    if not ans.is_valid:
        raise Exception("\"ans\" is not a valid [Multi]Polygon ({0:s})".format(shapely.validation.explain_validity(ans)))

    # Return answer ...
    return ans
