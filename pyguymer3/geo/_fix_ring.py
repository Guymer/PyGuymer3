def _fix_ring(ring, kwArgCheck = None, debug = False):
    """Fix a ring of points

    This function reads in an array of coordinates (in degrees) that exist as a
    ring on the surface of the Earth and returns a [Multi]Polygon of it,
    correcting for points which cross the poles/equator/anti-meridian.

    Parameters
    ----------
    ring : numpy.array
            the (npoints, 2) array of (lon,lat) coordinates of the ring (in degrees)
    debug : bool, optional
            print debug messages

    Returns
    -------
    buff : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
            the ring
    """

    # Import standard modules ...
    import copy

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.geometry
        import shapely.ops
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from ..interpolate import interpolate

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check arguments ...
    if not isinstance(ring, numpy.ndarray):
        raise TypeError("\"ring\" is not a numpy.ndarray") from None
    if len(ring.shape) != 2:
        raise TypeError("\"ring\" is not 2D") from None
    if ring.shape[1] != 2:
        raise TypeError("\"ring\" is not (N,2)") from None

    # **************************************************************************

    # Initialise flag, create empty lists and append first point ...
    flag = True
    part1 = []
    part2 = []
    part1.append((ring[0, 0], ring[0, 1]))

    # Loop over angles ...
    # NOTE: Start at 1 (not 0) because the first one will be checked when the
    #       last one is (as they are the same).
    for iang in range(1, ring.shape[0]):
        # Check if the anti-meridian has been crossed between this and the last
        # point ...
        if min(ring[iang - 1, 0], ring[iang, 0]) < -90.0 and max(ring[iang - 1, 0], ring[iang, 0]) > +90.0:
            # NOTE: There is no way with, at most, ~45 degree jumps and only, at
            #       most, half the Earth's circumference travelled that two
            #       neighbouring points can straddle *both* -90 longitude and
            #       +90 longitude.
            # NOTE: The negative one is East of the anti-meridian.
            # NOTE: The positive one is West of the anti-meridian.

            if debug:
                print(f"INFO: The anti-meridian has been crossed, from ({ring[iang - 1, 0]:f}°,{ring[iang - 1, 1]:f}°) to ({ring[iang, 0]:f}°,{ring[iang, 1]:f}°).")

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

    # **************************************************************************

    # Check flag ...
    # NOTE: The only way a continuous ring can cross the anti-meridian an odd
    #       number of times is if it also crosses one of the poles.
    if not flag:
        if debug:
            print("INFO: The anti-meridian has been crossed an odd number of times.")

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

        if debug:
            print(f"INFO: The North Pole has been crossed? {repr(north)}")
            print(f"INFO: The South Pole has been crossed? {repr(south)}")

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
    else:
        if debug:
            print("INFO: The anti-meridian has been crossed an even number of times.")

    # **************************************************************************

    # Remove sequential duplicate points ...
    if len(part1) >= 2:
        newPart1 = []
        newPart1.append(part1[0])
        for i in range(1, len(part1)):
            if part1[i - 1][0] == part1[i][0] and part1[i - 1][1] == part1[i][1]:
                continue
            newPart1.append(part1[i])
        part1 = copy.copy(newPart1)
    if len(part2) >= 2:
        newPart2 = []
        newPart2.append(part2[0])
        for i in range(1, len(part2)):
            if part2[i - 1][0] == part2[i][0] and part2[i - 1][1] == part2[i][1]:
                continue
            newPart2.append(part2[i])
        part2 = copy.copy(newPart2)

    # **************************************************************************

    # Create a list of [Multi]Polygons if there are enough unique points to make
    # a [Multi]Polygon ...
    buff = []
    if len(set(part1)) >= 3:
        tmp = shapely.geometry.polygon.Polygon(part1)
        if not tmp.is_valid:
            raise Exception(f"\"tmp\" is not a valid Polygon ({shapely.validation.explain_validity(tmp)})") from None
        buff.append(tmp)
    if len(set(part2)) >= 3:
        tmp = shapely.geometry.polygon.Polygon(part2)
        if not tmp.is_valid:
            raise Exception(f"\"tmp\" is not a valid Polygon ({shapely.validation.explain_validity(tmp)})") from None
        buff.append(tmp)

    # **************************************************************************

    # Convert list of [Multi]Polygons to (unified) [Multi]Polygon ...
    buff = shapely.ops.unary_union(buff)

    # Check [Multi]Polygon ...
    if not buff.is_valid:
        raise Exception(f"\"buff\" is not a valid [Multi]Polygon ({shapely.validation.explain_validity(buff)})") from None

    # Return answer ...
    return buff
