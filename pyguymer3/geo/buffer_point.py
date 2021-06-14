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
    import math

    # Load sub-functions ...
    from .buffer_point_crudely import buffer_point_crudely
    from .fix_ring import fix_ring

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

    # Convert the NumPy array to a [Multi]Polygon, whilst also fixing parts of
    # it that might have crossed the poles/equator/anti-meridian ...
    buff = fix_ring(ring)

    # Check if the user wants to simplify the [Multi]Polygon ...
    if simp > 0.0:
        # Simplify [Multi]Polygon ...
        buffSimp = buff.simplify(simp)

        # Check simplified [Multi]Polygon ...
        if buffSimp.is_valid:
            # Return simplified answer ...
            return buffSimp

    # Return answer ...
    return buff
