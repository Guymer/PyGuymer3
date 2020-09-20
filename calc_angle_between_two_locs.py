def calc_angle_between_two_locs(lon1_deg, lat1_deg, lon2_deg, lat2_deg):
    """
    This function reads in two coordinates (in degrees) on the surface of a
    sphere and calculates the angle (in degrees) between them.
    """

    # Import standard modules ...
    import math

    # Convert to radians ...
    lon1_rad = math.radians(lon1_deg)                                           # [rad]
    lat1_rad = math.radians(lat1_deg)                                           # [rad]
    lon2_rad = math.radians(lon2_deg)                                           # [rad]
    lat2_rad = math.radians(lat2_deg)                                           # [rad]

    # Calculate angle in radians ...
    distance_rad = 2.0 * math.asin(
        math.hypot(
            math.sin((lat1_rad - lat2_rad) / 2.0),
            math.cos(lat1_rad) * math.cos(lat2_rad) * math.sin((lon1_rad - lon2_rad) / 2.0)
        )
    )                                                                           # [rad]

    # Return angle ...
    return math.degrees(distance_rad)
