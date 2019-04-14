# -*- coding: utf-8 -*-

def find_point_on_great_circle(frac, lon1_deg, lat1_deg, lon2_deg, lat2_deg):
    """
    This function reads in two coordinates (in degrees) on the surface of a
    sphere and calculates the coordinates (in degrees) of an arbitrary point on
    the great circle that connects them.
    """

    # Import modules ...
    import math

    # Load sub-functions ...
    from .calc_angle_between_two_locs import calc_angle_between_two_locs

    # Convert to radians ...
    lon1_rad = math.radians(lon1_deg)                                           # [rad]
    lat1_rad = math.radians(lat1_deg)                                           # [rad]
    lon2_rad = math.radians(lon2_deg)                                           # [rad]
    lat2_rad = math.radians(lat2_deg)                                           # [rad]

    # Calculate mid-point ...
    rad = math.radians(calc_angle_between_two_locs(lon1_deg, lat1_deg, lon2_deg, lat2_deg))  # [rad]
    a = math.sin((1.0 - frac) * rad) / math.sin(rad)
    b = math.sin(frac * rad) / math.sin(rad)
    x = a * math.cos(lat1_rad) * math.cos(lon1_rad) + b * math.cos(lat2_rad) * math.cos(lon2_rad)
    y = a * math.cos(lat1_rad) * math.sin(lon1_rad) + b * math.cos(lat2_rad) * math.sin(lon2_rad)
    z = a * math.sin(lat1_rad) + b * math.sin(lat2_rad)
    lat3_rad = math.atan2(
        z,
        math.hypot(
            x,
            y
        )
    )                                                                           # [rad]
    lon3_rad = math.atan2(
        y,
        x
    )                                                                           # [rad]

    # Return point point ...
    return math.degrees(lon3_rad), math.degrees(lat3_rad)
