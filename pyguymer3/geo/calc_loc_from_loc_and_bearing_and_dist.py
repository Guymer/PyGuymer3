def calc_loc_from_loc_and_bearing_and_dist(lon1_deg, lat1_deg, alpha1_deg, s_m, kwArgCheck = None, eps = 1.0e-12, nmax = 100):
    """
    This function reads in coordinates (in degrees) on the surface of Earth
    and a heading (in degrees) and a distance (in metres) it then calculates the
    coordinates (in degrees) that are at the end of the vector.
    """

    # NOTE: https://en.wikipedia.org/wiki/Vincenty%27s_formulae
    # NOTE: https://www.movable-type.co.uk/scripts/latlong-vincenty.html
    # NOTE: "lambda" is a reserved word in Python so I use "lam" as my variable
    #       name.

    # Import standard modules ...
    import math

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Convert to radians ...
    lon1 = math.radians(lon1_deg)                                               # [rad]
    lat1 = math.radians(lat1_deg)                                               # [rad]
    alpha1 = math.radians(alpha1_deg)                                           # [rad]

    # Set constants ...
    a = 6378137.0                                                               # [m]
    f = 1.0 / 298.257223563
    b = (1.0 - f) * a                                                           # [m]
    u1 = math.atan((1.0 - f) * math.tan(lat1))                                  # [rad]
    sigma1 = math.atan2(
        math.tan(u1),
        math.cos(alpha1)
    )
    sin_alpha = math.cos(u1) * math.sin(alpha1)
    cosSq_alpha = 1.0 - sin_alpha ** 2
    c = f * cosSq_alpha * (4.0 + f * (4.0 - 3.0 * cosSq_alpha)) / 16.0
    uSq = cosSq_alpha * (a ** 2 - b ** 2) / b ** 2
    bigA = 1.0 + uSq * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq))) / 16384.0
    bigB = uSq * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq))) / 1024.0

    # Set initial value of sigma and initialize counter ...
    sigma = s_m / (b * bigA)
    i = 0

    # Start infinite loop ...
    while True:
        # Stop looping if the function has been called too many times ...
        if i >= nmax:
            raise Exception("failed to converge") from None

        # Find new value of sigma and increment counter ...
        two_sigma_m = 2.0 * sigma1 + sigma
        delta_sigma = bigB * math.sin(sigma) * (math.cos(two_sigma_m) + 0.25 * bigB * (math.cos(sigma) * (2.0 * math.cos(two_sigma_m) ** 2 - 1.0) - bigB * math.cos(two_sigma_m) * (4.0 * math.sin(sigma) ** 2 - 3.0) * (4.0 * math.cos(two_sigma_m) ** 2 - 3.0) / 6.0))
        sigmaNew = s_m / (b * bigA) + delta_sigma
        i += 1

        # Only check the solution after at least 3 function calls ...
        if i >= 3:
            if sigmaNew == sigma:
                # Replace old sigma with new sigma ...
                sigma = sigmaNew

                break
            if abs(sigmaNew - sigma) / abs(sigmaNew) <= eps:
                # Replace old sigma with new sigma ...
                sigma = sigmaNew

                break

        # Replace old sigma with new sigma ...
        sigma = sigmaNew

    # Calculate end point and forward azimuth ...
    lat2 = math.atan2(
        math.sin(u1) * math.cos(sigma) + math.cos(u1) * math.sin(sigma) * math.cos(alpha1),
        (1.0 - f) * math.hypot(
            sin_alpha,
            math.sin(u1) * math.sin(sigma) - math.cos(u1) * math.cos(sigma) * math.cos(alpha1)
        )
    )
    lam = math.atan2(
        math.sin(sigma) * math.sin(alpha1),
        math.cos(u1) * math.cos(sigma) - math.sin(u1) * math.sin(sigma) * math.cos(alpha1)
    )
    l = lam - (1.0 - c) * f * sin_alpha * (sigma + c * math.sin(sigma) * (math.cos(two_sigma_m) + c * math.cos(sigma) * (2.0 * math.cos(two_sigma_m) ** 2 - 1.0)))
    # lon2 = l + lon1
    lon2 = ((l + lon1 + 3.0 * math.pi) % (2.0 * math.pi)) - math.pi             # NOTE: Normalize to -180 <--> +180
    alpha2 = math.atan2(
        sin_alpha,
        math.cos(u1) * math.cos(sigma) * math.cos(alpha1) - math.sin(u1) * math.sin(sigma)
    )
    alpha2 = (alpha2 + 2.0 * math.pi) % (2.0 * math.pi)                         # NOTE: Normalize to +0 <--> +360

    # Return end point and forward azimuth ...
    return math.degrees(lon2), math.degrees(lat2), math.degrees(alpha2)
