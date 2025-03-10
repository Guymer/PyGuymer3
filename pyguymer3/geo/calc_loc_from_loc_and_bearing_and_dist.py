#!/usr/bin/env python3

# Define function ...
def calc_loc_from_loc_and_bearing_and_dist(
    lon1_deg,
    lat1_deg,
    alpha1_deg,
    s_m,
    /,
    *,
      eps = 1.0e-12,
    nIter = 100,
):
    """Calculate the location and reverse bearing from another location and a
    forward bearing and a distance.

    This function reads in coordinates (in degrees) on the surface of the Earth,
    a bearing (in degrees) and a distance (in metres). It then calculates the
    coordinates (in degrees) that are at the end of the vector and the bearing
    (in degrees) back to the first coordinate.

    Parameters
    ----------
    lon1_deg : float
        the longitude of the first coordinate (in degrees)
    lat1_deg : float
        the latitude of the first coordinate (in degrees)
    alpha1_deg : float
        the bearing to the second coordinate from the first coordinate (in
        degrees)
    s_m : float
        the distance between the two coordinates (in metres)
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)

    Returns
    -------
    lon2_deg : float
        the longitude of the second coordinate (in degrees)
    lat2_deg : float
        the latitude of the second coordinate (in degrees)
    alpha2_deg : float
        the bearing to the first coordinate from the second coordinate (in
        degrees)

    Notes
    -----
    This function uses `Vincenty's formulae
    <https://en.wikipedia.org/wiki/Vincenty%27s_formulae>`_ ; there is a
    `JavaScript implementation
    <https://www.movable-type.co.uk/scripts/latlong-vincenty.html>`_ online too.

    ``lambda`` is a reserved word in Python so I use ``lam`` as my variable name
    instead.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import math

    # **************************************************************************

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
    iIter = 0                                                                   # [#]

    # Start infinite loop ...
    while True:
        # Stop looping if the function has been called too many times ...
        if iIter >= nIter:
            raise Exception(f"failed to converge; loc = ({lon1_deg:+.9f}°,{lat1_deg:+.9f}°); alpha1 = {alpha1_deg:.15e}°, s = {s_m:.15e}m; eps = {eps:.15e}; nIter = {nIter:,d}") from None

        # Find new value of sigma and increment counter ...
        two_sigma_m = 2.0 * sigma1 + sigma
        delta_sigma = bigB * math.sin(sigma) * (math.cos(two_sigma_m) + 0.25 * bigB * (math.cos(sigma) * (2.0 * math.cos(two_sigma_m) ** 2 - 1.0) - bigB * math.cos(two_sigma_m) * (4.0 * math.sin(sigma) ** 2 - 3.0) * (4.0 * math.cos(two_sigma_m) ** 2 - 3.0) / 6.0))
        sigmaNew = s_m / (b * bigA) + delta_sigma
        iIter += 1                                                              # [#]

        # Only check the solution after at least 3 function calls ...
        if iIter >= 3:
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
    )                                                                           # [rad]
    lam = math.atan2(
        math.sin(sigma) * math.sin(alpha1),
        math.cos(u1) * math.cos(sigma) - math.sin(u1) * math.sin(sigma) * math.cos(alpha1)
    )
    l = lam - (1.0 - c) * f * sin_alpha * (sigma + c * math.sin(sigma) * (math.cos(two_sigma_m) + c * math.cos(sigma) * (2.0 * math.cos(two_sigma_m) ** 2 - 1.0)))
    lon2 = l + lon1                                                             # [rad]
    lon2 = ((lon2 + 3.0 * math.pi) % (2.0 * math.pi)) - math.pi                 # NOTE: Normalize to -180° <--> +180°
    alpha2 = math.atan2(
        sin_alpha,
        math.cos(u1) * math.cos(sigma) * math.cos(alpha1) - math.sin(u1) * math.sin(sigma)
    )                                                                           # [rad]
    alpha2 = (alpha2 + 2.0 * math.pi) % (2.0 * math.pi)                         # NOTE: Normalize to +0° <--> +360°

    # Return end point and forward azimuth ...
    return math.degrees(lon2), math.degrees(lat2), math.degrees(alpha2)
