#!/usr/bin/env python3

# Define function ...
def calc_dist_between_two_locs(
    lon1_deg,
    lat1_deg,
    lon2_deg,
    lat2_deg,
    /,
    *,
      eps = 1.0e-12,
    nIter = 100,
):
    """Calculate the distance between two coordinates.

    This function reads in two coordinates (in degrees) on the surface of the
    Earth and calculates the Geodesic distance (in metres) between them and the
    headings (in degrees) from each coordinate to the other one.

    Parameters
    ----------
    lon1_deg : float
        the longitude of the first coordinate (in degrees)
    lat1_deg : float
        the latitude of the first coordinate (in degrees)
    lon2_deg : float
        the longitude of the second coordinate (in degrees)
    lat2_deg : float
        the latitude of the second coordinate (in degrees)
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)

    Returns
    -------
    s_m : float
        the distance between the two coordinates (in metres)
    alpha1_deg : float
        the heading to the second coordinate from the first coordinate (in
        degrees)
    alpha2_deg : float
        the heading to the first coordinate from the second coordinate (in
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

    # Skip if the start- and end-points are the same ...
    if lon1_deg == lon2_deg and lat1_deg == lat2_deg:
        return 0.0, 0.0, 0.0

    # Convert to radians ...
    lon1 = math.radians(lon1_deg)                                               # [rad]
    lat1 = math.radians(lat1_deg)                                               # [rad]
    lon2 = math.radians(lon2_deg)                                               # [rad]
    lat2 = math.radians(lat2_deg)                                               # [rad]

    # Set constants ...
    a = 6378137.0                                                               # [m]
    f = 1.0 / 298.257223563
    b = (1.0 - f) * a                                                           # [m]
    l = lon2 - lon1                                                             # [rad]
    u1 = math.atan((1.0 - f) * math.tan(lat1))                                  # [rad]
    u2 = math.atan((1.0 - f) * math.tan(lat2))                                  # [rad]

    # Set initial value of lambda and initialize counter ...
    lam = l
    iIter = 0                                                                   # [#]

    # Start infinite loop ...
    while True:
        # Stop looping if the function has been called too many times ...
        if iIter >= nIter:
            raise Exception(f"failed to converge; loc1 = ({lon1_deg:+.9f}°,{lat1_deg:+.9f}°); loc2 = ({lon2_deg:+.9f}°,{lat2_deg:+.9f}°); eps = {eps:.15e}; nIter = {nIter:,d}") from None

        # Calculate new lambda and increment counter ...
        sin_sigma = math.hypot(
            math.cos(u2) * math.sin(lam),
            math.cos(u1) * math.sin(u2) - math.sin(u1) * math.cos(u2) * math.cos(lam)
        )
        if sin_sigma == 0.0:
            # NOTE: co-incident points
            return 0.0, 0.0, 0.0
        cos_sigma = math.sin(u1) * math.sin(u2) + math.cos(u1) * math.cos(u2) * math.cos(lam)
        sigma = math.atan2(
            sin_sigma,
            cos_sigma
        )
        sin_alpha = math.cos(u1) * math.cos(u2) * math.sin(lam) / sin_sigma
        cosSq_alpha = 1.0 - sin_alpha ** 2
        cos_two_sigma_m = cos_sigma - 2.0 * math.sin(u1) * math.sin(u2) / cosSq_alpha
        if math.isnan(cos_two_sigma_m):
            # NOTE: equatorial line
            cos_two_sigma_m = 0.0
        c = f * cosSq_alpha * (4.0 + f * (4.0 - 3.0 * cosSq_alpha)) / 16.0
        lamNew = l + (1.0 - c) * f * sin_alpha * (sigma + c * sin_sigma * (cos_two_sigma_m + c * cos_sigma * (2.0 * cos_two_sigma_m ** 2 - 1.0)))
        iIter += 1                                                              # [#]

        # Only check the solution after at least 3 function calls ...
        if iIter >= 3:
            if lamNew == lam:
                # Replace old lambda with new lambda ...
                lam = lamNew

                break
            if abs(lamNew - lam) / abs(lamNew) <= eps:
                # Replace old lambda with new lambda ...
                lam = lamNew

                break

        # Replace old lambda with new lambda ...
        lam = lamNew

    # Calculate ellipsoidal distance and forward azimuths ...
    uSq = cosSq_alpha * (a ** 2 - b ** 2) / b ** 2
    bigA = 1.0 + uSq * (4096.0 + uSq * (-768.0 + uSq * (320.0 - 175.0 * uSq))) / 16384.0
    bigB = uSq * (256.0 + uSq * (-128.0 + uSq * (74.0 - 47.0 * uSq))) / 1024.0
    delta_sigma = bigB * sin_sigma * (cos_two_sigma_m + 0.25 * bigB * (cos_sigma * (2.0 * cos_two_sigma_m ** 2 - 1.0) - bigB * cos_two_sigma_m * (4.0 * sin_sigma ** 2 - 3.0) * (4.0 * cos_two_sigma_m ** 2 - 3.0) / 6.0))
    s = b * bigA * (sigma - delta_sigma)                                        # [m]
    alpha1 = math.atan2(
        math.cos(u2) * math.sin(lam),
        math.cos(u1) * math.sin(u2) - math.sin(u1) * math.cos(u2) * math.cos(lam)
    )                                                                           # [rad]
    alpha2 = math.atan2(
        math.cos(u1) * math.sin(lam),
        math.sin(u1) * math.cos(u2) - math.cos(u1) * math.sin(u2) * math.cos(lam)
    )                                                                           # [rad]
    alpha1 = (alpha1 + 2.0 * math.pi) % (2.0 * math.pi)                         # NOTE: Normalize to +0° <--> +360°
    alpha2 = (alpha2 + 2.0 * math.pi) % (2.0 * math.pi)                         # NOTE: Normalize to +0° <--> +360°

    # Return distance and forward azimuths ...
    return s, math.degrees(alpha1), math.degrees(alpha2)
