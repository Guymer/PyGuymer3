def normLon(lon):
    """Normalize a longitude

    This function reads in a longitude (in degrees) and normalizes it to be in
    the interval (-180°,+180°).

    Parameters
    ----------
    lon : float
            the longitude (in degrees)

    Returns
    -------
    lon : float
            the normalized longitude (in degrees)
    """

    # Return answer ...
    return (lon + 540.0) % 360.0 - 180.0
