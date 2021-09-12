def normLat(lat):
    """Normalize a latitude

    This function reads in a latitude (in degrees) and normalizes it to be in
    the interval (-90°,+90°).

    Parameters
    ----------
    lat : float
            the latitude (in degrees)

    Returns
    -------
    lat : float
            the normalized latitude (in degrees)
    """

    # Return answer ...
    return (lat + 270.0) % 180.0 - 90.0
