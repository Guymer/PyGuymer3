#!/usr/bin/env python3

# Define function ...
def clipLatitude(
    lat,
    /,
):
    """Clip latitude to be between -90° (inclusive) and +90° (inclusive)

    Parameters
    ----------
    lat : float
        the latitude

    Returns
    -------
    ans : float
        the clipped latitude

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Return answer ...
    return max(-90.0, min(+90.0, lat))
