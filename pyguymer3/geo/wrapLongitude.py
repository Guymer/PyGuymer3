#!/usr/bin/env python3

# Define function ...
def wrapLongitude(
    lon,
    /,
):
    """Wrap longitude to be between -180° (inclusive) and +180° (exclusive)

    Parameters
    ----------
    lon : float
        the longitude

    Returns
    -------
    ans : float
        the wrapped longitude

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Return answer ...
    return ((lon + 180.0) % 360.0) - 180.0
