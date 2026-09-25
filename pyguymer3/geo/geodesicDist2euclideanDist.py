#!/usr/bin/env python3

# Define function ...
def geodesicDist2euclideanDist(
    geodesicDist,
    /,
):
    """Convert a Geodesic distance to a Euclidean distance

    Parameters
    ----------
    geodesicDist : float
        the Geodesic distance (in metres)

    Returns
    -------
    euclideanDist : float
        the Euclidean distance (in degrees)

    Notes
    -----
    This function assumes that the Earth is a perfect sphere.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import sub-functions ...
    from .._consts import RESOLUTION_OF_EARTH

    # Return answer ...
    return geodesicDist / RESOLUTION_OF_EARTH
