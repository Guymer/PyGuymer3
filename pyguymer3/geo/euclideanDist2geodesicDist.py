#!/usr/bin/env python3

# Define function ...
def euclideanDist2geodesicDist(
    euclideanDist,
    /,
):
    """Convert a Euclidean distance to a Geodesic distance

    Parameters
    ----------
    euclideanDist : float
        the Euclidean distance (in degrees)

    Returns
    -------
    geodesicDist : float
        the Geodesic distance (in metres)

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
    return euclideanDist * RESOLUTION_OF_EARTH
