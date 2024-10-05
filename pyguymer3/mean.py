#!/usr/bin/env python3

# Define function ...
def mean(
    arr,
    /,
    *,
    dof = 0,
):
    """Find the arithmetic mean of an array.

    This function finds the arithmetic mean of an array, with optionally
    specified degrees of freedom.

    Parameters
    ----------
    arr : numpy.ndarray
        the array
    dof : int, default=0
        the degrees of freedom

    Returns
    -------
    ans : float
        the arithmetic mean of the array

    Notes
    -----
    See `the NumPy documentation <https://numpy.org/doc/stable/reference/generated/numpy.mean.html>`_.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Check argument ...
    if not isinstance(arr, numpy.ndarray):
        raise TypeError("\"arr\" is not a NumPy array") from None

    # Return answer ...
    return arr.sum() / (arr.size - dof)
