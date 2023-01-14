#!/usr/bin/env python3

# Define function ...
def stderr(arr, kwArgCheck = None, dof = 1):
    """Find the standard error of an array.

    This function finds the standard error of an array, with optionally
    specified degrees of freedom.

    Parameters
    ----------
    arr : numpy.ndarray
        the array
    dof : int, default=1
        the degrees of freedom

    Returns
    -------
    ans : float
        the standard error of the array

    Notes
    -----
    See `the SciPy documentation <https://docs.scipy.org/doc/scipy/reference/generated/scipy.stats.sem.html>`_.

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

    # Import sub-functions ...
    from .stddev import stddev

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(arr, numpy.ndarray):
        raise TypeError("\"arr\" is not a NumPy array") from None

    # Return answer ...
    return stddev(arr, dof = dof) / numpy.sqrt(arr.size)
