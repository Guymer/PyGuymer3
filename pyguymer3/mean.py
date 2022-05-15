def mean(arr, kwArgCheck = None, dof = 0):
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
    mean : float
            the arithmetic mean of the array

    Notes
    -----
    See `the NumPy documentation <https://numpy.org/doc/stable/reference/generated/numpy.mean.html>`_.
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(arr, numpy.ndarray):
        raise TypeError("\"arr\" is not a NumPy array") from None

    # Return answer ...
    return arr.sum() / (arr.size - dof)
