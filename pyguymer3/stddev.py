def stddev(arr, kwArgCheck = None, dof = 0):
    """Find the standard deviation of an array.

    This function finds the standard deviation of an array, with optionally
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
            the standard deviation of the array

    Notes
    -----
    See `the NumPy documentation <https://numpy.org/doc/stable/reference/generated/numpy.std.html>`_.
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .var import var

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(arr, numpy.ndarray):
        raise TypeError("\"arr\" is not a NumPy array") from None

    # Return answer ...
    return numpy.sqrt(var(arr, dof = dof))
