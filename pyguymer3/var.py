def var(arr, kwArgCheck = None, dof = 0):
    """Find the variance of an array.

    This function finds the variance of an array, with optionally specified
    degrees of freedom.

    Parameters
    ----------
    arr : numpy.ndarray
            the array
    dof : int, default=0
            the degrees of freedom

    Returns
    -------
    ans : float
            the variance of the array

    Notes
    -----
    See `the NumPy documentation <https://numpy.org/doc/stable/reference/generated/numpy.var.html>`_.

    In standard statistical practice:

    * ``dof=0`` provides a maximum likelihood estimate of the variance for
      normally distributed variables; and
    * ``dof=1`` provides an unbiased estimator of the variance of a
      hypothetical infinite population.
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .mean import mean

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(arr, numpy.ndarray):
        raise TypeError("\"arr\" is not a NumPy array") from None

    # Calculate the squared deviations from the mean ...
    tmp = (arr - mean(arr, dof = 0)) ** 2

    # Return the answer ...
    return mean(tmp, dof = dof)
