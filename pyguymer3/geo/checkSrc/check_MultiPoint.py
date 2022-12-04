def check_MultiPoint(multipoint, kwArgCheck = None, prefix = "."):
    """Check MultiPoint

    This function checks if a MultiPoint is valid.

    Parameters
    ----------
    multipoint : shapely.geometry.multipoint.MultiPoint
        the MultiPoint

    Notes
    -----
    According to the Shapely documentation for the function shapely.geometry.polygon.orient():

        "A sign of 1.0 means that the coordinates of the product’s exterior ring
        will be oriented counter-clockwise."

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .._debug import _debug

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(multipoint, shapely.geometry.multipoint.MultiPoint):
        raise TypeError("\"multipoint\" is not a MultiPoint") from None
    if not multipoint.is_valid:
        _debug(multipoint, prefix = prefix)
        raise Exception(f"\"multipoint\" is not a valid MultiPoint ({shapely.validation.explain_validity(multipoint)})") from None
    if multipoint.is_empty:
        raise Exception("\"multipoint\" is an empty MultiPoint") from None

    # Return answer ...
    return True
