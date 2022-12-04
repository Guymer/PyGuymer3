def check_Point(point, kwArgCheck = None, prefix = "."):
    """Check Point

    This function checks if a Point is valid.

    Parameters
    ----------
    point : shapely.geometry.point.Point
        the Point

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
    if not isinstance(point, shapely.geometry.point.Point):
        raise TypeError("\"point\" is not a Point") from None
    if not point.is_valid:
        _debug(point, prefix = prefix)
        raise Exception(f"\"point\" is not a valid Point ({shapely.validation.explain_validity(point)})") from None
    if point.is_empty:
        raise Exception("\"point\" is an empty Point") from None

    # Return answer ...
    return True
