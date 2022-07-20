def check_Point(shape):
    """Check Point

    This function checks if a Point is valid.

    Parameters
    ----------
    shape : shapely.geometry.point.Point
            the Point

    Notes
    -----
    According to the Shapely documentation for the function shapely.geometry.polygon.orient():

        "A sign of 1.0 means that the coordinates of the product’s exterior ring will be oriented counter-clockwise."
    """

    # Import special modules ...
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._debug import _debug

    # Check argument ...
    if not isinstance(shape, shapely.geometry.point.Point):
        raise TypeError("\"shape\" is not a Point") from None
    if not shape.is_valid:
        _debug(shape)
        raise Exception(f"\"shape\" is not a valid Point ({shapely.validation.explain_validity(shape)})") from None
    if shape.is_empty:
        raise Exception("\"shape\" is an empty Point") from None

    # Return answer ...
    return True
