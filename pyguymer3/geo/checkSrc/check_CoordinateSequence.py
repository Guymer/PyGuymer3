def check_CoordinateSequence(coords):
    """Check CoordinateSequence

    This function checks if a CoordinateSequence is valid.

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
            the CoordinateSequence

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
    from .._debug import _debug

    # Check argument ...
    if not isinstance(coords, shapely.coords.CoordinateSequence):
        raise TypeError("\"coords\" is not a CoordinateSequence") from None
    if not coords.is_valid:
        _debug(coords)
        raise Exception(f"\"coords\" is not a valid CoordinateSequence ({shapely.validation.explain_validity(coords)})") from None
    if coords.is_empty:
        raise Exception("\"coords\" is an empty CoordinateSequence") from None

    # Return answer ...
    return True
