def check_LinearRing(ring):
    """Check LinearRing

    This function checks if a LinearRing is valid.

    Parameters
    ----------
    ring : shapely.geometry.polygon.LinearRing
            the LinearRing

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
    if not isinstance(ring, shapely.geometry.polygon.LinearRing):
        raise TypeError("\"ring\" is not a LinearRing") from None
    if not ring.is_valid:
        _debug(ring)
        raise Exception(f"\"ring\" is not a valid LinearRing ({shapely.validation.explain_validity(ring)})") from None
    if ring.is_empty:
        raise Exception("\"ring\" is an empty LinearRing") from None

    # Return answer ...
    return True
