#!/usr/bin/env python3

# Define function ...
def check_LinearRing(ring, /, *, prefix = "."):
    """Check LinearRing

    This function checks if a LinearRing is valid.

    Parameters
    ----------
    ring : shapely.geometry.polygon.LinearRing
        the LinearRing
    prefix : str, optional
        change the name of the output debugging CSVs

    Notes
    -----
    According to the `Shapely documentation for the function
    shapely.geometry.polygon.orient()
    <https://shapely.readthedocs.io/en/stable/manual.html#shapely.geometry.polygon.orient>`_ :

        "A sign of 1.0 means that the coordinates of the product's exterior ring
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

    # Check argument ...
    if not isinstance(ring, shapely.geometry.polygon.LinearRing):
        raise TypeError("\"ring\" is not a LinearRing") from None
    if not ring.is_valid:
        _debug(ring, prefix = prefix)
        raise Exception(f"\"ring\" is not a valid LinearRing ({shapely.validation.explain_validity(ring)})") from None
    if ring.is_empty:
        raise Exception("\"ring\" is an empty LinearRing") from None

    # Return answer ...
    return True
