#!/usr/bin/env python3

# Define function ...
def check_MultiLineString(multiline, kwArgCheck = None, prefix = "."):
    """Check MultiLineString

    This function checks if a MultiLineString is valid.

    Parameters
    ----------
    multiline : shapely.geometry.multilinestring.MultiLineString
        the MultiLineString

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
    if not isinstance(multiline, shapely.geometry.multilinestring.MultiLineString):
        raise TypeError("\"multiline\" is not a MultiLineString") from None
    if not multiline.is_valid:
        _debug(multiline, prefix = prefix)
        raise Exception(f"\"multiline\" is not a valid MultiLineString ({shapely.validation.explain_validity(multiline)})") from None
    if multiline.is_empty:
        raise Exception("\"multiline\" is an empty MultiLineString") from None

    # Return answer ...
    return True
