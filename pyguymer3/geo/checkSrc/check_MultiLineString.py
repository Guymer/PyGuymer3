#!/usr/bin/env python3

# Define function ...
def check_MultiLineString(
    multiline,
    /,
    *,
    prefix = ".",
):
    """Check MultiLineString

    This function checks if a MultiLineString is valid.

    Parameters
    ----------
    multiline : shapely.geometry.multilinestring.MultiLineString
        the MultiLineString
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

    # **************************************************************************

    # Check argument ...
    assert isinstance(multiline, shapely.geometry.multilinestring.MultiLineString), "\"multiline\" is not a MultiLineString"
    if not multiline.is_valid:
        _debug(multiline, prefix = prefix)
        raise Exception(f"\"multiline\" is not a valid MultiLineString ({shapely.validation.explain_validity(multiline)})") from None
    if multiline.is_empty:
        raise Exception("\"multiline\" is an empty MultiLineString") from None

    # Return answer ...
    return True
