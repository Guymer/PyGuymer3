#!/usr/bin/env python3

# Define function ...
def check_LineString(line, /, *, prefix = "."):
    """Check LineString

    This function checks if a LineString is valid.

    Parameters
    ----------
    line : shapely.geometry.linestring.LineString
        the LineString
    prefix : str, optional
        change the name of the output debugging CSVs

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

    # Check argument ...
    if not isinstance(line, shapely.geometry.linestring.LineString):
        raise TypeError("\"line\" is not a LineString") from None
    if not line.is_valid:
        _debug(line, prefix = prefix)
        raise Exception(f"\"line\" is not a valid LineString ({shapely.validation.explain_validity(line)})") from None
    if line.is_empty:
        raise Exception("\"line\" is an empty LineString") from None

    # Return answer ...
    return True
