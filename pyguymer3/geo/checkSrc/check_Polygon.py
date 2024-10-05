#!/usr/bin/env python3

# Define function ...
def check_Polygon(
    poly,
    /,
    *,
    prefix = ".",
):
    """Check Polygon

    This function checks if a Polygon is valid.

    Parameters
    ----------
    poly : shapely.geometry.polygon.Polygon
        the Polygon
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
    assert isinstance(poly, shapely.geometry.polygon.Polygon), "\"poly\" is not a Polygon"
    if not poly.is_valid:
        _debug(poly, prefix = prefix)
        raise Exception(f"\"poly\" is not a valid Polygon ({shapely.validation.explain_validity(poly)})") from None
    if poly.is_empty:
        raise Exception("\"poly\" is an empty Polygon") from None

    # Return answer ...
    return True
