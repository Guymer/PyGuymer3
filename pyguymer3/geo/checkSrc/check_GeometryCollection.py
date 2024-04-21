#!/usr/bin/env python3

# Define function ...
def check_GeometryCollection(geometrycollection, /, *, prefix = "."):
    """Check GeometryCollection

    This function checks if a GeometryCollection is valid.

    Parameters
    ----------
    geometrycollection : shapely.geometry.collection.GeometryCollection
        the GeometryCollection
    prefix : str, optional
        change the name of the output debugging CSVs

    Notes
    -----
    According to the `Shapely documentation for the function
    shapely.geometry.polygon.orient()
    <https://shapely.readthedocs.io/en/stable/manual.html#shapely.geometry.polygon.orient>`_ :

        "A sign of 1.0 means that the coordinates of the product's exterior ring
        will be oriented counter-clockwise."

    Notes
    -----
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
    if not isinstance(geometrycollection, shapely.geometry.collection.GeometryCollection):
        raise TypeError("\"geometrycollection\" is not a GeometryCollection") from None
    if not geometrycollection.is_valid:
        _debug(geometrycollection, prefix = prefix)
        raise Exception(f"\"geometrycollection\" is not a valid GeometryCollection ({shapely.validation.explain_validity(geometrycollection)})") from None
    if geometrycollection.is_empty:
        raise Exception("\"geometrycollection\" is an empty GeometryCollection") from None

    # Return answer ...
    return True
