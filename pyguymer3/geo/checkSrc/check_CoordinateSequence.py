#!/usr/bin/env python3

# Define function ...
def check_CoordinateSequence(
    coords,
    /,
):
    """Check CoordinateSequence

    This function checks if a CoordinateSequence is valid.

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
        the CoordinateSequence

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
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # **************************************************************************

    # Check argument ...
    assert isinstance(coords, shapely.coords.CoordinateSequence), "\"coords\" is not a CoordinateSequence"

    # Return answer ...
    return True
