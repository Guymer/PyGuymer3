#!/usr/bin/env python3

# Define function ...
def clean_LineString(
    line,
    /,
    *,
     debug = __debug__,
    prefix = ".",
       tol = 1.0e-10,
):
    """Clean a LineString

    This function cleans a LineString by removing bad points.

    Parameters
    ----------
    line : shapely.geometry.linestring.LineString
        the LineString
    debug : bool, optional
        print debug messages
    prefix : str, optional
        change the name of the output debugging CSVs
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    cleans : shapely.geometry.linestring.LineString
        the cleaned LineString

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
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .clean_CoordinateSequence import clean_CoordinateSequence

    # **************************************************************************

    # Check argument ...
    assert isinstance(line, shapely.geometry.linestring.LineString), "\"line\" is not a LineString"

    # Return cleaned LineString ...
    return clean_CoordinateSequence(
        line.coords,
         debug = debug,
        prefix = prefix,
           tol = tol,
    )
