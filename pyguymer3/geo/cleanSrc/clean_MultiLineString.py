#!/usr/bin/env python3

# Define function ...
def clean_MultiLineString(
    multiline,
    /,
    *,
     debug = __debug__,
    prefix = ".",
       tol = 1.0e-10,
):
    """Clean a MultiLineString

    This function cleans a MultiLineString by removing bad points.

    Parameters
    ----------
    line : shapely.geometry.multilinestring.MultiLineString
        the MultiLineString
    debug : bool, optional
        print debug messages
    prefix : str, optional
        change the name of the output debugging CSVs
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    cleans : shapely.geometry.multilinestring.MultiLineString
        the cleaned MultiLineString

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
    from ..check import check
    from .clean_LineString import clean_LineString

    # **************************************************************************

    # Check argument ...
    assert isinstance(multiline, shapely.geometry.multilinestring.MultiLineString), "\"multiline\" is not a MultiLineString"

    # Initialize list ...
    lines = []

    # Loop over LineStrings ...
    for line in multiline.geoms:
        # Append cleaned LineString to list ...
        lines.append(
            clean_LineString(
                line,
                 debug = debug,
                prefix = prefix,
                   tol = tol,
            )
        )

    # Convert list of LineStrings to a MultiLineString ...
    cleans = shapely.geometry.multilinestring.MultiLineString(lines)
    if debug:
        check(cleans, prefix = prefix)

    # Return answer ...
    return cleans
