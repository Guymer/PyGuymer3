#!/usr/bin/env python3

# Define function ...
def clean_LineString(line, /, *, debug = False, prefix = ".", tol = 1.0e-10):
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

    # Check argument ...
    if not isinstance(line, shapely.geometry.linestring.LineString):
        raise TypeError("\"line\" is not a LineString") from None

    # Return cleaned LineString ...
    return clean_CoordinateSequence(
        line.coords,
         debug = debug,
        prefix = prefix,
           tol = tol,
    )
