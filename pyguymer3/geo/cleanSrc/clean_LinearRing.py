def clean_LinearRing(ring, kwArgCheck = None, debug = False, tol = 1.0e-10):
    """Clean a LinearRing

    This function cleans a LinearRing by removing bad points.

    Parameters
    ----------
    ring : shapely.geometry.polygon.LinearRing
        the LinearRing
    debug : bool, optional
        print debug messages
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    cleans : shapely.geometry.linestring.LineString
        the cleaned LinearRing

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

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(ring, shapely.geometry.polygon.LinearRing):
        raise TypeError("\"ring\" is not a LinearRing") from None

    # Return cleaned LinearRing ...
    return clean_CoordinateSequence(
        ring.coords,
        debug = debug,
          tol = tol,
    )
