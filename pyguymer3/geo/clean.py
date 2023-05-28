#!/usr/bin/env python3

# Define function ...
def clean(shape, /, *, debug = False, prefix = ".", tol = 1.0e-10):
    """Clean a shape

    This function cleans a shape by removing bad points.

    Parameters
    ----------
    shape : shapely.coords.CoordinateSequence, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the shape
    debug : bool, optional
        print debug messages
    prefix : str, optional
        change the name of the output debugging CSVs
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    fills : shapely.coords.CoordinateSequence, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the cleaned shape

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
    from .cleanSrc import clean_CoordinateSequence
    from .cleanSrc import clean_LinearRing
    from .cleanSrc import clean_LineString
    from .cleanSrc import clean_MultiLineString
    from .cleanSrc import clean_MultiPolygon
    from .cleanSrc import clean_Polygon

    # Check if it is a CoordinateSequence and return it cleaned ...
    if isinstance(shape, shapely.coords.CoordinateSequence):
        return clean_CoordinateSequence(
            shape,
             debug = debug,
            prefix = prefix,
               tol = tol,
        )

    # Check if it is a LinearRing and return it cleaned ...
    if isinstance(shape, shapely.geometry.polygon.LinearRing):
        return clean_LinearRing(
            shape,
             debug = debug,
            prefix = prefix,
               tol = tol,
        )

    # Check if it is a LineString and return it cleaned ...
    if isinstance(shape, shapely.geometry.linestring.LineString):
        return clean_LineString(
            shape,
             debug = debug,
            prefix = prefix,
               tol = tol,
        )

    # Check if it is a MultiLineString and return it cleaned ...
    if isinstance(shape, shapely.geometry.multilinestring.MultiLineString):
        return clean_MultiLineString(
            shape,
             debug = debug,
            prefix = prefix,
               tol = tol,
        )

    # Check if it is a Polygon and return it cleaned ...
    if isinstance(shape, shapely.geometry.polygon.Polygon):
        return clean_Polygon(
            shape,
             debug = debug,
            prefix = prefix,
               tol = tol,
        )

    # Check if it is a MultiPolygon and return it cleaned ...
    if isinstance(shape, shapely.geometry.multipolygon.MultiPolygon):
        return clean_MultiPolygon(
            shape,
             debug = debug,
            prefix = prefix,
               tol = tol,
        )

    # Crash ...
    raise TypeError(f"\"shape\" is an unexpected type ({repr(type(shape))})") from None
