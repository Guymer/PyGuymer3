#!/usr/bin/env python3

# Define function ...
def buffer_LineString(line, dist, /, *, debug = False, eps = 1.0e-12, fill = 1.0, fillSpace = "EuclideanSpace", nang = 9, nmax = 100, prefix = ".", ramLimit = 1073741824, simp = 0.1, tol = 1.0e-10):
    """Buffer a LineString

    This function reads in a LineString that exists on the surface of the Earth
    and returns a [Multi]Polygon of the same LineString buffered by a constant
    distance (in metres).

    Parameters
    ----------
    line : shapely.geometry.linestring.LineString
        the LineString
    dist : float
        the Geodetic distance to buffer each point within the LineString by (in
        metres)
    debug : bool, optional
        print debug messages
    fill : float, optional
        the Euclidean or Geodetic distance to fill in between each point within
        the shapes by (in degrees or metres)
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    nang : int, optional
        the number of angles around each point within the LineString that are
        calculated when buffering
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array, in bytes
    simp : float, optional
        how much intermediary [Multi]Polygons are simplified by; negative values
        disable simplification (in degrees)
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    buffs : shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the buffered LineString

    Notes
    -----
    According to the `Shapely documentation for the method object.buffer()
    <https://shapely.readthedocs.io/en/stable/manual.html#object.buffer>`_ :

        "Passed a distance of 0, buffer() can sometimes be used to "clean"
        self-touching or self-crossing polygons such as the classic "bowtie".
        Users have reported that very small distance values sometimes produce
        cleaner results than 0. Your mileage may vary when cleaning surfaces."

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
    from .buffer_CoordinateSequence import buffer_CoordinateSequence

    # Check argument ...
    if not isinstance(line, shapely.geometry.linestring.LineString):
        raise TypeError("\"line\" is not a LineString") from None
    if debug:
        check(line, prefix = prefix)

    # Return buffered LineString ...
    return buffer_CoordinateSequence(
        line.coords,
        dist,
            debug = debug,
              eps = eps,
             fill = fill,
        fillSpace = fillSpace,
             nang = nang,
             nmax = nmax,
           prefix = prefix,
         ramLimit = ramLimit,
             simp = simp,
              tol = tol,
    )
