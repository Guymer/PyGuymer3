#!/usr/bin/env python3

# Define function ...
def length(
    shape,
    /,
    *,
          eps = 1.0e-12,
        nIter = 100,
    onlyValid = False,
):
    """Find the length of a shape.

    Parameters
    ----------
    shape : shapely.coords.CoordinateSequence, shapely.geometry.point.Point, shapely.geometry.multipoint.MultiPoint, shapely.geometry.polygon.LinearRing, shapely.geometry.linestring.LineString, shapely.geometry.multilinestring.MultiLineString, shapely.geometry.polygon.Polygon, shapely.geometry.multipolygon.MultiPolygon
        the shape
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)
    onlyValid : bool, optional
        only add valid LinearRings/LineStrings (checks for validity can take a
        while, if being being called often)

    Returns
    -------
    length : float
        the length (in metres)

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.ops
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .calc_dist_between_two_locs import calc_dist_between_two_locs
    from .extract_lines import extract_lines

    # **************************************************************************

    # Initialize total ...
    tot = 0.0                                                                   # [m]

    # Loop over the Polygons in the shape ...
    for shapePart in extract_lines(
        shape,
        onlyValid = onlyValid,
    ):
        # Convert the CoordinateSequence to a NumPy array ...
        coords = numpy.array(shapePart.coords)                                  # [°]

        # Find the Geodesic distance between each point ...
        lengths = numpy.zeros(
            coords.shape[0] - 1,
            dtype = numpy.float64,
        )                                                                       # [m]
        for iCoord in range(lengths.size):
            lengths[iCoord], _, _ = calc_dist_between_two_locs(
                coords[iCoord    , 0],
                coords[iCoord    , 1],
                coords[iCoord + 1, 0],
                coords[iCoord + 1, 1],
                  eps = eps,
                nIter = nIter,
            )                                                                   # [m]
        del coords

        # Increment total ...
        tot += float(lengths.sum())                                             # [m]
        del lengths

    # Return total ...
    return tot
