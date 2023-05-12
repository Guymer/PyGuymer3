#!/usr/bin/env python3

# Define function ...
def clean_CoordinateSequence(coords, /, *, debug = False, prefix = ".", tol = 1.0e-10):
    """Clean a CoordinateSequence

    This function cleans a CoordinateSequence by removing bad points.

    Parameters
    ----------
    coords : shapely.coords.CoordinateSequence
        the CoordinateSequence
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
        the cleaned CoordinateSequence

    Notes
    -----
    According to the Shapely documentation for the function shapely.geometry.polygon.orient():

        "A sign of 1.0 means that the coordinates of the product’s exterior ring
        will be oriented counter-clockwise."

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
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ..check import check

    # Check argument ...
    if not isinstance(coords, shapely.coords.CoordinateSequence):
        raise TypeError("\"coords\" is not a CoordinateSequence") from None

    # Convert the CoordinateSequence to a NumPy array ...
    points1 = numpy.array(coords)                                               # [°]

    # Create short-hand ...
    npoint = points1.shape[0]

    # Don't clean points ...
    if npoint == 1:
        return shapely.geometry.point.Point(coords)

    # Initialize list ...
    points2 = []                                                                # [°]
    points2.append(points1[0, :])                                               # [°]

    # Initialize flag ...
    skip = False

    # Loop over points ...
    for ipoint in range(1, npoint - 1):
        # Skip this point if it needs skipping ...
        if skip:
            # Re-set flag ...
            skip = False

            continue

        # Find the Euclidean distance between the points either side of this
        # point ...
        dx = points1[ipoint - 1, 0] - points1[ipoint + 1, 0]                    # [°]
        dy = points1[ipoint - 1, 1] - points1[ipoint + 1, 1]                    # [°]
        dr = numpy.hypot(dx, dy)                                                # [°]

        # Skip this point (and the next one) if it is a bad spike ...
        if dr < tol:
            if debug:
                print(f"INFO: Removing spike between ({points1[ipoint - 1, 0]:+.6f}°,{points1[ipoint - 1, 1]:+.6f}°) and ({points1[ipoint + 1, 0]:+.6f}°,{points1[ipoint + 1, 1]:+.6f}°), which are {dr:+.6f}° apart.")

            # Set flag ...
            skip = True

            continue

        # Append point to list ...
        points2.append(points1[ipoint, :])                                      # [°]

    # Append last point to list (if it shouldn't be skipped) ...
    if not skip:
        points2.append(points1[-1, :])                                          # [°]

    # Convert list of points into a LineString, or a Point if there ends up
    # being only one ...
    if len(points2) == 1:
        cleans = shapely.geometry.point.Point(points2[0][0], points2[0][1])
    else:
        cleans = shapely.geometry.linestring.LineString(points2)
        if cleans.length < tol:
            if debug:
                print(f"INFO: Truncating a tiny-length line at ({points2[0][0]:+.6f}°,{points2[0][1]:+.6f}°).")
            cleans = shapely.geometry.point.Point(points2[0][0], points2[0][1])
    if debug:
        check(cleans, prefix = prefix)

    # Return cleaned CoordinateSequence ...
    return cleans
