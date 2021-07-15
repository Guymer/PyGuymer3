def fillin_LinearRing(ring, fill, kwArgCheck = None, debug = False, tol = 1.0e-10):
    """Fill in a LinearRing

    This function reads in a LinearRing that exists on the surface of the Earth
    and returns a LinearRing of the same LinearRing filled in by a constant
    distance (in degrees).

    Parameters
    ----------
    ring : shapely.geometry.polygon.LinearRing
            the LinearRing
    fill : float
            the Euclidean distance to fill in between each point within the shape by (in degrees)
    debug : bool, optional
            print debug messages
    tol : float, optional
            the Euclidean distance that defines two points as being the same (in degrees)

    Returns
    -------
    fills : shapely.geometry.linestring.LineString
            the filled in LinearRing
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
        import shapely.geometry
        import shapely.validation
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Load sub-functions ...
    from .fillin import fillin

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(ring, shapely.geometry.polygon.LinearRing):
        raise TypeError("\"ring\" is not a LinearRing") from None
    if not ring.is_valid:
        raise Exception(f"\"ring\" is not a valid LinearRing ({shapely.validation.explain_validity(ring)})") from None
    if ring.is_empty:
        raise Exception("\"ring\" is an empty LinearRing") from None

    # **************************************************************************

    # Convert the CoordinateSequence to a NumPy array ...
    points1 = numpy.array(ring.coords)                                          # [°]

    # Initialize list ...
    points2 = []                                                                # [°]
    points2.append(points1[0, :])                                               # [°]

    # Initialize flag ...
    skip = False

    # Loop over points ...
    for ipoint in range(1, points1.shape[0] - 1):
        # Skip this point if it needs skipping ...
        if skip:
            # Re-set flag ...
            skip = False

            continue

        # Find the Euclidean distance between the points either side of this
        # point ...
        dr = numpy.hypot(points1[ipoint - 1, 0] - points1[ipoint + 1, 0], points1[ipoint - 1, 1] - points1[ipoint + 1, 1])  # [°]

        # Skip this point (and the next one) if it is a bad spike ...
        if dr < tol:
            if debug:
                print(f"INFO: Removing spike between ({points1[ipoint, 0]:.6f}°,{points1[ipoint, 1]:.6f}°) and ({points1[ipoint + 1, 0]:.6f}°,{points1[ipoint + 1, 1]:.6f}°).")

            # Set flag ...
            skip = True

            continue

        # Append point to list ...
        points2.append(points1[ipoint, :])                                      # [°]

    # Append last point to list (if it shouldn't be skipped) ...
    if not skip:
        points2.append(points1[-1, :])                                          # [°]

    # Clean up ...
    del points1

    # Convert list of points in to a LinearRing ...
    coords = shapely.geometry.polygon.LinearRing(points2)

    # Clean up ...
    del points2

    # Check LinearRing ...
    if not isinstance(coords, shapely.geometry.polygon.LinearRing):
        raise TypeError("\"coords\" is not a LinearRing") from None
    if not coords.is_valid:
        raise Exception(f"\"coords\" is not a valid LinearRing ({shapely.validation.explain_validity(coords)})") from None
    if coords.is_empty:
        raise Exception("\"coords\" is an empty LinearRing") from None

    # **************************************************************************

    # Return filled in LinearRing ...
    return fillin(coords.coords, fill, debug = debug)
