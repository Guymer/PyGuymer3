def great_circle(lon1, lat1, lon2, lat2, kwArgCheck = None, npoint = 5):
    """
    This function reads in two starting coordinates (in degrees) and two
    finishing coordinates (in degrees) on the surface of a sphere and calculates
    the great circle that connects them.
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .find_point_on_great_circle import find_point_on_great_circle

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize array ...
    circle = numpy.zeros((npoint, 2), dtype = numpy.float64)                    # [°]

    # Loop over points ...
    for ipoint in range(npoint):
        # Set point ...
        circle[ipoint, :] = find_point_on_great_circle(ipoint / float(npoint - 1), lon1, lat1, lon2, lat2)  # [°]

    # Return answer ...
    return circle
