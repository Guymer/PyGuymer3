def intersection(p1, p2, p3, p4):
    """
    This function finds the intersection of two line segments (defined by four
    points).
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Catch simple misses ...
    if max(p1[0], p2[0]) < min(p3[0], p4[0]):
        # NOTE: line segment #1 is left of line segment #2.
        return False
    if min(p1[0], p2[0]) > max(p3[0], p4[0]):
        # NOTE: line segment #1 is right of line segment #2.
        return False
    if max(p1[1], p2[1]) < min(p3[1], p4[1]):
        # NOTE: line segment #1 is below line segment #2.
        return False
    if min(p1[1], p2[1]) > max(p3[1], p4[1]):
        # NOTE: line segment #1 is above line segment #2.
        return False

    # Find gradients of the two lines ...
    m1 = (p2[1] - p1[1]) / (p2[0] - p1[0])
    m2 = (p4[1] - p3[1]) / (p4[0] - p3[0])

    # Find intercepts of the two lines ...
    c1 = p1[1] - m1 * p1[0]
    c2 = p3[1] - m2 * p3[0]

    # Assemble matrices ...
    a = numpy.array([[-m1, 1.0], [-m2, 1.0]])
    b = numpy.array([c1, c2])

    # Solve two linear equations ...
    p5 = numpy.linalg.solve(a, b)

    # Catch complex misses ...
    if not min(p1[0], p2[0]) <= p5[0] <= max(p1[0], p2[0]):
        # NOTE: intersection is either left or right of line segment #1.
        return False
    if not min(p1[1], p2[1]) <= p5[1] <= max(p1[1], p2[1]):
        # NOTE: intersection is either above or below line segment #1.
        return False
    if not min(p3[0], p4[0]) <= p5[0] <= max(p3[0], p4[0]):
        # NOTE: intersection is either left or right of line segment #2.
        return False
    if not min(p3[1], p4[1]) <= p5[1] <= max(p3[1], p4[1]):
        # NOTE: intersection is either above or below line segment #2.
        return False

    # Return answer ...
    return p5
