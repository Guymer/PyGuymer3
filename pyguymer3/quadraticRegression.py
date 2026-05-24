#!/usr/bin/env python3

# Define function ...
def quadraticRegression(
    x,
    y,
    /,
):
    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .linearRegression import linearRegression
    from .mean import mean

    # **************************************************************************

    # Check argument ...
    if not isinstance(x, numpy.ndarray):
        raise TypeError("\"x\" is not a NumPy array") from None
    if not isinstance(y, numpy.ndarray):
        raise TypeError("\"y\" is not a NumPy array") from None
    if x.size != y.size:
        raise Exception("\"x\" is not the same size as \"y\"") from None

    # **************************************************************************

    # Calculate gradients and their mid-points ...
    dydx = (y[1:] - y[:-1]) / (x[1:] - x[:-1])
    midx = 0.5 * (x[:-1] + x[1:])


    # Calculate linear regression parameters and set (two out of three) answers ...
    a, b = linearRegression(midx, dydx)
    a *= 0.5

    # Create short-hands ...
    xbar = mean(x)
    x2bar = mean(numpy.float_power(x, 2))
    ybar = mean(y)

    # Set (final) answer ...
    c = ybar - a * x2bar - b * xbar

    # Return answers ...
    return a, b, c
