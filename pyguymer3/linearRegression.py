#!/usr/bin/env python3

# Define function ...
def linearRegression(
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

    # Create short-hands ...
    xbar = mean(x, dof = 0)
    ybar = mean(y, dof = 0)

    # Calculate parts of fraction ...
    top = ((x - xbar) * y).sum()
    bot = numpy.float_power(x - xbar, 2).sum()

    # Return answer ...
    return top / bot, ybar - top * xbar / bot
