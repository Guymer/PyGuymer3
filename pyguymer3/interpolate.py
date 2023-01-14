#!/usr/bin/env python3

# Define function ...
def interpolate(x1, x2, y1, y2, x):
    """
    This function extrapolates/interpolates a line segment (defined by two
    points).
    """

    # Return answer ...
    return (y1 * (x2 - x) + y2 * (x - x1)) / (x2 - x1)
