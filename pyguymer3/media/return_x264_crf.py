#!/usr/bin/env python3

# Define function ...
def return_x264_crf(
    w,
    h,
    /,
):
    # Import modules ...
    import math

    # Define points ...
    x1 = 720.0 * 426.0
    y1 = 20.0
    x2 = 1920.0 * 800.0
    y2 = 22.0

    # Fit a straight line to the two point in log(x)/linear(y) space ...
    xBar = (math.log(x1) + math.log(x2)) / 2.0
    yBar = (y1 + y2) / 2.0
    xDelta = math.log(x2) - math.log(x1)
    yDelta = y2 - y1
    m = yDelta / xDelta
    c = yBar - m * xBar

    # Return answer ...
    return m * math.log(float(w * h)) + c
