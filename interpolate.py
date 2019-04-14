def interpolate(x1, x2, y1, y2, x):
    return (y1 * (x2 - x) + y2 * (x - x1)) / (x2 - x1)
