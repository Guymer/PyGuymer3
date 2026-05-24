#!/usr/bin/env python3

# Define function ...
def solve_quadratic_equation(
    a,
    b,
    c,
    /,
):
    # Import standard modules ...
    import math

    # **************************************************************************

    # Check sign of b ...
    if b >= 0.0:
        # Return answer ...
        return (-b - math.sqrt(b ** 2 - 4.0 * a * c)) / (2.0 * a), (2.0 * c) / (-b - math.sqrt(b ** 2 - 4.0 * a * c))

    # Return answer ...
    return (2.0 * c) / (-b + math.sqrt(b ** 2 - 4.0 * a * c)), (-b + math.sqrt(b ** 2 - 4.0 * a * c)) / (2.0 * a)
