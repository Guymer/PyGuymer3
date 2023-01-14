#!/usr/bin/env python3

# Define function ...
def paeth_filter(a, b, c):
    # Find differences ...
    pi = a + b - c
    pa = abs(pi - a)
    pb = abs(pi - b)
    pc = abs(pi - c)

    # Return best point ...
    if pa <= pb and pa <= pc:
        return a
    if pb <= pc:
        return b
    return c
