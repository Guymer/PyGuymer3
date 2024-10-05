#!/usr/bin/env python3

# Define function ...
def nlines(
    fname,
    /,
):
    # Initialize answer ...
    n = 0                                                                       # [#]

    # Open the input file ...
    with open(fname, "rt", encoding = "utf-8") as fObj:
        # Load the source and strip whitespace ...
        src = fObj.read().strip()

        # Set answer ...
        n = len(src.splitlines())                                               # [#]

    # Return answer ...
    return n
