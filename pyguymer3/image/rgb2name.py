#!/usr/bin/env python3

# Define function ...
def rgb2name(r, g, b, /):
    """
    This function says if one channel is brighter than the others in an RGB
    triplet.
    """

    # Catch channel ...
    if r > g and r > b:
        return "redish"

    # Catch channel ...
    if g > r and g > b:
        return "greenish"

    # Catch channel ...
    if b > r and b > g:
        return "blueish"

    # Set default ...
    return "it's complicated"
