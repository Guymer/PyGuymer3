#!/usr/bin/env python3

# Define function ...
def wrapLongitude(lon):
    return ((lon + 180.0) % 360.0) - 180.0 # -180° ≤ lon < +180° (i.e., +180° is not accepted, it is wrapped around to -180°)
