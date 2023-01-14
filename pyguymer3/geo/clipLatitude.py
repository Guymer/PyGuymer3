#!/usr/bin/env python3

# Define function ...
def clipLatitude(lat):
    return max(-90.0, min(+90.0, lat)) # -90° ≤ lat ≤ +90°
