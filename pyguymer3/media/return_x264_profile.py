#!/usr/bin/env python3

# Define function ...
def return_x264_profile(
    w,
    h,
    /,
):
    # Check if the resolution is ≥ 1 MiP ...
    if w * h >= 1048576:
        # Return answer ...
        return "high"

    # Return answer ...
    return "baseline"
