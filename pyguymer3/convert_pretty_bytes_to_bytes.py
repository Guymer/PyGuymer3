#!/usr/bin/env python3

# Define function ...
def convert_pretty_bytes_to_bytes(string, /):
    # Import standard modules ...
    import re

    # Extract digits (with decimal point) and letters separately ...
    val = float(re.sub(r"[A-Z]", "", string.upper()))                           # [?]
    units = re.sub(r"[0-9\.]", "", string).upper()

    # Scale value ...
    if units in ["KB", "KiB"]:
        val *= 1024.0                                                           # [B]
    elif units in ["MB", "MiB"]:
        val *= 1024.0 * 1024.0                                                  # [B]
    elif units in ["GB", "GiB"]:
        val *= 1024.0 * 1024.0 * 1024.0                                         # [B]
    elif units in ["TB", "TiB"]:
        val *= 1024.0 * 1024.0 * 1024.0 * 1024.0                                # [B]
    elif units in ["PB", "PiB"]:
        val *= 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0                       # [B]

    # Return answer ...
    return val
