def convert_pretty_bytes_to_bytes(string):
    # Import modules ...
    import re

    # Extract digits (with decimal point) and letters separately ...
    val = float(re.sub(r"[A-Z]", "", string.upper()))                           # [?]
    units = re.sub(r"[0-9\.]", "", string).upper()

    # Scale value ...
    if units == "KB" or units == "KiB":
        val *= 1024.0                                                           # [B]
    elif units == "MB" or units == "MiB":
        val *= 1024.0 * 1024.0                                                  # [B]
    elif units == "GB" or units == "GiB":
        val *= 1024.0 * 1024.0 * 1024.0                                         # [B]
    elif units == "TB" or units == "TiB":
        val *= 1024.0 * 1024.0 * 1024.0 * 1024.0                                # [B]
    elif units == "PB" or units == "PiB":
        val *= 1024.0 * 1024.0 * 1024.0 * 1024.0 * 1024.0                       # [B]

    # Return answer ...
    return val
