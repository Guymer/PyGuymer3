#!/usr/bin/env python3

# Define function ...
def load_EXIF1(
    fname,
    /,
):
    # Import special modules ...
    try:
        import exifread
    except:
        raise Exception("\"exifread\" is not installed; run \"pip install --user ExifRead\"") from None

    # Open RAW file read-only ...
    with open(fname, "rb") as fObj:
        # Load EXIF tags ...
        ans = exifread.process_file(fObj, details = False)

    # Return answer ...
    return ans
