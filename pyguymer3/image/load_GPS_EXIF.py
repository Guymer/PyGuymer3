#!/usr/bin/env python3

# Define function ...
def load_GPS_EXIF(
    fName,
    /,
    *,
        cacheDir = "~/.cache/pyguymer3",
      compressed = False,
           debug = __debug__,
       ensureNFC = True,
    exiftoolPath = None,
          python = True,
         timeout = 60.0,
):
    # Import sub-functions ...
    from .load_GPS_EXIF1 import load_GPS_EXIF1
    from .load_GPS_EXIF2 import load_GPS_EXIF2

    # **************************************************************************

    # Check what the user wants ...
    if python:
        # Will use the Python module "exifread" ...
        return load_GPS_EXIF1(
            fName,
            cacheDir = cacheDir,
               debug = debug,
        )

    # Will use the binary "exiftool" ...
    return load_GPS_EXIF2(
        fName,
            cacheDir = cacheDir,
          compressed = compressed,
               debug = debug,
           ensureNFC = ensureNFC,
        exiftoolPath = exiftoolPath,
             timeout = timeout,
    )
