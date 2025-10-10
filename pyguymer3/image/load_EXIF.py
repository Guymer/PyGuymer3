#!/usr/bin/env python3

# Define function ...
def load_EXIF(
    fname,
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
    from .load_EXIF1 import load_EXIF1
    from .load_EXIF2 import load_EXIF2

    # Check what the user wants ...
    if python:
        # Will use the Python module "exifread" ...
        return load_EXIF1(fname)

    # Will use the binary "exiftool" ...
    return load_EXIF2(
        fname,
            cacheDir = cacheDir,
          compressed = compressed,
               debug = debug,
           ensureNFC = ensureNFC,
        exiftoolPath = exiftoolPath,
             timeout = timeout,
    )
