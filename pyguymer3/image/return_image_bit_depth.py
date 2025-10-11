#!/usr/bin/env python3

# Define function ...
def return_image_bit_depth(
    fName,
    /,
    *,
      cacheDir = "~/.cache/pyguymer3",
    compressed = False,
         debug = __debug__,
):
    # Import global (subclassed) dictionary ...
    from .__pillow__ import __pillow__

    # **************************************************************************

    # Configure global (subclassed) dictionary ...
    __pillow__.cacheDir = cacheDir
    __pillow__.compressed = compressed
    __pillow__.debug = debug

    # **************************************************************************

    # Return the answer ...
    return __pillow__[fName]["bits"]                                            # [b]
