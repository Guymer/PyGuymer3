#!/usr/bin/env python3

# Define function ...
def dot2png(
    dot,
    png,
    /,
    *,
    chunksize = 1048576,
        debug = __debug__,
      dotPath = None,
        strip = False,
      timeout = 60.0,
):
    # Import standard modules ...
    import shutil
    import subprocess

    # Import sub-functions ...
    from .optimize_image import optimize_image

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if dotPath is None:
        dotPath = shutil.which("dot")
    assert dotPath is not None, "\"dot\" is not installed"

    # Create image ...
    subprocess.run(
        [
            dotPath,
            "-Tpng",
            dot,
            "-o", png,
        ],
           check = True,
        encoding = "utf-8",
          stderr = subprocess.DEVNULL,
          stdout = subprocess.DEVNULL,
         timeout = timeout,
    )

    # Optimize PNG ...
    optimize_image(
        png,
        chunksize = chunksize,
            debug = debug,
             pool = None,
            strip = strip,
          timeout = timeout,
    )
