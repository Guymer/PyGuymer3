#!/usr/bin/env python3

# Define function ...
def dot2png(
    dot,
    png,
    /,
    *,
    chunksize = 1048576,
        debug = __debug__,
        strip = False,
      timeout = 60.0,
):
    # Import standard modules ...
    import shutil
    import subprocess

    # Import sub-functions ...
    from .optimize_image import optimize_image

    # Check that "dot" is installed ...
    if shutil.which("dot") is None:
        raise Exception("\"dot\" is not installed") from None

    # Create image ...
    subprocess.run(
        ["dot", "-Tpng", dot, "-o", png],
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
