#!/usr/bin/env python3

# Define function ...
def dot2png(dot, png, kwArgCheck = None, chunksize = 1048576, debug = False, strip = False):
    # Import standard modules ...
    import shutil
    import subprocess

    # Import sub-functions ...
    from .optimize_image import optimize_image

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

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
    )

    # Optimize PNG ...
    optimize_image(png, chunksize = chunksize, debug = debug, strip = strip)
