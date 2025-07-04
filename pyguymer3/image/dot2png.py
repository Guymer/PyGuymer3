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
    exiftoolPath = None,
    gifsiclePath = None,
    jpegtranPath = None,
     optipngPath = None,
           strip = False,
         timeout = 60.0,
):
    """
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    """

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
        exiftoolPath = exiftoolPath,
        gifsiclePath = gifsiclePath,
        jpegtranPath = jpegtranPath,
         optipngPath = optipngPath,
                pool = None,
               strip = strip,
             timeout = timeout,
    )
