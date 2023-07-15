#!/usr/bin/env python3

# Define function ...
def optimize_image(fname, /, *, chunksize = 1048576, debug = False, strip = False, timeout = 60.0):
    """
    Please read the documentation for the four functions: "exiftool",
    "gifsicle", "jpegtran" and "optipng". It is not safe to keep on running the
    programs "gifsicle" and "jpegtran" on images, but it is safe to keep on
    running all of my wrapper functions on images instead.
    """

    # Import standard modules ...
    import os

    # Import sub-functions ...
    from .exiftool import exiftool
    from .gifsicle import gifsicle
    from .jpegtran import jpegtran
    from .optipng import optipng

    # Check that the image exists ...
    if not os.path.exists(fname):
        raise Exception(f"\"{fname}\" does not exist") from None

    # Extract file extension ...
    ext = os.path.splitext(fname)[1]

    # Optimize image depending the file extension ...
    if ext.lower() in [".gif"]:
        gifsicle(fname, chunksize = chunksize, debug = debug, timeout = timeout)
    elif ext.lower() in [".jpg", ".jpeg"]:
        jpegtran(fname, chunksize = chunksize, debug = debug, timeout = timeout)
    elif ext.lower() in [".png"]:
        optipng(fname, timeout = timeout)
    else:
        if debug:
            print(f"WARNING: \"{ext}\" is not a recognised file extension, no optimization will take place.")

    # Strip metadata ...
    if strip:
        exiftool(fname, timeout = timeout)
