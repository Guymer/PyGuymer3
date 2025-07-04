#!/usr/bin/env python3

# Define function ...
def optimize_image(
    fname,
    /,
    *,
       chunksize = 1048576,
           debug = __debug__,
    exiftoolPath = None,
    gifsiclePath = None,
    jpegtranPath = None,
     optipngPath = None,
            pool = None,
           strip = False,
         timeout = 60.0,
):
    """
    Please read the documentation for the four functions: "exiftool",
    "gifsicle", "jpegtran" and "optipng". It is not safe to keep on running the
    programs "gifsicle" and "jpegtran" on images, but it is safe to keep on
    running all of my wrapper functions on images instead.

    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    """

    # Import standard modules ...
    import multiprocessing
    import multiprocessing.pool
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
    match ext.lower():
        case ".gif":
            gifsicle(
                fname,
                   chunksize = chunksize,
                       debug = debug,
                gifsiclePath = gifsiclePath,
                     timeout = timeout,
            )
        case ".jpg" | ".jpeg":
            jpegtran(
                fname,
                   chunksize = chunksize,
                       debug = debug,
                jpegtranPath = jpegtranPath,
                     timeout = timeout,
            )
        case ".png":
            optipng(
                fname,
                optipngPath = optipngPath,
                       pool = pool,
                    timeout = timeout,
            )
        case _:
            # Crash ...
            raise ValueError(f"\"ext.lower()\" is an unexpected value ({repr(ext.lower())})") from None

    # Strip metadata synchronously ...
    if strip and not isinstance(pool, multiprocessing.pool.Pool):
        exiftool(
            fname,
            exiftoolPath = exiftoolPath,
                 timeout = timeout,
        )
