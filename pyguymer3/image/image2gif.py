#!/usr/bin/env python3

# Define function ...
def image2gif(
    img,
    gif,
    /,
    *,
       chunksize = 1048576,
           debug = __debug__,
    exiftoolPath = None,
    gifsiclePath = None,
    jpegtranPath = None,
            mode = "RGB",
        optimize = True,
     optipngPath = None,
    screenHeight = -1,
     screenWidth = -1,
           strip = False,
         timeout = 60.0,
):
    """Save an image as a GIF

    This function accepts either a PIL Image or a file path and saves the image
    as a GIF.

    Parameters
    ----------
    img : PIL.Image.Image or str
        the input PIL Image or path to the input image
    gif : str
        the path to the output GIF
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    debug : bool, optional
        print debug messages (default False)
    exiftoolPath : str, optional
        the path to the "exiftool" binary (if not provided then Python will attempt to
        find the binary itself)
    gifsiclePath : str, optional
        the path to the "gifsicle" binary (if not provided then Python will attempt to
        find the binary itself)
    jpegtranPath : str, optional
        the path to the "jpegtran" binary (if not provided then Python will attempt to
        find the binary itself)
    mode : str, optional
        the mode of the outout GIF (default "RGB")
    optimize : bool, optional
        optimize the output GIF (default True)
    optipngPath : str, optional
        the path to the "optipng" binary (if not provided then Python will attempt to
        find the binary itself)
    screenHeight : int, optional
        the height of the screen to downscale the input image to fit within,
        currently only implemented if "img" is a str (default -1; integers less
        than 100 imply no downscaling)
    screenWidth : int, optional
        the width of the screen to downscale the input image to fit within,
        currently only implemented if "img" is a str (default -1; integers less
        than 100 imply no downscaling)
    strip : bool, optional
        strip metadata from the output GIF (default False)
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import sub-functions ...
    from .optimize_image import optimize_image

    # Find out what the user supplied ...
    match img:
        case str():
            # Open image as RGB (even if it is paletted) ...
            with PIL.Image.open(img) as iObj:
                tmpImg = iObj.convert("RGB")

            # Check if the user wants to scale the image down to fit within a
            # screen size ...
            if screenWidth >= 100 and screenHeight >= 100:
                # Resize image in place ...
                tmpImg.thumbnail(
                    (screenWidth, screenHeight),
                    resample = PIL.Image.Resampling.LANCZOS,
                )

            # Convert it to whatever mode the user asked for ...
            tmpImg = tmpImg.convert(mode)
        case PIL.Image.Image():
            # Convert image to whatever mode the user asked for ...
            tmpImg = img.convert(mode)
        case _:
            # Crash ...
            raise TypeError(f"\"img\" is an unexpected type ({repr(type(img))})") from None

    # Save it as a GIF ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#gif
    tmpImg.save(
        gif,
        optimize = optimize,
    )

    # Optimize GIF ...
    if optimize or strip:
        optimize_image(
            gif,
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
