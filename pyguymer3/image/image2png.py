#!/usr/bin/env python3

# Define function ...
def image2png(
    img,
    png,
    /,
    *,
       chunksize = 1048576,
           debug = __debug__,
            exif = None,
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
    """Save an image as a PNG

    This function accepts either a PIL Image or a file path and saves the image
    as a PNG.

    Parameters
    ----------
    img : PIL.Image.Image or str
        the input PIL Image or path to the input image
    png : str
        the path to the output PNG
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    debug : bool, optional
        print debug messages (default False)
    exif : dict, optional
        a dictionary of EXIF data to save in the output PNG (default None)
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
        the mode of the outout PNG (default "RGB")
    optimize : bool, optional
        optimize the output PNG (default True)
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
        strip metadata from the output PNG (default False)
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
    from .dict2exif import dict2exif
    from .optimize_image import optimize_image

    # Check input ...
    if exif is not None and strip:
        print("WARNING: You have provided EXIF data but then asked to strip metadata.")

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

    # Save it as a PNG ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#png
    tmpImg.save(
        png,
            exif = dict2exif(exif, mode = mode),
        optimize = optimize,
    )

    # Optimize PNG ...
    if optimize or strip:
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
