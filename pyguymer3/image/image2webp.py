#!/usr/bin/env python3

# Define function ...
def image2webp(img, webp, /, *, chunksize = 1048576, debug = False, exif = None, lossless = False, method = 6, mode = "RGB", quality = 100, screenHeight = -1, screenWidth = -1, strip = False, timeout = 60.0):
    """Save an image as a WEBP

    This function accepts either a PIL Image or a file path and saves the image
    as a WEBP.

    Parameters
    ----------
    img : PIL.Image.Image or str
        the input PIL Image or path to the input image
    webp : str
        the path to the output WEBP
    debug : bool, optional
        print debug messages (default False)
    exif : dict, optional
        a dictionary of EXIF data to save in the output WEBP (default None)
    lossless : bool, optional
        save a lossless WEBP (default False)
    method : int, optional
        the method to use when saving the WEBP (default 6)
    mode : str, optional
        the mode of the outout WEBP (default "RGB")
    quality : int, optional
        the quality of the output WEBP (default 100)
    screenHeight : int, optional
        the height of the screen to downscale the input image to fit within,
        currently only implemented if "img" is a str (default -1; integers less
        than 100 imply no downscaling)
    screenWidth : int, optional
        the width of the screen to downscale the input image to fit within,
        currently only implemented if "img" is a str (default -1; integers less
        than 100 imply no downscaling)
    strip : bool, optional
        strip metadata from the output WEBP (default False)
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
    if isinstance(img, str):
        # Open image as RGB (even if it is paletted) ...
        with PIL.Image.open(img) as iObj:
            tmpImg = iObj.convert("RGB")

        # Check if the user wants to scale the image down to fit within a screen
        # size ...
        if screenWidth >= 100 and screenHeight >= 100:
            # Resize image in place ...
            tmpImg.thumbnail(
                (screenWidth, screenHeight),
                resample = PIL.Image.Resampling.LANCZOS,
            )

        # Convert it to whatever mode the user asked for ...
        tmpImg = tmpImg.convert(mode)
    elif isinstance(img, PIL.Image.Image):
        # Convert image to whatever mode the user asked for ...
        tmpImg = img.convert(mode)
    else:
        raise TypeError(f"\"img\" is an unexpected type ({repr(type(img))})") from None

    # Save it as a WEBP ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#webp
    tmpImg.save(
        webp,
            exif = dict2exif(exif, mode = mode),
        lossless = lossless,
          method = method,
         quality = quality,
    )

    # Optimize WEBP ...
    if strip:
        optimize_image(webp, chunksize = chunksize, debug = debug, strip = strip, timeout = timeout)
