def image2png(img, png, kwArgCheck = None, chunksize = 1048576, debug = False, exif = None, mode = "RGB", optimize = True, strip = False):
    """Save an image as a PNG

    This function accepts either a PIL Image or a file path and saves the image
    as a PNG.

    Parameters
    ----------
    img : PIL.Image.Image or str
        the input PIL Image or path to the input image
    png : str
        the path to the output PNG
    debug : bool, optional
        print debug messages (default False)
    exif : dict, optional
        a dictionary of EXIF data to save in the output PNG (default None)
    mode : str, optional
        the mode of the outout PNG (default "RGB")
    optimize : bool, optional
        optimize the output PNG (default True)
    strip : bool, optional
        strip metadata from the output PNG (default False)

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

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check input ...
    if exif is not None and strip:
        print("WARNING: You have provided EXIF data but then asked to strip metadata.")

    # Find out what the user supplied ...
    if isinstance(img, str):
        # Open image as RGB (even if it is paletted) ...
        with PIL.Image.open(img) as iObj:
            tmpImg = iObj.convert("RGB")

        # Convert it to whatever mode the user asked for ...
        tmpImg = tmpImg.convert(mode)
    elif isinstance(img, PIL.Image.Image):
        # Convert image to whatever mode the user asked for ...
        tmpImg = img.convert(mode)
    else:
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
        optimize_image(png, chunksize = chunksize, debug = debug, strip = strip)
