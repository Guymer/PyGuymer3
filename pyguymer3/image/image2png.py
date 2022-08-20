def image2png(img, png, kwArgCheck = None, debug = False, exif = None, mode = "RGB", optimize = True, strip = False):
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
    Copyright 2018 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import PIL
        import PIL.Image
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

    # Configure PIL to open images up to 1 GiP ...
    PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                             # [px]

    # Create a PIL Image class ...
    if isinstance(img, str):
        tmpImg = PIL.Image.open(img)
    elif isinstance(img, PIL.Image.Image):
        tmpImg = img.copy()
    else:
        raise TypeError(f"\"img\" is an unexpected type ({repr(type(img))})") from None

    # Convert image and save it as a PNG ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#png
    tmpImg.convert(mode).save(png, exif = dict2exif(exif, mode = mode), optimize = optimize)

    # Optimize PNG ...
    if optimize or strip:
        optimize_image(png, debug = debug, strip = strip)
