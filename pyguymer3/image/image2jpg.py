def image2jpg(img, jpg, kwArgCheck = None, debug = False, exif = None, mode = "RGB", optimize = True, progressive = False, quality = 95, strip = False):
    """Save an image as a JPG

    This function accepts either a PIL Image or a file path and saves the image
    as a JPG.

    Parameters
    ----------
    img : PIL.Image.Image or str
        the input PIL Image or path to the input image
    jpg : str
        the path to the output JPG
    debug : bool, optional
        print debug messages (default False)
    exif : dict, optional
        a dictionary of EXIF data to save in the output JPG (default None)
    mode : str, optional
        the mode of the outout JPG (default "RGB")
    optimize : bool, optional
        optimize the output JPG (default True)
    progressive : bool, optional
        save a progressive JPG (default False)
    quality : int, optional
        the quality of the output JPG (default 95)
    strip : bool, optional
        strip metadata from the output JPG (default False)

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

    # Save it as a JPG ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#jpeg
    tmpImg.save(
        jpg,
               exif = dict2exif(exif, mode = mode),
           optimize = optimize,
        progressive = progressive,
            quality = quality,
    )

    # Optimize JPG ...
    if optimize or strip:
        optimize_image(jpg, debug = debug, strip = strip)
