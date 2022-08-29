def image2gif(img, gif, kwArgCheck = None, debug = False, mode = "RGB", optimize = True, strip = False):
    """Save an image as a GIF

    This function accepts either a PIL Image or a file path and saves the image
    as a GIF.

    Parameters
    ----------
    img : PIL.Image.Image or str
        the input PIL Image or path to the input image
    gif : str
        the path to the output GIF
    debug : bool, optional
        print debug messages (default False)
    mode : str, optional
        the mode of the outout GIF (default "RGB")
    optimize : bool, optional
        optimize the output GIF (default True)
    strip : bool, optional
        strip metadata from the output GIF (default False)

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
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import sub-functions ...
    from .optimize_image import optimize_image

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

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

    # Save it as a GIF ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#gif
    tmpImg.save(
        gif,
        optimize = optimize,
    )

    # Optimize GIF ...
    if optimize or strip:
        optimize_image(gif, debug = debug, strip = strip)
