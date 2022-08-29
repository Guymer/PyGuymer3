def images2webp(imgs, webp, kwArgCheck = None, debug = False, exif = None, fps = 25.0, lossless = False, method = 6, minimize_size = True, mode = "RGB", quality = 100, strip = False):
    """Convert a sequence of images to a WEBP animation.

    This function makes a WEBP animation from either a list of PIL Images or a
    list of file paths. The user is able to set the framerate.

    Parameters
    ----------
    imgs : list of PIL.Image.Image or list of str
        the list of input PIL Images or list of paths to the input images
    webp : str
        the path to the output WEBP
    debug : bool, optional
        print debug messages (default False)
    exif : dict, optional
        a dictionary of EXIF data to save in the output WEBP (default None)
    fps : float, optional
        the framerate, default 25.0
    lossless : bool, optional
        save a lossless WEBP (default False)
    method : int, optional
        the method to use when saving the WEBP (default 6)
    minimize_size : bool, optional
        minimize the output size (default True)
    mode : str, optional
        the mode of the outout WEBP (default "RGB")
    quality : int, optional
        the quality of the output WEBP (default 100)
    strip : bool, optional
        strip metadata from the output WEBP (default False)

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
    from ..image.dict2exif import dict2exif
    from ..image.optimize_image import optimize_image

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check input ...
    if exif is not None and strip:
        print("WARNING: You have provided EXIF data but then asked to strip metadata.")

    # Configure PIL to open images up to 1 GiP ...
    PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                             # [px]

    # Initialize list ...
    tmpImgs = []

    # Loop over input ...
    for img in imgs:
        # Find out what the user supplied ...
        if isinstance(img, str):
            # Open image as RGB (even if it is paletted) ...
            with PIL.Image.open(img) as iObj:
                tmpImg = iObj.convert("RGB")

            # Convert it to whatever mode the user asked for ...
            tmpImgs.append(tmpImg.convert(mode))
        elif isinstance(img, PIL.Image.Image):
            # Convert image to whatever mode the user asked for ...
            tmpImgs.append(img.convert(mode))
        else:
            raise TypeError(f"\"img\" is an unexpected type ({repr(type(img))})") from None

    # Save it as a WEBP ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#webp
    tmpImgs[0].save(
        webp,
            append_images = tmpImgs[1:],
                 duration = round(1000.0 / fps),
                     exif = dict2exif(exif, mode = mode),
                     loop = 0,
                 lossless = lossless,
                   method = method,
            minimize_size = minimize_size,
                  quality = quality,
                 save_all = True,
    )

    # Optimize WEBP ...
    if strip:
        optimize_image(webp, debug = debug, strip = strip)
