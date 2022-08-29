def images2gif(imgs, gif, kwArgCheck = None, debug = False, fps = 25.0, mode = "RGB", optimize = True, strip = False):
    """Convert a sequence of images to a GIF animation.

    This function makes a GIF animation from either a list of PIL Images or a
    list of file paths. The user is able to set the framerate.

    Parameters
    ----------
    imgs : list of PIL.Image.Image or list of str
        the list of input PIL Images or list of paths to the input images
    gif : str
        the path to the output GIF
    debug : bool, optional
        print debug messages (default False)
    fps : float, optional
        the framerate, default 25.0
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
    from ..image.optimize_image import optimize_image

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

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

    # Save it as a GIF ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#gif
    tmpImgs[0].save(
        gif,
            append_images = tmpImgs[1:],
                 duration = round(1000.0 / fps),
                     loop = 0,
                 optimize = optimize,
                 save_all = True,
    )

    # Optimize GIF ...
    if strip:
        optimize_image(gif, debug = debug, strip = strip)
