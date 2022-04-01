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

    # Create a PIL Image class ...
    if isinstance(img, str):
        tmpImg = PIL.Image.open(img)
    elif isinstance(img, PIL.Image.Image):
        tmpImg = img.copy()
    else:
        raise TypeError(f"\"img\" is an unexpected type ({repr(type(img))})") from None

    # Convert image and save it as a GIF ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#gif
    tmpImg.convert(mode).save(gif, optimize = optimize)

    # Optimize GIF ...
    if optimize or strip:
        optimize_image(gif, debug = debug, strip = strip)
