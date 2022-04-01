def image2webp(img, webp, kwArgCheck = None, exif = None, lossless = False, method = 6, mode = "RGB", quality = 100):
    """Save an image as a WEBP

    This function accepts either a PIL Image or a file path and saves the image
    as a WEBP.

    Parameters
    ----------
    img : PIL.Image.Image or str
            the input PIL Image or path to the input image
    webp : str
            the path to the output WEBP
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
    """

    # Import special modules ...
    try:
        import PIL
        import PIL.Image
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import sub-functions ...
    from .dict2exif import dict2exif

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

    # Convert image and save it as a WEBP ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#webp
    tmpImg.convert(mode).save(webp, exif = dict2exif(exif, mode = mode), lossless = lossless, method = method, quality = quality)
