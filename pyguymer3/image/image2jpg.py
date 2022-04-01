def image2jpg(img, jpg, kwArgCheck = None, debug = False, exif = None, optimize = True, progressive = False, quality = 95, strip = False):
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

    # Configure PIL to open images up to 1 GiP ...
    PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                             # [px]

    # Open image and save it as a JPG ...
    # NOTE: See https://pillow.readthedocs.io/en/stable/handbook/image-file-formats.html#jpeg
    PIL.Image.open(img).convert("RGB").save(jpg, exif = dict2exif(exif), optimize = optimize, progressive = progressive, quality = quality)

    # Optimize JPG ...
    optimize_image(jpg, debug = debug, strip = strip)
