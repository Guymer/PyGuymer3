def png2jpg(png, jpg, kwArgCheck = None, debug = False, strip = False):
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

    # Open PNG and save it as a JPG ...
    PIL.Image.open(png).convert("RGB").save(jpg, optimize = True, quality = 95)

    # Optimize JPG ...
    optimize_image(jpg, debug = debug, strip = strip)
