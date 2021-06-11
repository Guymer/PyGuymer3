def jpg2png(jpg, png, kwArgCheck = None, debug = False, strip = False):
    # Import special modules ...
    try:
        import PIL
        import PIL.Image
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Load sub-functions ...
    from .optimize_image import optimize_image

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Configure PIL to open images up to 1 GiP ...
    PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                             # [px]

    # Open JPG and save it as a PNG ...
    PIL.Image.open(jpg).convert("RGB").save(png, optimize = True)

    # Optimize PNG ...
    optimize_image(png, debug = debug, strip = strip)
