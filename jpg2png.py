def jpg2png(jpg, png, debug = False, strip = False):
    # Import special modules ...
    try:
        import PIL.Image
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Load sub-functions ...
    from .optimize_image import optimize_image

    # Configure PIL to open images up to 1 GiP ...
    PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                             # [px]

    # Open JPG and save it as a PNG ...
    PIL.Image.open(jpg).convert("RGB").save(png, optimize = True)

    # Optimize PNG ...
    optimize_image(png, debug = debug, strip = strip)