def return_image_size(fname):
    # Import special modules ...
    try:
        import PIL
        import PIL.Image
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Configure PIL to open images up to 1 GiP ...
    PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                             # [px]

    # Open image as RGB (even if it is paletted) ...
    with PIL.Image.open(fname) as iObj:
        im = iObj.convert("RGB")

    # Return answer ...
    return im.size
