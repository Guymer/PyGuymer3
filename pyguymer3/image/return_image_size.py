#!/usr/bin/env python3

# Define function ...
def return_image_size(
    fname,
    /,
    *,
    compressed = False,
):
    # Import standard modules ...
    import gzip

    # Import special modules ...
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Check if the image is compressed ...
    if compressed:
        # Open compressed file ...
        with gzip.open(fname, mode = "rb") as gzObj:
            # Open image as RGB (even if it is paletted) ...
            with PIL.Image.open(gzObj) as iObj:
                im = iObj.convert("RGB")
    else:
        # Open image as RGB (even if it is paletted) ...
        with PIL.Image.open(fname) as iObj:
            im = iObj.convert("RGB")

    # Return answer ...
    return im.size
