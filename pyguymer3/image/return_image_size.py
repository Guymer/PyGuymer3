#!/usr/bin/env python3

# Define function ...
def return_image_size(fname):
    # Import special modules ...
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Open image as RGB (even if it is paletted) ...
    with PIL.Image.open(fname) as iObj:
        im = iObj.convert("RGB")

    # Return answer ...
    return im.size
