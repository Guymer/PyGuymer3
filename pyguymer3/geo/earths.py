#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.10/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import json
    import os

    # Import special modules ...
    try:
        import PIL
        import PIL.Image
        import PIL.ImageDraw
        import PIL.ImageFont
        import PIL.ImageOps
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    # Configure PIL to open images up to 1 GiP ...
    PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                             # [px]

    # **************************************************************************

    # Load image metadata ...
    with open(f'{os.environ["CARTOPY_USER_BACKGROUNDS"]}/images.json', "rt", encoding = "utf-8") as fobj:
        meta = json.load(fobj)

    # Find the path to the lowest resolution image ...
    path = f'{os.environ["CARTOPY_USER_BACKGROUNDS"]}/{meta["natural-earth-1"]["large0512px"]}'

    # Load the lowest resolution image and draw a red border around it ...
    with PIL.Image.open(path) as iObj:
        earth = PIL.ImageOps.expand(iObj.convert("RGB"), border = 2, fill = (255, 0, 0))

    # Create a flipped copy to save time ...
    flippedEarth = earth.transpose(PIL.Image.FLIP_TOP_BOTTOM).transpose(PIL.Image.FLIP_LEFT_RIGHT)

    # **************************************************************************

    # Create an empty image to hold all the images ...
    earths = PIL.Image.new("RGB", (3 * earth.width, 3 * earth.height), color = (255, 255, 255))

    # Loop over columns ...
    for i in range(2):
        # Draw first row ...
        earths.paste(flippedEarth, box = (i * earth.width + earth.width // 2,               0))

    # Loop over columns ...
    for i in range(3):
        # Draw second row ...
        earths.paste(earth       , box = (i * earth.width                   ,    earth.height))

    # Loop over columns ...
    for i in range(2):
        # Draw third row ...
        earths.paste(flippedEarth, box = (i * earth.width + earth.width // 2, 2 * earth.height))

    # Create a drawing object ...
    draw = PIL.ImageDraw.Draw(earths)

    # Create a font object ...
    font = PIL.ImageFont.truetype(font = "Arial Unicode.ttf", size = 40)

    # Label first row ...
    draw.text((    earth.width                   ,                    earth.height // 2), "A", anchor = "mm", fill = (255, 0, 0), font = font)
    draw.text((2 * earth.width                   ,                    earth.height // 2), "B", anchor = "mm", fill = (255, 0, 0), font = font)

    # Label second row ...
    draw.text((                  earth.width // 2,     earth.height + earth.height // 2), "C", anchor = "mm", fill = (255, 0, 0), font = font)
    draw.text((    earth.width + earth.width // 2,     earth.height + earth.height // 2), "D", anchor = "mm", fill = (255, 0, 0), font = font)
    draw.text((2 * earth.width + earth.width // 2,     earth.height + earth.height // 2), "E", anchor = "mm", fill = (255, 0, 0), font = font)

    # Label third row ...
    draw.text((    earth.width                   , 2 * earth.height + earth.height // 2), "F", anchor = "mm", fill = (255, 0, 0), font = font)
    draw.text((2 * earth.width                   , 2 * earth.height + earth.height // 2), "G", anchor = "mm", fill = (255, 0, 0), font = font)

    # **************************************************************************

    # Save and optimize image ...
    earths.save("earths.png")
    pyguymer3.image.optimize_image("earths.png", strip = True)
