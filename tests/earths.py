#!/usr/bin/env python3

# Import standard modules ...
import json
import os

# Import special modules ...
try:
    import PIL
    import PIL.Image
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
PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                                 # [px]

# ******************************************************************************

# Load image metadata ...
meta = json.load(open(f'{os.environ["CARTOPY_USER_BACKGROUNDS"]}/images.json', "rt"))

# Find the path to the lowest resolution image ...
path = f'{os.environ["CARTOPY_USER_BACKGROUNDS"]}/{meta["natural-earth-1"]["large0512px"]}'

# Load the lowest resolution image and draw a red border around it ...
earth = PIL.ImageOps.expand(PIL.Image.open(path).convert("RGB"), border = 2, fill = (255, 0, 0))

# Create a flipped copy to save time ...
flippedEarth = earth.transpose(PIL.Image.FLIP_TOP_BOTTOM).transpose(PIL.Image.FLIP_LEFT_RIGHT)

# ******************************************************************************

# Create an empty image to hold all the images ...
earths = PIL.Image.new("RGB", (3 * earth.width, 3 * earth.height), color = (255, 255, 255))

# Loop over columns ...
for i in range(4):
    # Draw first row ...
    earths.paste(flippedEarth, box = (i * earth.width - earth.width // 2, 0))

# Loop over columns ...
for i in range(3):
    # Draw second row ...
    earths.paste(earth, box = (i * earth.width, earth.height))

# Loop over columns ...
for i in range(4):
    # Draw third row ...
    earths.paste(flippedEarth, box = (i * earth.width - earth.width // 2, 2 * earth.height))

# ******************************************************************************

# Save and optimize image ...
earths.save("earths.png")
pyguymer3.image.optimize_image("earths.png", strip = True)
