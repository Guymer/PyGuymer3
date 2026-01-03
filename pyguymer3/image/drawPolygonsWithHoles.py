#!/usr/bin/env python3

# Define function ...
def drawPolygonsWithHoles(
    img,
    polys,
    holes,
    color,
    /,
    *,
    maxImagePixels = 1073741824,
):
    """Draw some polygons (which have holes) on an image

    Parameters
    ----------
    img : PIL.Image.Image
        the image to be drawn on
    polys : list of list of tuples of float
        the polygons (without holes) to draw
    holes : list of list of tuples of float
        the holes to draw
    color : int or tuple of int
        the fill colour of the polygons
    maxImagePixels : int, optional
        The maximum number of pixels in an image, to prevent decompression bombs.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = maxImagePixels                             # [px]
        import PIL.ImageDraw
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # **************************************************************************

    # Create a mask and a drawing object ...
    # NOTE: The Pillow documentation says:
    #          "Where the mask is 255, the given image is copied as is. Where
    #           the mask is 0, the current value is preserved. Intermediate
    #           values will mix the two images together, including their alpha
    #           channels if they have them."
    mask = PIL.Image.new(
        color = 0,
         mode = "L",
         size = img.size,
    )
    draw = PIL.ImageDraw.Draw(mask)

    # Loop over polygons ...
    for poly in polys:
        # Draw the polygon ...
        draw.polygon(
            poly,
             fill = 255,
            width = 0,
        )

    # Loop over holes ...
    for hole in holes:
        # Draw the hole ...
        draw.polygon(
            hole,
             fill = 0,
            width = 0,
        )

    # Clean up ...
    del draw

    # Create a blank image which is uniformly filled with the polgons' fill
    # colour ...
    fill = PIL.Image.new(
        color = color,
         mode = img.mode,
         size = img.size,
    )

    # Paste the polgons' fill colour, correctly masking for the presence of
    # polygons and their holes ...
    img.paste(
         box = (0, 0),
          im = fill,
        mask = mask,
    )

    # Clean up ...
    del mask, fill
