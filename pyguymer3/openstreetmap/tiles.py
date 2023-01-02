def tiles(lonC_deg, latC_deg, zoom, width, height, sess, kwArgCheck = None, background = (255, 255, 255), cookies = None, debug = False, fill = (255, 0, 0, 127), headers = None, radius = None, timeout = 10.0, verify = True):
    # Import special modules ...
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
        import PIL.ImageDraw
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import sub-functions ...
    from .deg2num import deg2num
    from .num2deg import num2deg
    from .tile import tile

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check inputs ...
    if not 0 <= zoom <= 19:
        raise Exception(f"zoom is not in the required range ({zoom:d})") from None

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Find which tile contains the location ...
    xtileC, ytileC = deg2num(lonC_deg, latC_deg, zoom)                          # [#], [#]

    # Find where exactly the location is within the central tile (assuming that
    # both the Euclidean and Geodesic spaces are both rectilinear and uniform) ...
    lonCW_deg, latCN_deg = num2deg(xtileC, ytileC, zoom)                        # [°], [°]
    lonCE_deg, latCS_deg = num2deg(xtileC + 1, ytileC + 1, zoom)                # [°], [°]
    xoffset = int(256.0 * (lonCW_deg - lonC_deg) / (lonCW_deg - lonCE_deg))     # [px]
    yoffset = int(256.0 * (latCN_deg - latC_deg) / (latCN_deg - latCS_deg))     # [px]

    # Find out where to start and finish the loops ...
    xtileW = xtileC - (width // 2 // 256) - 1                                   # [#]
    xtileE = xtileC + (width // 2 // 256) + 1                                   # [#]
    ytileN = ytileC - (height // 2 // 256) - 1                                  # [#]
    ytileS = ytileC + (height // 2 // 256) + 1                                  # [#]

    # **************************************************************************

    # Make blank map ...
    tilesIm = PIL.Image.new(
        "RGB",
        (width, height),
        background,
    )

    # Make drawing object, if the user wants to draw circle ...
    if radius is not None:
        draw = PIL.ImageDraw.Draw(tilesIm, "RGBA")

    # Loop over columns ...
    for xtile in range(xtileW, xtileE + 1):
        # Find where to put the top-left corner of this tile ...
        x = width // 2 - xoffset - (xtileC - xtile) * 256                       # [px]

        # Loop over rows ...
        for ytile in range(ytileN, ytileS + 1):
            # Find where to put the top-left corner of this tile ...
            y = height // 2 - yoffset - (ytileC - ytile) * 256                  # [px]

            # Obtain the tile ...
            tileIm = tile(
                xtile,
                ytile,
                zoom,
                sess,
                cookies = cookies,
                  debug = debug,
                headers = headers,
                timeout = timeout,
                 verify = verify,
            )

            # Paste the tile onto the map ...
            tilesIm.paste(tileIm, (x, y))

            # Clean up ...
            del tileIm

    # Draw circle, if the user wants to ...
    if radius is not None:
        draw.ellipse(
            [width // 2 - radius, height // 2 - radius, width // 2 + radius, height // 2 + radius],
            fill = fill,
        )

    # Return answer ...
    return tilesIm
