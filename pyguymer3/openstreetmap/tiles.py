def tiles(xtileW, xtileE, ytileN, ytileS, zoom, sess, kwArgCheck = None, cookies = None, debug = False, headers = None, mode = "RGB", timeout = 10.0, verify = True):
    # Import special modules ...
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import sub-functions ...
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

    # Make blank map ...
    tilesIm = PIL.Image.new(
        "RGB",
        ((xtileE - xtileW + 1) * 256, (ytileN - ytileS + 1) * 256),
        (255, 255, 255),
    )

    # Initialize counter ...
    i = 0                                                                       # [#]

    # Loop over columns ...
    for xtile in range(xtileW, xtileE + 1):
        # Initialize counter ...
        j = 0                                                                   # [#]

        # Loop over rows ...
        for ytile in range(ytileN, ytileS + 1):
            # Obtain the tile ...
            tileIm = tile(
                xtile,
                ytile,
                zoom,
                sess,
                cookies = cookies,
                  debug = debug,
                headers = headers,
                   mode = mode,
                timeout = timeout,
                 verify = verify,
            )

            # Paste the tile onto the map ...
            tilesIm.paste(tileIm, (i * 256, j * 256))

            # Clean up ...
            del tileIm

            # Increment counter ...
            j += 1                                                              # [#]

        # Increment counter ...
        i += 1                                                                  # [#]

    # Return answer ...
    return tilesIm
