#!/usr/bin/env python3

# Define function ...
def tiles(
    lonC_deg,
    latC_deg,
    zoom,
    width,
    height,
    sess,
    /,
    *,
      background = (255, 255, 255),
       chunksize = 1048576,
         cookies = None,
           debug = __debug__,
    exiftoolPath = None,
            fill = (255, 0, 0, 127),
    gifsiclePath = None,
         headers = None,
    jpegtranPath = None,
     optipngPath = None,
          radius = None,
         timeout = 10.0,
          verify = True,
):
    """Merge some OpenStreetMap tiles around a location into one large tile

    This function reads in a location, a zoom and an image size. It then fetches
    all of the OpenStreetMap tiles that are in that field-of-view and returns an
    image of them all merged together.

    Parameters
    ----------
    lonC_deg : float
        the central longitude (in degrees)
    latC_deg : float
        the central latitude (in degrees)
    zoom : int
        the OpenStreetMap zoom level
    width : int
        the width of the merged tile (in pixels)
    height : int
        the height of the merged tile (in pixels)
    sess : requests.Session
        the session for any requests calls
    background : tuple of int, optional
        the background colour of the merged tile
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    cookies : dict, optional
        extra cookies for any requests calls
    debug : bool, optional
        print debug messages
    exiftoolPath : str, optional
        the path to the "exiftool" binary (if not provided then Python will attempt to
        find the binary itself)
    fill : tuple of int, optional
        the fill colour of the circle around the central location, if drawn
    gifsiclePath : str, optional
        the path to the "gifsicle" binary (if not provided then Python will attempt to
        find the binary itself)
    headers : dict, optional
        extra headers for any requests calls
    jpegtranPath : str, optional
        the path to the "jpegtran" binary (if not provided then Python will attempt to
        find the binary itself)
    optipngPath : str, optional
        the path to the "optipng" binary (if not provided then Python will attempt to
        find the binary itself)
    radius : int, optional
        the radius of the circle around the central location, if None then no
        circle is drawn (in pixels)
    timeout : float, optional
        the timeout for any requests/subprocess calls (in seconds)
    verify : bool, optional
        verify the server's certificates for any requests calls

    Returns
    -------
    tilesIm : PIL.Image
        the merged tile

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
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
        import PIL.ImageDraw
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import sub-functions ...
    from .deg2num import deg2num
    from .num2deg import num2deg
    from .tile import tile

    # Check inputs ...
    if not 0 <= zoom <= 19:
        raise Exception(f"\"zoom\" is not in the required range ({zoom:d})") from None

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Create short-hand ...
    n = pow(2, zoom)

    # Find which tile contains the location ...
    xtileC, ytileC = deg2num(lonC_deg, latC_deg, zoom)                          # [#], [#]

    # Find where exactly the location is within the central tile (assuming that
    # both the Euclidean and Geodesic spaces are both rectilinear and uniform
    # within a single tile) ...
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
                xtile % n,
                ytile % n,
                zoom,
                sess,
                   chunksize = chunksize,
                     cookies = cookies,
                       debug = debug,
                exiftoolPath = exiftoolPath,
                gifsiclePath = gifsiclePath,
                     headers = headers,
                jpegtranPath = jpegtranPath,
                 optipngPath = optipngPath,
                     timeout = timeout,
                      verify = verify,
            )

            # Check if the tile doesn't exist ...
            if tileIm is None:
                # Skip this tile ...
                continue

            # Paste the tile onto the map ...
            tilesIm.paste(tileIm, (x, y))

    # Draw circle, if the user wants to ...
    if radius is not None:
        draw.ellipse(
            [width // 2 - radius, height // 2 - radius, width // 2 + radius, height // 2 + radius],
            fill = fill,
        )

    # Return answer ...
    return tilesIm
