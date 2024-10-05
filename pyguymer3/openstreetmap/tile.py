#!/usr/bin/env python3

# Define function ...
def tile(
    xtile,
    ytile,
    zoom,
    sess,
    /,
    *,
    chunksize = 1048576,
      cookies = None,
        debug = __debug__,
      headers = None,
      timeout = 60.0,
       verify = True,
):
    """Fetch an OpenStreetMap tile

    This function reads in a tile number and then checks to see if the tile
    already exists in the local cache. It will load up the PNG image if it
    exists or download the OpenStreetMap tile (and save it as both a NPY array
    and a PNG image) if it is missing.

    Parameters
    ----------
    xtile : int
        the tile x location
    ytile : int
        the tile y location
    zoom : int
        the OpenStreetMap zoom level
    sess : requests.Session
        the session for any requests calls
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    cookies : dict, optional
        extra cookies for any requests calls
    debug : bool, optional
        print debug messages
    headers : dict, optional
        extra headers for any requests calls
    timeout : float, optional
        the timeout for any requests/subprocess calls (in seconds)
    verify : bool, optional
        verify the server's certificates for any requests calls

    Returns
    -------
    image : PIL.Image
        the OpenStreetMap tile

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os
    import time

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import sub-functions ...
    from ..download_file import download_file
    from ..image import optimize_image

    # Check inputs ...
    if not 0 <= xtile < pow(2, zoom):
        raise Exception(f"\"xtile\" is not in the required range ({xtile:d})") from None
    if not 0 <= ytile < pow(2, zoom):
        raise Exception(f"\"ytile\" is not in the required range ({ytile:d})") from None
    if not 0 <= zoom <= 19:
        raise Exception(f"\"zoom\" is not in the required range ({zoom:d})") from None

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Deduce tile names ...
    npy = os.path.expanduser(f"~/.local/share/cartopy_cache/OSM/{xtile:d}_{ytile:d}_{zoom:d}.npy")
    png = os.path.expanduser(f"~/.local/share/openstreetmap/tiles/{zoom:d}/{xtile:d}/{ytile:d}.png")

    # **************************************************************************

    # Check if the tile is missing ...
    if not os.path.exists(png):
        # Deduce tile URL ...
        url = f"https://tile.openstreetmap.org/{zoom:d}/{xtile:d}/{ytile:d}.png"

        if debug:
            print(f"INFO: Downloading \"{url}\" to \"{png}\" ...")

        # Download tile ...
        resp = download_file(
            sess,
            url,
            png,
            cookies = cookies,
            headers = headers,
            timeout = timeout,
             verify = verify,
        )

        # Check if the download failed ...
        if resp is False:
            # Return answer ...
            return None

        # Optimize tile ...
        optimize_image(
            png,
            chunksize = chunksize,
                debug = debug,
                 pool = None,
                strip = True,
              timeout = timeout,
        )

        # Sleep ...
        time.sleep(1.0)

    # **************************************************************************

    # Open image as RGB (even if it is paletted) ...
    with PIL.Image.open(png) as imageObj:
        image = imageObj.convert("RGB")

    # **************************************************************************

    # Check if the tile is missing ...
    if not os.path.exists(npy):
        if debug:
            print(f"INFO: Making \"{npy}\" from \"{png}\" ...")

        # Save tile ...
        numpy.save(npy, image, allow_pickle = False)

    # **************************************************************************

    # Return answer ...
    return image
