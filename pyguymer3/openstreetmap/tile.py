#!/usr/bin/env python3

# Define function ...
def tile(xtile, ytile, zoom, sess, /, *, chunksize = 1048576, cookies = None, debug = False, headers = None, timeout = 10.0, verify = True):
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
    if not 0 <= zoom <= 19:
        raise Exception(f"zoom is not in the required range ({zoom:d})") from None

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
                strip = True,
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
