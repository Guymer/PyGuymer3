def tile(xtile, ytile, zoom, sess, kwArgCheck = None, cookies = None, debug = False, headers = None, timeout = 10.0, verify = True):
    # Import standard modules ...
    import os

    # Import special modules ...
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Import sub-functions ...
    from ..download_file import download_file
    from ..image.optimize_image import optimize_image

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

    # Deduce tile name ...
    png = os.path.expanduser(f"~/.local/share/openstreetmap/tiles/{zoom:d}/{xtile:d}/{ytile:d}.png")

    # **************************************************************************

    # Check if the tile is missing ...
    if not os.path.exists(png):
        # Deduce tile URL ...
        url = f"https://tile.openstreetmap.org/{zoom:d}/{xtile:d}/{ytile:d}.png"

        if debug:
            print(f"INFO: Downloading \"{url}\" to \"{png}\" ...")

        # Download tile ...
        download_file(
            sess,
            url,
            png,
            cookies = cookies,
            headers = headers,
            timeout = timeout,
             verify = verify,
        )

        # Optimize tile ...
        optimize_image(
            png,
            debug = debug,
            strip = True,
        )

    # **************************************************************************

    # Open image as RGB (even if it is paletted) ...
    with PIL.Image.open(png) as imageObj:
        image = imageObj.convert("RGB")

    # Return answer ...
    return image
