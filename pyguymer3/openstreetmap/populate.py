#!/usr/bin/env python3

# Define function ...
def populate(
    maxZoom,
    sess,
    /,
    *,
           chunksize = 1048576,
             cookies = None,
               debug = __debug__,
        exiftoolPath = None,
        gifsiclePath = None,
             headers = None,
        jpegtranPath = None,
      maxImagePixels = 1073741824,
         optipngPath = None,
               scale = 1,
               sleep = 1.0,
    thunderforestKey = None,
    thunderforestMap = "atlas",
             timeout = 60.0,
              verify = True,
):
    """Fetch OpenStreetMap tiles

    This function populates your local tile cache to allow you to generate tiled
    maps later on when you are offline. Be careful when specifying a large value
    of "maxZoom" as the number of tiles that it corresponds to increases
    exponentially.

    Parameters
    ----------
    maxZoom : int
        the maximum OpenStreetMap zoom level
    sess : requests.Session
        the session for any requests calls
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    cookies : dict, optional
        extra cookies for any requests calls
    debug : bool, optional
        print debug messages
    exiftoolPath : None or str, optional
        the path to the "exiftool" binary (if not provided then Python will
        attempt to find the binary itself)
    gifsiclePath : None or str, optional
        the path to the "gifsicle" binary (if not provided then Python will
        attempt to find the binary itself)
    headers : dict, optional
        extra headers for any requests calls
    jpegtranPath : None or str, optional
        the path to the "jpegtran" binary (if not provided then Python will
        attempt to find the binary itself)
    maxImagePixels : int, optional
        the maximum number of pixels in an image, to prevent decompression bombs
    optipngPath : None or str, optional
        the path to the "optipng" binary (if not provided then Python will
        attempt to find the binary itself)
    scale : int, optional
        the scale of the tile
    sleep : float, optional
        the sleep after any requests/subprocess calls (in seconds)
    thunderforestKey : str, optional
        your personal API key for the Thunderforest service (if provided then it
        is assumed that you want to use the Thunderforest service)
    thunderforestMap : str, optional
        the Thunderforest map style (see https://www.thunderforest.com/maps/)
    timeout : float, optional
        the timeout for any requests/subprocess calls (in seconds)
    verify : bool, optional
        verify the server's certificates for any requests calls

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import sub-functions ...
    from .tile import tile

    # **************************************************************************

    # Loop over zooms ...
    for zoom in range(0, maxZoom + 1):
        # Loop over x-tiles ...
        for xTile in range(0, pow(2, zoom)):
            # Loop over y-tiles ...
            for yTile in range(0, pow(2, zoom)):
                # Obtain the tile ...
                tileIm = tile(
                    xTile,
                    yTile,
                    zoom,
                    sess,
                           chunksize = chunksize,
                             cookies = cookies,
                               debug = debug,
                        exiftoolPath = exiftoolPath,
                        gifsiclePath = gifsiclePath,
                             headers = headers,
                        jpegtranPath = jpegtranPath,
                      maxImagePixels = maxImagePixels,
                         optipngPath = optipngPath,
                               scale = scale,
                               sleep = sleep,
                    thunderforestKey = thunderforestKey,
                    thunderforestMap = thunderforestMap,
                             timeout = timeout,
                              verify = verify,
                )

                # Check if the tile doesn't exist ...
                if tileIm is None:
                    # Skip this tile ...
                    print(f"WARNING: Failed to obtain the tile for x={xTile:d}, y={yTile:d}, scale={scale:d} and zoom={zoom:d}.")
