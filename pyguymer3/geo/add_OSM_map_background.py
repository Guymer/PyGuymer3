#!/usr/bin/env python3

# Define function ...
def add_OSM_map_background(
    ax,
    midLat,
    res,
    /,
    *,
               debug = __debug__,
       interpolation = None,
        regrid_shape = 750,
            resample = None,
               scale = 1,
    thunderforestKey = None,
    thunderforestMap = "atlas",
):
    """Add OpenStreetMap map tiles as a background to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the OpenStreetMap map tiles as a background to
    midLat : float
        the latitude of the middle of the figure (in degrees)
    res : float
        the resolution of the figure (in m/px)
    debug : bool, optional
        print debug statements
    interpolation : str, optional
        The interpolation method used when drawing the final merged and warped
        image on the figure. Due to the use of **kwargs within Cartopy, this is
        passed all the way down the stack to the MatPlotLib ".imshow()" call.
    regrid_shape: int, optional
        The smallest dimension of the merged image of all of the tiles **after**
        it has been warped by Cartopy to be the same projection as the figure
        (in pixels). Due to the use of **kwargs within Cartopy, this is passed
        all the way down the stack to the Cartopy ".imshow()" call. The size of
        a figure, in inches, can be found by calling "fg.get_size_inches()". The
        resolution of a figure can be found by interrogating "fg.dpi". The size
        of a figure, in pixels, can be found by multiplying the tuple by the
        scalar.
    resample : bool, optional
        Use a full resampling method when drawing the final merged and warped
        image on the figure. Due to the use of **kwargs within Cartopy, this is
        passed all the way down the stack to the MatPlotLib ".imshow()" call.
    scale : int, optional
        the scale of the tiles
    thunderforestKey : str, optional
        your personal API key for the Thunderforest service (if provided then it
        is assumed that you want to use the Thunderforest service)
    thunderforestMap : str, optional
        the Thunderforest map style (see https://www.thunderforest.com/maps/)

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import pathlib

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy").expanduser(),
            }
        )
        import cartopy.io.img_tiles
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None

    # Import sub-functions ...
    from ..openstreetmap import zoom

    # **************************************************************************

    # Calculate the zoom depending on the central latitude and the resolution of
    # the figure ...
    z = zoom(midLat, res, scale = scale)
    if debug:
        print(f"INFO: The resolution is {res:,.1f} m/px and the OpenStreetMap zoom is {z:d}.")

    # Add OpenStreetMap tiles ...
    if thunderforestKey is not None:
        osm = cartopy.io.img_tiles.ThunderforestTiles(
                apikey = thunderforestKey,
                 cache = True,
            resolution = "" if scale == 1 else f"@{scale:d}x",
                 style = thunderforestMap,
        )
    else:
        if scale != 1 and debug:
            print(f'WARNING: \"scale\" equals \"{scale:d}\" - OpenStreetMap only supports a \"scale\" of \"1\".')
        osm = cartopy.io.img_tiles.OSM(
                 cache = True,
            resolution = "",
                 style = None,
        )
    ax.add_image(
        osm,
        z,
        interpolation = interpolation,
         regrid_shape = regrid_shape,
             resample = resample,
    )
