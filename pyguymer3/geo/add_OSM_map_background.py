#!/usr/bin/env python3

# Define function ...
def add_OSM_map_background(
    ax,
    midLat,
    res,
    /,
    *,
    debug = __debug__,
    scale = 1,
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
    scale : int, optional
        the scale of the tiles

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy_cache").expanduser(),
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
        print(f"INFO: The resolution is {0.001 * res:,.1f} km/px and the OpenStreetMap zoom is {z:d}.")

    # Add OpenStreetMap tiles ...
    # NOTE: It appears that the background image is drawn at only 72 dpi. If you
    #       zoom in then there are around 4 pixels in the red lines joining the
    #       points together for every pixel in the background image (300 / 72 ~=
    #       4). Try regenerating a map at 72 dpi and the background image will
    #       be the same resolution as the red lines joining the points together.
    # NOTE: Line 528 of "cartopy/lib/cartopy/mpl/geoaxes.py" is the "imshow()"
    #       call.
    # NOTE: Line 71 of "cartopy/lib/cartopy/io/img_tiles.py" is the
    #       "image_for_domain()" definition.
    # NOTE: Line 588 of "cartopy/lib/cartopy/io/img_tiles.py" is the
    #       "_merge_tiles()" definition. My only guess is that some pixels are
    #       not getting sliced correctly when making the merged image because
    #       you should never do floating-point equalities on line 608.
    osm = cartopy.io.img_tiles.OSM(
        cache = True,
    )
    ax.add_image(
        osm,
        z,
        interpolation = "none",         # NOTE: Due to the use of **kwargs
                                        #       within Cartopy, this is passed
                                        #       all the way down the stack to
                                        #       the ".imshow()" call.
    )
