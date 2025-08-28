#!/usr/bin/env python3

# Define function ...
def add_OSM_map_background(
    ax,
    midLat,
    res,
    /,
    *,
            debug = __debug__,
    interpolation = "auto",
     regrid_shape = 750,
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
    interpolation : str, optional
        The interpolation method used when drawing the final merged and warped
        image on the figure.
    regrid_shape: int, optional
        The smallest dimension of the merged image of all of the tiles **after**
        it has been warped by Cartopy to be the same projection as the figure
        (in pixels)
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
    import pathlib

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
        print(f"INFO: The resolution is {res:,.1f} m/px and the OpenStreetMap zoom is {z:d}.")

    # Add OpenStreetMap tiles ...
    # NOTE: By default, the background images appear to be drawn at a lower
    #       resolution than expected. For a long time, I believed that the
    #       images were drawn at 72 dpi and that either Cartopy or MatPlotLib
    #       were not dpi-aware. However, after cloning the Cartopy repository
    #       and adding some debugging statements (such as saving temporary
    #       arrays as images so that they can be viewed externally) I have now
    #       come to realise that the culprit is that, by default, Cartopy
    #       rescales all images such that their smallest dimension is 750
    #       pixels.
    # NOTE: When using a "cartopy.io.img_tiles()" class, two things happen:
    #         * all the required tiles are merged together to make a single
    #           image; and
    #         * the single image is warped to transform it from its own
    #           projection to the figure's projection.
    #       This all takes place in "lib/cartopy/mpl/geoaxes.py":
    #         * The definition of the "draw()" method loops over all image
    #           factories and uses the "image_for_domain()" method to make a
    #           single image of all of the tiles merged together before it calls
    #           the "imshow()" method.
    #         * However, the "imshow()" method that it calls is Cartopy's own
    #           method rather than MatPlotLib's method.
    #         * If the projection of the merged image is not the same as the
    #           figure's projection then the "warp_array()" function is used to
    #           transform the merged image to the figure's projection. No
    #           "*args" or "**kwargs" are passed to the "warp_array()" function
    #           call, so the user cannot alter how this is done **except** via
    #           the "regrid_shape" keyword argument which, by default, is 750.
    #         * This means that, even if the user provides a gigapixel image, it
    #           will be **downscaled** such that it's shortest dimension is only
    #           750 pixels.
    # NOTE: The size of a figure, in inches, can be found by calling
    #       "fg.get_size_inches()". The resolution of a figure can be found by
    #       interrogating "fg.dpi". The size of a figure, in pixels, can be
    #       found by multiplying the tuple by the scalar.
    osm = cartopy.io.img_tiles.OSM(
        cache = True,
    )
    ax.add_image(
        osm,
        z,
        interpolation = interpolation,  # NOTE: Due to the use of **kwargs
                                        #       within Cartopy, this is passed
                                        #       all the way down the stack to
                                        #       the MatPlotLib ".imshow()" call.
         regrid_shape = regrid_shape,
    )
