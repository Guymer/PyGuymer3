#!/usr/bin/env python3

# Define function ...
def add_map_background(
    ax,
    /,
    *,
            debug = __debug__,
           extent = None,
    interpolation = "none",
             name = "natural-earth-1",
     regrid_shape = 750,
         resample = False,
       resolution = "medium0512px",
):
    """Add an image of a map as a background to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the background image to
    debug : bool, optional
        print debug messages
    extent : list of floats
        for high-resolution images, save time by specifying the extent that is
        to be added
    interpolation : str, optional
        The interpolation method used when drawing the final warped image on the
        figure. Due to the use of **kwargs within Cartopy, this is passed all
        the way down the stack to the MatPlotLib ".imshow()" call.
    name : str, optional
        the name of the image in the database
    regrid_shape: int or tuple of int, optional
        The smallest dimension of the image **after** it has been warped by
        Cartopy to be the same projection as the figure (in pixels). Due to the
        use of **kwargs within Cartopy, this is passed all the way down the
        stack to the Cartopy ".imshow()" call. The size of a figure, in inches,
        can be found by calling "fg.get_size_inches()". The resolution of a
        figure can be found by calling "fg.get_dpi()". The size of a figure, in
        pixels, can be found by multiplying the tuple by the scalar.
    resample : bool, optional
        Use a full resampling method when drawing the final warped image on the
        figure. Due to the use of **kwargs within Cartopy, this is passed all
        the way down the stack to the MatPlotLib ".imshow()" call.
    resolution : str, optional
        the resolution of the image in the database

    Notes
    -----
    If the specified image cannot be found then a default map will be used
    instead. See the `Cartopy documentation of background_img()
    <https://scitools.org.uk/cartopy/docs/latest/matplotlib/geoaxes.html#cartopy.mpl.geoaxes.GeoAxes.background_img>`_
    and the `Cartopy documentation of stock_img()
    <https://scitools.org.uk/cartopy/docs/latest/matplotlib/geoaxes.html#cartopy.mpl.geoaxes.GeoAxes.stock_img>`_ .

    See the `MatPlotLib documentation about interpolation methods
    <https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html>`_
    and the `MatPlotLib documentation about anti-aliasing
    <https://matplotlib.org/stable/gallery/images_contours_and_fields/image_antialiasing.html>`_.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import json
    import os

    # **************************************************************************

    # Initialize trigger ...
    default = True

    # Check if the environment variable has been defined ...
    if "CARTOPY_USER_BACKGROUNDS" in os.environ:
        # Determine JSON path and check it exists ...
        jpath = f'{os.environ["CARTOPY_USER_BACKGROUNDS"]}/images.json'
        if os.path.exists(jpath):
            # Load JSON and check keys exist ...
            with open(jpath, "rt", encoding = "utf-8") as fObj:
                info = json.load(fObj)
            if name in info:
                if resolution in info[name]:
                    # Determine image path and check it exists ...
                    ipath = f'{os.environ["CARTOPY_USER_BACKGROUNDS"]}/{info[name][resolution]}'
                    if os.path.exists(ipath):
                        default = False

    # Draw default background image ...
    if default:
        if debug:
            print("INFO: Drawing default background.")
        ax.stock_img(
            interpolation = interpolation,
             regrid_shape = regrid_shape,
                 resample = resample,
        )
        return

    # Draw user-requested background image ...
    if debug:
        print("INFO: Drawing user-requested background.")
    ax.background_img(
               extent = extent,
        interpolation = interpolation,
                 name = name,
         regrid_shape = regrid_shape,
             resample = resample,
           resolution = resolution,
    )
