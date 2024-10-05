#!/usr/bin/env python3

# Define function ...
def add_map_background(
    ax,
    /,
    *,
         debug = __debug__,
        extent = None,
          name = "natural-earth-1",
    resolution = "medium0512px",
):
    """Add an image of a map as a background to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the background image to
    debug : bool, optional
        print debug statements
    extent : list of floats
        for high-resolution images, save time by specifying the extent that is
        to be added
    name : str, optional
        the name of the image in the database
    resolution : str, optional
        the resolution of the image in the database

    Notes
    -----
    If the specified image cannot be found then a default map will be used
    instead. See the `Cartopy documentation of background_img()
    <https://scitools.org.uk/cartopy/docs/latest/matplotlib/geoaxes.html#cartopy.mpl.geoaxes.GeoAxes.background_img>`_
    and the `Cartopy documentation of stock_img()
    <https://scitools.org.uk/cartopy/docs/latest/matplotlib/geoaxes.html#cartopy.mpl.geoaxes.GeoAxes.stock_img>`_ .

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

    # Draw background image ...
    if default:
        if debug:
            print("INFO: Drawing default background.")
        ax.stock_img()
    else:
        if debug:
            print("INFO: Drawing user-requested background.")
        ax.background_img(name = name, resolution = resolution, extent = extent)
