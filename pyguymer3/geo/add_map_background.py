def add_map_background(axis, kwArgCheck = None, debug = False, name = "natural-earth-1", resolution = "medium0512px", extent = None):
    """Add an image of a map as a background to a plot.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the background image to
    debug : bool, optional
        print debug statements
    name : str, optional
        the name of the image in the database
    resolution : str, optional
        the resolution of the image in the database
    extent : list of floats
        for high-resolution images, save time by specifying the extent that is
        to be added

    Notes
    -----
    If the specified image cannot be found then a default map will be used
    instead. See the `Cartopy documentation of background_img() <https://scitools.org.uk/cartopy/docs/latest/matplotlib/geoaxes.html#cartopy.mpl.geoaxes.GeoAxes.background_img>`_
    and the `Cartopy documentation of stock_img() <https://scitools.org.uk/cartopy/docs/latest/matplotlib/geoaxes.html#cartopy.mpl.geoaxes.GeoAxes.stock_img>`_.
    """

    # Import standard modules ...
    import json
    import os

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize trigger ...
    default = True

    # Check if the environment variable has been defined ...
    if "CARTOPY_USER_BACKGROUNDS" in os.environ:
        # Determine JSON path and check it exists ...
        jpath = os.path.join(os.environ["CARTOPY_USER_BACKGROUNDS"], "images.json")
        if os.path.exists(jpath):
            # Load JSON and check keys exist ...
            info = json.load(open(jpath, "rt"))
            if name in info:
                if resolution in info[name]:
                    # Determine image path and check it exists ...
                    ipath = os.path.join(os.environ["CARTOPY_USER_BACKGROUNDS"], info[name][resolution])
                    if os.path.exists(ipath):
                        default = False

            # Clean up ...
            del info

    # Draw background image ...
    if default:
        if debug:
            print("INFO: Drawing default background.")
        axis.stock_img()
    else:
        if debug:
            print("INFO: Drawing user-requested background.")
        axis.background_img(name = name, resolution = resolution, extent = extent)
