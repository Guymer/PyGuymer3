def add_map_background(axis, debug = False, name = "natural-earth-1", resolution = "medium0512px", extent = None):
    # Import standard modules ...
    import json
    import os

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

    # Draw background image ...
    if default:
        if debug:
            print("INFO: Drawing default background.")
        axis.stock_img()
    else:
        if debug:
            print("INFO: Drawing user-requested background.")
        axis.background_img(name = name, resolution = resolution, extent = extent)
