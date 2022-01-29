def add_map_underlay(axis, kwArgCheck = None, debug = False, resolution = "10m"):
    # Import sub-functions ...
    from ._add_bathymetry import _add_bathymetry
    from ._add_land import _add_land

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Add datasets ...
    _add_bathymetry(axis, debug = debug, resolution = resolution)
    _add_land(axis, debug = debug, resolution = resolution)
