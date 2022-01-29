def add_map_underlay(axis, kwArgCheck = None, debug = False, resolution = "10m"):
    # Import sub-functions ...
    from ._add_bathymetry import _add_bathymetry
    from ._add_land import _add_land
    from ._add_minorIslands import _add_minorIslands
    from ._add_reefs import _add_reefs

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Add datasets ...
    _add_bathymetry(axis, debug = debug, resolution = resolution)
    _add_land(axis, debug = debug, resolution = resolution)
    _add_minorIslands(axis, debug = debug, resolution = resolution)
    _add_reefs(axis, debug = debug, resolution = resolution)
