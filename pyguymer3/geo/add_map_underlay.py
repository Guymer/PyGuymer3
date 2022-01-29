def add_map_underlay(axis, kwArgCheck = None, cultural = True, debug = False, physical = True, resolution = "10m"):
    # TODO: Cultural datasets:
    #         * https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
    # TODO: Physical datasets:
    #         * https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-rivers-lake-centerlines/
    #         * https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-antarctic-ice-shelves/

    # Import sub-functions ...
    from ._add_bathymetry import _add_bathymetry
    from ._add_glaciatedAreas import _add_glaciatedAreas
    from ._add_lakes import _add_lakes
    from ._add_land import _add_land
    from ._add_minorIslands import _add_minorIslands
    from ._add_playas import _add_playas
    from ._add_reefs import _add_reefs

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Add physical datasets ...
    if physical:
        # Water ...
        _add_bathymetry(axis, debug = debug, resolution = resolution)

        # Water overlays ...
        _add_reefs(axis, debug = debug, resolution = resolution)

        # Land ...
        _add_land(axis, debug = debug, resolution = resolution)
        _add_minorIslands(axis, debug = debug, resolution = resolution)

        # Land overlays ...
        _add_glaciatedAreas(axis, debug = debug, resolution = resolution)
        _add_lakes(axis, debug = debug, resolution = resolution)
        _add_playas(axis, debug = debug, resolution = resolution)

    # Add cultural datasets ...
    if cultural:
        True
