def add_map_underlay(axis, kwArgCheck = None, cultural = True, debug = False, physical = True, resolution = "10m"):
    # TODO: Cultural datasets:
    #         * https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
    # TODO: Physical datasets:
    #         * https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-rivers-lake-centerlines/
    #         * https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-lakes/
    #         * https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-playas/
    #         * https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-antarctic-ice-shelves/
    #         * https://www.naturalearthdata.com/downloads/10m-physical-vectors/10m-glaciated-areas/

    # Import sub-functions ...
    from ._add_bathymetry import _add_bathymetry
    from ._add_land import _add_land
    from ._add_minorIslands import _add_minorIslands
    from ._add_reefs import _add_reefs

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Add physical datasets ...
    if physical:
        _add_bathymetry(axis, debug = debug, resolution = resolution)
        _add_land(axis, debug = debug, resolution = resolution)
        _add_minorIslands(axis, debug = debug, resolution = resolution)
        _add_reefs(axis, debug = debug, resolution = resolution)

    # Add cultural datasets ...
    if cultural:
        True
