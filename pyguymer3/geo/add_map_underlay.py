def add_map_underlay(axis, kwArgCheck = None, background = True, cultural = True, debug = False, linestyle = "solid", linewidth = 0.5, physical = True, resolution = "110m"):
    # Import sub-functions ...
    from ._add_antarcticIceShelves import _add_antarcticIceShelves
    from ._add_background import _add_background
    from ._add_bathymetry import _add_bathymetry
    from ._add_glaciatedAreas import _add_glaciatedAreas
    from ._add_lakes import _add_lakes
    from ._add_land import _add_land
    from ._add_minorIslands import _add_minorIslands
    from ._add_playas import _add_playas
    from ._add_reefs import _add_reefs
    from ._add_rivers import _add_rivers

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Add background ...
    if background:
        # Background ...
        _add_background(axis, debug = debug)

    # Add physical datasets ...
    if physical:
        # Water ...
        _add_bathymetry(axis, debug = debug, resolution = resolution)

        # Water overlays ...
        _add_antarcticIceShelves(axis, debug = debug, resolution = resolution)
        _add_reefs(axis, debug = debug, linestyle = linestyle, linewidth = linewidth, resolution = resolution)

        # Land ...
        _add_land(axis, debug = debug, resolution = resolution)
        _add_minorIslands(axis, debug = debug, resolution = resolution)

        # Land overlays ...
        _add_glaciatedAreas(axis, debug = debug, resolution = resolution)
        _add_lakes(axis, debug = debug, resolution = resolution)
        _add_playas(axis, debug = debug, linestyle = linestyle, linewidth = linewidth, resolution = resolution)
        _add_rivers(axis, debug = debug, linestyle = linestyle, linewidth = linewidth, resolution = resolution)

    # Add cultural datasets ...
    if cultural:
        # TODO: https://www.naturalearthdata.com/downloads/10m-cultural-vectors/
        True
