#!/usr/bin/env python3

# Define function ...
def add_map_underlay(axis, kwArgCheck = None, background = True, cultural = True, debug = False, linestyle = "solid", linewidth = 0.5, maxElev = 8850.0, onlyValid = False, physical = True, repair = False, resolution = "110m"):
    # Import sub-functions ...
    from ._add_antarcticIceShelves import _add_antarcticIceShelves
    from ._add_background import _add_background
    from ._add_bathymetry import _add_bathymetry
    from ._add_elevation import _add_elevation
    from ._add_glaciatedAreas import _add_glaciatedAreas
    from ._add_lakes import _add_lakes
    from ._add_land import _add_land
    from ._add_minorIslands import _add_minorIslands
    from ._add_playas import _add_playas
    from ._add_railroads import _add_railroads
    from ._add_reefs import _add_reefs
    from ._add_rivers import _add_rivers
    from ._add_roads import _add_roads
    from ._add_urbanAreas import _add_urbanAreas

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Add background ...
    if background:
        # Water ...
        _add_background(
            axis,
            debug = debug,
        )

    # Add physical Polygon datasets ...
    if physical:
        # Water ...
        _add_bathymetry(
            axis,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )

        # Water overlays ...
        _add_antarcticIceShelves(
            axis,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )
        _add_reefs(
            axis,
                 debug = debug,
             linestyle = linestyle,
             linewidth = linewidth,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )

        # Land ...
        _add_land(
            axis,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )
        _add_minorIslands(
            axis,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )
        _add_elevation(
            axis,
                 debug = debug,
               maxElev = maxElev,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )

        # Land overlays ...
        _add_glaciatedAreas(
            axis,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )
        _add_lakes(
            axis,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )
        _add_playas(
            axis,
                 debug = debug,
             linestyle = linestyle,
             linewidth = linewidth,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )

    # Add cultural Polygon datasets ...
    if cultural:
        # Land ...
        _add_urbanAreas(
            axis,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )

    # Add physical LineString datasets ...
    if physical:
        # Land overlays ...
        _add_rivers(
            axis,
                 debug = debug,
             linestyle = linestyle,
             linewidth = linewidth,
             onlyValid = onlyValid,
            resolution = resolution,
        )

    # Add cultural LineString datasets ...
    if cultural:
        # Land overlays ...
        _add_railroads(
            axis,
                 debug = debug,
             linestyle = linestyle,
             linewidth = linewidth,
             onlyValid = onlyValid,
            resolution = resolution,
        )
        _add_roads(
            axis,
                 debug = debug,
             linestyle = linestyle,
             linewidth = linewidth,
             onlyValid = onlyValid,
            resolution = resolution,
        )
