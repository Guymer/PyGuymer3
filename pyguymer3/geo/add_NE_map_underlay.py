#!/usr/bin/env python3

# Define function ...
def add_NE_map_underlay(
    ax,
    /,
    *,
    background = True,
      cultural = True,
         debug = __debug__,
     linestyle = "solid",
     linewidth = 0.5,
       maxElev = 8850.0,
     onlyValid = False,
      physical = True,
        repair = False,
    resolution = "10m",
):
    """Add an underlay to a Cartopy axis from Natural Earth.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the underlay to
    background : bool, optional
        add background
    cultural : bool, optional
        add cultural datasets
    debug : bool, optional
        print debug messages
    linestyle : str, optional
        the style of the lines
    linewidth : float, optional
        the width of the lines
    maxElev : float, optional
        the maximum elevation of the colour scale and acts as an upper bound or
        clip (in metres)
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    physical : bool, optional
        add physical datasets
    repair : bool, optional
        attempt to repair invalid Polygons
    resolution : str, optional
        the resolution of the reefs

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

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

    # **************************************************************************

    # Add background ...
    if background:
        # Water ...
        _add_background(
            ax,
            debug = debug,
        )

    # Add physical Polygon datasets ...
    if physical:
        # Water ...
        _add_bathymetry(
            ax,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )

        # Water overlays ...
        _add_antarcticIceShelves(
            ax,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )
        _add_reefs(
            ax,
                 debug = debug,
             linestyle = linestyle,
             linewidth = linewidth,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )

        # Land ...
        _add_land(
            ax,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )
        _add_minorIslands(
            ax,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )
        _add_elevation(
            ax,
                 debug = debug,
               maxElev = maxElev,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )

        # Land overlays ...
        _add_glaciatedAreas(
            ax,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )
        _add_lakes(
            ax,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )
        _add_playas(
            ax,
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
            ax,
                 debug = debug,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
        )

    # Add physical LineString datasets ...
    if physical:
        # Land overlays ...
        _add_rivers(
            ax,
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
            ax,
                 debug = debug,
             linestyle = linestyle,
             linewidth = linewidth,
             onlyValid = onlyValid,
            resolution = resolution,
        )
        _add_roads(
            ax,
                 debug = debug,
             linestyle = linestyle,
             linewidth = linewidth,
             onlyValid = onlyValid,
            resolution = resolution,
        )
