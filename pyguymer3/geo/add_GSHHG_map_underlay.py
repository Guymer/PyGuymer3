#!/usr/bin/env python3

# Define function ...
def add_GSHHG_map_underlay(
    ax,
    /,
    *,
    background = True,
         debug = False,
      iceOcean = True,
    islandLake = True,
      lakeLand = True,
     landOcean = True,
     linewidth = 0.5,
     onlyValid = False,
    pondIsland = True,
        repair = False,
    resolution = "i",
):
    """Add an underlay to a Cartopy axis from the `Global Self-Consistent
    Hierarchical High-Resolution Geography dataset`

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis
    background : bool, optional
        add background
    debug : bool, optional
        print debug messages
    iceOcean : bool, optional
        add ice-ocean boundaries
    islandLake : bool, optional
        add island-lake boundaries
    lakeLand : bool, optional
        add lake-land boundaries
    landOcean : bool, optional
        add land-ocean boundaries
    linewidth : float, optional
        the linewidth to draw the boundaries with
    onlyValid : bool, optional
        only return valid Polygons (checks for validity can take a while, if
        being called often)
    pondIsland : bool, optional
        add pond-island boundaries
    repair : bool, optional
        attempt to repair invalid Polygons
    resolution : str, optional
        the resolution of the boundaries

    Notes
    -----
    There is one argument relating to the `Global Self-Consistent Hierarchical
    High-Resolution Geography dataset <https://www.ngdc.noaa.gov/mgg/shorelines/>`_ :

    * *resolution*.

    There are five resolutions to choose from:

    * crude ("c");
    * low ("l");
    * intermediate ("i");
    * high ("h"); and
    * full ("f").

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import sub-functions ...
    from ._add_background import _add_background
    from ._add_coastlines import _add_coastlines

    # **************************************************************************

    # Add background ...
    if background:
        # Water ...
        _add_background(
            ax,
            debug = debug,
        )

    # Add ice-ocean boundaries ...
    if iceOcean:
        # Ice ...
        _add_coastlines(
            ax,
                 debug = debug,
             edgecolor = "blue",
             facecolor = "aliceblue",
                levels = [5],
             linestyle = "solid",
             linewidth = linewidth,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
                zorder = 1.5,
        )

    # Add land-ocean boundaries ...
    if landOcean:
        # Land ...
        _add_coastlines(
            ax,
                 debug = debug,
             edgecolor = "green",
             facecolor = "darkkhaki",
                levels = [1],
             linestyle = "solid",
             linewidth = linewidth,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
                zorder = 1.6,
        )

        # Snow ...
        _add_coastlines(
            ax,
                 debug = debug,
             edgecolor = "white",
             facecolor = "snow",
                levels = [6],
             linestyle = "solid",
             linewidth = linewidth,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
                zorder = 1.6,
        )

    # Add lake-land boundaries ...
    if lakeLand:
        # Lake ...
        _add_coastlines(
            ax,
                 debug = debug,
             edgecolor = "none",
             facecolor = "lightblue",
                levels = [2],
             linestyle = "solid",
             linewidth = linewidth,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
                zorder = 1.7,
        )

    # Add island-lake boundaries ...
    if islandLake:
        # Island ...
        _add_coastlines(
            ax,
                 debug = debug,
             edgecolor = "green",
             facecolor = "darkkhaki",
                levels = [3],
             linestyle = "solid",
             linewidth = linewidth,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
                zorder = 1.8,
        )

    # Add pond-island boundaries ...
    if pondIsland:
        # Pond ...
        _add_coastlines(
            ax,
                 debug = debug,
             edgecolor = "none",
             facecolor = "lightblue",
                levels = [4],
             linestyle = "solid",
             linewidth = linewidth,
             onlyValid = onlyValid,
                repair = repair,
            resolution = resolution,
                zorder = 1.9,
        )
