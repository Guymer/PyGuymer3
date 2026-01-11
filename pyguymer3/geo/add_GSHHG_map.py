#!/usr/bin/env python3

# Define function ...
def add_GSHHG_map(
    ax,
    /,
    *,
    background = True,
         debug = __debug__,
           fov = None,
      gshhgRes = "i",
      iceOcean = True,
    islandLake = True,
      lakeLand = True,
     landOcean = True,
     linewidth = 0.5,
     onlyValid = False,
    pondIsland = True,
        repair = False,
):
    """Add an underlay to a Cartopy axis from the `Global Self-Consistent
    Hierarchical High-Resolution Geography dataset`

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxes
        the axis
    background : bool, optional
        add background
    debug : bool, optional
        print debug messages
    fov : None or shapely.geometry.polygon.Polygon, optional
        clip the plotted shapes to the provided field-of-view to work around
        occaisional MatPlotLib or Cartopy plotting errors when shapes much
        larger than the field-of-view are plotted
    gshhgRes : str, optional
        the resolution of the coastline boundaries from GSHHG [2]_
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

    Notes
    -----
    There is one argument relating to the `Global Self-Consistent Hierarchical
    High-Resolution Geography dataset <https://www.ngdc.noaa.gov/mgg/shorelines/>`_ :

    * *gshhgRes*.

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
    .. [2] Global Self-consistent Hierarchical High-resolution Geography, https://www.ngdc.noaa.gov/mgg/shorelines/
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
                    fov = fov,
            gshhgLevels = (5,),
               gshhgRes = gshhgRes,
              linestyle = "solid",
              linewidth = linewidth,
              onlyValid = onlyValid,
                 repair = repair,
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
                    fov = fov,
            gshhgLevels = (1,),
               gshhgRes = gshhgRes,
              linestyle = "solid",
              linewidth = linewidth,
              onlyValid = onlyValid,
                 repair = repair,
                 zorder = 1.6,
        )

        # Snow ...
        _add_coastlines(
            ax,
                  debug = debug,
              edgecolor = "white",
              facecolor = "snow",
                    fov = fov,
            gshhgLevels = (6,),
               gshhgRes = gshhgRes,
              linestyle = "solid",
              linewidth = linewidth,
              onlyValid = onlyValid,
                 repair = repair,
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
                    fov = fov,
            gshhgLevels = (2,),
               gshhgRes = gshhgRes,
              linestyle = "solid",
              linewidth = linewidth,
              onlyValid = onlyValid,
                 repair = repair,
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
                    fov = fov,
            gshhgLevels = (3,),
               gshhgRes = gshhgRes,
              linestyle = "solid",
              linewidth = linewidth,
              onlyValid = onlyValid,
                 repair = repair,
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
                    fov = fov,
            gshhgLevels = (4,),
               gshhgRes = gshhgRes,
              linestyle = "solid",
              linewidth = linewidth,
              onlyValid = onlyValid,
                 repair = repair,
                 zorder = 1.9,
        )
