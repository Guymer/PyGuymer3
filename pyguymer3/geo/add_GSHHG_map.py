#!/usr/bin/env python3

# Define function ...
def add_GSHHG_map(
    ax,
    /,
    *,
        background = True,
             debug = __debug__,
           elevInt = 250,
        elevSource = "GLOBE",
               fov = None,
         globePath = None,
        globeScale = "32km",
          gshhgRes = "i",
          iceOcean = True,
        islandLake = True,
          lakeLand = True,
         landOcean = True,
         linewidth = 0.5,
           maxElev = 1000,
         onlyValid = False,
     osTerrainPath = None,
    osTerrainScale = "400m",
        pondIsland = True,
            prefix = ".",
            repair = False,
               tol = 1.0e-10,
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
    elevInt : int, optional
        the interval of the elevation bands to shade (in metres)
    elevSource : None or str, optional
        the source of the Polygons of elevation
    fov : None or shapely.geometry.polygon.Polygon, optional
        clip the plotted shapes to the provided field-of-view to work around
        occasional MatPlotLib or Cartopy plotting errors when shapes much larger
        than the field-of-view are plotted
    globePath : None str, optional
        the path to the root folder containing the GeoJSON files derived from
        the GLOBE [3]_ dataset
    globeScale : str, optional
        the scale of the Polygons of elevation from the GLOBE [3]_ dataset
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
    maxElev : int, optional
        the maximum elevation of the colour scale which acts as an upper bound
        or clip (in metres)
    onlyValid : bool, optional
        only return valid Polygons (checks for validity can take a while, if
        being called often)
    osTerrainPath : None str, optional
        the path to the root folder containing the GeoJSON files derived from
        the OS Terrain 50 [4]_ dataset
    osTerrainScale : str, optional
        the scale of the Polygons of elevation from the OS Terrain 50 [4]_
        dataset
    pondIsland : bool, optional
        add pond-island boundaries
    prefix : str, optional
        change the name of the output debugging CSVs
    repair : bool, optional
        attempt to repair invalid Polygons
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

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
    .. [3] Global Land One-km Base Elevation, https://www.ngdc.noaa.gov/mgg/topo/globe.html
    .. [4] OS Terrain 50, https://www.ordnancesurvey.co.uk/products/os-terrain-50
    """

    # Import sub-functions ...
    from ._add_background import _add_background
    from ._add_coastlines import _add_coastlines
    from ._add_GLOBE_elevation import _add_GLOBE_elevation
    from ._add_OSterrain_elevation import _add_OSterrain_elevation

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
        match elevSource:
            case None | "none":
                pass
            case "GLOBE":
                _add_GLOBE_elevation(
                    ax,
                         debug = debug,
                       elevInt = elevInt,
                           fov = fov,
                     globePath = globePath,
                    globeScale = globeScale,
                       maxElev = maxElev,
                     onlyValid = onlyValid,
                        repair = repair,
                )
            case "OS Terrain 50":
                _add_OSterrain_elevation(
                    ax,
                             debug = debug,
                           elevInt = elevInt,
                               fov = fov,
                           maxElev = maxElev,
                         onlyValid = onlyValid,
                     osTerrainPath = osTerrainPath,
                    osTerrainScale = osTerrainScale,
                            prefix = prefix,
                            repair = repair,
                               tol = tol,
                )
            case _:
                raise Exception(f"\"{elevSource}\" is an unrecognized elevation source") from None

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
