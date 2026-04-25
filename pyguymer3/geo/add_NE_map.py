#!/usr/bin/env python3

# Define function ...
def add_NE_map(
    ax,
    /,
    *,
        background = True,
          cultural = True,
             debug = __debug__,
           elevInt = 250,
        elevSource = "GLOBE",
               fov = None,
         globePath = None,
        globeScale = "32km",
         linestyle = "solid",
         linewidth = 0.5,
           maxElev = 1000,
             neRes = "10m",
         onlyValid = False,
     osTerrainPath = None,
    osTerrainScale = "400m",
          physical = True,
            prefix = ".",
            repair = False,
               tol = 1.0e-10,
):
    """Add an underlay to a Cartopy axis from Natural Earth.

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxes
        the axis to add the underlay to
    background : bool, optional
        add background
    cultural : bool, optional
        add cultural datasets
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
    linestyle : str, optional
        the style of the lines
    linewidth : float, optional
        the width of the lines
    maxElev : int, optional
        the maximum elevation of the colour scale which acts as an upper bound
        or clip (in metres)
    neRes : str, optional
        The resolution of the Natural Earth [2]_ datasets.
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    osTerrainPath : None str, optional
        the path to the root folder containing the GeoJSON files derived from
        the OS Terrain 50 [4]_ dataset
    osTerrainScale : str, optional
        the scale of the Polygons of elevation from the OS Terrain 50 [4]_
        dataset
    physical : bool, optional
        add physical datasets
    prefix : str, optional
        change the name of the output debugging CSVs
    repair : bool, optional
        attempt to repair invalid Polygons
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] Natural Earth, https://www.naturalearthdata.com/
    .. [3] Global Land One-km Base Elevation, https://www.ngdc.noaa.gov/mgg/topo/globe.html
    .. [4] OS Terrain 50, https://www.ordnancesurvey.co.uk/products/os-terrain-50
    """

    # Import standard modules ...
    import os

    # Import sub-functions ...
    from ._add_antarcticIceShelves import _add_antarcticIceShelves
    from ._add_background import _add_background
    from ._add_bathymetry import _add_bathymetry
    from ._add_glaciatedAreas import _add_glaciatedAreas
    from ._add_GLOBE_elevation import _add_GLOBE_elevation
    from ._add_lakes import _add_lakes
    from ._add_land import _add_land
    from ._add_minorIslands import _add_minorIslands
    from ._add_OSterrain_elevation import _add_OSterrain_elevation
    from ._add_playas import _add_playas
    from ._add_railroads import _add_railroads
    from ._add_reefs import _add_reefs
    from ._add_rivers import _add_rivers
    from ._add_roads import _add_roads
    from ._add_urbanAreas import _add_urbanAreas

    # **************************************************************************

    # Find the path to the GeoJSON files derived from the GLOBE dataset ...
    if globePath is None:
        globePath = os.path.abspath(f"{os.path.dirname(__file__)}/../data/geojson/globe")
    if not os.path.exists(globePath):
        if debug:
            print(f"INFO: \"{globePath}\" does not exist.")
        return
    if debug:
        print(f"INFO: The GeoJSON files derived from the GLOBE dataset are in \"{globePath}\".")

    # Find the path to the GeoJSON files derived from the OS Terrain 50 dataset ...
    if osTerrainPath is None:
        osTerrainPath = os.path.abspath(f"{os.path.dirname(__file__)}/../data/geojson/osTerrain")
    if not os.path.exists(osTerrainPath):
        if debug:
            print(f"INFO: \"{osTerrainPath}\" does not exist.")
        return
    if debug:
        print(f"INFO: The GeoJSON files derived from the OS Terrain 50 dataset are in \"{osTerrainPath}\".")

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
                  fov = fov,
                neRes = neRes,
            onlyValid = onlyValid,
               repair = repair,
        )

        # Water overlays ...
        _add_antarcticIceShelves(
            ax,
                debug = debug,
                  fov = fov,
                neRes = neRes,
            onlyValid = onlyValid,
               repair = repair,
        )
        _add_reefs(
            ax,
                debug = debug,
                  fov = fov,
            linestyle = linestyle,
            linewidth = linewidth,
                neRes = neRes,
            onlyValid = onlyValid,
               repair = repair,
        )

        # Land ...
        _add_land(
            ax,
                debug = debug,
                  fov = fov,
                neRes = neRes,
            onlyValid = onlyValid,
               repair = repair,
        )
        _add_minorIslands(
            ax,
                debug = debug,
                  fov = fov,
                neRes = neRes,
            onlyValid = onlyValid,
               repair = repair,
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

        # Land overlays ...
        _add_glaciatedAreas(
            ax,
                debug = debug,
                  fov = fov,
                neRes = neRes,
            onlyValid = onlyValid,
               repair = repair,
        )
        _add_lakes(
            ax,
                debug = debug,
                  fov = fov,
                neRes = neRes,
            onlyValid = onlyValid,
               repair = repair,
        )
        _add_playas(
            ax,
                debug = debug,
                  fov = fov,
            linestyle = linestyle,
            linewidth = linewidth,
                neRes = neRes,
            onlyValid = onlyValid,
               repair = repair,
        )

    # Add cultural Polygon datasets ...
    if cultural:
        # Land ...
        _add_urbanAreas(
            ax,
                debug = debug,
                  fov = fov,
                neRes = neRes,
            onlyValid = onlyValid,
               repair = repair,
        )

    # Add physical LineString datasets ...
    if physical:
        # Land overlays ...
        _add_rivers(
            ax,
                debug = debug,
                  fov = fov,
            linestyle = linestyle,
            linewidth = linewidth,
                neRes = neRes,
            onlyValid = onlyValid,
        )

    # Add cultural LineString datasets ...
    if cultural:
        # Land overlays ...
        _add_railroads(
            ax,
                debug = debug,
                  fov = fov,
            linestyle = linestyle,
            linewidth = linewidth,
                neRes = neRes,
            onlyValid = onlyValid,
        )
        _add_roads(
            ax,
                debug = debug,
                  fov = fov,
            linestyle = linestyle,
            linewidth = linewidth,
                neRes = neRes,
            onlyValid = onlyValid,
        )
