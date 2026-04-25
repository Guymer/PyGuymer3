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
           fov = None,
     globePath = None,
    globeScale = "32km",
     linestyle = "solid",
     linewidth = 0.5,
       maxElev = 1000,
         neRes = "10m",
     onlyValid = False,
      physical = True,
        repair = False,
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
    physical : bool, optional
        add physical datasets
    repair : bool, optional
        attempt to repair invalid Polygons

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] Natural Earth, https://www.naturalearthdata.com/
    .. [3] Global Land One-km Base Elevation, https://www.ngdc.noaa.gov/mgg/topo/globe.html
    """

    # Import standard modules ...
    import os

    # Import sub-functions ...
    from ._add_antarcticIceShelves import _add_antarcticIceShelves
    from ._add_background import _add_background
    from ._add_bathymetry import _add_bathymetry
    from ._add_GLOBE_elevation import _add_GLOBE_elevation
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

    # Find the path to the GeoJSON files derived from the GLOBE dataset ...
    if globePath is None:
        globePath = os.path.abspath(f"{os.path.dirname(__file__)}/../data/geojson/globe")
    if not os.path.exists(globePath):
        if debug:
            print(f"INFO: \"{globePath}\" does not exist.")
        return
    if debug:
        print(f"INFO: The GeoJSON files derived from the GLOBE dataset are in \"{globePath}\".")

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
