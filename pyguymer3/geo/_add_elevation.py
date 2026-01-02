#!/usr/bin/env python3

# Define function ...
def _add_elevation(
    ax,
    /,
    *,
         debug = __debug__,
       elevInt = 250,
           fov = None,
       maxElev = 8000,
     onlyValid = False,
        repair = False,
         scale = "32km",
):
    """Add elevation to a Cartopy axis.

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxes
        the axis to add the elevation to
    debug : bool, optional
        print debug messages
    elevInt : int, optional
        the interval of the elevation bands to shade (in metres)
    fov : None or shapely.geometry.polygon.Polygon, optional
        clip the plotted shapes to the provided field-of-view to work around
        occaisional MatPlotLib or Cartopy plotting errors when shapes much
        larger than the field-of-view are plotted
    maxElev : int, optional
        the maximum elevation of the colour scale and acts as an upper bound or
        clip (in metres)
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons
    scale : str, optional
        the scale of the elevation

    Notes
    -----
    This function uses `CSS4 named colours
    <https://matplotlib.org/stable/gallery/color/named_colors.html>`_ .

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os
    import pathlib

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy").expanduser(),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import geojson
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",
                     "image.resample" : False,
            }
        )
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .extract_polys import extract_polys

    # **************************************************************************

    # Create suitable colour map ...
    cmap = matplotlib.colors.LinearSegmentedColormap.from_list(
        "elevation",
        [
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["olivedrab"]),
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightgrey"]),
        ]
    )

    # Loop over elevations ...
    # NOTE: Rounded to the nearest integer, Mount Everest is 8,849m ASL.
    for elevation in range(elevInt, 9000, elevInt):
        # Create short-hand ...
        name = f"{elevation:04d}m"

        # Create suitable colour ...
        # NOTE: Rounded to the nearest integer, Mount Everest is 8,849m ASL.
        facecolor = cmap(float(elevation) / float(maxElev))
        if debug:
            print(f"INFO: \"{name}\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

        # Find file containing the shapes ...
        sfile = f"{os.path.dirname(__file__)}/../data/geojson/globe/scale={scale}/elev={elevation:04d}m.geojson"
        if not os.path.exists(sfile):
            continue
        if debug:
            print(f"INFO: \"{name}\" is \"{sfile}\".")

        # Load the GeoJSON geometry collection and convert it to a Shapely
        # geometry collection ...
        with open(sfile, "rt", encoding = "utf-8") as fObj:
            coll = geojson.load(fObj)
        coll = shapely.geometry.shape(coll)

        # Create a list of Polygons to plot (taking in to account if the user
        # provided a field-of-view to clip them by) ...
        polys = []
        for poly in extract_polys(
            coll,
            onlyValid = onlyValid,
               repair = repair,
        ):
            if fov is None:
                polys.append(poly)
                continue
            if poly.disjoint(fov):
                continue
            polys.append(poly.intersection(fov))

        # Plot geometry ...
        ax.add_geometries(
            polys,
            cartopy.crs.PlateCarree(),
            edgecolor = "none",
            facecolor = facecolor,
        )
