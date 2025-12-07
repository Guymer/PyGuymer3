#!/usr/bin/env python3

# Define function ...
def _add_glaciatedAreas(
    ax,
    /,
    *,
         debug = __debug__,
           fov = None,
     onlyValid = False,
        repair = False,
    resolution = "10m",
):
    """Add glaciated areas to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the glaciated areas to
    debug : bool, optional
        print debug messages
    fov : None or shapely.geometry.polygon.Polygon, optional
        clip the plotted shapes to the provided field-of-view to work around
        occaisional MatPlotLib or Cartopy plotting errors when shapes much
        larger than the field-of-view are plotted
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons
    resolution : str, optional
        the resolution of the glaciated areas

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
    import pathlib
    import urllib

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
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "backend" : "Agg",                                       # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                    "figure.dpi" : 300,
                "figure.figsize" : (9.6, 7.2),                                  # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                     "font.size" : 8,
            }
        )
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None

    # Import sub-functions ...
    from .extract_polys import extract_polys

    # **************************************************************************

    # Create suitable colour ...
    facecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["snow"])
    if debug:
        print(f"INFO: \"glaciated_areas\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

    # Find file containing the shapes ...
    try:
        sfile = cartopy.io.shapereader.natural_earth(
            resolution = resolution,
              category = "physical",
                  name = "glaciated_areas",
        )
    except urllib.error.HTTPError:
        return
    if debug:
        print(f"INFO: \"glaciated_areas\" is \"{sfile}\".")

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(sfile).records():
        # Skip bad records ...
        if not hasattr(record, "geometry"):
            continue

        # Create a list of Polygons to plot (taking in to account if the user
        # provided a field-of-view to clip them by) ...
        polys = []
        for poly in extract_polys(
            record.geometry,
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
