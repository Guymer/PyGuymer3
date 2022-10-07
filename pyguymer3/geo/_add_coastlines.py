def _add_coastlines(axis, kwArgCheck = None, debug = False, level = 1, linestyle = "solid", linewidth = 0.5, resolution = "c"):
    """Add coastlines to an axis.

    This function adds coastline boundaries to a Cartopy axis. The resolution of
    the boundaries and *what* the boundaries delineate, are both configurable.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis
    debug : bool, optional
        print debug messages
    level : int, optional
        the level of the coastline boundary
    linestyle : str, optional
        the linestyle to draw the coastline boundary with
    linewidth : float, optional
        the linewidth to draw the coastline boundary with
    resolution : str, optional
        the resolution of the coastline boundary

    Notes
    -----
    There are two arguments relating to the Global Self-Consistent Hierarchical
    High-Resolution Geography dataset [1]_ :
    * level; and
    * resolution.

    There are four levels to choose from:
    * boundary between land and ocean (1);
    * boundary between lake and land (2);
    * boundary between island-in-lake and lake (3); and
    * boundary between pond-in-island and island-in-lake (4).

    There are five resolutions to choose from:
    * crude ("c");
    * low ("l");
    * intermediate ("i");
    * high ("h"); and
    * full ("f").

    This function uses CSS4 named colours [2]_ .

    Copyright 2017 Thomas Guymer [3]_

    References
    ----------
    .. [1] Global Self-Consistent Hierarchical High-Resolution Geography, https://www.ngdc.noaa.gov/mgg/shorelines/
    .. [2] MatPlotLib CSS4 Named Colours, https://matplotlib.org/stable/gallery/color/named_colors.html
    .. [3] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import urllib

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import shapely
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .extract_polys import extract_polys

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Create suitable colour ...
    edgecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["black"])
    if debug:
        print(f"INFO: \"coastlines\" is ({edgecolor[0]:.6f},{edgecolor[1]:.6f},{edgecolor[2]:.6f},{edgecolor[3]:.6f}).")

    # Find Shapefile ...
    try:
        sfile = cartopy.io.shapereader.gshhs(
            level = level,
            scale = resolution,
        )
    except urllib.error.HTTPError:
        return

    # Initialize list ...
    polys = []

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(sfile).records():
        # Skip bad records ...
        if record.geometry is None:
            print(f"WARNING: Skipping a collection of coastlines in \"{sfile}\" as it is None.")
            continue
        if not record.geometry.is_valid:
            print(f"WARNING: Skipping a collection of coastlines in \"{sfile}\" as it is not valid.")
            continue
        if record.geometry.is_empty:
            print(f"WARNING: Skipping a collection of coastlines in \"{sfile}\" as it is empty.")
            continue

        # Check type ...
        if not isinstance(record.geometry, shapely.geometry.polygon.Polygon) and not isinstance(record.geometry, shapely.geometry.multipolygon.MultiPolygon):
            print(f"WARNING: Skipping a collection of coastlines in \"{sfile}\" as it is not a [Multi]Polygon.")
            continue

        # Loop over Polygons ...
        for poly in extract_polys(record.geometry):
            # Skip bad Polygons ...
            if poly is None:
                print(f"WARNING: Skipping a piece of coastline in \"{sfile}\" as it is None.")
                continue
            if not poly.is_valid:
                print(f"WARNING: Skipping a piece of coastline in \"{sfile}\" as it is not valid.")
                continue
            if poly.is_empty:
                print(f"WARNING: Skipping a piece of coastline in \"{sfile}\" as it is empty.")
                continue

            # Append the Polygon to the list ...
            polys.append(poly)

    # Plot geometry ...
    axis.add_geometries(
        polys,
        cartopy.crs.PlateCarree(),
        edgecolor = edgecolor,
        facecolor = "none",
        linestyle = linestyle,
        linewidth = linewidth,
    )
