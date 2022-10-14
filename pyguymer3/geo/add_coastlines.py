def add_coastlines(axis, kwArgCheck = None, colorName = "black", debug = False, level = 1, linestyle = "solid", linewidth = 0.5, resolution = "c"):
    """Add coastlines to an axis.

    This function adds coastline boundaries to a Cartopy axis. The resolution of
    the boundaries and *what* the boundaries delineate, are both configurable.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis
    colorName : str, optional
        the CSS4 named colour to draw the coastline boundary with
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

    There are six levels to choose from:
    * boundary between land and ocean (1);
    * boundary between lake and land (2);
    * boundary between island-in-lake and lake (3);
    * boundary between pond-in-island and island-in-lake (4);
    * boundary between Antarctica ice and ocean (5); and
    * boundary between Antarctica grounding-line and ocean (6).

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

    # Import sub-functions ...
    from .extract_polys import extract_polys

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Find the colour ...
    edgecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS[colorName])
    if debug:
        print(f"INFO: \"coastlines\" is \"{colorName}\", which is ({edgecolor[0]:.6f},{edgecolor[1]:.6f},{edgecolor[2]:.6f},{edgecolor[3]:.6f}).")

    # Find the Shapefile ...
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
        # Add Polygons to the list ...
        polys += extract_polys(record.geometry)

    # Plot geometry ...
    axis.add_geometries(
        polys,
        cartopy.crs.PlateCarree(),
        edgecolor = edgecolor,
        facecolor = "none",
        linestyle = linestyle,
        linewidth = linewidth,
    )
