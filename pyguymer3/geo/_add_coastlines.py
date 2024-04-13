#!/usr/bin/env python3

# Define function ...
def _add_coastlines(axis, /, *, colorName = "black", debug = False, faceOpac = -1.0, levels = None, linestyle = "solid", linewidth = 0.5, onlyValid = False, repair = False, resolution = "i", zorder = 1.5):
    """Add coastlines to an axis.

    This function adds coastline boundaries to a Cartopy axis. The resolution of
    the boundaries and *what* the boundaries delineate, are both configurable.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis
    colorName : str, optional
        the CSS4 named colour to draw the coastline boundaries with
    debug : bool, optional
        print debug messages
    faceOpac : float, optional
        if ≥ 0.0 and ≤ 1.0 then shade the Polygon faces the same colour as the
        Polygon edges with this opacity
    levels : list of int, optional
        the levels of the coastline boundaries (if None then default to
        ``[1, 6]``)
    linestyle : str, optional
        the linestyle to draw the coastline boundaries with
    linewidth : float, optional
        the linewidth to draw the coastline boundaries with
    onlyValid : bool, optional
        only return valid Polygons (checks for validity can take a while, if
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons
    resolution : str, optional
        the resolution of the coastline boundaries
    zorder : float, optional
        the zorder to draw the coastline boundaries with (the default value has
        been chosen to match the value that it ends up being if the coastline
        boundaries are not drawn with the zorder keyword specified -- obtained
        by manual inspection on 5/Dec/2023)

    Notes
    -----
    There are two arguments relating to the `Global Self-Consistent Hierarchical
    High-Resolution Geography dataset <https://www.ngdc.noaa.gov/mgg/shorelines/>`_ :

    * *level*; and
    * *resolution*.

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

    This function uses `CSS4 named colours
    <https://matplotlib.org/stable/gallery/color/named_colors.html>`_ .

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os
    import urllib

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : os.path.expanduser("~/.local/share/cartopy_cache"),
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

    # Check inputs ...
    if levels is None:
        levels = [1, 6]

    # Find the edge colour ...
    edgecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS[colorName])
    if debug:
        print(f"INFO: \"coastlines\" is \"{colorName}\", which is ({edgecolor[0]:.6f},{edgecolor[1]:.6f},{edgecolor[2]:.6f},{edgecolor[3]:.6f}).")

    # Find the face colour ...
    if 0.0 <= faceOpac <= 1.0:
        facecolor = (edgecolor[0], edgecolor[1], edgecolor[2], faceOpac)
    else:
        facecolor = "none"

    # Loop over levels ...
    for level in levels:
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
            polys += extract_polys(record.geometry, onlyValid = onlyValid, repair = repair)

        # Plot geometry ...
        axis.add_geometries(
            polys,
            cartopy.crs.PlateCarree(),
            edgecolor = edgecolor,
            facecolor = facecolor,
            linestyle = linestyle,
            linewidth = linewidth,
               zorder = zorder,
        )
