#!/usr/bin/env python3

# Define function ...
def _add_rivers(axis, /, *, debug = False, linestyle = "solid", linewidth = 0.5, onlyValid = False, resolution = "110m"):
    """Add rivers to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the rivers to
    debug : bool, optional
        print debug messages
    linestyle : str, optional
        the style of the rivers
    linewidth : float, optional
        the width of the rivers
    onlyValid : bool, optional
        only add valid LinearRings and LineStrings (checks for validity can take
        a while, if being being called often)
    resolution : str, optional
        the resolution of the rivers

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
    from .extract_lines import extract_lines

    # **************************************************************************

    # Create suitable colour ...
    edgecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightblue"])
    if debug:
        print(f"INFO: \"rivers\" is ({edgecolor[0]:.6f},{edgecolor[1]:.6f},{edgecolor[2]:.6f},{edgecolor[3]:.6f}).")

    # Define names ...
    names = [
        "rivers_australia",
        "rivers_europe",
        "rivers_lake_centerlines",
        "rivers_north_america",
    ]

    # Loop over names ...
    for name in names:
        # Find file containing the shapes ...
        try:
            sfile = cartopy.io.shapereader.natural_earth(
                resolution = resolution,
                  category = "physical",
                      name = name,
            )
        except urllib.error.HTTPError:
            continue
        if debug:
            print(f"INFO: \"{name}\" is \"{sfile}\".")

        # Loop over records ...
        for record in cartopy.io.shapereader.Reader(sfile).records():
            # Plot geometry ...
            axis.add_geometries(
                extract_lines(record.geometry, onlyValid = onlyValid),
                cartopy.crs.PlateCarree(),
                edgecolor = edgecolor,
                facecolor = "none",
                linestyle = linestyle,
                linewidth = linewidth,
            )
