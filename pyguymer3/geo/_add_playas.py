#!/usr/bin/env python3

# Define function ...
def _add_playas(axis, /, *, debug = False, linestyle = "solid", linewidth = 0.5, onlyValid = False, repair = False, resolution = "110m"):
    """Add playas to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the playas to
    debug : bool, optional
        print debug messages
    linestyle : str, optional
        the style of the exterior of the playas
    linewidth : float, optional
        the width of the exterior of the playas
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons
    resolution : str, optional
        the resolution of the playas

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
    import urllib

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                   "backend" : "Agg",                                           # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                "figure.dpi" : 300,
                 "font.size" : 8,
            }
        )
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None

    # Import sub-functions ...
    from .extract_polys import extract_polys

    # **************************************************************************

    # Create suitable colours ...
    edgecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["cornflowerblue"])
    facecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["khaki"])
    if debug:
        print(f"INFO: \"playas\" is ({edgecolor[0]:.6f},{edgecolor[1]:.6f},{edgecolor[2]:.6f},{edgecolor[3]:.6f}) and ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

    # Find file containing the shapes ...
    try:
        sfile = cartopy.io.shapereader.natural_earth(
            resolution = resolution,
              category = "physical",
                  name = "playas",
        )
    except urllib.error.HTTPError:
        return
    if debug:
        print(f"INFO: \"playas\" is \"{sfile}\".")

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(sfile).records():
        # Plot geometry ...
        axis.add_geometries(
            extract_polys(record.geometry, onlyValid = onlyValid, repair = repair),
            cartopy.crs.PlateCarree(),
            edgecolor = edgecolor,
            facecolor = facecolor,
            linestyle = linestyle,
            linewidth = linewidth,
        )
