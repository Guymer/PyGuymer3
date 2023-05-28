#!/usr/bin/env python3

# Define function ...
def _add_lakes(axis, /, *, debug = False, onlyValid = False, repair = False, resolution = "110m"):
    """Add lakes to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the lakes to
    debug : bool, optional
        print debug messages
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons
    resolution : str, optional
        the resolution of the lakes

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

    # Create suitable colour ...
    facecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightblue"])
    if debug:
        print(f"INFO: \"lakes\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

    # Define names ...
    names = [
        "lakes",
        "lakes_australia",
        "lakes_europe",
        "lakes_north_america",
        "lakes_pluvial",
    ]

    # Loop over names ...
    for name in names:
        try:
            # Find file containing the shapes ...
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
                extract_polys(record.geometry, onlyValid = onlyValid, repair = repair),
                cartopy.crs.PlateCarree(),
                edgecolor = "none",
                facecolor = facecolor,
            )
