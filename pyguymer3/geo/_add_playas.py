def _add_playas(axis, kwArgCheck = None, debug = False, keepInvalid = False, linestyle = "solid", linewidth = 0.5, resolution = "110m"):
    # NOTE: This function uses CSS4 named colours, see:
    #         * https://matplotlib.org/stable/gallery/color/named_colors.html

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
            extract_polys(record.geometry, keepInvalid = keepInvalid),
            cartopy.crs.PlateCarree(),
            edgecolor = edgecolor,
            facecolor = facecolor,
            linestyle = linestyle,
            linewidth = linewidth,
        )
