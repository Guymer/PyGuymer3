def _add_lakes(axis, kwArgCheck = None, debug = False, onlyValid = False, repair = False, resolution = "110m"):
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
