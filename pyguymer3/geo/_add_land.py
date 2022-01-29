def _add_land(axis, kwArgCheck = None, debug = False, resolution = "10m"):
    # NOTE: This function uses CSS4 named colours, see:
    #         * https://matplotlib.org/stable/gallery/color/named_colors.html

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
    color = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["darkkhaki"])
    if debug:
        print(f"INFO: \"land\" is ({color[0]:.6f},{color[1]:.6f},{color[2]:.6f},{color[3]:.6f}).")

    # Find file containing the shapes ...
    sfile = cartopy.io.shapereader.natural_earth(
        resolution = resolution,
          category = "physical",
              name = "land",
    )
    if debug:
        print(f"INFO: \"land\" is \"{sfile}\".")

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(sfile).records():
        # Plot geometry ...
        axis.add_geometries(
            extract_polys(record.geometry),
            cartopy.crs.PlateCarree(),
            edgecolor = "none",
            facecolor = color,
        )
