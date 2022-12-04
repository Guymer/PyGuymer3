def _add_bathymetry(axis, kwArgCheck = None, debug = False, keepInvalid = False, resolution = "110m"):
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

    # Create suitable colour map ...
    cmap = matplotlib.colors.LinearSegmentedColormap.from_list(
        "bathymetry",
        [
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightblue"]),
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["darkblue"]),
        ]
    )

    # Define depths ...
    depths = [
        (    0, "bathymetry_L_0"    ),
        (  200, "bathymetry_K_200"  ),
        ( 1000, "bathymetry_J_1000" ),
        ( 2000, "bathymetry_I_2000" ),
        ( 3000, "bathymetry_H_3000" ),
        ( 4000, "bathymetry_G_4000" ),
        ( 5000, "bathymetry_F_5000" ),
        ( 6000, "bathymetry_E_6000" ),
        ( 7000, "bathymetry_D_7000" ),
        ( 8000, "bathymetry_C_8000" ),
        ( 9000, "bathymetry_B_9000" ),
        (10000, "bathymetry_A_10000"),
    ]

    # Loop over depths ...
    for depth, name in depths:
        # Create suitable colour ...
        facecolor = cmap(float(depth) / 10000.0)
        if debug:
            print(f"INFO: \"{name}\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

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
                extract_polys(record.geometry, keepInvalid = keepInvalid),
                cartopy.crs.PlateCarree(),
                edgecolor = "none",
                facecolor = facecolor,
            )
