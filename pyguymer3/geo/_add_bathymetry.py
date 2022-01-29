def _add_bathymetry(axis, kwArgCheck = None, debug = False, resolution = "10m"):
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
        color = cmap(float(depth) / 10000.0)
        if debug:
            print(f"INFO: \"bathymetry {depth:5d}m\" is ({color[0]:.6f},{color[1]:.6f},{color[2]:.6f},{color[3]:.6f}).")

        # Find file containing the shapes ...
        sfile = cartopy.io.shapereader.natural_earth(
            resolution = resolution,
              category = "physical",
                  name = name,
        )
        if debug:
            print(f"INFO: \"bathymetry {depth:5d}m\" is \"{sfile}\".")

        # Loop over records ...
        for record in cartopy.io.shapereader.Reader(sfile).records():
            # Plot geometry ...
            axis.add_geometries(
                extract_polys(record.geometry),
                cartopy.crs.PlateCarree(),
                edgecolor = "none",
                facecolor = color,
            )
