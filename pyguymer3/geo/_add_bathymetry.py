#!/usr/bin/env python3

# Define function ...
def _add_bathymetry(
    ax,
    /,
    *,
         debug = __debug__,
     onlyValid = False,
        repair = False,
    resolution = "10m",
):
    """Add bathymetry to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the bathymetry to
    debug : bool, optional
        print debug messages
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons
    resolution : str, optional
        the resolution of the bathymetry

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
    from .extract_polys import extract_polys

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
            # Skip bad records ...
            if not hasattr(record, "geometry"):
                continue

            # Plot geometry ...
            ax.add_geometries(
                extract_polys(record.geometry, onlyValid = onlyValid, repair = repair),
                cartopy.crs.PlateCarree(),
                edgecolor = "none",
                facecolor = facecolor,
            )
