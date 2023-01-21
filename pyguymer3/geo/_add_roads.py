#!/usr/bin/env python3

# Define function ...
def _add_roads(axis, kwArgCheck = None, debug = False, linestyle = "solid", linewidth = 0.5, onlyValid = False, resolution = "110m"):
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
    from .extract_lines import extract_lines

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Create suitable colour ...
    edgecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["red"])
    if debug:
        print(f"INFO: \"roads\" is ({edgecolor[0]:.6f},{edgecolor[1]:.6f},{edgecolor[2]:.6f},{edgecolor[3]:.6f}).")

    # Define names ...
    names = [
        "roads",
        "roads_north_america",
    ]

    # Loop over names ...
    for name in names:
        # Find file containing the shapes ...
        try:
            sfile = cartopy.io.shapereader.natural_earth(
                resolution = resolution,
                  category = "cultural",
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
