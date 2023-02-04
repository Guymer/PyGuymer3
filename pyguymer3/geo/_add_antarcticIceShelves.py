#!/usr/bin/env python3

# Define function ...
def _add_antarcticIceShelves(axis, /, *, debug = False, onlyValid = False, repair = False, resolution = "110m"):
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
    from .extract_polys import extract_polys

    # **************************************************************************

    # Create suitable colour ...
    facecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["aliceblue"])
    if debug:
        print(f"INFO: \"antarctic_ice_shelves_polys\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

    # Find file containing the shapes ...
    try:
        sfile = cartopy.io.shapereader.natural_earth(
            resolution = resolution,
              category = "physical",
                  name = "antarctic_ice_shelves_polys",
        )
    except urllib.error.HTTPError:
        return
    if debug:
        print(f"INFO: \"antarctic_ice_shelves_polys\" is \"{sfile}\".")

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(sfile).records():
        # Plot geometry ...
        axis.add_geometries(
            extract_polys(record.geometry, onlyValid = onlyValid, repair = repair),
            cartopy.crs.PlateCarree(),
            edgecolor = "none",
            facecolor = facecolor,
        )
