def _add_elevation(axis, kwArgCheck = None, debug = False, keepInvalid = False, maxElev = 8850.0, resolution = "110m"):
    # NOTE: This function uses CSS4 named colours, see:
    #         * https://matplotlib.org/stable/gallery/color/named_colors.html

    # Import standard modules ...
    import os

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import geojson
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import matplotlib
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .extract_polys import extract_polys

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Create suitable colour map ...
    cmap = matplotlib.colors.LinearSegmentedColormap.from_list(
        "elevation",
        [
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["olivedrab"]),
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightgrey"]),
        ]
    )

    # Define scales ...
    scale = {
         "10m" : "08km",
         "50m" : "16km",
        "110m" : "32km",
    }
    if resolution not in scale:
        raise Exception(f"\"{resolution}\" is not a recognised resolution; \"{__name__}\" needs updating") from None

    # Loop over elevations ...
    # NOTE: Rounded to the nearest integer, Mount Everest is 8,849m ASL.
    for elevation in range(0, 8900, 50):
        # Create short-hand ...
        name = f"{elevation:04d}m"

        # Create suitable colour ...
        # NOTE: Rounded to the nearest integer, Mount Everest is 8,849m ASL.
        facecolor = cmap(float(elevation) / maxElev)
        if debug:
            print(f"INFO: \"{name}\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

        # Find file containing the shapes ...
        sfile = f"{os.path.dirname(__file__)}/../data/geojson/GLOBE/scale={scale[resolution]}/elev={elevation:04d}m.geojson"
        if not os.path.exists(sfile):
            continue
        if debug:
            print(f"INFO: \"{name}\" is \"{sfile}\".")

        # Load the GeoJSON geometry collection and convert it to a Shapely
        # geometry collection ...
        with open(sfile, "rt", encoding = "utf-8") as fobj:
            coll = geojson.load(fobj)
        coll = shapely.geometry.shape(coll)

        # Plot geometry ...
        axis.add_geometries(
            extract_polys(coll, keepInvalid = keepInvalid),
            cartopy.crs.PlateCarree(),
            edgecolor = "none",
            facecolor = facecolor,
        )
