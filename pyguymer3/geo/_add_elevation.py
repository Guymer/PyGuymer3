#!/usr/bin/env python3

# Define function ...
def _add_elevation(axis, /, *, debug = False, maxElev = 8850.0, onlyValid = False, repair = False, resolution = "10m"):
    """Add elevation to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the elevation to
    debug : bool, optional
        print debug messages
    maxElev : float, optional
        the maximum elevation of the colour scale and acts as an upper bound or
        clip (in metres)
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons
    resolution : str, optional
        the resolution of the elevation

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
        import geojson
        geojson.geometry.Geometry.__init__.__defaults__ = (None, False, 12)     # NOTE: See https://github.com/jazzband/geojson/issues/135#issuecomment-596509669
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
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
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .extract_polys import extract_polys

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
    for elevation in range(250, 9000, 250):
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
        with open(sfile, "rt", encoding = "utf-8") as fObj:
            coll = geojson.load(fObj)
        coll = shapely.geometry.shape(coll)

        # Plot geometry ...
        axis.add_geometries(
            extract_polys(coll, onlyValid = onlyValid, repair = repair),
            cartopy.crs.PlateCarree(),
            edgecolor = "none",
            facecolor = facecolor,
        )
