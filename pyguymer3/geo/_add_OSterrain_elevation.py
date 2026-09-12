#!/usr/bin/env python3

# Define function ...
def _add_OSterrain_elevation(
    ax,
    /,
    *,
          dataPath = None,
             debug = __debug__,
           elevInt = 100,
               fov = None,
           maxElev = 1000,
         onlyValid = False,
    osTerrainScale = "400m",
            prefix = ".",
            repair = False,
               tol = 1.0e-10,
):
    """Add Polygons of elevation from the OS Terrain 50 [2]_ dataset to a
    Cartopy axis.

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxes
        the axis to add the Polygons of elevation to
    dataPath : None or str, optional
        the path to the PyGuymer3 "data" folder
    debug : bool, optional
        print debug messages
    elevInt : int, optional
        the interval of the elevation bands to shade (in metres)
    fov : None or shapely.geometry.polygon.Polygon, optional
        clip the plotted shapes to the provided field-of-view to work around
        occasional MatPlotLib or Cartopy plotting errors when shapes much larger
        than the field-of-view are plotted
    maxElev : int, optional
        the maximum elevation of the colour scale which acts as an upper bound
        or clip (in metres)
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    osTerrainScale : str, optional
        the scale of the Polygons of elevation from the OS Terrain 50 [2]_
        dataset
    prefix : str, optional
        change the name of the output debugging CSVs
    repair : bool, optional
        attempt to repair invalid Polygons
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Notes
    -----
    This function uses `CSS4 named colours
    <https://matplotlib.org/stable/gallery/color/named_colors.html>`_ .

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] OS Terrain 50, https://www.ordnancesurvey.co.uk/products/os-terrain-50
    """

    # Import standard modules ...
    import os
    import pathlib

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy").expanduser(),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import geojson
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "axes.xmargin" : 0.01,
                       "axes.ymargin" : 0.01,
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",                                 # NOTE: See https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html
                     "image.resample" : False,
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
    from .en2ll import en2ll
    from .extract_polys import extract_polys
    from .._consts import PLATECARREE

    # **************************************************************************

    # Create suitable colour map ...
    cmap = matplotlib.colors.LinearSegmentedColormap.from_list(
        "elevation",
        [
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["olivedrab"]),
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightgrey"]),
        ]
    )

    # Find the path to the PyGuymer3 "data" folder ...
    if dataPath is None:
        dataPath = os.path.abspath(f"{os.path.dirname(__file__)}/../data")
    if not os.path.exists(dataPath):
        if debug:
            print(f"INFO: \"{dataPath}\" does not exist.")
        return
    if debug:
        print(f"INFO: The PyGuymer3 \"data\" folder is \"{dataPath}\".")

    # Loop over elevations ...
    # NOTE: Rounded to the nearest integer, Ben Nevis is 1,345m ASL.
    for elevation in range(elevInt, maxElev + elevInt, elevInt):
        # Create short-hand ...
        name = f"{elevation:04d}m"

        # Create suitable colour ...
        facecolor = cmap(float(elevation) / float(maxElev))
        if debug:
            print(f"INFO: \"{name}\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

        # Find GeoJSON file containing the shapes ...
        gName = f"{dataPath}/geojson/osTerrain/scale={osTerrainScale}/elev={elevation:04d}m.geojson"
        if not os.path.exists(gName):
            if debug:
                print(f"INFO: \"{gName}\" does not exist.")
            continue
        if debug:
            print(f"INFO: \"{name}\" is \"{gName}\".")

        # Load the GeoJSON geometry collection and convert it to a Shapely
        # geometry collection ...
        with open(gName, mode = "rt", encoding = "utf-8") as fObj:
            coll = geojson.load(fObj)
        coll = shapely.geometry.shape(coll)

        # Create a list of Polygons to plot (taking in to account if the user
        # provided a field-of-view to clip them by) ...
        polys = []
        for badPoly in extract_polys(
            coll,
            onlyValid = onlyValid,
               repair = repair,
        ):
            poly = en2ll(
                badPoly,
                 debug = debug,
                prefix = prefix,
                   tol = tol,
            )
            if fov is None:
                polys.append(poly)
                continue
            if poly.disjoint(fov):
                continue
            polys.append(shapely.geometry.polygon.orient(poly.intersection(fov)))

        # Plot geometry ...
        ax.add_geometries(
            polys,
            PLATECARREE,
            edgecolor = "none",
            facecolor = facecolor,
               zorder = 1.65,
        )
