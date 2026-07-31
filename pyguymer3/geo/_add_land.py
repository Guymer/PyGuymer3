#!/usr/bin/env python3

# Define function ...
def _add_land(
    ax,
    /,
    *,
        debug = __debug__,
          fov = None,
        neRes = "10m",
    onlyValid = False,
       repair = False,
):
    """Add land to a Cartopy axis.

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxes
        the axis to add the land to
    debug : bool, optional
        print debug messages
    fov : None or shapely.geometry.polygon.Polygon, optional
        clip the plotted shapes to the provided field-of-view to work around
        occasional MatPlotLib or Cartopy plotting errors when shapes much larger
        than the field-of-view are plotted
    neRes : str, optional
        the resolution of the land from Natural Earth [2]_
    onlyValid : bool, optional
        only add valid Polygons (checks for validity can take a while, if being
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons

    Notes
    -----
    There is one argument relating to the `Natural Earth dataset
    <https://www.naturalearthdata.com>`_ :

    * *neRes*.

    There are three resolutions to choose from:

    * large scale data 1:10m ("10m");
    * medium scale data 1:50m ("50m"); and
    * small scale data 1:110m ("110m").

    This function uses `CSS4 named colours
    <https://matplotlib.org/stable/gallery/color/named_colors.html>`_ .

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] Natural Earth, https://www.naturalearthdata.com/
    """

    # Import standard modules ...
    import pathlib
    import urllib

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
    from .extract_polys import extract_polys
    from .geodetic2platecarree import geodetic2platecarree
    from .._consts import PLATECARREE

    # **************************************************************************

    # Create suitable colour ...
    facecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["darkkhaki"])
    if debug:
        print(f"INFO: \"land\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

    # Find file containing the shapes ...
    try:
        sfile = cartopy.io.shapereader.natural_earth(
              category = "physical",
                  name = "land",
            resolution = neRes,
        )
    except RuntimeError:
        if debug:
            print("INFO: Skipping (runtime error).")
        return
    except urllib.error.HTTPError:
        if debug:
            print("INFO: Skipping (HTTP error).")
        return
    if debug:
        print(f"INFO: \"land\" is \"{sfile}\".")

    # Initialize list ...
    polys = []

    # Loop over records ...
    for record in cartopy.io.shapereader.Reader(sfile).records():
        # Skip bad records ...
        if not hasattr(record, "geometry"):
            continue

        # Append Polygons to list (taking in to account if the user provided a
        # field-of-view to clip them by) ...
        for poly in extract_polys(
            record.geometry,
            onlyValid = onlyValid,
               repair = repair,
        ):
            if fov is None:
                polys.append(poly)
                continue
            if poly.disjoint(fov):
                continue
            polys.append(shapely.geometry.polygon.orient(poly.intersection(fov)))

    # Plot geometry (converting from an elliptical description of Earth to a
    # circular description of Earth) ...
    # NOTE: See https://cartopy.readthedocs.io/stable/gallery/lines_and_polygons/effects_of_the_ellipse.html
    ax.add_geometries(
        geodetic2platecarree(polys),
        PLATECARREE,
        edgecolor = "none",
        facecolor = facecolor,
    )
