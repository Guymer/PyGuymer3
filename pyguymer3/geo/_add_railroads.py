#!/usr/bin/env python3

# Define function ...
def _add_railroads(
    ax,
    /,
    *,
         debug = __debug__,
           fov = None,
     linestyle = "solid",
     linewidth = 0.5,
     onlyValid = False,
    resolution = "10m",
):
    """Add railroads to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the railroads to
    debug : bool, optional
        print debug messages
    fov : None or shapely.geometry.polygon.Polygon, optional
        clip the plotted shapes to the provided field-of-view to work around
        occaisional MatPlotLib or Cartopy plotting errors when shapes much
        larger than the field-of-view are plotted
    linestyle : str, optional
        the style of the railroads
    linewidth : float, optional
        the width of the railroads
    onlyValid : bool, optional
        only add valid LinearRings and LineStrings (checks for validity can take
        a while, if being being called often)
    resolution : str, optional
        the resolution of the railroads

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
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",
                     "image.resample" : False,
            }
        )
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None

    # Import sub-functions ...
    from .extract_lines import extract_lines

    # **************************************************************************

    # Create suitable colour ...
    edgecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["black"])
    if debug:
        print(f"INFO: \"railroads\" is ({edgecolor[0]:.6f},{edgecolor[1]:.6f},{edgecolor[2]:.6f},{edgecolor[3]:.6f}).")

    # Define names ...
    names = [
        "railroads",
        "railroads_north_america",
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
            # Skip bad records ...
            if not hasattr(record, "geometry"):
                continue

            # Create a list of LineStrings to plot (taking in to account if the
            # user provided a field-of-view to clip them by) ...
            lines = []
            for line in extract_lines(
                record.geometry,
                onlyValid = onlyValid,
            ):
                if fov is None:
                    lines.append(line)
                    continue
                if line.disjoint(fov):
                    continue
                lines.append(line.intersection(fov))

            # Plot geometry ...
            ax.add_geometries(
                lines,
                cartopy.crs.PlateCarree(),
                edgecolor = edgecolor,
                facecolor = "none",
                linestyle = linestyle,
                linewidth = linewidth,
            )
