#!/usr/bin/env python3

# Define function ...
def _add_background(
    ax,
    /,
    *,
    debug = __debug__,
):
    """Add background to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the background to
    debug : bool, optional
        print debug messages

    Notes
    -----
    This function uses `CSS4 named colours
    <https://matplotlib.org/stable/gallery/color/named_colors.html>`_ .

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
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

    # **************************************************************************

    # Create suitable colour ...
    facecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightblue"])
    if debug:
        print(f"INFO: \"background\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

    # Set background ...
    ax.set_facecolor(facecolor)
