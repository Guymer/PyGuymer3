#!/usr/bin/env python3

# Define function ...
def _add_background(axis, kwArgCheck = None, debug = False):
    # NOTE: This function uses CSS4 named colours, see:
    #         * https://matplotlib.org/stable/gallery/color/named_colors.html

    # Import special modules ...
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

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # **************************************************************************

    # Create suitable colour ...
    facecolor = matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightblue"])
    if debug:
        print(f"INFO: \"background\" is ({facecolor[0]:.6f},{facecolor[1]:.6f},{facecolor[2]:.6f},{facecolor[3]:.6f}).")

    # Set background ...
    axis.set_facecolor(facecolor)
