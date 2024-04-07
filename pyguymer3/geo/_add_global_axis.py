#!/usr/bin/env python3

# Define function ...
def add_global_axis(fg, /, *, add_gridlines = False, color = "black", gridline_int = 1, gs = None, index = None, linestyle = ":", linewidth = 0.5, ncols = None, nrows = None, zorder = 2.0):
    """Add a global Robinson axis

    Parameters
    ----------
    fg : matplotlib.figure.Figure
        the figure to add the axis to
    add_gridlines : bool, optional
        add gridlines every degree of longitude and latitude
    color : str, optional
        the colour of the gridlines
    gridline_int : int, optional
        the interval between gridlines, best results if ``90 % gridline_int == 0``
        (in degrees)
    gs : matplotlib.gridspec.SubplotSpec, optional
        the subset of a gridspec to locate the axis
    index : int or tuple of int, optional
        the index of the axis in the array of axes
    linestyle : str, optional
        the style of the gridlines
    linewidth : float, optional
        the width of the gridlines
    ncols : int, optional
        the number of columns in the array of axes
    nrows : int, optional
        the number of rows in the array of axes
    zorder : float, optional
        the zorder to draw the gridlines with (the default value has been chosen
        to match the value that it ends up being if the gridlines are not drawn
        with the zorder keyword specified -- obtained by manual inspection on
        5/Dec/2023)

    Returns
    -------
    ax : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis

    Notes
    -----
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

    # Import sub-functions ...
    from .add_horizontal_gridlines import add_horizontal_gridlines
    from .add_vertical_gridlines import add_vertical_gridlines

    # **************************************************************************

    # Check where the axis should be created ...
    if gs is not None:
        # Create axis ...
        ax = fg.add_subplot(
            gs,
            projection = cartopy.crs.Robinson(),
        )
    elif nrows is not None and ncols is not None and index is not None:
        # Create axis ...
        ax = fg.add_subplot(
            nrows,
            ncols,
            index,
            projection = cartopy.crs.Robinson(),
        )
    else:
        # Create axis ...
        ax = fg.add_subplot(
            projection = cartopy.crs.Robinson(),
        )

    # Configure axis ...
    ax.set_global()

    # Check if the user wants to add gridlines ...
    if add_gridlines:
        # Add gridlines ...
        add_horizontal_gridlines(
            ax,
                color = color,
            linestyle = linestyle,
            linewidth = linewidth,
                 locs = range( -90,  +90 + gridline_int, gridline_int),
                ngrid = -1,
               npoint = 361,
               zorder = zorder,
        )
        add_vertical_gridlines(
            ax,
                color = color,
            linestyle = linestyle,
            linewidth = linewidth,
                 locs = range(-180, +180 + gridline_int, gridline_int),
                ngrid = -1,
               npoint = 181,
               zorder = zorder,
        )

    # Return answer ...
    return ax
