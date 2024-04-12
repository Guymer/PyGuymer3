#!/usr/bin/env python3

# Define function ...
def _add_global_axis(
    fg,
    /,
    *,
          add_gridlines = True,
          gridlines_int = None,
    gridlines_linecolor = "black",
    gridlines_linestyle = ":",
    gridlines_linewidth = 0.5,
       gridlines_zorder = 2.0,
                     gs = None,
                  index = None,
                  ncols = None,
                  nrows = None,
):
    """Add a global Robinson axis

    Parameters
    ----------
    fg : matplotlib.figure.Figure
        the figure to add the axis to
    add_gridlines : bool, optional
        add gridlines of longitude and latitude
    gridlines_int : int, optional
        the interval between gridlines, best results if ``90 % gridlines_int == 0``;
        the default will be 45° (in degrees)
    gridlines_linecolor : str, optional
        the colour of the gridlines
    gridlines_linestyle : str, optional
        the style of the gridlines
    gridlines_linewidth : float, optional
        the width of the gridlines
    gridlines_zorder : float, optional
        the zorder to draw the gridlines with (the default value has been chosen
        to match the value that it ends up being if the gridlines are not drawn
        with the zorder keyword specified -- obtained by manual inspection on
        5/Dec/2023)
    gs : matplotlib.gridspec.SubplotSpec, optional
        the subset of a gridspec to locate the axis
    index : int or tuple of int, optional
        the index of the axis in the array of axes
    ncols : int, optional
        the number of columns in the array of axes
    nrows : int, optional
        the number of rows in the array of axes

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

    # Check inputs ...
    if gridlines_int is None:
        gridlines_int = 45                                                      # [°]

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
                color = gridlines_linecolor,
            linestyle = gridlines_linestyle,
            linewidth = gridlines_linewidth,
                 locs = range( -90,  +90 + gridlines_int, gridlines_int),
                ngrid = -1,
               npoint = 3601,
               zorder = gridlines_zorder,
        )
        add_vertical_gridlines(
            ax,
                color = gridlines_linecolor,
            linestyle = gridlines_linestyle,
            linewidth = gridlines_linewidth,
                 locs = range(-180, +180 + gridlines_int, gridlines_int),
                ngrid = -1,
               npoint = 1801,
               zorder = gridlines_zorder,
        )

    # Return answer ...
    return ax
