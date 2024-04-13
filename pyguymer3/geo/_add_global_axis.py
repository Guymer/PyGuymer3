#!/usr/bin/env python3

# Define function ...
def _add_global_axis(
    fg,
    /,
    *,
           add_coastlines = True,
            add_gridlines = True,
     coastlines_edgecolor = "black",
     coastlines_facecolor = "none",
        coastlines_levels = None,
     coastlines_linestyle = "solid",
     coastlines_linewidth = 0.5,
    coastlines_resolution = "f",
        coastlines_zorder = 1.5,
            gridlines_int = None,
      gridlines_linecolor = "black",
      gridlines_linestyle = ":",
      gridlines_linewidth = 0.5,
         gridlines_zorder = 2.0,
                       gs = None,
                    index = None,
                    ncols = None,
                    nrows = None,
                onlyValid = False,
                   repair = False,
):
    """Add a global Robinson axis

    Parameters
    ----------
    fg : matplotlib.figure.Figure
        the figure to add the axis to
    add_coastlines : bool, optional
        add coastline boundaries
    add_gridlines : bool, optional
        add gridlines of longitude and latitude
    coastlines_edgecolor : str, optional
        the colour of the edges of the coastline Polygons
    coastlines_facecolor : str, optional
        the colour of the faces of the coastline Polygons
    coastlines_levels : list of int, optional
        the levels of the coastline boundaries (if None then default to
        ``[1, 6]``)
    coastlines_linestyle : str, optional
        the linestyle to draw the coastline boundaries with
    coastlines_linewidth : float, optional
        the linewidth to draw the coastline boundaries with
    coastlines_resolution : str, optional
        the resolution of the coastline boundaries
    coastlines_zorder : float, optional
        the zorder to draw the coastline boundaries with (the default value has
        been chosen to match the value that it ends up being if the coastline
        boundaries are not drawn with the zorder keyword specified -- obtained
        by manual inspection on 5/Dec/2023)
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
    onlyValid : bool, optional
        only return valid Polygons (checks for validity can take a while, if
        being called often)
    repair : bool, optional
        attempt to repair invalid Polygons

    Returns
    -------
    ax : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis

    Notes
    -----
    There are two arguments relating to the `Global Self-Consistent Hierarchical
    High-Resolution Geography dataset <https://www.ngdc.noaa.gov/mgg/shorelines/>`_ :

    * *coastlines_levels*; and
    * *coastlines_resolution*.

    There are six levels to choose from:

    * boundary between land and ocean (1);
    * boundary between lake and land (2);
    * boundary between island-in-lake and lake (3);
    * boundary between pond-in-island and island-in-lake (4);
    * boundary between Antarctica ice and ocean (5); and
    * boundary between Antarctica grounding-line and ocean (6).

    There are five resolutions to choose from:

    * crude ("c");
    * low ("l");
    * intermediate ("i");
    * high ("h"); and
    * full ("f").

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
    from ._add_coastlines import _add_coastlines
    from ._add_horizontal_gridlines import _add_horizontal_gridlines
    from ._add_vertical_gridlines import _add_vertical_gridlines

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

    # Check if the user wants to add coastline boundaries ...
    if add_coastlines:
        # Add coastline boundaries ...
        _add_coastlines(
            ax,
             edgecolor = coastlines_edgecolor,
             facecolor = coastlines_facecolor,
                levels = coastlines_levels,
             linestyle = coastlines_linestyle,
             linewidth = coastlines_linewidth,
             onlyValid = onlyValid,
                repair = repair,
            resolution = coastlines_resolution,
                zorder = coastlines_zorder,
        )

    # Check if the user wants to add gridlines ...
    if add_gridlines:
        # Add gridlines ...
        _add_horizontal_gridlines(
            ax,
                color = gridlines_linecolor,
            linestyle = gridlines_linestyle,
            linewidth = gridlines_linewidth,
                 locs = range( -90,  +90 + gridlines_int, gridlines_int),
                ngrid = -1,
               npoint = 3601,
               zorder = gridlines_zorder,
        )
        _add_vertical_gridlines(
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
