#!/usr/bin/env python3

# Define function ...
def add_axis(fg, /, *, add_gridlines = False, color = "black", debug = False, dist = 1.0e99, eps = 1.0e-12, gridline_int = 1, gs = None, index = None, lat = None, linestyle = ":", linewidth = 0.5, lon = None, ncols = None, nmax = 100, nrows = None, prefix = ".", ramLimit = 1073741824, tol = 1.0e-10, zorder = 2.0):
    """Add either a global Robinson axis or an Orthographic axis centred above a
    point with optionally a field-of-view based on a circle around the point on
    the surface of the Earth

    Parameters
    ----------
    fg : matplotlib.figure.Figure
        the figure to add the axis to
    add_gridlines : bool, optional
        add gridlines every degree of longitude and latitude
    color : str, optional
        the colour of the gridlines
    debug : bool, optional
        print debug messages and draw the circle on the axis
    dist : float, optional
        the radius of the circle around the point, if larger than the Earth then
        make the axis of global extent (in metres)
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    gridline_int : int, optional
        the interval between gridlines, best results if ``90 % gridline_int == 0``
        (in degrees)
    gs : matplotlib.gridspec.SubplotSpec, optional
        the subset of a gridspec to locate the axis
    index : int or tuple of int, optional
        the index of the axis in the array of axes
    lat : float, optional
        the latitude of the point (in degrees)
    linestyle : str, optional
        the style of the gridlines
    linewidth : float, optional
        the width of the gridlines
    lon : float, optional
        the longitude of the point (in degrees)
    ncols : int, optional
        the number of columns in the array of axes
    nmax : int, optional
        the maximum number of the Vincenty formula iterations
    nrows : int, optional
        the number of rows in the array of axes
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)
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

    # Import sub-functions ...
    from .add_global_axis import add_global_axis
    from .add_topDown_axis import add_topDown_axis

    # **************************************************************************

    # Check what projection should be used ...
    if lon is not None and lat is not None:
        # Return answer ...
        return add_topDown_axis(
            fg,
            lon,
            lat,
            add_gridlines = add_gridlines,
                    color = color,
                    debug = debug,
                     dist = dist,
                      eps = eps,
             gridline_int = gridline_int,
                       gs = gs,
                    index = index,
                linestyle = linestyle,
                linewidth = linewidth,
                    ncols = ncols,
                     nmax = nmax,
                    nrows = nrows,
                   prefix = prefix,
                 ramLimit = ramLimit,
                      tol = tol,
                   zorder = zorder,
        )

    # Return answer ...
    return add_global_axis(
        fg,
        add_gridlines = add_gridlines,
                color = color,
         gridline_int = gridline_int,
                   gs = gs,
                index = index,
            linestyle = linestyle,
            linewidth = linewidth,
                ncols = ncols,
                nrows = nrows,
               zorder = zorder,
    )
