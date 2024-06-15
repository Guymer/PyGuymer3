#!/usr/bin/env python3

# Define function ...
def add_axis(
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
    coastlines_resolution = "i",
        coastlines_zorder = 1.5,
                    debug = False,
                     dist = 1.0e99,
                      eps = 1.0e-12,
            gridlines_int = None,
      gridlines_linecolor = "black",
      gridlines_linestyle = ":",
      gridlines_linewidth = 0.5,
         gridlines_zorder = 2.0,
                       gs = None,
                    index = None,
                      lat = None,
                      lon = None,
                    ncols = None,
                     nmax = 100,
                    nrows = None,
                onlyValid = False,
                   prefix = ".",
                 ramLimit = 1073741824,
                   repair = False,
                      tol = 1.0e-10,
):
    """Add either a global Robinson axis or an Orthographic axis centred above a
    point with optionally a field-of-view based on a circle around the point on
    the surface of the Earth

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
    debug : bool, optional
        print debug messages and draw the circle on the axis
    dist : float, optional
        the radius of the circle around the point, if larger than the Earth then
        make the axis of global extent (in metres)
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    gridlines_int : int, optional
        the interval between gridlines, best results if ``90 % gridlines_int == 0``;
        if the axis is of global extent then the default will be 45° else it
        will be 1° (in degrees)
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
    lat : float, optional
        the latitude of the point (in degrees)
    lon : float, optional
        the longitude of the point (in degrees)
    ncols : int, optional
        the number of columns in the array of axes
    nmax : int, optional
        the maximum number of the Vincenty formula iterations
    nrows : int, optional
        the number of rows in the array of axes
    onlyValid : bool, optional
        only return valid Polygons (checks for validity can take a while, if
        being called often)
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)
    repair : bool, optional
        attempt to repair invalid Polygons
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

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

    # Import sub-functions ...
    from ._add_global_axis import _add_global_axis
    from ._add_topDown_axis import _add_topDown_axis

    # **************************************************************************

    # Check what projection should be used ...
    if lon is not None and lat is not None:
        # Return answer ...
        return _add_topDown_axis(
            fg,
            lon,
            lat,
                   add_coastlines = add_coastlines,
                    add_gridlines = add_gridlines,
             coastlines_edgecolor = coastlines_edgecolor,
             coastlines_facecolor = coastlines_facecolor,
                coastlines_levels = coastlines_levels,
             coastlines_linestyle = coastlines_linestyle,
             coastlines_linewidth = coastlines_linewidth,
            coastlines_resolution = coastlines_resolution,
                coastlines_zorder = coastlines_zorder,
                            debug = debug,
                             dist = dist,
                              eps = eps,
                    gridlines_int = gridlines_int,
              gridlines_linecolor = gridlines_linecolor,
              gridlines_linestyle = gridlines_linestyle,
              gridlines_linewidth = gridlines_linewidth,
                 gridlines_zorder = gridlines_zorder,
                               gs = gs,
                            index = index,
                            ncols = ncols,
                             nmax = nmax,
                            nrows = nrows,
                        onlyValid = onlyValid,
                           prefix = prefix,
                         ramLimit = ramLimit,
                           repair = repair,
                              tol = tol,
        )

    # Return answer ...
    return _add_global_axis(
        fg,
               add_coastlines = add_coastlines,
                add_gridlines = add_gridlines,
         coastlines_edgecolor = coastlines_edgecolor,
         coastlines_facecolor = coastlines_facecolor,
            coastlines_levels = coastlines_levels,
         coastlines_linestyle = coastlines_linestyle,
         coastlines_linewidth = coastlines_linewidth,
        coastlines_resolution = coastlines_resolution,
            coastlines_zorder = coastlines_zorder,
                        debug = debug,
                gridlines_int = gridlines_int,
          gridlines_linecolor = gridlines_linecolor,
          gridlines_linestyle = gridlines_linestyle,
          gridlines_linewidth = gridlines_linewidth,
             gridlines_zorder = gridlines_zorder,
                           gs = gs,
                        index = index,
                        ncols = ncols,
                        nrows = nrows,
                    onlyValid = onlyValid,
                       repair = repair,
    )
