#!/usr/bin/env python3

# Define function ...
def _add_topDown_axis(
    fg,
    lon,
    lat,
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
                    ncols = None,
                     nMax = 100,
                    nrows = None,
                onlyValid = False,
                   prefix = ".",
                 ramLimit = 1073741824,
                   repair = False,
                      tol = 1.0e-10,
):
    """Add an Orthographic axis centred above a point with optionally a
    field-of-view based on a circle around the point on the surface of the Earth

    Parameters
    ----------
    fg : matplotlib.figure.Figure
        the figure to add the axis to
    lon : float
        the longitude of the point (in degrees)
    lat : float
        the latitude of the point (in degrees)
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
    ncols : int, optional
        the number of columns in the array of axes
    nMax : int, optional
        the maximum number of Vincenty formula iterations
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
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._add_coastlines import _add_coastlines
    from ._add_horizontal_gridlines import _add_horizontal_gridlines
    from ._add_vertical_gridlines import _add_vertical_gridlines
    from .buffer import buffer
    from .calc_loc_from_loc_and_bearing_and_dist import calc_loc_from_loc_and_bearing_and_dist
    from ..consts import CIRCUMFERENCE_OF_EARTH

    # **************************************************************************

    # Check inputs ...
    if gridlines_int is None:
        if dist > 0.25 * CIRCUMFERENCE_OF_EARTH:
            gridlines_int = 45                                                  # [°]
        else:
            gridlines_int = 1                                                   # [°]

    # Create a Point ...
    point = shapely.geometry.point.Point(lon, lat)

    # Check where the axis should be created ...
    if gs is not None:
        # Create axis ...
        ax = fg.add_subplot(
            gs,
            projection = cartopy.crs.Orthographic(
                central_longitude = point.x,
                 central_latitude = point.y,
            ),
        )
    elif nrows is not None and ncols is not None and index is not None:
        # Create axis ...
        ax = fg.add_subplot(
            nrows,
            ncols,
            index,
            projection = cartopy.crs.Orthographic(
                central_longitude = point.x,
                 central_latitude = point.y,
            ),
        )
    else:
        # Create axis ...
        ax = fg.add_subplot(
            projection = cartopy.crs.Orthographic(
                central_longitude = point.x,
                 central_latitude = point.y,
            ),
        )

    # Check if the field-of-view is too large ...
    if dist > 0.25 * CIRCUMFERENCE_OF_EARTH:
        # Configure axis ...
        ax.set_global()
    else:
        # Buffer the Point ...
        polygon1 = buffer(
            point,
            dist,
                    debug = debug,
                      eps = eps,
                     fill = +1.0,
                fillSpace = "EuclideanSpace",
            keepInteriors = False,
                     nang = 361,
                     nMax = nMax,
                   prefix = prefix,
                 ramLimit = ramLimit,
                     simp = -1.0,
                      tol = tol,
        )

        # Calculate Northern extent ...
        tmpLon, tmpLat, _ = calc_loc_from_loc_and_bearing_and_dist(
            lon,
            lat,
            0.0,
            dist,
             eps = 1.0e-12,
            nMax = 100,
        )                                                                       # [°], [°]
        ymax = shapely.geometry.point.Point(tmpLon, tmpLat)
        ymax = ax.projection.project_geometry(ymax).y

        # Calculate Eastern extent ...
        tmpLon, tmpLat, _ = calc_loc_from_loc_and_bearing_and_dist(
            lon,
            lat,
            90.0,
            dist,
             eps = 1.0e-12,
            nMax = 100,
        )                                                                       # [°], [°]
        xmax = shapely.geometry.point.Point(tmpLon, tmpLat)
        xmax = ax.projection.project_geometry(xmax).x

        # Calculate Southern extent ...
        tmpLon, tmpLat, _ = calc_loc_from_loc_and_bearing_and_dist(
            lon,
            lat,
            180.0,
            dist,
             eps = 1.0e-12,
            nMax = 100,
        )                                                                       # [°], [°]
        ymin = shapely.geometry.point.Point(tmpLon, tmpLat)
        ymin = ax.projection.project_geometry(ymin).y

        # Calculate Western extent ...
        tmpLon, tmpLat, _ = calc_loc_from_loc_and_bearing_and_dist(
            lon,
            lat,
            270.0,
            dist,
             eps = 1.0e-12,
            nMax = 100,
        )                                                                       # [°], [°]
        xmin = shapely.geometry.point.Point(tmpLon, tmpLat)
        xmin = ax.projection.project_geometry(xmin).x

        # Configure axis ...
        ax.set_xlim(xmin, xmax)
        ax.set_ylim(ymin, ymax)

        # Check if the user wants to draw the circle ...
        if debug:
            # Draw the circle ...
            ax.add_geometries(
                [polygon1],
                cartopy.crs.PlateCarree(),
                edgecolor = (0.0, 0.0, 1.0, 1.0),
                facecolor = (0.0, 0.0, 1.0, 0.5),
                linewidth = 1.0,
            )

    # Check if the user wants to add coastline boundaries ...
    if add_coastlines:
        # Add coastline boundaries ...
        _add_coastlines(
            ax,
                 debug = debug,
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
