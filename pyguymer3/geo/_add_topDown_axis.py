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
           configureAgain = False,
                    debug = __debug__,
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
                    nIter = 100,
                    nrows = None,
                onlyValid = False,
                   prefix = ".",
                 ramLimit = 1073741824,
                   repair = False,
         satellite_height = False,
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
    configureAgain : bool, optional
        configure the axis a second time (this is a hack to make narrow
        field-of-view top-down axes work correctly with OpenStreetMap tiles)
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
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)
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
    satellite_height : bool, optional
        if a distance is provided then use a "NearsidePerspective" projection at
        an altitude which has the same field-of-view as the distance
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
    import math
    import pathlib

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy_cache").expanduser(),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "backend" : "Agg",                                       # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                    "figure.dpi" : 300,
                "figure.figsize" : (9.6, 7.2),                                  # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                     "font.size" : 8,
            }
        )
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
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
    from .clean import clean
    from ..consts import CIRCUMFERENCE_OF_EARTH, RADIUS_OF_EARTH

    # **************************************************************************

    # Create short-hand ...
    globalFieldOfView = bool(dist > 0.25 * CIRCUMFERENCE_OF_EARTH)

    # Check inputs ...
    if gridlines_int is None:
        if globalFieldOfView:
            gridlines_int = 45                                                  # [°]
        else:
            gridlines_int = 1                                                   # [°]
    if not globalFieldOfView and satellite_height:
        alt = RADIUS_OF_EARTH / math.cos(dist / RADIUS_OF_EARTH) - RADIUS_OF_EARTH  # [m]

    # Create a Point ...
    point1 = shapely.geometry.point.Point(lon, lat)

    # Check where the axis should be created ...
    # NOTE: See https://scitools.org.uk/cartopy/docs/latest/reference/projections.html
    if gs is not None:
        # Check if a NearsidePerspective axis can be used ...
        if not globalFieldOfView and satellite_height:
            # Create NearsidePerspective axis ...
            ax = fg.add_subplot(
                gs,
                projection = cartopy.crs.NearsidePerspective(
                    central_longitude = point1.x,
                     central_latitude = point1.y,
                     satellite_height = alt,
                ),
            )
        else:
            # Create Orthographic axis ...
            ax = fg.add_subplot(
                gs,
                projection = cartopy.crs.Orthographic(
                    central_longitude = point1.x,
                     central_latitude = point1.y,
                ),
            )
    elif nrows is not None and ncols is not None and index is not None:
        # Check if a NearsidePerspective axis can be used ...
        if not globalFieldOfView and satellite_height:
            # Create NearsidePerspective axis ...
            ax = fg.add_subplot(
                nrows,
                ncols,
                index,
                projection = cartopy.crs.NearsidePerspective(
                    central_longitude = point1.x,
                     central_latitude = point1.y,
                     satellite_height = alt,
                ),
            )
        else:
            # Create Orthographic axis ...
            ax = fg.add_subplot(
                nrows,
                ncols,
                index,
                projection = cartopy.crs.Orthographic(
                    central_longitude = point1.x,
                     central_latitude = point1.y,
                ),
            )
    else:
        # Check if a NearsidePerspective axis can be used ...
        if not globalFieldOfView and satellite_height:
            # Create NearsidePerspective axis ...
            ax = fg.add_subplot(
                projection = cartopy.crs.NearsidePerspective(
                    central_longitude = point1.x,
                     central_latitude = point1.y,
                     satellite_height = alt,
                ),
            )
        else:
            # Create Orthographic axis ...
            ax = fg.add_subplot(
                projection = cartopy.crs.Orthographic(
                    central_longitude = point1.x,
                     central_latitude = point1.y,
                ),
            )

    # Check if the field-of-view is too large ...
    if globalFieldOfView:
        # Configure axis ...
        ax.set_global()
    elif not satellite_height:
        # Buffer the Point ...
        polygon1 = buffer(
            point1,
            dist,
                    debug = debug,
                      eps = eps,
                     fill = +1.0,
                fillSpace = "EuclideanSpace",
            keepInteriors = False,
                     nAng = 361,
                    nIter = nIter,
                   prefix = prefix,
                 ramLimit = ramLimit,
                     simp = -1.0,
                      tol = tol,
        )

        # Calculate Northern extent ...
        tmpLon, latMax, _ = calc_loc_from_loc_and_bearing_and_dist(
            lon,
            lat,
            0.0,
            dist,
              eps = 1.0e-12,
            nIter = 100,
        )                                                                       # [°], [°]
        tmpPnt = shapely.geometry.point.Point(tmpLon, latMax)
        yMax = ax.projection.project_geometry(tmpPnt).y                         # [?]

        # Calculate Eastern extent ...
        lonIter, tmpLat, _ = calc_loc_from_loc_and_bearing_and_dist(
            lon,
            lat,
            90.0,
            dist,
              eps = 1.0e-12,
            nIter = 100,
        )                                                                       # [°], [°]
        tmpPnt = shapely.geometry.point.Point(lonIter, tmpLat)
        xMax = ax.projection.project_geometry(tmpPnt).x                         # [?]

        # Calculate Southern extent ...
        tmpLon, latMin, _ = calc_loc_from_loc_and_bearing_and_dist(
            lon,
            lat,
            180.0,
            dist,
              eps = 1.0e-12,
            nIter = 100,
        )                                                                       # [°], [°]
        tmpPnt = shapely.geometry.point.Point(tmpLon, latMin)
        yMin = ax.projection.project_geometry(tmpPnt).y                         # [?]

        # Calculate Western extent ...
        lonMin, tmpLat, _ = calc_loc_from_loc_and_bearing_and_dist(
            lon,
            lat,
            270.0,
            dist,
              eps = 1.0e-12,
            nIter = 100,
        )                                                                       # [°], [°]
        tmpPnt = shapely.geometry.point.Point(lonMin, tmpLat)
        xMin = ax.projection.project_geometry(tmpPnt).x                         # [?]

        # Project the Point ...
        point2 = ax.projection.project_geometry(point1)

        # Create a correctly oriented Polygon from scratch that is the Point
        # buffered in MatPlotLib space with the same fuzziness as Cartopy does
        # internally ...
        radius2 = numpy.array(
            [
                point2.x - xMin,
                xMax - point2.x,
                point2.y - yMin,
                yMax - point2.y,
            ],
            dtype = numpy.float64,
        ).mean()                                                                # [?]
        polygon2 = point2.buffer(radius2 * 0.99999)
        polygon2 = clean(
            polygon2,
             debug = debug,
            prefix = prefix,
               tol = tol,
        )

        # Convert the exterior ring of the Polygon to a Path ...
        path = matplotlib.path.Path(polygon2.exterior.coords)

        # Configure axis ...
        # NOTE: The orthographic projection does not have the ability to set
        #       either the altitude or the field-of-view. I manually do this,
        #       which involves setting the boundary and the limits for the
        #       MatPlotLib axis.
        ax.set_boundary(path)
        ax.set_xlim(xMin, xMax)
        ax.set_ylim(yMin, yMax)

        # Check if the user wants to configure the axis a second time ...
        if configureAgain:
            # Configure axis again ...
            # NOTE: For some reason, "cartopy.io.img_tiles.OSM()" doesn't work
            #       unless the first of the following protected members is also
            #       set. All other interactions with the axis appear to be fine
            #       without it being set though. Annoyingly, once the first
            #       protected member is set, all other interactions with the
            #       axis fail unless the second and third protected members are
            #       also set. If the second and third protected members are set
            #       then the resulting gridlines do not extend all the way to
            #       the edge of the map. Therefore, I have chosen to not set the
            #       second and third protected members and instead I protect
            #       setting the first protect member by checking if the
            #       background is going to be "OSM".
            ax.projection._boundary = polygon2.exterior                         # pylint: disable=W0212
            # ax.projection._cw_boundary = polygon2.exterior.reverse()
            # ax.projection._ccw_boundary = polygon2.exterior

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
