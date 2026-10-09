#!/usr/bin/env python3

# Define function ...
def _add_topDown_axis(
    fg,
    lon,
    lat,
    /,
    *,
           add_background = False,
           add_coastlines = True,
            add_gridlines = True,
           attemptFortran = True,
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
                      fov = None,
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
                      tol = 1.0e-10,
):
    """Add an AzimuthalEquidistant axis centred above a point with optionally a
    field-of-view based on a circle around the point on the surface of the Earth

    Parameters
    ----------
    fg : matplotlib.figure.Figure
        the figure to add the axis to
    lon : float
        the longitude of the point (in degrees)
    lat : float
        the latitude of the point (in degrees)
    add_background : bool, optional
        add background
    add_coastlines : bool, optional
        add coastline boundaries
    add_gridlines : bool, optional
        add gridlines of longitude and latitude
    attemptFortran : bool, optional
        attempt to use a f2py implementation
    coastlines_edgecolor : str, optional
        the colour of the edges of the coastline Polygons
    coastlines_facecolor : str, optional
        the colour of the faces of the coastline Polygons
    coastlines_levels : list of int, optional
        the levels of the coastline boundaries (if None then default to
        ``(1, 5, 6,)``)
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
    fov : None or shapely.geometry.polygon.Polygon, optional
        clip the plotted shapes to the provided field-of-view to work around
        occasional MatPlotLib or Cartopy plotting errors when shapes much larger
        than the field-of-view are plotted
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
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

    Returns
    -------
    ax : cartopy.mpl.geoaxes.GeoAxes
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
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy").expanduser(),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "axes.xmargin" : 0.01,
                       "axes.ymargin" : 0.01,
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",                                 # NOTE: See https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html
                     "image.resample" : False,
            }
        )
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from ._add_background import _add_background
    from ._add_coastlines import _add_coastlines
    from ._add_horizontal_gridlines import _add_horizontal_gridlines
    from ._add_vertical_gridlines import _add_vertical_gridlines
    from ._set_axis_boundary import _set_axis_boundary
    from .buffer import buffer
    from .calc_dist_between_two_locs import calc_dist_between_two_locs
    from .clean import clean
    from .extract_polys import extract_polys
    from .._consts import GEODETIC, MAXIMUM_VINCENTY, RADIUS_OF_EARTH, WGS84

    # **************************************************************************

    # Create short-hand ...
    huge = bool(dist > 0.5 * MAXIMUM_VINCENTY)

    # Check inputs ...
    if gridlines_int is None:
        if huge:
            gridlines_int = 45                                                  # [°]
        else:
            gridlines_int = 1                                                   # [°]

    # Create a Point ...
    point1 = shapely.geometry.point.Point(lon, lat)

    # Check where the axis should be created ...
    # NOTE: See https://scitools.org.uk/cartopy/docs/latest/reference/projections.html
    if gs is not None:
        # Create AzimuthalEquidistant axis ...
        ax = fg.add_subplot(
            gs,
            projection = cartopy.crs.AzimuthalEquidistant(
                central_longitude = point1.x,
                 central_latitude = point1.y,
                            globe = WGS84,
            ),
        )
    elif nrows is not None and ncols is not None and index is not None:
        # Create AzimuthalEquidistant axis ...
        ax = fg.add_subplot(
            nrows,
            ncols,
            index,
            projection = cartopy.crs.AzimuthalEquidistant(
                central_longitude = point1.x,
                 central_latitude = point1.y,
                            globe = WGS84,
            ),
        )
    else:
        # Create AzimuthalEquidistant axis ...
        ax = fg.add_subplot(
            projection = cartopy.crs.AzimuthalEquidistant(
                central_longitude = point1.x,
                 central_latitude = point1.y,
                            globe = WGS84,
            ),
        )

    # Check if the field-of-view is too large ...
    if huge:
        # Configure axis ...
        ax.set_global()
    else:
        # Buffer the Point ...
        polygon1 = buffer(
            point1,
            dist,
            attemptFortran = attemptFortran,
                     debug = debug,
                       eps = eps,
                      fill = +1.0,                                              # NOTE: Need to fill in the result to
                 fillSpace = "EuclideanSpace",                                  #       undo the ".simplify(tol)" in
             keepInteriors = False,                                             #       "buffer_CoordinateSequence()".
                      nAng = 361,
                     nIter = nIter,
                    prefix = prefix,
                  ramLimit = ramLimit,
                      simp = -1.0,
                       tol = tol,
        )

        # Set extent ...
        _, _ = _set_axis_boundary(
            ax,
            point1,
            polygon1,
            dist,
              eps = eps,
            nIter = nIter,
              tol = tol,
        )

        # Check if the user wants to draw the circle ...
        if debug:
            # Draw the circle ...
            ax.add_geometries(
                [polygon1],
                GEODETIC,
                edgecolor = (0.0, 0.0, 1.0, 1.0),
                facecolor = (0.0, 0.0, 1.0, 0.5),
                linewidth = 1.0,
            )

    # Check if the user wants to add background ...
    if add_background:
        # Add background ...
        _add_background(
            ax,
            debug = debug,
        )

    # Check if the user wants to add coastline boundaries ...
    if add_coastlines:
        # Add coastline boundaries ...
        _add_coastlines(
            ax,
                  debug = debug,
              edgecolor = coastlines_edgecolor,
              facecolor = coastlines_facecolor,
                    fov = fov,
            gshhgLevels = coastlines_levels,
               gshhgRes = coastlines_resolution,
              linestyle = coastlines_linestyle,
              linewidth = coastlines_linewidth,
              onlyValid = onlyValid,
                 repair = repair,
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
