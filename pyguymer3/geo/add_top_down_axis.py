#!/usr/bin/env python3

# Define function ...
def add_top_down_axis(fg, lon, lat, dist, /, *, add_gridlines = False, debug = False, eps = 1.0e-12, fill = 1.0, fillSpace = "EuclideanSpace", gridline_int = 1, gs = None, index = None, keepInteriors = True, nang = 9, ncols = None, nmax = 100, nrows = None, prefix = ".", ramLimit = 1073741824, simp = 0.1, tol = 1.0e-10):
    """Add an Orthographic axis to a figure with a field-of-view based on a
    circle around a point on the surface of the Earth

    Parameters
    ----------
    fg : matplotlib.figure.Figure
        the figure to add the axis to
    lon : float
        the longitude of the point (in degrees)
    lat : float
        the latitude of the point (in degrees)
    dist : float
        the radius of the circle around the point (in metres)
    add_gridlines : bool, optional
        add gridlines every degree of longitude and latitude
    debug : bool, optional
        print debug messages and draw the circle on the axis
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    fill : float, optional
        the Euclidean or Geodetic distance to fill in between each point within
        the shapes by (in degrees or metres)
    fillSpace : str, optional
        the geometric space to perform the filling in (either "EuclideanSpace"
        or "GeodesicSpace")
    gridline_int : int, optional
        the interval between gridlines, best results if ``90 % gridline_int == 0`` (in degrees)
    gs : matplotlib.gridspec.SubplotSpec, optional
        the subset of a gridspec to locate the axis
    index : int or tuple of int, optional
        the index of the axis in the array of axes
    keepInteriors : bool, optional
        keep the interiors of the Polygon
    nang : int, optional
        the number of angles around each point within the shape that are
        calculated when buffering
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
    simp : float, optional
        how much intermediary [Multi]Polygons are simplified by; negative values
        disable simplification (in degrees)
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)

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

    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .add_horizontal_gridlines import add_horizontal_gridlines
    from .add_vertical_gridlines import add_vertical_gridlines
    from .buffer import buffer
    from ..consts import CIRCUMFERENCE_OF_EARTH

    # **************************************************************************

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
                     fill = fill,
                fillSpace = fillSpace,
            keepInteriors = keepInteriors,
                     nang = nang,
                     nmax = nmax,
                   prefix = prefix,
                 ramLimit = ramLimit,
                     simp = simp,
                      tol = tol,
        )

        # Project the Polygon into the axis' units ...
        polygon2 = ax.projection.project_geometry(polygon1)

        # Create short-hands ...
        xmin, ymin, xmax, ymax = polygon2.bounds

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

    # Check if the user wants to add gridlines ...
    if add_gridlines:
        # Add gridlines ...
        add_horizontal_gridlines(
            ax,
            locs = range( -90,  +90 + gridline_int, gridline_int),
        )
        add_vertical_gridlines(
            ax,
            locs = range(-180, +180 + gridline_int, gridline_int),
        )

    # Return answer ...
    return ax
