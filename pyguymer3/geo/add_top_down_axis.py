#!/usr/bin/env python3

# Define function ...
def add_top_down_axis(fg, lon, lat, dist, /, *, add_gridlines = False, debug = False, gs = None, index = None, ncols = None, nrows = None):
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
    gs : matplotlib.gridspec.SubplotSpec, optional
        the subset of a gridspec to locate the axis
    nrows : int, optional
        the number of rows in the array of axes
    ncols : int, optional
        the number of columns in the array of axes
    index : int or tuple of int, optional
        the index of the axis in the array of axes

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
             fill = +1.0,
             nang = 361,
             simp = -1.0,
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
            locs = range(-90, +91),
        )
        add_vertical_gridlines(
            ax,
            locs = range(-180, +181),
        )

    # Return answer ...
    return ax
