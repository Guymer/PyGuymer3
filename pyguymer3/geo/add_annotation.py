#!/usr/bin/env python3

# Define function ...
def add_annotation(
    ax,
    locLon,
    locLat,
    annotation,
    /,
    *,
             arrowprops = None,
                  color = "black",
               fontsize = 8,
    horizontalalignment = "center",
                 txtLat = None,
                 txtLon = None,
             txtOffsetX = None,
             txtOffsetY = None,
      verticalalignment = "center",
):
    """Add an annotation to a Cartopy axis.

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the annotation to
    locLon : float
        the longitude of the annotation location (in degrees)
    locLat : float
        the latitude of the annotation location (in degrees)
    annotation : str
        the annotation text
    arrowprops : dict, optional
        the properties for the arrow connecting the annotation text to the
        annotation location
    color : str, optional
        the colour of the annotation text
    fontsize : int, optional
        the font size of the annotation text
    horizontal alignment : str, optional
        the vertical alignment of the annotation text
    txtLon : float, optional
        the longitude of the annotation text, which implies an arrow to connect
        it to the annotated location (in degrees)
    txtLat : float, optional
        the latitude of the annotation text, which implies an arrow to connect
        it to the annotated location (in degrees)
    txtOffsetX : int or float, optional
        the horizontal offset of the annotation text, which implies an arrow to
        connect it to the annotated location (in points)
    txtOffsetY : int or float, optional
        the vertical offset of the annotation text, which implies an arrow to
        connect it to the annotated location (in points)
    vertical alignment : str, optional
        the vertical alignment of the annotation text

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
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
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # **************************************************************************

    # Create Point(s) ...
    point1loc = shapely.geometry.point.Point(locLon, locLat)
    if txtLon is not None and txtLat is not None:
        point1txt = shapely.geometry.point.Point(txtLon, txtLat)

    # Project the Point(s) into the axis' units ...
    point2loc = ax.projection.project_geometry(point1loc)
    if txtLon is not None and txtLat is not None:
        point2txt = ax.projection.project_geometry(point1txt)

    # Annotate the axis ...
    if txtLon is None and txtLat is None and txtOffsetX is None and txtOffsetY is None:
        ax.annotate(
            annotation,
            (point2loc.coords[0][0], point2loc.coords[0][1]),
                          color = color,
                       fontsize = fontsize,
            horizontalalignment = horizontalalignment,
              verticalalignment = verticalalignment,
        )
    elif txtOffsetX is not None and txtOffsetY is not None:
        ax.annotate(
            annotation,
            (point2loc.coords[0][0], point2loc.coords[0][1]),
                     arrowprops = arrowprops,
                          color = color,
                       fontsize = fontsize,
            horizontalalignment = horizontalalignment,
                     textcoords = "offset points",
              verticalalignment = verticalalignment,
                         xytext = (txtOffsetX, txtOffsetY),
        )
    elif txtLon is not None and txtLat is not None:
        ax.annotate(
            annotation,
            (point2loc.coords[0][0], point2loc.coords[0][1]),
                     arrowprops = arrowprops,
                          color = color,
                       fontsize = fontsize,
            horizontalalignment = horizontalalignment,
              verticalalignment = verticalalignment,
                         xytext = (point2txt.coords[0][0], point2txt.coords[0][1]),
        )
    else:
        raise Exception("there is a bizarre combination of \"txtLon\", \"txtLat\", \"txtOffsetX\" and \"txtOffsetY\"") from None
