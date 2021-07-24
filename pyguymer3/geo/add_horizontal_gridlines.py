def add_horizontal_gridlines(ax, kwArgCheck = None, color = "black", ext = [], linestyle = ":", linewidth = 0.5, locs = [], ngrid = -1, npoint = 50):
    """Add horizontal gridlines to a plot.

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the gridlines to
    color : str, optional
        the colour of the gridlines
    ext : list of float, optional
        the extent of the axis (in degrees)
    linestyle : str, optional
        the style of the gridlines
    linewidth : float, optional
        the width of the gridlines
    locs : list of float, optional
        the locations of the gridlines (in degrees)
    ngrid : int, optional
        the number of gridlines to draw (requires "ext")
    npoint : int, optional
        the number of points along each gridline to draw

    Notes
    -----
    If "ngrid" is more than "1" then it is used to create the gridline locations, along with "ext". If it is not, then the gridline locations will attempt to be made from "locs" instead.
    """

    # Import special modules ...
    try:
        import cartopy
        import cartopy.crs
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check inputs ...
    if ngrid > 1 and len(ext) != 4:
        raise Exception("\"ngrid > 1\" but \"len(ext) != 4\"") from None

    # Determine y-locations depending on inputs ...
    if ngrid > 1:
        ylocs = numpy.linspace(ext[2], ext[3], num = ngrid)                     # [°]
    elif len(locs) > 0:
        ylocs = numpy.array(locs)                                               # [°]
    else:
        raise Exception("\"ngrid > 1\" is not True and \"len(locs) > 0\" is not True") from None

    # Loop over y-locations ...
    for yloc in ylocs:
        # Add gridline to axis ...
        ax.plot(
            numpy.linspace(ext[0], ext[1], num = npoint),
            yloc * numpy.ones(npoint),
            transform = cartopy.crs.PlateCarree(),
                color = color,
            linestyle = linestyle,
            linewidth = linewidth
        )

    # Clean up ...
    del ylocs
