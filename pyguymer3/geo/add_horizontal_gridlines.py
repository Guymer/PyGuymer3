def add_horizontal_gridlines(ax, ext, kwArgCheck = None, color = "black", linestyle = ":", linewidth = 0.5, ngrid = 5, npoint = 50):
    """Add horizontal gridlines to a plot.

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the gridlines to
    ext : list of float
        the extent of the axis (in degrees)
    color : str, optional
        the colour of the gridlines
    linestyle : str, optional
        the style of the gridlines
    linewidth : float, optional
        the width of the gridlines
    ngrid : int, optional
        the number of gridlines to draw
    npoint : int, optional
        the number of points along each gridline to draw
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

    # Determine y-locations ...
    ylocs = numpy.linspace(ext[2], ext[3], num = ngrid)                         # [°]

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
