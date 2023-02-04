#!/usr/bin/env python3

# Define function ...
def add_vertical_gridlines(ax, ext, /, *, color = "black", linestyle = ":", linewidth = 0.5, locs = None, ngrid = -1, npoint = 50):
    """Add vertical gridlines to a plot.

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
    locs : list of float, optional
        the locations of the gridlines (in degrees)
    ngrid : int, optional
        the number of gridlines to draw
    npoint : int, optional
        the number of points along each gridline to draw

    Notes
    -----
    If "ngrid" is more than "1" then it is used to create the gridline
    locations, along with "ext". If it is not, then the gridline locations will
    attempt to be made from "locs" instead.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
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

    # Populate default values ...
    if locs is None:
        locs = []

    # Determine x-locations depending on inputs ...
    if ngrid > 1:
        xlocs = numpy.linspace(ext[0], ext[1], num = ngrid)                     # [°]
    elif len(locs) > 0:
        xlocs = numpy.array(locs)                                               # [°]
    else:
        raise Exception("\"ngrid > 1\" is not True and \"len(locs) > 0\" is not True") from None

    # Loop over x-locations ...
    for xloc in xlocs:
        # Add gridline to axis ...
        ax.plot(
            xloc * numpy.ones(npoint),
            numpy.linspace(ext[2], ext[3], num = npoint),
                color = color,
            linestyle = linestyle,
            linewidth = linewidth,
            transform = cartopy.crs.PlateCarree(),
        )

    # Clean up ...
    del xlocs
