#!/usr/bin/env python3

# Define function ...
def add_horizontal_gridlines(axis, /, *, color = "black", linestyle = ":", linewidth = 0.5, locs = None, ngrid = -1, npoint = 361):
    """Add horizontal gridlines to a Cartopy axis.

    Parameters
    ----------
    axis : cartopy.mpl.geoaxes.GeoAxesSubplot
        the axis to add the gridlines to
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
    If ``ngrid`` is more than "1" then it is used to create the gridline
    locations. If it is not, then the gridline locations will attempt to be made
    from ``locs`` instead.

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
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Create short-hand ...
    ext = axis.get_extent(crs = cartopy.crs.PlateCarree())                        # [°]

    # Determine y-locations depending on inputs ...
    if ngrid > 1:
        ylocs = numpy.linspace(ext[2], ext[3], num = ngrid)                     # [°]
    elif locs is not None:
        ylocs = numpy.array(locs)                                               # [°]
    else:
        raise Exception("\"ngrid > 1\" is not True and \"locs is not None\" is not True") from None

    # Loop over y-locations ...
    for yloc in ylocs:
        # Add gridline to axis ...
        axis.plot(
            numpy.linspace(ext[0], ext[1], num = npoint),
            yloc * numpy.ones(npoint),
                color = color,
            linestyle = linestyle,
            linewidth = linewidth,
            transform = cartopy.crs.PlateCarree(),
        )
