#!/usr/bin/env python3

# Define function ...
def _add_vertical_gridlines(
    ax,
    /,
    *,
        color = "black",
    linestyle = ":",
    linewidth = 0.5,
         locs = None,
        ngrid = -1,
       npoint = 181,
       zorder = 2.0,
):
    """Add vertical gridlines to a Cartopy axis.

    Parameters
    ----------
    ax : cartopy.mpl.geoaxes.GeoAxesSubplot
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
    zorder : float, optional
        the zorder to draw the gridlines with (the default value has been chosen
        to match the value that it ends up being if the gridlines are not drawn
        with the zorder keyword specified -- obtained by manual inspection on
        5/Dec/2023)

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

    # Determine x-locations depending on inputs ...
    if ngrid > 1:
        xlocs = numpy.linspace(-180.0, +180.0, num = ngrid)                     # [°]
    elif locs is not None:
        xlocs = numpy.array(locs)                                               # [°]
    else:
        raise Exception("\"ngrid > 1\" is not True and \"locs is not None\" is not True") from None

    # Loop over x-locations ...
    for xloc in xlocs:
        # Add gridline to axis ...
        ax.plot(
            xloc * numpy.ones(npoint),
            numpy.linspace(-90.0, +90.0, num = npoint),
                color = color,
            linestyle = linestyle,
            linewidth = linewidth,
            transform = cartopy.crs.PlateCarree(),
               zorder = zorder,
        )
