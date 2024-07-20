#!/usr/bin/env python3

# Define function ...
def create_map_of_points(
    lons,
    lats,
    pngOut,
    /,
    *,
    chunksize = 1048576,
        debug = False,
          eps = 1.0e-12,
        nIter = 10,
         nmax = 100,
    onlyValid = False,
       prefix = ".",
     ramLimit = 1073741824,
       repair = False,
      timeout = 60.0,
         tol1 = 1.0e-10,
         tol2 = 1.0e3,
):
    """Save a PNG map of a sequence of points

    This function accepts a sequence of longitudes and latitudes then saves a
    PNG map containing all of them drawn together in a big line.

    Parameters
    ----------
    lons : numpy.ndarray
        the sequence of longitudes
    lats : numpy.ndarray
        the sequence of latitudes
    pngOut : str
        the name of the output PNG
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    debug : bool, optional
        print debug messages and draw the circle on the axis
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    nIter : int, optional
        the maximum number of iterations
    nmax : int, optional
        the maximum number of the Vincenty formula iterations
    onlyValid : bool, optional
        only return valid Polygons (checks for validity can take a while, if
        being called often)
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)
    repair : bool, optional
        attempt to repair invalid Polygons
    timeout : float, optional
        the timeout for any requests/subprocess calls (in seconds)
    tol1 : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)
    tol2 : float, optional
        the Geodetic distance that defines the middle as being converged (in
        metres)

    Notes
    -----
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
        import cartopy.io.img_tiles
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

    # Import sub-functions ...
    from .add_axis import add_axis
    from .find_middle_of_locs import find_middle_of_locs
    from ..image import optimize_image
    from ..openstreetmap import zoom

    # **************************************************************************

    # Find the centre of the points ...
    midLon, midLat, maxDist = find_middle_of_locs(
        lons,
        lats,
        debug = debug,
          eps = eps,
        nIter = nIter,
         nmax = nmax,
          pad = 12.0 * 1852.0,
          tol = tol2,
    )                                                                           # [°], [°], [m]
    if debug:
        print(f"INFO: Centre at (lon={midLon:+.6f}°, lat={midLat:+.6f}°) with a {0.001 * maxDist:,.1f} km radius.")

    # **************************************************************************

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (7.2, 7.2))

    # Create axis ...
    ax = add_axis(
        fg,
            debug = debug,
             dist = maxDist,
              eps = eps,
              lat = midLat,
              lon = midLon,
             nmax = nmax,
        onlyValid = onlyValid,
           prefix = prefix,
         ramLimit = ramLimit,
           repair = repair,
              tol = tol1,
    )

    # Calculate the resolution (and zoom) depending on the half-height of the
    # figure and the resolution of the figure ...
    res = maxDist / (0.5 * fg.get_size_inches()[1] * fg.dpi)                    # [m/px]
    z = zoom(midLat, res)
    if debug:
        print(f"INFO: The resolution is {0.001 * res:,.1f} km/px and the OpenStreetMap zoom is {z:d}.")

    # Add OpenStreetMap tiles ...
    # NOTE: It appears that the background image is drawn at only 72 dpi. If you
    #       zoom in then there are around 4 pixels in the red lines joining the
    #       points together for every pixel in the background image (300 / 72 ~=
    #       4). Try regenerating a map at 72 dpi and the background image will
    #       be the same resolution as the red lines joining the points together.
    # NOTE: Line 528 of "cartopy/lib/cartopy/mpl/geoaxes.py" is the "imshow()"
    #       call.
    # NOTE: Line 71 of "cartopy/lib/cartopy/io/img_tiles.py" is the
    #       "image_for_domain()" definition.
    # NOTE: Line 588 of "cartopy/lib/cartopy/io/img_tiles.py" is the
    #       "_merge_tiles()" definition. My only guess is that some pixels are
    #       not getting sliced correctly when making the merged image because
    #       you should never do floating-point equalities on line 608.
    osm = cartopy.io.img_tiles.OSM(
        cache = True,
    )
    ax.add_image(
        osm,
        z,
        interpolation = "none",         # NOTE: Due to the use of **kwargs
                                        #       within Cartopy, this is passed
                                        #       all the way down the stack to
                                        #       the ".imshow()" call.
    )

    # Plot locations ...
    # NOTE: As of 5/Dec/2023, the default "zorder" of the coastlines is 1.5, the
    #       default "zorder" of the gridlines is 2.0 and the default "zorder" of
    #       the scattered points is 1.0.
    ax.plot(
        lons,
        lats,
            color = (1.0, 0.0, 0.0, 0.5),
        linewidth = 1.0,
        transform = cartopy.crs.Geodetic(),
    )
    ax.scatter(
        lons,
        lats,
             color = (1.0, 0.0, 0.0, 0.5),
        edgecolors = (1.0, 0.0, 0.0, 1.0),
         linewidth = 0.1,
                 s = 64.0,
         transform = cartopy.crs.Geodetic(),
            zorder = 5.0,
    )

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(pngOut)
    matplotlib.pyplot.close(fg)

    # Optimize PNG ...
    optimize_image(
        pngOut,
        chunksize = chunksize,
            debug = debug,
            strip = True,
          timeout = timeout,
    )
