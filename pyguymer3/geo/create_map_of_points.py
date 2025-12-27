#!/usr/bin/env python3

# Define function ...
def create_map_of_points(
    pntLons,
    pntLats,
    pngOut,
    /,
    *,
             angConv = 0.1,
          background = "NE",
                ceil = True,
           chunksize = 1048576,
                conv = 1.0e3,
               debug = __debug__,
                 eps = 1.0e-12,
        exiftoolPath = None,
              extent = None,
           fillColor = (255.0 / 255.0,   0.0 / 255.0,   0.0 / 255.0),
               floor = False,
                 fov = None,
        gifsiclePath = None,
       interpolation = "none",
        jpegtranPath = None,
              method = "GeodesicBox",
                name = "natural-earth-1",
                nAng = 9,
               nIter = 100,
             nRefine = 1,
           onlyValid = False,
         optipngPath = None,
             padDist = 12.0 * 1852.0,
              prefix = ".",
            ramLimit = 1073741824,
              repair = False,
            resample = False,
          resolution = "10m",
               route = None,
      routeFillColor = (  0.0 / 255.0, 128.0 / 255.0,   0.0 / 255.0),
    satellite_height = False,
               scale = 1,
       skipFillColor = (255.0 / 255.0, 165.0 / 255.0,   0.0 / 255.0),
               skips = None,
    thunderforestKey = None,
    thunderforestMap = "atlas",
             timeout = 60.0,
               title = None,
                 tol = 1.0e-10,
            useSciPy = False,
                   z = None,
):
    """Save a PNG map of a sequence of points

    This function accepts a sequence of longitudes and latitudes then saves a
    PNG map containing all of them drawn together in a big line.

    Parameters
    ----------
    pntLons : numpy.ndarray
        the sequence of longitudes
    pntLats : numpy.ndarray
        the sequence of latitudes
    pngOut : str
        the name of the output PNG
    angConv : float, optional
        the angle change which classifies as converged (in degrees)
    background : str, optional
        the type of background to add (recognised values are: "GSHHG"; "image";
        "NE"; "none"; and "OSM")
    ceil : bool, optional
        When calculating the tile zoom level from the resolution of the figure
        convert the floating-point answer to an integer using ``math.ceil()``.
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    conv : float, optional
        the Geodesic distance that defines the middle as being converged (in
        metres)
    debug : bool, optional
        print debug messages and draw the circle on the axis
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    exiftoolPath : str, optional
        the path to the "exiftool" binary (if not provided then Python will attempt to
        find the binary itself)
    extent : list of floats
        for high-resolution images, save time by specifying the extent that is
        to be added
    fillColor : tuple of int, optional
        the fill colour of the points
    floor : bool, optional
        When calculating the tile zoom level from the resolution of the figure
        convert the floating-point answer to an integer using ``math.floor()``.
    fov : None or shapely.geometry.polygon.Polygon, optional
        clip the plotted shapes to the provided field-of-view to work around
        occaisional MatPlotLib or Cartopy plotting errors when shapes much
        larger than the field-of-view are plotted
    gifsiclePath : str, optional
        the path to the "gifsicle" binary (if not provided then Python will attempt to
        find the binary itself)
    interpolation : str, optional
        The interpolation method used when drawing the final warped image, or
        the final merged and warped image, on the figure. Due to the use of
        **kwargs within Cartopy, this is passed all the way down the stack to
        the MatPlotLib ".imshow()" call.
    jpegtranPath : str, optional
        the path to the "jpegtran" binary (if not provided then Python will attempt to
        find the binary itself)
    method : str, optional
        the method for finding the middle of the points
    name : str, optional
        the name of the image in the database
    nAng : int, optional
        the number of angles around the middle location to search over
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)
    nRefine : int, optional
        the number of refinements to make (each refinement halves the "conv"
        distance)
    onlyValid : bool, optional
        only return valid Polygons (checks for validity can take a while, if
        being called often)
    optipngPath : str, optional
        the path to the "optipng" binary (if not provided then Python will attempt to
        find the binary itself)
    padDist : float, optional
        the padding to draw around the points (in metres)
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)
    repair : bool, optional
        attempt to repair invalid Polygons
    resample : bool, optional
        Use a full resampling method when drawing the final warped image, or
        the final merged and warped image, on the figure. Due to the use of
        **kwargs within Cartopy, this is passed all the way down the stack to
        the MatPlotLib ".imshow()" call.
    resolution : str, optional
        the resolution of the image or NE dataset or GSHHG dataset
    route : shapely.geometry.linestring.LineString, optional
        an extra line to draw on the map
    routeFillColor : tuple of int, optional
        the fill colour of the extra route
    satellite_height : float, optional
        if a distance is provided then use a "NearsidePerspective" projection at
        an altitude which has the same field-of-view as the distance
    scale : int, optional
        the scale of the tiles
    skipFillColor : tuple of int, optional
        the fill colour of the skipped points
    skips : numpy.ndarray, optional
        an array of booleans as to whether to include/exclude each individual
        point from calculating the image's field-of-view (this allows the great
        circles from flights to be drawn but for them to not expand the image to
        fit in the departing airport); if not provided then all points are used
    thunderforestKey : str, optional
        your personal API key for the Thunderforest service (if provided then it
        is assumed that you want to use the Thunderforest service)
    thunderforestMap : str, optional
        the Thunderforest map style (see https://www.thunderforest.com/maps/)
    timeout : float, optional
        the timeout for any requests/subprocess calls (in seconds)
    title : str, optional
        the title
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)
    useSciPy : bool, optional
        use "scipy.optimize.minimize" or my own minimizer
    z : int, optional
        the OpenStreetMap zoom level

    Notes
    -----
    See the `MatPlotLib documentation about interpolation methods
    <https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html>`_
    and the `MatPlotLib documentation about anti-aliasing
    <https://matplotlib.org/stable/gallery/images_contours_and_fields/image_antialiasing.html>`_.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
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
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",
                     "image.resample" : False,
            }
        )
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import sub-functions ...
    from .add_axis import add_axis
    from .add_GSHHG_map_underlay import add_GSHHG_map_underlay
    from .add_map_background import add_map_background
    from .add_NE_map_underlay import add_NE_map_underlay
    from .add_OSM_map_background import add_OSM_map_background
    from .extract_lines import extract_lines
    from .find_middle_of_locs import find_middle_of_locs
    from .great_circle import great_circle
    from .._consts import CIRCUMFERENCE_OF_EARTH, RESOLUTION_OF_EARTH
    from ..image import optimise_image

    # **************************************************************************

    # Check inputs ...
    if skips is None:
        skips = numpy.zeros(pntLons.size, dtype = bool)

    # **************************************************************************

    # Create figure ...
    fg = matplotlib.pyplot.figure(figsize = (7.2, 7.2))

    # Calculate the regrid shape based off the resolution and the size of the
    # figure, as well as a safety factor of 2 (remembering Nyquist) ...
    regrid_shape = (
        round(2.0 * fg.get_figwidth() * fg.get_dpi()),
        round(2.0 * fg.get_figheight() * fg.get_dpi()),
    )                                                                           # [px], [px]

    if debug:
        print(f"INFO: The final image will be scaled to {regrid_shape[0]:,d} px × {regrid_shape[1]:,d} px.")

    # **************************************************************************

    # Find the centre of the points very quickly ...
    midLonQuick, midLatQuick, maxDistQuick = find_middle_of_locs(
        pntLons[numpy.logical_not(skips)],
        pntLats[numpy.logical_not(skips)],
         angConv = None,
            conv = None,
           debug = debug,
             eps = eps,
          method = "EuclideanBox",
          midLat = None,
          midLon = None,
            nAng = None,
           nIter = nIter,
         nRefine = nRefine,
             pad = -1.0,
        useSciPy = None,
    )                                                                           # [°]

    # Check if the points are so widely spread that the map has to have global
    # extent to show them all ...
    if maxDistQuick > 90.0:
        # Create axis ...
        ax = add_axis(
            fg,
            add_coastlines = False,
             add_gridlines = True,
            configureAgain = bool(background == "OSM"),
                     debug = debug,
                       eps = eps,
                       fov = fov,
                        gs = None,
                     index = None,
                     ncols = None,
                     nIter = nIter,
                     nrows = None,
                 onlyValid = onlyValid,
                    prefix = prefix,
                  ramLimit = ramLimit,
                    repair = repair,
                       tol = tol,
        )

        # Create short-hand ...
        midLat = 0.0                                                            # [°]
    else:
        # If the user asked for a Euclidean method then the padding distance
        # needs converting from metres in to degrees ...
        match method:
            case "EuclideanBox" | "EuclideanCircle":
                padDist /= RESOLUTION_OF_EARTH                                  # [°]
            case "GeodesicBox" | "GeodesicCircle":
                pass
            case _:
                # Crash ...
                raise ValueError(f"\"method\" is an unexpected value ({repr(method)})") from None

        # Find the centre of the points ...
        midLon, midLat, maxDist = find_middle_of_locs(
            pntLons[numpy.logical_not(skips)],
            pntLats[numpy.logical_not(skips)],
             angConv = angConv,
                conv = conv,
               debug = debug,
                 eps = eps,
              method = method,
              midLat = midLatQuick,
              midLon = midLonQuick,
                nAng = nAng,
               nIter = nIter,
             nRefine = nRefine,
                 pad = padDist,
            useSciPy = useSciPy,
        )                                                                       # [°], [°], [°] or [m]

        # Check what method the user wants ...
        match method:
            case "EuclideanBox" | "EuclideanCircle":
                if debug:
                    print(f"INFO: Centre at (lon={midLon:+.6f}°, lat={midLat:+.6f}°) with a {maxDist:.6f}° radius.")
            case "GeodesicBox" | "GeodesicCircle":
                if debug:
                    print(f"INFO: Centre at (lon={midLon:+.6f}°, lat={midLat:+.6f}°) with a {0.001 * maxDist:,.1f} km radius.")
            case _:
                # Crash ...
                raise ValueError(f"\"method\" is an unexpected value ({repr(method)})") from None

        # If the user asked for a Euclidean method then the maximum distance
        # needs converting from degrees in to metres ...
        match method:
            case "EuclideanBox" | "EuclideanCircle":
                maxDist *= RESOLUTION_OF_EARTH                                  # [m]
                if debug:
                    print(f"INFO: Centre at (lon={midLon:+.6f}°, lat={midLat:+.6f}°) with a {0.001 * maxDist:,.1f} km radius.")
            case "GeodesicBox" | "GeodesicCircle":
                pass
            case _:
                # Crash ...
                raise ValueError(f"\"method\" is an unexpected value ({repr(method)})") from None

        # Create axis ...
        ax = add_axis(
            fg,
              add_coastlines = False,
               add_gridlines = True,
              configureAgain = bool(background == "OSM"),
                       debug = debug,
                        dist = maxDist,
                         eps = eps,
                         fov = fov,
                          gs = None,
                       index = None,
                         lat = midLat,
                         lon = midLon,
                       ncols = None,
                       nIter = nIter,
                       nrows = None,
                   onlyValid = onlyValid,
                      prefix = prefix,
                    ramLimit = ramLimit,
                      repair = repair,
            satellite_height = satellite_height,
                         tol = tol,
        )

    # **************************************************************************

    # Check which background the user wants ...
    match background:
        case "GSHHG":
            # Add GSHHG background ...
            add_GSHHG_map_underlay(
                ax,
                background = True,
                     debug = debug,
                       fov = fov,
                  iceOcean = True,
                islandLake = True,
                  lakeLand = True,
                 landOcean = True,
                 linewidth = 0.5,
                 onlyValid = onlyValid,
                pondIsland = True,
                    repair = repair,
                resolution = resolution,
            )
        case "image":
            # Add image background ...
            add_map_background(
                ax,
                        debug = debug,
                       extent = extent,
                interpolation = interpolation,
                         name = name,
                 regrid_shape = regrid_shape,
                     resample = resample,
                   resolution = resolution,
            )
        case "NE":
            # Add NE background ...
            add_NE_map_underlay(
                ax,
                background = True,
                  cultural = True,
                     debug = debug,
                       fov = fov,
                 linestyle = "solid",
                 linewidth = 0.5,
                   maxElev = 8850.0,
                 onlyValid = onlyValid,
                  physical = True,
                    repair = repair,
                resolution = resolution,
            )
        case "none":
            # Don't add any background ...
            pass
        case "OSM":
            # Check if the points are so widely spread that the map has to have
            # global extent to show them all ...
            if maxDistQuick > 90.0:
                # Calculate the resolution depending on the half-width, or
                # the half-height, of the figure and the size of Earth ...
                res = min(
                    CIRCUMFERENCE_OF_EARTH / (fg.get_figwidth() * fg.get_dpi()),
                    0.5 * CIRCUMFERENCE_OF_EARTH / (fg.get_figheight() * fg.get_dpi()),
                )                                                               # [m/px]
            else:
                # Calculate the resolution depending on the half-width, or
                # the half-height, of the figure and the maximum distance ...
                res = min(
                    2.0 * maxDist / (fg.get_figwidth() * fg.get_dpi()),
                    2.0 * maxDist / (fg.get_figheight() * fg.get_dpi()),
                )                                                               # [m/px]

            # Add OpenStreetMap background ...
            add_OSM_map_background(
                ax,
                midLat,
                res,
                            ceil = ceil,
                           debug = debug,
                           floor = floor,
                   interpolation = interpolation,
                    regrid_shape = regrid_shape,
                        resample = resample,
                           scale = scale,
                thunderforestKey = thunderforestKey,
                thunderforestMap = thunderforestMap,
                               z = z,
            )
        case _:
            # Crash ...
            raise ValueError(f"\"background\" is an unexpected value ({repr(background)})") from None

    # Plot locations ...
    # NOTE: As of 5/Dec/2023, the default "zorder" of the coastlines is 1.5, the
    #       default "zorder" of the gridlines is 2.0 and the default "zorder" of
    #       the scattered points is 1.0.
    ax.scatter(
        pntLons[numpy.logical_not(skips)],
        pntLats[numpy.logical_not(skips)],
        edgecolor = "none",
        facecolor = fillColor,
        linewidth = 0.1,
                s = 64.0,
        transform = cartopy.crs.Geodetic(),
           zorder = 5.0,
    )

    # Plot locations ...
    # NOTE: As of 5/Dec/2023, the default "zorder" of the coastlines is 1.5, the
    #       default "zorder" of the gridlines is 2.0 and the default "zorder" of
    #       the scattered points is 1.0.
    ax.scatter(
        pntLons[skips],
        pntLats[skips],
        edgecolor = "none",
        facecolor = skipFillColor,
        linewidth = 0.1,
                s = 64.0,
        transform = cartopy.crs.Geodetic(),
           zorder = 5.0,
    )

    # Loop over locations ...
    for iPnt in range(pntLons.size - 1):
        # Find the great circle ...
        circle = great_circle(
            pntLons[iPnt],
            pntLats[iPnt],
            pntLons[iPnt + 1],
            pntLats[iPnt + 1],
               debug = debug,
                 eps = eps,
             maxdist = 12.0 * 1852.0,
               nIter = nIter,
              npoint = None,
              prefix = prefix,
            ramLimit = ramLimit,
        )

        # Draw the great circle ...
        ax.add_geometries(
            extract_lines(circle, onlyValid = onlyValid),
            cartopy.crs.PlateCarree(),
            edgecolor = skipFillColor if skips[iPnt] or skips[iPnt + 1] else fillColor,
            facecolor = "none",
            linewidth = 1.0,
               zorder = 5.0,
        )

    # Check that an extra route was passed ...
    if route is not None:
        # Draw the extra route ...
        ax.add_geometries(
            extract_lines(route, onlyValid = onlyValid),
            cartopy.crs.PlateCarree(),
            edgecolor = routeFillColor,
            facecolor = "none",
            linewidth = 1.0,
               zorder = 5.0,
        )

    # Configure axis ...
    if title is not None:
        ax.set_title(title)

    # Configure figure ...
    fg.tight_layout()

    # Save figure ...
    fg.savefig(pngOut)
    matplotlib.pyplot.close(fg)

    # Optimise PNG ...
    optimise_image(
        pngOut,
           chunksize = chunksize,
               debug = debug,
        exiftoolPath = exiftoolPath,
        gifsiclePath = gifsiclePath,
        jpegtranPath = jpegtranPath,
         optipngPath = optipngPath,
                pool = None,
               strip = True,
             timeout = timeout,
    )
