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
             elevInt = 250,
                 eps = 1.0e-12,
        exiftoolPath = None,
              extent = None,
           fillColor = (255.0 / 255.0,   0.0 / 255.0,   0.0 / 255.0),
               floor = False,
        gifsiclePath = None,
          globeScale = "32km",
            gshhgRes = "i",
       interpolation = "none",
        jpegtranPath = None,
             maxElev = 1000,
      maxImagePixels = 1073741824,
      mergedTileName = None,
              method = "GeodesicBox",
                name = "natural-earth-1",
                nAng = 9,
               neRes = "10m",
               nIter = 100,
             nRefine = 1,
           onlyValid = False,
         optipngPath = None,
             padDist = 12.0 * 1852.0,
              prefix = ".",
            ramLimit = 1073741824,
              repair = False,
            resample = False,
               route = None,
      routeFillColor = (  0.0 / 255.0, 128.0 / 255.0,   0.0 / 255.0),
    satellite_height = False,
       skipFillColor = (255.0 / 255.0, 165.0 / 255.0,   0.0 / 255.0),
               skips = None,
             subName = "medium0512px",
    thunderforestKey = None,
    thunderforestMap = "atlas",
            tileGrid = "18x9",
           tileScale = 1,
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
        The type of background to add. Recognised values are: "GLOBE-tiles";
        "GLOBE+GSHHG-tiles"; "GLOBE+NE-tiles"; "GSHHG-map" (and "GSHHG");
        "GSHHG-tiles"; "image"; "NE-map" (and "NE"); "NE-tiles"; "none"; "OSM";
        and "OSterrain-tiles".
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
    elevInt : int, optional
        The interval between different shaded bands of elevation used by the
        colour map of the tiles (in metres).
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    exiftoolPath : None or str, optional
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
    gifsiclePath : None or str, optional
        the path to the "gifsicle" binary (if not provided then Python will attempt to
        find the binary itself)
    globeScale : str, optional
        The scale of the elevation from the GLOBE [4]_ dataset.
    gshhgRes : str, optional
        the resolution of the coastline boundaries from GSHHG [2]_
    interpolation : str, optional
        The interpolation method used when drawing the final warped image, or
        the final merged and warped image, on the figure. Due to the use of
        **kwargs within Cartopy, this is passed all the way down the stack to
        the MatPlotLib ".imshow()" call.
    jpegtranPath : None or str, optional
        the path to the "jpegtran" binary (if not provided then Python will attempt to
        find the binary itself)
    maxElev : int, optional
        The maximum elevation used by the colour map of the tiles (in metres).
    maxImagePixels : int, optional
        The maximum number of pixels in an image, to prevent decompression bombs.
    mergedTileName : None or str, optional
        If provided, then save the merged tile to this file.
    method : str, optional
        the method for finding the middle of the points
    name : str, optional
        The name of the image in the database.
    nAng : int, optional
        the number of angles around the middle location to search over
    neRes : str, optional
        The resolution of the Natural Earth [3]_ datasets.
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)
    nRefine : int, optional
        the number of refinements to make (each refinement halves the "conv"
        distance)
    onlyValid : bool, optional
        only return valid Polygons (checks for validity can take a while, if
        being called often)
    optipngPath : None or str, optional
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
    route : shapely.geometry.linestring.LineString, optional
        an extra line to draw on the map
    routeFillColor : tuple of int, optional
        the fill colour of the extra route
    satellite_height : float, optional
        if a distance is provided then use a "NearsidePerspective" projection at
        an altitude which has the same field-of-view as the distance
    skipFillColor : tuple of int, optional
        the fill colour of the skipped points
    skips : numpy.ndarray, optional
        an array of booleans as to whether to include/exclude each individual
        point from calculating the image's field-of-view (this allows the great
        circles from flights to be drawn but for them to not expand the image to
        fit in the departing airport); if not provided then all points are used
    subName : str, optional
        The sub-name of the image in the database. Typically, databases use
        ``name`` to refer to an image and then they use ``subName`` to refer to
        a particular rendering, or size, of the image. This allows users to get
        a quicker, or smaller, version of their desired image.
    thunderforestKey : str, optional
        your personal API key for the Thunderforest service (if provided then it
        is assumed that you want to use the Thunderforest service)
    thunderforestMap : str, optional
        the Thunderforest map style (see https://www.thunderforest.com/maps/)
    tileGrid : str, optional
        The grid to fetch tiles from.
    tileScale : int, optional
        The scale of the tiles.
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
    There is one argument relating to the `Global Self-Consistent Hierarchical
    High-Resolution Geography dataset <https://www.ngdc.noaa.gov/mgg/shorelines/>`_ :

    * *gshhgRes*.

    There are five resolutions to choose from:

    * crude ("c");
    * low ("l");
    * intermediate ("i");
    * high ("h"); and
    * full ("f").

    There is one argument relating to the `Natural Earth dataset
    <https://www.naturalearthdata.com>`_ :

    * *neRes*.

    There are three resolutions to choose from:

    * large scale data 1:10m ("10m");
    * medium scale data 1:50m ("50m"); and
    * small scale data 1:110m ("110m").

    See the `MatPlotLib documentation about interpolation methods
    <https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html>`_
    and the `MatPlotLib documentation about anti-aliasing
    <https://matplotlib.org/stable/gallery/images_contours_and_fields/image_antialiasing.html>`_.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    .. [2] Global Self-consistent Hierarchical High-resolution Geography, https://www.ngdc.noaa.gov/mgg/shorelines/
    .. [3] Natural Earth, https://www.naturalearthdata.com/
    .. [4] Global Land One-km Base Elevation, https://www.ngdc.noaa.gov/mgg/topo/globe.html
    """

    # Import standard modules ...
    import copy
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
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .add_axis import add_axis
    from .add_Cartopy_tiles import add_Cartopy_tiles
    from .add_GLOBE_and_GSHHG_tiles import add_GLOBE_and_GSHHG_tiles
    from .add_GLOBE_and_NE_tiles import add_GLOBE_and_NE_tiles
    from .add_GLOBE_tiles import add_GLOBE_tiles
    from .add_GSHHG_map import add_GSHHG_map
    from .add_GSHHG_tiles import add_GSHHG_tiles
    from .add_map_background import add_map_background
    from .add_NE_map import add_NE_map
    from .add_NE_tiles import add_NE_tiles
    from .add_OSterrain_tiles import add_OSterrain_tiles
    from .buffer import buffer
    from .extract_lines import extract_lines
    from .find_middle_of_locs import find_middle_of_locs
    from .great_circle import great_circle
    from .._consts import CIRCUMFERENCE_OF_EARTH, EARTH, RESOLUTION_OF_EARTH
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
        # Create short-hands ...
        fov = copy.copy(EARTH)
        midLat = 0.0                                                            # [°]

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

        # Find the field-of-view ...
        fov = buffer(
            shapely.geometry.point.Point(midLon, midLat),
            maxDist,
                    debug = debug,
                      eps = eps,
                     fill = -1.0,
                fillSpace = "EuclideanSpace",
            keepInteriors = False,
                     nAng = 361,
                    nIter = nIter,
                   prefix = prefix,
                 ramLimit = ramLimit,
                     simp = -1.0,
                      tol = tol,
        )

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
        case "GLOBE-tiles":
            # Add GLOBE tiles background ...
            add_GLOBE_tiles(
                ax,
                     chunksize = chunksize,
                         debug = debug,
                  exiftoolPath = exiftoolPath,
                           fov = fov,
                  gifsiclePath = gifsiclePath,
                 interpolation = interpolation,
                  jpegtranPath = jpegtranPath,
                       maxElev = maxElev,
                maxImagePixels = maxImagePixels,
                mergedTileName = mergedTileName,
                   optipngPath = optipngPath,
                          pool = None,
                  regrid_shape = regrid_shape,
                      resample = resample,
                      tileGrid = tileGrid,
                       timeout = timeout,
            )
        case "GLOBE+GSHHG-tiles":
            # Add GLOBE+GSHHG tiles background ...
            add_GLOBE_and_GSHHG_tiles(
                ax,
                     chunksize = chunksize,
                         debug = debug,
                       elevInt = elevInt,
                  exiftoolPath = exiftoolPath,
                           fov = fov,
                  gifsiclePath = gifsiclePath,
                      gshhgRes = gshhgRes,
                 interpolation = interpolation,
                  jpegtranPath = jpegtranPath,
                       maxElev = maxElev,
                maxImagePixels = maxImagePixels,
                mergedTileName = mergedTileName,
                   optipngPath = optipngPath,
                          pool = None,
                  regrid_shape = regrid_shape,
                      resample = resample,
                      tileGrid = tileGrid,
                       timeout = timeout,
            )
        case "GLOBE+NE-tiles":
            # Add GLOBE+NE tiles background ...
            add_GLOBE_and_NE_tiles(
                ax,
                     chunksize = chunksize,
                         debug = debug,
                       elevInt = elevInt,
                  exiftoolPath = exiftoolPath,
                           fov = fov,
                  gifsiclePath = gifsiclePath,
                 interpolation = interpolation,
                  jpegtranPath = jpegtranPath,
                       maxElev = maxElev,
                maxImagePixels = maxImagePixels,
                mergedTileName = mergedTileName,
                         neRes = neRes,
                   optipngPath = optipngPath,
                          pool = None,
                  regrid_shape = regrid_shape,
                      resample = resample,
                      tileGrid = tileGrid,
                       timeout = timeout,
            )
        case "GSHHG" | "GSHHG-map":
            # Add GSHHG map background ...
            add_GSHHG_map(
                ax,
                background = True,
                     debug = debug,
                       fov = fov,
                  gshhgRes = gshhgRes,
                  iceOcean = True,
                islandLake = True,
                  lakeLand = True,
                 landOcean = True,
                 linewidth = 0.5,
                 onlyValid = onlyValid,
                pondIsland = True,
                    repair = repair,
            )
        case "GSHHG-tiles":
            # Add GSHHG tiles background ...
            add_GSHHG_tiles(
                ax,
                     chunksize = chunksize,
                         debug = debug,
                  exiftoolPath = exiftoolPath,
                           fov = fov,
                  gifsiclePath = gifsiclePath,
                      gshhgRes = gshhgRes,
                 interpolation = interpolation,
                  jpegtranPath = jpegtranPath,
                maxImagePixels = maxImagePixels,
                mergedTileName = mergedTileName,
                   optipngPath = optipngPath,
                          pool = None,
                  regrid_shape = regrid_shape,
                      resample = resample,
                      tileGrid = tileGrid,
                       timeout = timeout,
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
                      subName = subName,
            )
        case "NE" | "NE-map":
            # Add NE map background ...
            add_NE_map(
                ax,
                background = True,
                  cultural = True,
                     debug = debug,
                   elevInt = elevInt,
                       fov = fov,
                globeScale = globeScale,
                 linestyle = "solid",
                 linewidth = 0.5,
                   maxElev = maxElev,
                     neRes = neRes,
                 onlyValid = onlyValid,
                  physical = True,
                    repair = repair,
            )
        case "NE-tiles":
            # Add NE tiles background ...
            add_NE_tiles(
                ax,
                     chunksize = chunksize,
                         debug = debug,
                  exiftoolPath = exiftoolPath,
                           fov = fov,
                  gifsiclePath = gifsiclePath,
                 interpolation = interpolation,
                  jpegtranPath = jpegtranPath,
                maxImagePixels = maxImagePixels,
                mergedTileName = mergedTileName,
                         neRes = neRes,
                   optipngPath = optipngPath,
                          pool = None,
                  regrid_shape = regrid_shape,
                      resample = resample,
                      tileGrid = tileGrid,
                       timeout = timeout,
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

            # Add OpenStreetMap-style tiles background ...
            add_Cartopy_tiles(
                ax,
                midLat,
                res,
                            ceil = ceil,
                           debug = debug,
                           floor = floor,
                   interpolation = interpolation,
                    regrid_shape = regrid_shape,
                        resample = resample,
                thunderforestKey = thunderforestKey,
                thunderforestMap = thunderforestMap,
                       tileScale = tileScale,
                               z = z,
            )
        case "OSterrain-tiles":
            # Add "OS Terrain 50" tiles background ...
            add_OSterrain_tiles(
                ax,
                     chunksize = chunksize,
                         debug = debug,
                  exiftoolPath = exiftoolPath,
                           fov = fov,
                  gifsiclePath = gifsiclePath,
                 interpolation = interpolation,
                  jpegtranPath = jpegtranPath,
                       maxElev = maxElev,
                maxImagePixels = maxImagePixels,
                mergedTileName = mergedTileName,
                   optipngPath = optipngPath,
                          pool = None,
                        prefix = prefix,
                  regrid_shape = regrid_shape,
                      resample = resample,
                      tileGrid = tileGrid,
                       timeout = timeout,
                           tol = tol,
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
