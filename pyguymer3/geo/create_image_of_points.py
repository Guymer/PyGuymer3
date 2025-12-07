#!/usr/bin/env python3

# Define function ...
def create_image_of_points(
    pntLons,
    pntLats,
    z,
    sess,
    pngOut,
    /,
    *,
          background = (255, 255, 255),
           chunksize = 1048576,
             cookies = None,
               debug = __debug__,
    drawGreatCircles = True,
    drawPointBuffers = False,
          drawPoints = True,
                 eps = 1.0e-12,
        exiftoolPath = None,
           fillColor = (255,   0,   0),
        gifsiclePath = None,
        globalExtent = False,
         globalRatio = 16.0 / 9.0,
             headers = None,
        jpegtranPath = None,
                nAng = 9,
               nIter = 100,
           onlyValid = False,
         optipngPath = None,
             padDist = 12.0 * 1852.0,
              prefix = ".",
            ramLimit = 1073741824,
              repair = False,
               route = None,
      routeFillColor = (  0, 128,   0),
               scale = 1,
       skipFillColor = (255, 165,   0),
               skips = None,
    thunderforestKey = None,
    thunderforestMap = "atlas",
             timeout = 60.0,
                 tol = 1.0e-10,
              verify = True,
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
    z : int
        the OpenStreetMap zoom level
    sess : requests.Session
        the session for any requests calls
    pngOut : str
        the name of the output PNG
    background : tuple of int, optional
        the background colour of the merged tile
    chunksize : int, optional
        the size of the chunks of any files which are read in (in bytes)
    cookies : dict, optional
        extra cookies for any requests calls
    debug : bool, optional
        print debug messages and draw the circle on the axis
    drawGreatCircles : bool, optional
        whether to draw the great circles between the points
    drawPointBuffers : bool, optional
        whether to draw the buffers around the points
    drawPoints : bool, optional
        whether to draw the points
    eps : float, optional
        the tolerance of the Vincenty formula iterations
    exiftoolPath : str, optional
        the path to the "exiftool" binary (if not provided then Python will
        attempt to find the binary itself)
    fillColor : tuple of int, optional
        the fill colour of the points
    gifsiclePath : str, optional
        the path to the "gifsicle" binary (if not provided then Python will
        attempt to find the binary itself)
    globalExtent : bool, optional
        whether to override the calculation of the extent of the points and just
        make the image of global extent
    globalRatio : float, optional
        the ratio to make the image when it is global extent (because the
        Mercator projection looks silly at the poles)
    headers : dict, optional
        extra headers for any requests calls
    jpegtranPath : str, optional
        the path to the "jpegtran" binary (if not provided then Python will
        attempt to find the binary itself)
    nAng : int, optional
        the number of angles around the middle location to search over
    nIter : int, optional
        the maximum number of iterations (particularly the Vincenty formula)
    onlyValid : bool, optional
        only return valid Polygons (checks for validity can take a while, if
        being called often)
    optipngPath : str, optional
        the path to the "optipng" binary (if not provided then Python will
        attempt to find the binary itself)
    padDist : float, optional
        the padding to draw around the points (in metres)
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)
    repair : bool, optional
        attempt to repair invalid Polygons
    route : shapely.geometry.linestring.LineString, optional
        an extra line to draw on the map
    routeFillColor : tuple of int, optional
        the fill colour of the extra route
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
    tol : float, optional
        the Euclidean distance that defines two points as being the same (in
        degrees)
    verify : bool, optional
        verify the server's certificates for any requests calls

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
        import PIL.ImageDraw
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import sub-functions ...
    from .buffer import buffer
    from .extract_lines import extract_lines
    from .extract_polys import extract_polys
    from .great_circle import great_circle
    from .ll2mer import ll2mer
    from .mer2ll import mer2ll
    from ..image import image2png
    from ..openstreetmap import tiles

    # **************************************************************************

    # Check inputs ...
    if skips is None:
        skips = numpy.zeros(pntLons.size, dtype = bool)

    # **************************************************************************

    # Create short-hands ...
    n = pow(2, z)

    # Create a [Multi]Point from the lists of longitudes and latitudes ...
    pntsLonLat = []
    for pntLon, pntLat, skip in zip(pntLons, pntLats, skips, strict = True):
        if skip:
            continue
        pntsLonLat.append(shapely.geometry.point.Point(pntLon, pntLat))
    pntsLonLat = shapely.geometry.multipoint.MultiPoint(pntsLonLat)
    if debug:
        print(f"DEBUG: The points extend from {pntsLonLat.bounds[0]:+.6f}° to {pntsLonLat.bounds[2]:+.6f}° longitude.")
        print(f"DEBUG: The points extend from {pntsLonLat.bounds[1]:+.6f}° to {pntsLonLat.bounds[3]:+.6f}° latitude.")

    # Buffer the [Multi]Point ...
    polysLonLat = buffer(
        pntsLonLat,
        padDist,
                debug = debug,
                  eps = eps,
                 fill = -1.0,
            fillSpace = "EuclideanSpace",
        keepInteriors = False,
                 nAng = nAng,
                nIter = nIter,
               prefix = prefix,
             ramLimit = ramLimit,
                 simp = -1.0,
                  tol = tol,
    )
    if debug:
        print(f"DEBUG: The {0.001 * padDist:,.1f} km buffer of the points extends from {polysLonLat.bounds[0]:+.6f}° to {polysLonLat.bounds[2]:+.6f}° longitude.")
        print(f"DEBUG: The {0.001 * padDist:,.1f} km buffer of the points extends from {polysLonLat.bounds[1]:+.6f}° to {polysLonLat.bounds[3]:+.6f}° latitude.")

    # Convert [Multi]Polygon to the Mercator projection and create short-hands ...
    polysMer = ll2mer(
        polysLonLat,
         debug = debug,
        prefix = prefix,
           tol = tol,
    )
    if debug:
        print(f"DEBUG: The Mercator projection of the {0.001 * padDist:,.1f} km buffer of the points extends from {polysMer.bounds[0]:.6f} to {polysMer.bounds[2]:.6f} in the x-axis of the Mercator projection.")
        print(f"DEBUG: The Mercator projection of the {0.001 * padDist:,.1f} km buffer of the points extends from {polysMer.bounds[1]:.6f} to {polysMer.bounds[3]:.6f} in the y-axis of the Mercator projection.")
    if globalExtent:
        # NOTE: I want the final image to have an aspect ratio of 16:9, which
        #       means that if the width is 1.0 then the height should be 0.5625,
        #       which means that the top should start at 0.21875 and the bottom
        #       should finish at 0.78125.
        globalBand = 1.0 - (1.0 / globalRatio)                                  # [#]
        minMerX = 0.0                                                           # [#]
        minMerY = 0.5 * globalBand                                              # [#]
        maxMerX = 1.0                                                           # [#]
        maxMerY = 1.0 - 0.5 * globalBand                                        # [#]
        midMerX = 0.5                                                           # [#]
        midMerY = 0.5                                                           # [#]
    else:
        minMerX, minMerY, maxMerX, maxMerY = polysMer.bounds                    # [#], [#], [#], [#]
        midMerX = 0.5 * (minMerX + maxMerX)                                     # [#]
        midMerY = 0.5 * (minMerY + maxMerY)                                     # [#]
    if debug:
        print(f"DEBUG: The middle of the Mercator projection of the {0.001 * padDist:,.1f} km buffer of the points is at ({midMerX:.6f}, {midMerY:.6f}) in the Mercator projection.")

    # Convert the middle from Mercator projection back in to longitude and
    # latitude ...
    midLon, midLat = mer2ll(
        shapely.geometry.point.Point(midMerX, midMerY),
         debug = debug,
        prefix = prefix,
           tol = tol,
    ).coords[0]                                                                 # [°], [°]
    if debug:
        print(f"DEBUG: The middle of the Mercator projection of the {0.001 * padDist:,.1f} km buffer of the points is at ({midLon:+.6f}°, {midLat:+.6f}°).")

    # Calculate the size of the image ...
    imgWidth = (maxMerX - minMerX) * float(n * scale * 256)                     # [px]
    imgHeight = (maxMerY - minMerY) * float(n * scale * 256)                    # [px]
    imgWidth = 2 * round(imgWidth / 2.0)                                        # [px]
    imgHeight = 2 * round(imgHeight / 2.0)                                      # [px]
    if debug:
        print(f"DEBUG: The image is {imgWidth:,d} px × {imgHeight:,d} px.")

    # Make the image ...
    img = tiles(
        midLon,
        midLat,
        z,
        imgWidth,
        imgHeight,
        sess,
              background = background,
               chunksize = chunksize,
                 cookies = cookies,
                   debug = debug,
            exiftoolPath = exiftoolPath,
                    fill = fillColor,
            gifsiclePath = gifsiclePath,
                 headers = headers,
            jpegtranPath = jpegtranPath,
             optipngPath = optipngPath,
                  radius = None,
                   scale = scale,
        thunderforestKey = thunderforestKey,
        thunderforestMap = thunderforestMap,
                 timeout = timeout,
                  verify = verify,
    )

    # Create short-hands ...
    midImgX = imgWidth // 2                                                     # [px]
    midImgY = imgHeight // 2                                                    # [px]
    if debug:
        print(f"DEBUG: The middle of the image image is at ({midImgX:,d} px, {midImgY:,d} px).")

    # Create short-hand ...
    draw = PIL.ImageDraw.Draw(img, "RGBA")

    # Check if the user wants to draw the buffers of the points ...
    if drawPointBuffers:
        # Loop over Polygons in the buffer of the points ...
        for polyMer in extract_polys(polysMer, onlyValid = onlyValid, repair = repair):
            # Convert LineString to the image projection ...
            coordsMer = numpy.array(polyMer.exterior.coords)                    # [#]
            coordsImgX = float(midImgX) + (coordsMer[:, 0] - midMerX) * float(n * scale * 256)  # [px]
            coordsImgY = float(midImgY) + (coordsMer[:, 1] - midMerY) * float(n * scale * 256)  # [px]

            # Draw the Polygon ...
            draw.polygon(
                list(zip(coordsImgX, coordsImgY, strict = True)),
                fill = fillColor,
            )

    # Check if the user wants to draw the points ...
    if drawPoints:
        # Loop over points ...
        for pntLon, pntLat, skip in zip(pntLons, pntLats, skips, strict = True):
            # Draw the point ...
            pntMerX, pntMerY = ll2mer(
                shapely.geometry.point.Point(pntLon, pntLat),
                 debug = debug,
                prefix = prefix,
                   tol = tol,
            ).coords[0]                                                         # [#], [#]
            difMerX = pntMerX - midMerX                                         # [#]
            difMerY = pntMerY - midMerY                                         # [#]
            difImgX = difMerX * float(n * scale * 256)                          # [px]
            difImgY = difMerY * float(n * scale * 256)                          # [px]
            pntImgX = float(midImgX) + difImgX                                  # [px]
            pntImgY = float(midImgY) + difImgY                                  # [px]
            draw.ellipse(
                [
                    pntImgX - 10.0,
                    pntImgY - 10.0,
                    pntImgX + 10.0,
                    pntImgY + 10.0,
                ],
                fill = skipFillColor if skip else fillColor,
            )

    # Check if the user wants to draw the great circles between points ...
    if drawGreatCircles:
        # Loop over points ...
        for iPnt in range(pntLons.size - 1):
            # Find the great circle from this point to the next ...
            circleLonLat = great_circle(
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

            # Loop over LineStrings in the great circle ...
            for lineLonLat in extract_lines(circleLonLat, onlyValid = onlyValid):
                # Convert LineString to the Mercator projection ...
                lineMer = ll2mer(
                    lineLonLat,
                     debug = debug,
                    prefix = prefix,
                       tol = tol,
                )

                # Convert LineString to the image projection ...
                coordsMer = numpy.array(lineMer.coords)                         # [#]
                coordsImgX = float(midImgX) + (coordsMer[:, 0] - midMerX) * float(n * scale * 256)  # [px]
                coordsImgY = float(midImgY) + (coordsMer[:, 1] - midMerY) * float(n * scale * 256)  # [px]

                # Draw the line ...
                draw.line(
                    list(zip(coordsImgX, coordsImgY, strict = True)),
                     fill = skipFillColor if skips[iPnt] or skips[iPnt + 1] else fillColor,
                    width = 4,
                )

    # Check that an extra route was passed ...
    if route is not None:
        # Loop over LineStrings in the extra route ...
        for lineLonLat in extract_lines(route, onlyValid = onlyValid):
            # Convert LineString to the Mercator projection ...
            lineMer = ll2mer(
                lineLonLat,
                 debug = debug,
                prefix = prefix,
                   tol = tol,
            )

            # Convert LineString to the image projection ...
            coordsMer = numpy.array(lineMer.coords)                             # [#]
            coordsImgX = float(midImgX) + (coordsMer[:, 0] - midMerX) * float(n * scale * 256)  # [px]
            coordsImgY = float(midImgY) + (coordsMer[:, 1] - midMerY) * float(n * scale * 256)  # [px]

            # Draw the line ...
            draw.line(
                list(zip(coordsImgX, coordsImgY, strict = True)),
                 fill = routeFillColor,
                width = 4,
            )

    # Save map ...
    image2png(
        img,
        pngOut,
           chunksize = chunksize,
               debug = debug,
                exif = {
                      "Artist" : "OpenStreetMap contributors",
                   "Copyright" : "All Rights Reserved",
            "ImageDescription" : "https://www.openstreetmap.org",
        },
        exiftoolPath = exiftoolPath,
        gifsiclePath = gifsiclePath,
        jpegtranPath = jpegtranPath,
                mode = "RGB",
            optimise = True,
         optipngPath = optipngPath,
        screenHeight = -1,
         screenWidth = -1,
               strip = False,
             timeout = timeout,
    )
