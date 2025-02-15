#!/usr/bin/env python3

# Define function ...
def create_image_of_points(
    pntLons,
    pntLats,
    zoom,
    sess,
    pngOut,
    /,
    *,
          background = (255, 255, 255),
           chunksize = 1048576,
             cookies = None,
               debug = __debug__,
                 eps = 1.0e-12,
        exiftoolPath = None,
                fill = (255, 0, 0, 127),
        gifsiclePath = None,
             headers = None,
        jpegtranPath = None,
                nAng = 9,
               nIter = 100,
           onlyValid = False,
         optipngPath = None,
              prefix = ".",
            ramLimit = 1073741824,
              repair = False,
               scale = 1,
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
    lons : numpy.ndarray
        the sequence of longitudes
    lats : numpy.ndarray
        the sequence of latitudes
    pngOut : str
        the name of the output PNG
    angConv : float, optional
        the angle change which classifies as converged (in degrees)
    background : str, optional
        the type of background to add (recognised values are: "GSHHG"; "image";
        "NE"; "none"; and "OSM")
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
    gifsiclePath : str, optional
        the path to the "gifsicle" binary (if not provided then Python will attempt to
        find the binary itself)
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
    prefix : str, optional
        change the name of the output debugging CSVs
    ramLimit : int, optional
        the maximum RAM usage of each "large" array (in bytes)
    repair : bool, optional
        attempt to repair invalid Polygons
    resolution : str, optional
        the resolution of the image or NE dataset or GSHHG dataset
    satellite_height : bool, optional
        if a distance is provided then use a "NearsidePerspective" projection at
        an altitude which has the same field-of-view as the distance
    scale : int, optional
        the scale of the tiles
    thunderforestMap : string, optional
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

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import math

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
    from ..image import image2png
    from ..openstreetmap import tiles

    # **************************************************************************

    # Create short-hands ...
    n = pow(2, zoom)
    nPnts = len(pntLons)                                                        # [#]

    # Create a MultiPoint from the lists of longitudes and latitudes ...
    pnts = []                                                                   # [°], [°]
    for pntLon, pntLat in zip(pntLons, pntLats):
        pnts.append(shapely.geometry.point.Point(pntLon, pntLat))               # [°], [°]
    pnts = shapely.geometry.multipoint.MultiPoint(pnts)

    # Buffer the MultiPoint and clean up ...
    polys = buffer(
        pnts,
        12.0 * 1852.0,
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
    del pnts

    # Find the bounds and middle of the buffer in the Mercator projection and
    # clean up ...
    pntMerXs = numpy.zeros(0, dtype = numpy.float64)                            # [°]
    pntMerYs = numpy.zeros(0, dtype = numpy.float64)                            # [°]
    for poly in extract_polys(
        polys,
        onlyValid = onlyValid,
           repair = repair,
    ):
        coords = numpy.array(poly.exterior.coords)                              # [°]
        pntMerXs = numpy.append(pntMerXs, coords[:, 0])                         # [°]
        pntMerYs = numpy.append(pntMerYs, coords[:, 1])                         # [°]
        del coords
    del polys
    pntMerXs = (pntMerXs + 180.0) / 360.0 * float(n)                            # [#]
    pntMerYs = (1.0 - numpy.arcsinh(numpy.tan(numpy.radians(pntMerYs))) / numpy.pi) / 2.0 * float(n)    # [#]
    minMerX = pntMerXs.min()                                                    # [°]
    maxMerX = pntMerXs.max()                                                    # [°]
    minMerY = pntMerYs.min()                                                    # [°]
    maxMerY = pntMerYs.max()                                                    # [°]
    del pntMerXs, pntMerYs
    midMerX = 0.5 * (minMerX + maxMerX)                                         # [°]
    midMerY = 0.5 * (minMerY + maxMerY)                                         # [°]

    # Convert the middle from Mercator projection back in to longitude and
    # latitude ...
    midLon = midMerX / float(n) * 360.0 - 180.0                                 # [°]
    midLat = math.degrees(math.atan(math.sinh(math.pi * (1.0 - 2.0 * midMerY / float(n))))) # [°]

    # Calculate the size of the image ...
    imgWidth = (maxMerX - minMerX) * float(scale * 256)                         # [px]
    imgHeight = (maxMerY - minMerY) * float(scale * 256)                        # [px]
    imgWidth = 2 * round(imgWidth / 2.0)                                        # [px]
    imgHeight = 2 * round(imgHeight / 2.0)                                      # [px]

    # Make the image ...
    img = tiles(
        midLon,
        midLat,
        zoom,
        imgWidth,
        imgHeight,
        sess,
              background = background,
               chunksize = chunksize,
                 cookies = cookies,
                   debug = debug,
            exiftoolPath = exiftoolPath,
                    fill = fill,
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
    midImgX = float(imgWidth // 2)                                              # [px]
    midImgY = float(imgHeight // 2)                                             # [px]

    # Draw the points ...
    draw = PIL.ImageDraw.Draw(img, "RGBA")
    for pntLon, pntLat in zip(pntLons, pntLats):
        pntMerX = (pntLon + 180.0) / 360.0 * float(n)                           # [#]
        pntMerY = (1.0 - math.asinh(math.tan(math.radians(pntLat))) / math.pi) / 2.0 * float(n) # [#]
        difMerX = pntMerX - midMerX                                             # [#]
        difMerY = pntMerY - midMerY                                             # [#]
        difImgX = difMerX * float(scale * 256)                                  # [px]
        difImgY = difMerY * float(scale * 256)                                  # [px]
        pntImgX = midImgX + difImgX                                             # [px]
        pntImgY = midImgY + difImgY                                             # [px]
        draw.ellipse(
            [pntImgX - 10.0, pntImgY - 10.0, pntImgX + 10.0, pntImgY + 10.0],
            fill = fill,
        )

    # Loop over locations ...
    for iPnt in range(nPnts - 1):
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

        # Loop over lines in the great circle ...
        for line in extract_lines(circle, onlyValid = onlyValid):
            # Make arrays of the points in the Mercator projection ...
            coordsMer = []                                                      # [#], [#]
            for coord in line.coords:
                coordsMer.append(ll2mer(coord[0], coord[1]))                    # [#], [#]
            coordsMerX, coordsMerY = zip(*coordsMer)                            # [#], [#]
            del coordsMer
            coordsMerX = numpy.array(coordsMerX)                                # [#], [#]
            coordsMerY = numpy.array(coordsMerY)                                # [#], [#]
            coordsMerX *= float(n)                                              # [#], [#]
            coordsMerY *= float(n)                                              # [#], [#]

            # Make an array of the points in the image and clean up ...
            coordsImgX = midImgX + (coordsMerX - midMerX) * float(scale * 256)  # [px], [px]
            coordsImgY = midImgY + (coordsMerY - midMerY) * float(scale * 256)  # [px], [px]
            del coordsMerX, coordsMerY

            # Draw the line and clean up ...
            draw.line(
                list(zip(coordsImgX, coordsImgY)),
                 fill = fill,
                width = 4,
            )
            del coordsImgX, coordsImgY

        # Clean up ...
        del circle

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
            optimize = True,
         optipngPath = optipngPath,
        screenHeight = -1,
         screenWidth = -1,
               strip = False,
             timeout = timeout,
    )
