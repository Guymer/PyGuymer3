#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import glob
    import json
    import math
    import os
    import pathlib
    import shutil
    import warnings

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy").expanduser(),
            }
        )
        import cartopy.io
        import cartopy.io.img_tiles
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None
    try:
        import geojson
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
    try:
        import matplotlib
        matplotlib.rcParams.update(
            {
                       "axes.xmargin" : 0.01,
                       "axes.ymargin" : 0.01,
                            "backend" : "Agg",                                  # NOTE: See https://matplotlib.org/stable/gallery/user_interfaces/canvasagg.html
                         "figure.dpi" : 300,
                     "figure.figsize" : (9.6, 7.2),                             # NOTE: See https://github.com/Guymer/misc/blob/main/README.md#matplotlib-figure-sizes
                          "font.size" : 8,
                "image.interpolation" : "none",                                 # NOTE: See https://matplotlib.org/stable/gallery/images_contours_and_fields/interpolation_methods.html
                     "image.resample" : False,
            }
        )
        import matplotlib.pyplot
    except:
        raise Exception("\"matplotlib\" is not installed; run \"pip install --user matplotlib\"") from None
    try:
        import PIL
        import PIL.Image
        import PIL.ImageDraw
        PIL.Image.MAX_IMAGE_PIXELS = 1073741824                                 # [px]
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None
    try:
        import shapely
        import shapely.geometry
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Find suitable projections (for a top-down axis).",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--chunksize",
        default = 1048576,
           help = "the size of the chunks of any files which are read in (in bytes)",
           type = int,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--eps",
        default = 1.0e-12,
           dest = "eps",
           help = "the tolerance of the Vincenty formula iterations",
           type = float,
    )
    parser.add_argument(
        "--exiftool-path",
        default = shutil.which("exiftool"),
           dest = "exiftoolPath",
           help = "the path to the \"exiftool\" binary",
           type = str,
    )
    parser.add_argument(
        "--gifsicle-path",
        default = shutil.which("gifsicle"),
           dest = "gifsiclePath",
           help = "the path to the \"gifsicle\" binary",
           type = str,
    )
    parser.add_argument(
        "--jpegtran-path",
        default = shutil.which("jpegtran"),
           dest = "jpegtranPath",
           help = "the path to the \"jpegtran\" binary",
           type = str,
    )
    parser.add_argument(
        "--nIter",
        default = 1000000,
           dest = "nIter",
           help = "the maximum number of iterations (particularly the Vincenty formula)",
           type = int,
    )
    parser.add_argument(
        "--optipng-path",
        default = shutil.which("optipng"),
           dest = "optipngPath",
           help = "the path to the \"optipng\" binary",
           type = str,
    )
    parser.add_argument(
        "--RAM-limit",
        default = 1073741824,
           dest = "ramLimit",
           help = "the maximum RAM usage of each \"large\" array (in bytes)",
           type = int,
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    parser.add_argument(
        "--tolerance",
        default = 1.0e-10,
           dest = "tol",
           help = "the Euclidean distance that defines two points as being the same (in degrees)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # NOTE: https://github.com/SciTools/cartopy/pull/2378
    # NOTE: https://cartopy.readthedocs.io/stable/gallery/lines_and_polygons/effects_of_the_ellipse.html

    # Create short-hands and make output directory ...
    # NOTE: 7.2 inches at 100 DPI is 720 pixels.
    height = 720                                                                # [px]
    margin = 5                                                                  # [px]
    width = 720                                                                 # [px]
    stub1 = __file__.removesuffix(".py")
    if not os.path.exists(stub1):
        os.mkdir(stub1)

    # **************************************************************************
    # **************************************************************************
    # **************************************************************************

    # Initialize list ...
    projectionNames = []

    # Loop over all items within the Cartopy CRS sub-module ...
    for projectionName in dir(cartopy.crs):
        # Create short-hand and skip if it is not callable ...
        projectionClass = getattr(cartopy.crs, projectionName)
        if not callable(projectionClass):
            continue

        # Start a context manager for warnings ...
        with warnings.catch_warnings():
            # Hide "ignore" level warnings ...
            warnings.simplefilter("ignore")

            # Try to make a projection (over a random location) and skip if
            # unsuccessful ...
            try:
                projection = projectionClass(
                     central_latitude = 45.0,
                    central_longitude = 45.0,
                                globe = pyguymer3.WGS84,
                )
            except:
                continue

            # Skip if it cannot handle ellipses ...
            if not getattr(projection, "_handles_ellipses", False):
                print(f"Skipping \"{projectionName}\" as it cannot handle ellipses.")
                continue

            # Skip if the boundary does not make sense ...
            if not isinstance(projection.boundary, shapely.geometry.polygon.LinearRing):
                print(f"Skipping \"{projectionName}\" as the boundary is not a LinearRing.")
                continue

            # Append projection name to list ...
            projectionNames.append(projectionName)

    # Create short-hand ...
    nProjs = len(projectionNames)                                               # [#]

    # Save list of projection names ...
    with open(
        f"{stub1}/projectionNames.json",
        encoding = "utf-8",
            mode = "wt",
    ) as fObj:
        json.dump(
            projectionNames,
            fObj,
            ensure_ascii = False,
                  indent = 4,
               sort_keys = True,
        )

    # **************************************************************************
    # **************************************************************************
    # **************************************************************************

    # Initialize counter and list ...
    nLoc = 0                                                                    # [#]
    stub2s = []

    # Loop over centres, distances and zooms ...
    for iLoc, (lon, lat, dist, gshhgRes, zoom, gridlines_int) in enumerate(
        [
            (  -4.0, +53.0,    50.0e3, "f", 9, 1),  # ~Snowdonia holiday
            (+157.0, -31.0,  3000.0e3, "c", 4, 5),  # ~Australia holiday                OSM does not work
            (+157.0, -31.0,  1000.0e3, "i", 6, 2),  # ~Australia holiday                OSM works
            (-180.0, +90.0,  1000.0e3, "i", 6, 2),  # Satisfies test A, C, D, F
            ( -90.0, +45.0,  1000.0e3, "i", 6, 2),  # Satisfies test A
            (   0.0,   0.0,  1000.0e3, "i", 6, 2),  # Satisfies test A, B
            ( +90.0, -45.0,  1000.0e3, "i", 6, 2),  # Satisfies test A
            (+180.0, -90.0,  1000.0e3, "i", 6, 2),  # Satisfies test A, C, D, F
            (+170.0, +10.0,  4000.0e3, "c", 4, 5),  # Satisfies test B, C, E            OSM does not work
            (+170.0, +10.0,  1000.0e3, "i", 6, 2),  # Satisfies test B, C, E            OSM works
            (+170.0, +80.0,  4000.0e3, "c", 4, 5),  # Satisfies test C, D, F
            (   0.0, +83.0,  1000.0e3, "i", 6, 2),  # Satisfies test C, D, F
            ( -90.0, -83.0,  1000.0e3, "i", 6, 2),  # Satisfies test C, D, F
            (   0.0,   0.0, 10000.0e3, "c", 4, 5),  # Satisfies test A, B
        ]
    ):
        print(f"Processing ({lon:+.1f}°,{lat:+.1f}°) with {round(0.001 * dist):,d} km ...")

        # Create short-hands, increment counter and buffer the centre by the
        # distance ...
        nLoc += 1                                                               # [#]
        point1 = shapely.geometry.point.Point(lon, lat)
        polygon1 = pyguymer3.geo.buffer(
            point1,
            dist,
                debug = args.debug,
                  eps = args.eps,
                 fill = +1.0,                                                   # NOTE: Need to fill in the result to
            fillSpace = "EuclideanSpace",                                       #       undo the ".simplify(tol)" in
                 nAng = 361,                                                    #       "buffer_CoordinateSequence()".
                nIter = args.nIter,
             ramLimit = args.ramLimit,
                 simp = -1.0,
                  tol = args.tol,
        )
        stub2 = f"{stub1}/(lon={lon:+.1f}°,lat={lat:+.1f}°) dist={round(0.001 * dist):,d}km"
        stub2s.append(stub2)

        # Save Polygon as a GeoJSON ...
        # NOTE: As of 4/Aug/2025, the Python module "geojson" just converts the
        #       object to a Python dictionary and then it just calls the
        #       standard "json.dump()" function to format the Python dictionary
        #       as text. There is no way to specify the precision of the written
        #       string. Fortunately, if you have no shame, then you can load and
        #       then dump the string again, see:
        #         * https://stackoverflow.com/a/29066406
        with open(
            f"{stub2}.geojson",
            encoding = "utf-8",
                mode = "wt",
        ) as fObj:
            json.dump(
                json.loads(
                    geojson.dumps(
                        polygon1,
                        ensure_ascii = False,
                              indent = 4,
                           sort_keys = True,
                    ),
                    parse_float = lambda x: round(float(x), 4),                 # NOTE: 0.0001° is approximately 11.1 m.
                ),
                fObj,
                ensure_ascii = False,
                      indent = 4,
                   sort_keys = True,
            )

        # **********************************************************************

        # Loop over all projection names ...
        for projectionName in projectionNames:
            # Create short-hand ...
            projectionClass = getattr(cartopy.crs, projectionName)

            # Start a context manager for warnings ...
            with warnings.catch_warnings():
                # Hide "ignore" level warnings ...
                warnings.simplefilter("ignore")

                # Try to make a projection and skip if unsuccessful ...
                try:
                    projection = projectionClass(
                         central_latitude = lat,
                        central_longitude = lon,
                                    globe = pyguymer3.WGS84,
                    )
                except:
                    continue

                # Create short-hand and skip if it failed ...
                point2 = projection.project_geometry(point1)
                if math.isnan(point2.x) or math.isnan(point2.y):
                    print(f"  Skipping \"{projectionName}\" as it cannot project the Point.")
                    continue

                # Create short-hand and skip if it does not contain the Point ...
                boundary2 = shapely.geometry.polygon.Polygon(projection.boundary)
                if not boundary2.contains(point2):
                    print(f"  Skipping \"{projectionName}\" as it does not contain the Point.")
                    continue

                print(f"  Processing \"{projectionName}\" ...")

                # **************************************************************
                # **************************************************************
                # **************************************************************

                # Create short-hands and make output directory if it is missing ...
                dName = f"{stub2}/global"
                pNameGlobal = f"{dName}/{projectionName}.png"
                if not os.path.exists(dName):
                    os.makedirs(dName)

                # Check if the map needs making ...
                if not os.path.exists(pNameGlobal):
                    print(f"    Making \"{pNameGlobal}\" ...")

                    # Make figure and axis using the projection ...
                    fg = matplotlib.pyplot.figure(
                            dpi = 100,
                        figsize = (7.2, 7.2),
                    )
                    ax = fg.add_subplot(
                        projection = projection,
                    )

                    # Set extent ...
                    ax.set_global()

                    # Add background and buffer ...
                    ax.add_image(
                        cartopy.io.img_tiles.OSM(
                            cache = True,
                        ),
                        4,
                    )
                    # ax.stock_img()
                    ax.add_geometries(
                        pyguymer3.geo.extract_polys(
                            polygon1,
                            onlyValid = True,
                               repair = False,
                        ),
                        pyguymer3.GEODETIC,
                        edgecolor = (0.0, 0.0, 1.0, 1.0),
                        facecolor = (0.0, 0.0, 1.0, 0.2),
                        linewidth = 1.0,
                    )
                    ax.scatter(
                        [lon,],
                        [lat,],
                        edgecolor = "black",
                        facecolor = "gold",
                        linewidth = 1.0,
                           marker = "*",
                                s = 64.0,
                        transform = pyguymer3.GEODETIC,
                           zorder = 2.0,
                    )
                    pyguymer3.geo._add_coastlines(
                        ax,
                              debug = args.debug,
                                fov = pyguymer3.EARTH,
                        gshhgLevels = (1, 5, 6,),
                           gshhgRes = "c",
                          onlyValid = True,
                             repair = False,
                    )
                    pyguymer3.geo._add_horizontal_gridlines(
                        ax,
                        locs = range( -90,  +95, 5),
                    )
                    pyguymer3.geo._add_vertical_gridlines(
                        ax,
                        locs = range(-180, +185, 5),
                    )

                    # Configure figure ...
                    # NOTE: Setting the background patch to be a colour with an
                    #       alpha channel means that the resultant PNG is RGBA.
                    if projectionName == "AzimuthalEquidistant":
                        fg.patch.set_facecolor((1.0, 0.0, 0.0, 0.5))
                    fg.suptitle(projectionName)
                    fg.tight_layout()

                    # Try to save figure ...
                    try:
                        fg.savefig(pNameGlobal)
                        pyguymer3.image.optimise_image(
                            pNameGlobal,
                               chunksize = args.chunksize,
                                   debug = args.debug,
                            exiftoolPath = args.exiftoolPath,
                            gifsiclePath = args.gifsiclePath,
                            jpegtranPath = args.jpegtranPath,
                             optipngPath = args.optipngPath,
                                    pool = None,
                                   strip = True,
                                 timeout = args.timeout,
                        )
                    except Exception as e:
                        print(f"    WARNING: Saving the figure failed ({e}).")

                    # Clean up ...
                    matplotlib.pyplot.close(fg)
                    del ax, fg

                # **************************************************************
                # **************************************************************
                # **************************************************************

                # Create short-hands and make output directory if it is missing ...
                dName = f"{stub2}/local"
                pNameLocal = f"{dName}/{projectionName}.png"
                if not os.path.exists(dName):
                    os.makedirs(dName)

                # Check if the map needs making ...
                if not os.path.exists(pNameLocal):
                    print(f"    Making \"{pNameLocal}\" ...")

                    # Make figure and axis using the projection ...
                    fg = matplotlib.pyplot.figure(
                            dpi = 100,
                        figsize = (7.2, 7.2),
                    )
                    ax = fg.add_subplot(
                        projection = projection,
                    )

                    # Set extent ...
                    minR, maxR = pyguymer3.geo._set_axis_boundary(
                        ax,
                        point1,
                        polygon1,
                        dist,
                          eps = args.eps,
                        nIter = args.nIter,
                          tol = args.tol,
                    )

                    # Add background and buffer ...
                    ax.add_image(
                        cartopy.io.img_tiles.OSM(
                            cache = True,
                        ),
                        zoom,
                    )
                    # ax.stock_img()
                    ax.add_geometries(
                        pyguymer3.geo.extract_polys(
                            polygon1,
                            onlyValid = True,
                               repair = False,
                        ),
                        pyguymer3.GEODETIC,
                        edgecolor = (0.0, 0.0, 1.0, 1.0),
                        facecolor = (0.0, 0.0, 1.0, 0.2),
                        linewidth = 1.0,
                    )
                    ax.scatter(
                        [lon,],
                        [lat,],
                        edgecolor = "black",
                        facecolor = "gold",
                        linewidth = 1.0,
                           marker = "*",
                                s = 64.0,
                        transform = pyguymer3.GEODETIC,
                           zorder = 2.0,
                    )
                    pyguymer3.geo._add_coastlines(
                        ax,
                              debug = args.debug,
                                fov = polygon1,
                        gshhgLevels = (1, 5, 6,),
                           gshhgRes = gshhgRes,
                          onlyValid = True,
                             repair = False,
                    )
                    pyguymer3.geo._add_horizontal_gridlines(
                        ax,
                        locs = range( -90,  +90 + gridlines_int, gridlines_int),
                    )
                    pyguymer3.geo._add_vertical_gridlines(
                        ax,
                        locs = range(-180, +180 + gridlines_int, gridlines_int),
                    )

                    # Configure figure ...
                    # NOTE: Setting the background patch to be a colour with an
                    #       alpha channel means that the resultant PNG is RGBA.
                    if projectionName == "AzimuthalEquidistant":
                        fg.patch.set_facecolor((1.0, 0.0, 0.0, 0.5))
                    fg.suptitle(f"{projectionName} : {100.0 * minR:5.1f}% : {100.0 * maxR:5.1f}%")
                    fg.tight_layout()

                    # Try to save figure and optimise PNG ...
                    try:
                        fg.savefig(pNameLocal)
                        pyguymer3.image.optimise_image(
                            pNameLocal,
                               chunksize = args.chunksize,
                                   debug = args.debug,
                            exiftoolPath = args.exiftoolPath,
                            gifsiclePath = args.gifsiclePath,
                            jpegtranPath = args.jpegtranPath,
                             optipngPath = args.optipngPath,
                                    pool = None,
                                   strip = True,
                                 timeout = args.timeout,
                        )
                    except Exception as e:
                        print(f"    WARNING: Saving the figure failed ({e}).")

                    # Clean up ...
                    matplotlib.pyplot.close(fg)
                    del ax, fg

                # **************************************************************
                # **************************************************************
                # **************************************************************

                # Create short-hand ...
                pNameBoth = f"{stub2}/{projectionName}.png"

                # Check if the map comparison needs making ...
                if not os.path.exists(pNameBoth):
                    print(f"    Making \"{pNameBoth}\" ...")

                    # Create empty image to hold both the PNGs and initialize
                    # the drawing object ...
                    bothIm = PIL.Image.new(
                        "RGBA",
                        (
                            2 * (width  + 2 * margin),
                                 height + 2 * margin ,
                        ),
                        (
                            127,
                            127,
                            127,
                        ),
                    )
                    draw = PIL.ImageDraw.Draw(bothIm)

                    # Check if the global map exists ...
                    if os.path.exists(pNameGlobal):
                        # Open image ...
                        with PIL.Image.open(
                            pNameGlobal,
                            mode = "r",
                        ) as globalIm:
                            # Either paste the RGB image on the main image or
                            # overlay the RGBA image over a white rectangle on
                            # the main image ...
                            match globalIm.mode:
                                case "RGB":
                                    bothIm.paste(
                                        globalIm,
                                        (
                                            margin,
                                            margin,
                                        ),
                                    )
                                case "RGBA":
                                    draw.rectangle(
                                        [
                                            margin,
                                            margin,
                                            margin + width  - 1,
                                            margin + height - 1,
                                        ],
                                        fill = (
                                            255,
                                            255,
                                            255,
                                        ),
                                    )
                                    bothIm.alpha_composite(
                                        globalIm,
                                        dest = (
                                            margin,
                                            margin,
                                        ),
                                    )
                                case _:
                                    raise Exception(globalIm.mode) from None

                        # Clean up ...
                        globalIm.close()
                        del globalIm

                    # Check if the local map exists ...
                    if os.path.exists(pNameLocal):
                        # Open image ...
                        with PIL.Image.open(
                            pNameLocal,
                            mode = "r",
                        ) as localIm:
                            # Either paste the RGB image on the main image or
                            # overlay the RGBA image over a white rectangle on
                            # the main image ...
                            match localIm.mode:
                                case "RGB":
                                    bothIm.paste(
                                        localIm,
                                        (
                                            width + 3 * margin,
                                                        margin,
                                        ),
                                    )
                                case "RGBA":
                                    draw.rectangle(
                                        [
                                            width + 3 * margin,
                                                        margin,
                                            width + 3 * margin + width  - 1,
                                                        margin + height - 1,
                                        ],
                                        fill = (
                                            255,
                                            255,
                                            255,
                                        ),
                                    )
                                    bothIm.alpha_composite(
                                        localIm,
                                        dest = (
                                            width + 3 * margin,
                                                        margin,
                                        ),
                                    )
                                case _:
                                    raise Exception(localIm.mode) from None

                        # Clean up ...
                        localIm.close()
                        del localIm

                    # Save image ...
                    pyguymer3.image.image2png(
                        bothIm,
                        pNameBoth,
                             chunksize = args.chunksize,
                                 debug = args.debug,
                          exiftoolPath = args.exiftoolPath,
                          gifsiclePath = args.gifsiclePath,
                          jpegtranPath = args.jpegtranPath,
                        maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS,
                                  mode = "RGB",
                              optimise = True,
                           optipngPath = args.optipngPath,
                                 strip = True,
                               timeout = args.timeout,
                    )

                    # Clean up ...
                    bothIm.close()
                    del bothIm, draw

        # **********************************************************************

        # Create short-hand ...
        pNameAll = f"{stub2}.png"

        # Check if the map comparison needs making ...
        if not os.path.exists(pNameAll):
            print(f"  Making \"{pNameAll}\" ...")

            # Create empty image to hold all the PNGs ...
            allIm = PIL.Image.new(
                "RGB",
                (
                         2 * (width  + 2 * margin),
                    nProjs * (height + 2 * margin),
                ),
                (
                    127,
                    127,
                    127,
                ),
            )

            # Loop over projection names ...
            for iProj, projectionName in enumerate(projectionNames):
                # Create short-hand and skip if the image is missing ...
                pNameBoth = f"{stub2}/{projectionName}.png"
                if not os.path.exists(pNameBoth):
                    continue

                # Open image ...
                with PIL.Image.open(
                    pNameBoth,
                    mode = "r",
                ) as bothIm:
                    # Convert image to RGB and paste it on to the main image ...
                    allIm.paste(
                        bothIm.convert("RGB"),
                        (
                            0,
                            iProj * bothIm.height,
                        ),
                    )

                # Clean up ...
                bothIm.close()
                del bothIm

            # Save image ...
            pyguymer3.image.image2png(
                allIm,
                pNameAll,
                     chunksize = args.chunksize,
                         debug = args.debug,
                  exiftoolPath = args.exiftoolPath,
                  gifsiclePath = args.gifsiclePath,
                  jpegtranPath = args.jpegtranPath,
                maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS,
                          mode = "RGB",
                      optimise = True,
                   optipngPath = args.optipngPath,
                         strip = True,
                       timeout = args.timeout,
            )

            # Clean up ...
            allIm.close()
            del allIm

    # **************************************************************************
    # **************************************************************************
    # **************************************************************************

    # Initialize list ...
    goodProjectionNames = []

    # Loop over projection names ...
    for projectionName in projectionNames:
        # Skip this projection name if there aren't the correct number of global
        # maps ...
        if nLoc != len(glob.glob(f"{stub1}/(lon=*°,lat=*°) dist=*km/global/{projectionName}.png")):
            print(f"Rejecting {projectionName} because it does not have all the global maps.")
            continue

        # Skip this projection name if there aren't the correct number of local
        # maps ...
        if nLoc != len(glob.glob(f"{stub1}/(lon=*°,lat=*°) dist=*km/local/{projectionName}.png")):
            print(f"Rejecting {projectionName} because it does not have all the local maps.")
            continue

        # Append projection name to list ...
        goodProjectionNames.append(projectionName)

    # Ensure that interesting ones are included ...
    if "AzimuthalEquidistant" not in goodProjectionNames:
        goodProjectionNames.append("AzimuthalEquidistant")
    if "Stereographic" not in goodProjectionNames:
        goodProjectionNames.append("Stereographic")
    goodProjectionNames.sort()

    # Create short-hand ...
    nGoodProjs = len(goodProjectionNames)                                       # [#]

    # Save list of good projection names ...
    with open(
        f"{stub1}/goodProjectionNames.json",
        encoding = "utf-8",
            mode = "wt",
    ) as fObj:
        json.dump(
            goodProjectionNames,
            fObj,
            ensure_ascii = False,
                  indent = 4,
               sort_keys = True,
        )

    # **************************************************************************

    print(f"Making \"{stub1}/global.png\" ...")

    # Create empty image to hold all the PNGs and initialize the drawing object ...
    goodGlobalIm = PIL.Image.new(
        "RGBA",
        (
                  nLoc * (width  + 2 * margin),
            nGoodProjs * (height + 2 * margin),
        ),
        (
            127,
            127,
            127,
        ),
    )
    draw = PIL.ImageDraw.Draw(goodGlobalIm)

    # Loop over good projection names ...
    for iGoodProj, goodProjectionName in enumerate(goodProjectionNames):
        # Loop over stubs ...
        for iLoc, stub2 in enumerate(stub2s):
            # Create short-hand and skip if it does not exist ...
            pNameGlobal = f"{stub2}/global/{goodProjectionName}.png"
            if not os.path.exists(pNameGlobal):
                continue

            # Open image ...
            with PIL.Image.open(
                pNameGlobal,
                mode = "r",
            ) as globalIm:
                # Either paste the RGB image on the main image or overlay the
                # RGBA image over a white rectangle on the main image ...
                match globalIm.mode:
                    case "RGB":
                        goodGlobalIm.paste(
                            globalIm,
                            (
                                     iLoc * (width  + 2 * margin) + margin,
                                iGoodProj * (height + 2 * margin) + margin,
                            ),
                        )
                    case "RGBA":
                        draw.rectangle(
                            [
                                     iLoc * (width  + 2 * margin) + margin,
                                iGoodProj * (height + 2 * margin) + margin,
                                     iLoc * (width  + 2 * margin) + margin + width  - 1,
                                iGoodProj * (height + 2 * margin) + margin + height - 1,
                            ],
                            fill = (
                                255,
                                255,
                                255,
                            ),
                        )
                        goodGlobalIm.alpha_composite(
                            globalIm,
                            dest = (
                                     iLoc * (width  + 2 * margin) + margin,
                                iGoodProj * (height + 2 * margin) + margin,
                            ),
                        )
                    case _:
                        raise Exception(globalIm.mode) from None

            # Clean up ...
            globalIm.close()
            del globalIm

    # Save image ...
    pyguymer3.image.image2png(
        goodGlobalIm,
        f"{stub1}/global.png",
             chunksize = args.chunksize,
                 debug = args.debug,
          exiftoolPath = args.exiftoolPath,
          gifsiclePath = args.gifsiclePath,
          jpegtranPath = args.jpegtranPath,
        maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS,
                  mode = "RGB",
              optimise = True,
           optipngPath = args.optipngPath,
                 strip = True,
               timeout = args.timeout,
    )

    # Clean up ...
    goodGlobalIm.close()
    del goodGlobalIm, draw

    # **************************************************************************

    print(f"Making \"{stub1}/local.png\" ...")

    # Create empty image to hold all the PNGs and initialize the drawing object ...
    goodLocalIm = PIL.Image.new(
        "RGBA",
        (
                  nLoc * (width  + 2 * margin),
            nGoodProjs * (height + 2 * margin),
        ),
        (
            127,
            127,
            127,
        ),
    )
    draw = PIL.ImageDraw.Draw(goodLocalIm)

    # Loop over good projection names ...
    for iGoodProj, goodProjectionName in enumerate(goodProjectionNames):
        # Loop over stubs ...
        for iLoc, stub2 in enumerate(stub2s):
            # Create short-hand and skip if it does not exist ...
            pNameLocal = f"{stub2}/local/{goodProjectionName}.png"
            if not os.path.exists(pNameLocal):
                continue

            # Open image ...
            with PIL.Image.open(
                pNameLocal,
                mode = "r",
            ) as localIm:
                # Either paste the RGB image on the main image or overlay the
                # RGBA image over a white rectangle on the main image ...
                match localIm.mode:
                    case "RGB":
                        goodLocalIm.paste(
                            localIm,
                            (
                                     iLoc * (width  + 2 * margin) + margin,
                                iGoodProj * (height + 2 * margin) + margin,
                            ),
                        )
                    case "RGBA":
                        draw.rectangle(
                            [
                                     iLoc * (width  + 2 * margin) + margin,
                                iGoodProj * (height + 2 * margin) + margin,
                                     iLoc * (width  + 2 * margin) + margin + width  - 1,
                                iGoodProj * (height + 2 * margin) + margin + height - 1,
                            ],
                            fill = (
                                255,
                                255,
                                255,
                            ),
                        )
                        goodLocalIm.alpha_composite(
                            localIm,
                            dest = (
                                     iLoc * (width  + 2 * margin) + margin,
                                iGoodProj * (height + 2 * margin) + margin,
                            ),
                        )
                    case _:
                        raise Exception(localIm.mode) from None

            # Clean up ...
            localIm.close()
            del localIm

    # Save image ...
    pyguymer3.image.image2png(
        goodLocalIm,
        f"{stub1}/local.png",
             chunksize = args.chunksize,
                 debug = args.debug,
          exiftoolPath = args.exiftoolPath,
          gifsiclePath = args.gifsiclePath,
          jpegtranPath = args.jpegtranPath,
        maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS,
                  mode = "RGB",
              optimise = True,
           optipngPath = args.optipngPath,
                 strip = True,
               timeout = args.timeout,
    )

    # Clean up ...
    goodLocalIm.close()
    del goodLocalIm, draw

    # **************************************************************************
    # **************************************************************************
    # **************************************************************************

    # It has to be a pleasing global projection and a local projection where the
    # boundary/exterior is approximately circular.
