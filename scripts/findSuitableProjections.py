#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import glob
    import json
    import math
    import os
    import pathlib
    import resource
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

    # Set the maximum area of address space which may be taken by the process ...
    try:
        resource.setrlimit(resource.RLIMIT_AS, (16 * 1024 * 1024 * 1024, resource.RLIM_INFINITY))
    except:
        print("WARNING: Failed to limit the maximum area of address space which may be taken by the process - prepare for OOM errors.")

    # **************************************************************************

    # NOTE: https://github.com/SciTools/cartopy/pull/2378
    # NOTE: https://cartopy.readthedocs.io/stable/gallery/lines_and_polygons/effects_of_the_ellipse.html

    # Create short-hands ...
    height = 720                                                                # [px]
    margin = 5                                                                  # [px]
    width = 720                                                                 # [px]

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

    # Save list of projection names ...
    with open(
        f'{__file__.removesuffix(".py")}/projectionNames.json',
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

    # Loop over centres, distances and zooms ...
    for iLoc, (lon, lat, dist, gshhgRes, zoom, gridlines_int) in enumerate(
        [
            (  -4.0, +53.0,    50.0e3, "f", 9, 1),  # ~Snowdonia holiday
            (+157.0, -31.0,  3000.0e3, "c", 4, 5),  # ~Australia holiday
            (-180.0, +90.0,  1000.0e3, "i", 6, 2),  # Satisfies test A, C, D, F
            ( -90.0, +45.0,  1000.0e3, "i", 6, 2),  # Satisfies test A
            (   0.0,   0.0,  1000.0e3, "i", 6, 2),  # Satisfies test A, B
            ( +90.0, -45.0,  1000.0e3, "i", 6, 2),  # Satisfies test A
            (+180.0, -90.0,  1000.0e3, "i", 6, 2),  # Satisfies test A, C, D, F
            (+170.0, +10.0,  4000.0e3, "c", 4, 5),  # Satisfies test B, C, E
            (+170.0, +80.0,  4000.0e3, "c", 4, 5),  # Satisfies test C, D, F
            (   0.0, +83.0,  1000.0e3, "i", 6, 2),  # Satisfies test C, D, F
            ( -90.0, -83.0,  1000.0e3, "i", 6, 2),  # Satisfies test C, D, F
            (   0.0,   0.0, 10000.0e3, "c", 4, 5),  # Satisfies test A, B
        ]
    ):
        print(f"Processing ({lon:+.1f}°,{lat:+.1f}°) with {round(0.001 * dist):,d} km ...")

        # Create short-hands and buffer the centre by the distance ...
        point1 = shapely.geometry.point.Point(lon, lat)
        polygon1 = pyguymer3.geo.buffer(
            point1,
            dist,
            debug = False,
             fill = +1.0,                                                       # NOTE: Need to fill in the result to
             nAng = 361,                                                        #       undo the ".simplify(tol)" in
             simp = -1.0,                                                       #       "buffer_CoordinateSequence()".
        )
        stub1 = f'{__file__.removesuffix(".py")}/(lon={lon:+.1f}°,lat={lat:+.1f}°) dist={round(0.001 * dist):,d}km'

        # Save Polygon as a GeoJSON ...
        # NOTE: As of 4/Aug/2025, the Python module "geojson" just converts the
        #       object to a Python dictionary and then it just calls the
        #       standard "json.dump()" function to format the Python dictionary
        #       as text. There is no way to specify the precision of the written
        #       string. Fortunately, if you have no shame, then you can load and
        #       then dump the string again, see:
        #         * https://stackoverflow.com/a/29066406
        with open(
            f"{stub1}.geojson",
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
                dName = f"{stub1}/global"
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
                    # ax.add_image(
                    #     cartopy.io.img_tiles.OSM(
                    #         cache = True,
                    #     ),
                    #     4,
                    # )
                    ax.stock_img()
                    ax.add_geometries(
                        pyguymer3.geo.extract_polys(
                            polygon1,
                            onlyValid = True,
                               repair = False,
                        ),
                        pyguymer3.GEODETIC,
                        edgecolor = (0.0, 0.0, 1.0, 1.0),
                        facecolor = (0.0, 0.0, 1.0, 0.5),
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
                            debug = False,
                              fov = pyguymer3.EARTH,
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
                    if projectionName == "AzimuthalEquidistant":
                        fg.patch.set_facecolor((1.0, 0.0, 0.0, 0.5))
                    fg.suptitle(projectionName)
                    fg.tight_layout()

                    # Try to save figure ...
                    try:
                        fg.savefig(pNameGlobal)
                        pyguymer3.image.optimise_image(
                            pNameGlobal,
                            strip = True,
                        )
                    except:
                        print("    WARNING: Saving the figure failed.")

                    # Clean up ...
                    matplotlib.pyplot.close(fg)
                    del ax, fg

                # **************************************************************
                # **************************************************************
                # **************************************************************

                # Create short-hands and make output directory if it is missing ...
                dName = f"{stub1}/local"
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

                    # Catch some common errors ...
                    try:
                        # Set extent ...
                        minR, maxR = pyguymer3.geo._set_axis_boundary(ax, point1, polygon1, dist)

                        # Add background and buffer ...
                        # ax.add_image(
                        #     cartopy.io.img_tiles.OSM(
                        #         cache = True,
                        #     ),
                        #     zoom,
                        # )
                        ax.stock_img()
                        ax.add_geometries(
                            pyguymer3.geo.extract_polys(
                                polygon1,
                                onlyValid = True,
                                   repair = False,
                            ),
                            pyguymer3.GEODETIC,
                            edgecolor = (0.0, 0.0, 1.0, 1.0),
                            facecolor = (0.0, 0.0, 1.0, 0.5),
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
                                debug = False,
                                  fov = polygon1,
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
                        if projectionName == "AzimuthalEquidistant":
                            fg.patch.set_facecolor((1.0, 0.0, 0.0, 0.5))
                        fg.suptitle(f"{projectionName} : {100.0 * minR:5.1f}% : {100.0 * maxR:5.1f}%")
                        fg.tight_layout()

                        # Try to save figure and optimise PNG ...
                        try:
                            fg.savefig(pNameLocal)
                            pyguymer3.image.optimise_image(
                                pNameLocal,
                                strip = True,
                            )
                        except:
                            print("    WARNING: Saving the figure failed.")
                    except shapely.errors.GEOSException:
                        print("    WARNING: Setting the boundary failed.")

                    # Clean up ...
                    matplotlib.pyplot.close(fg)
                    del ax, fg

                # **************************************************************
                # **************************************************************
                # **************************************************************

                # Create short-hand ...
                pNameBoth = f"{stub1}/{projectionName}.png"

                # Check if the map comparison needs making ...
                if not os.path.exists(pNameBoth):
                    print(f"    Making \"{pNameBoth}\" ...")

                    # Create empty image to hold both the PNGs and initialize
                    # the drawing object ...
                    bothIm = PIL.Image.new(
                        "RGBA",
                        (
                            2 * width + 3 * margin,
                               height + 2 * margin,
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
                                            width + 2 * margin,
                                                        margin,
                                        ),
                                    )
                                case "RGBA":
                                    draw.rectangle(
                                        [
                                            width + 2 * margin,
                                                        margin,
                                            width + 2 * margin + width  - 1,
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
                                            width + 2 * margin,
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
                         mode = "RGB",
                        strip = True,
                    )

                    # Clean up ...
                    bothIm.close()
                    del bothIm, draw

        # **********************************************************************

        # Create short-hand ...
        pNameAll = f"{stub1}.png"

        # Check if the map comparison needs making ...
        if not os.path.exists(pNameAll):
            print(f"  Making \"{pNameAll}\" ...")

            # Create empty image to hold all the PNGs ...
            allIm = PIL.Image.new(
                "RGB",
                (
                                            2 * width + 3 * margin ,
                    len(projectionNames) * (   height + 2 * margin),
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
                pNameBoth = f"{stub1}/{projectionName}.png"
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
                 mode = "RGB",
                strip = True,
            )

            # Clean up ...
            allIm.close()
            del allIm

    # **************************************************************************
    # **************************************************************************
    # **************************************************************************

    # It has to be a pleasing global projection and a local projection where the
    # boundary/exterior is approximately circular.

    # AzimuthalEquidistant           <--------------------
    # Stereographic
    # LambertAzimuthalEqualArea (does not work for Australia)
