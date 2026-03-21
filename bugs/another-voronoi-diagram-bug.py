#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import os
    import pathlib
    import shutil

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
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
    try:
        import shapely
    except:
        raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
        import pyguymer3.media
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Demonstrate another bug in \"shapely.ops.voronoi_diagram\".",
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

    # Loop over records in the GSHHG Shapefile for this level and resolution ...
    for record in cartopy.io.shapereader.Reader(
        cartopy.io.shapereader.gshhs(
            level = 1,
            scale = "f",
        )
    ).records():
        # Skip this record if it is not the chosen one ...
        if record.attributes["id"] != "39218":
            continue

        # Create short-hands ...
        pNames = []
        area = 1.0e6 * record.attributes["area"]                                # [m2]

        # Calculate the minimum possible radius for this area ...
        minRadius = numpy.sqrt(area / numpy.pi)                                 # [m]

        # Make a circle which matches this minimum possible radius ...
        circle = pyguymer3.geo.buffer(
            record.geometry.centroid,
            minRadius,
               debug = args.debug,
                 eps = args.eps,
                fill = -1.0,
                nAng = 361,
               nIter = args.nIter,
            ramLimit = args.ramLimit,
                simp = -1.0,
                 tol = args.tol,
        )

        # Create short-hands and make the field-of-view for the map ...
        midLon = record.geometry.centroid.x                                     # [°]
        midLat = record.geometry.centroid.y                                     # [°]
        maxDist = 2.0 * minRadius                                               # [m]
        fov = pyguymer3.geo.buffer(
            record.geometry.centroid,
            maxDist,
               debug = args.debug,
                 eps = args.eps,
                fill = -1.0,
                nAng = 361,
               nIter = args.nIter,
            ramLimit = args.ramLimit,
                simp = -1.0,
                 tol = args.tol,
        )

        # Find the Voronoi diagram ...
        voronois = pyguymer3.geo.extract_polys(
            shapely.ops.voronoi_diagram(record.geometry),
            onlyValid = True,
               repair = False,
        )

        # Loop over Polygons in the Voronoi diagram ...
        for iVoronoi, voronoi in enumerate(voronois):
            # Find the intersection of the record and this Polygon in the
            # Voronoi diagram ...
            voronoiInters = pyguymer3.geo.extract_polys(
                record.geometry.intersection(voronoi),
                onlyValid = True,
                   repair = False,
            )

            # Loop over Polygons in the intersection of the record and this
            # Polygon in the Voronoi diagram ...
            for iVoronoiInter, voronoiInter in enumerate(voronoiInters):
                # Find the triangles in this Polygon in the intersection of the
                # record and this Polygon in the Voronoi diagram ...
                voronoiInterTris = pyguymer3.geo.extract_polys(
                    shapely.ops.triangulate(voronoiInter),
                    onlyValid = True,
                       repair = False,
                )

                # Loop over triangles in this Polygon in the intersection of the
                # record and this Polygon in the Voronoi diagram ...
                for iVoronoiInterTri, voronoiInterTri in enumerate(voronoiInterTris):
                    # Create short-hand, append to list and skip if this map
                    # already exists ...
                    pName = f'{__file__.removesuffix(".py")}_iVoronoi={iVoronoi:02d}_iVoronoiInter={iVoronoiInter:02d}_iVoronoiInterTri={iVoronoiInterTri:02d}.png'
                    pNames.append(pName)
                    if os.path.exists(pName):
                        continue

                    print(f"Making \"{pName}\" ...")

                    # Create figure ...
                    fg = matplotlib.pyplot.figure(figsize = (4 * 7.2, 7.2))

                    # Create axes ...
                    ax = [
                        pyguymer3.geo.add_axis(
                            fg,
                            add_coastlines = False,
                             add_gridlines = False,
                                     debug = args.debug,
                                      dist = maxDist,
                                       eps = args.eps,
                                       fov = fov,
                                     index = i + 1,
                                       lat = midLat,
                                       lon = midLon,
                                     ncols = 4,
                                     nIter = args.nIter,
                                     nrows = 1,
                                 onlyValid = True,
                                  ramLimit = args.ramLimit,
                                    repair = False,
                                       tol = args.tol,
                        ) for i in range(4)
                    ]

                    # Plot record ...
                    ax[0].add_geometries(
                        record.geometry,
                        cartopy.crs.PlateCarree(),
                        edgecolor = "none",
                        facecolor = "red",
                    )
                    ax[1].add_geometries(
                        record.geometry,
                        cartopy.crs.PlateCarree(),
                            alpha = 0.25,
                        edgecolor = "none",
                        facecolor = "red",
                    )
                    ax[2].add_geometries(
                        record.geometry,
                        cartopy.crs.PlateCarree(),
                            alpha = 0.25,
                        edgecolor = "none",
                        facecolor = "red",
                    )
                    ax[3].add_geometries(
                        record.geometry,
                        cartopy.crs.PlateCarree(),
                            alpha = 0.25,
                        edgecolor = "none",
                        facecolor = "red",
                    )

                    # Plot Voronoi diagram of the record ...
                    ax[1].add_geometries(
                        [voronoi],
                        cartopy.crs.PlateCarree(),
                        edgecolor = "none",
                        facecolor = "green",
                    )
                    ax[2].add_geometries(
                        [voronoi],
                        cartopy.crs.PlateCarree(),
                            alpha = 0.25,
                        edgecolor = "none",
                        facecolor = "green",
                    )
                    ax[3].add_geometries(
                        [voronoi],
                        cartopy.crs.PlateCarree(),
                            alpha = 0.25,
                        edgecolor = "none",
                        facecolor = "green",
                    )

                    # Plot intersection of the record and the Voronoi diagram of
                    # the record ...
                    ax[2].add_geometries(
                        [voronoiInter],
                        cartopy.crs.PlateCarree(),
                        edgecolor = "none",
                        facecolor = "blue",
                    )
                    ax[3].add_geometries(
                        [voronoiInter],
                        cartopy.crs.PlateCarree(),
                            alpha = 0.25,
                        edgecolor = "none",
                        facecolor = "blue",
                    )

                    # Plot triangle in the intersection of the record and the
                    # Voronoi diagram of the record ...
                    ax[3].add_geometries(
                        [voronoiInterTri],
                        cartopy.crs.PlateCarree(),
                        edgecolor = "none",
                        facecolor = "black",
                    )

                    # Configure axes ...
                    ax[0].set_title("record")
                    ax[1].set_title(f"Voronoi diagram of the record\nPolygon #{iVoronoi + 1:d} of {len(voronois):d}")
                    ax[2].set_title(f"intersection of the record and the Voronoi diagram of the record\nPolygon #{iVoronoiInter + 1:d} of {len(voronoiInters):d}")
                    ax[3].set_title(f"triangle in the intersection of the record and the Voronoi diagram of the record\nPolygon #{iVoronoiInterTri + 1:d} of {len(voronoiInterTris):d}")

                    # Configure figure ...
                    fg.suptitle(f"ID \"39218\" in level 1 of \"f\" resolution of GSHHG\n({midLon:.6f}°, {midLat:.6f}°) ± {maxDist:,.1f}m")
                    fg.tight_layout(rect = (0.0, 0.0, 1.0, 0.96))

                    # Save figure ...
                    fg.savefig(pName)
                    matplotlib.pyplot.close(fg)

                    # Optimise PNG ...
                    pyguymer3.image.optimise_image(
                        pName,
                           chunksize = args.chunksize,
                               debug = args.debug,
                        exiftoolPath = args.exiftoolPath,
                        gifsiclePath = args.gifsiclePath,
                        jpegtranPath = args.jpegtranPath,
                         optipngPath = args.optipngPath,
                               strip = True,
                             timeout = args.timeout,
                    )

        # **********************************************************************

        # Create short-hand ...
        wName = f"{__file__.removesuffix(".py")}_fullSize.webp"

        # Check if WEBP needs making ...
        if not os.path.exists(wName):
            print(f"Making \"{wName}\" ...")

            # Make 25 fps WEBP ...
            pyguymer3.media.images2webp(
                pNames,
                wName,
                fps = 25.0,
            )

        # Loop over maximum sizes ...
        for maxSize in [
             256,
             512,
            1024,
            2048,
            4096,
            8192,
        ]:
            # Create short-hand ...
            wName = f"{__file__.removesuffix(".py")}_{maxSize:04d}px.webp"

            # Check if WEBP needs making ...
            if not os.path.exists(wName):
                print(f"Making \"{wName}\" ...")

                # Make 25 fps WEBP ...
                pyguymer3.media.images2webp(
                    pNames,
                    wName,
                             fps = 25.0,
                    screenHeight = maxSize,
                     screenWidth = maxSize,
                )
