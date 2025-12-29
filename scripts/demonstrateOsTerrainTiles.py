#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import os
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
            description = "Demonstrate the tiles.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--absolute-path-to-repository",
        default = os.path.dirname(os.path.dirname(__file__)),
           dest = "absPathToRepo",
           help = "the absolute path to the PyGuymer3 repository",
           type = str,
    )
    parser.add_argument(
        "--coastlines-resolution",
        choices = [
            "c",                        # crude
            "l",                        # low
            "i",                        # intermediate
            "h",                        # high
            "f",                        # full
        ],
        default = "f",                  # full
           dest = "coastlinesRes",
           help = "the resolution of the coastlines",
           type = str,
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
        "--geodesic-convergence",
        default = 10000.0,
           dest = "geodesicConv",
           help = "the *initial* Geodesic distance that defines the middle as being converged (in metres)",
           type = float,
    )
    parser.add_argument(
        "--maximum-elevation",
        default = 1000,
           dest = "maxElev",
           help = "the maximum elevation (in metres)",
           type = int,
    )
    parser.add_argument(
        "--nAng",
        default = 361,
           dest = "nAng",
           help = "the number of angles around each circle",
           type = int,
    )
    parser.add_argument(
        "--nIter",
        default = 1000000,
           dest = "nIter",
           help = "the maximum number of iterations (particularly the Vincenty formula)",
           type = int,
    )
    parser.add_argument(
        "--nRefine",
        default = 6,
           dest = "nRefine",
           help = "the number of refinements to make (each refinement halves the \"geodesic-convergence\" distance)",
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
    parser.add_argument(
        "--UK-resolution",
        choices = [
             "10m",
             "50m",
            "110m",
        ],
        default = "110m",
           dest = "ukRes",
           help = "the resolution of the UK when finding its centre",
           type = str,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Create short-hands ...
    # NOTE: See "pyguymer3/data/png/README.md".
    origRes = 50                                                                # [m/px]
    origNumX = 13200                                                            # [px]
    origNumY = 24600                                                            # [px]
    origWidth = origRes * origNumX                                              # [m]
    origHeight = origRes * origNumY                                             # [m]
    tileSize = 300                                                              # [px]

    # **************************************************************************

    print("Finding the United Kingdom mainland ...")

    # Find the Polygon within the United Kingdom which has the largest Euclidean
    # area ...
    polys = []
    for record in cartopy.io.shapereader.Reader(
        cartopy.io.shapereader.natural_earth(
              category = "cultural",
                  name = "admin_0_countries",
            resolution = args.ukRes,
        )
    ).records():
        if pyguymer3.geo.getRecordAttribute(record, "NAME") != "United Kingdom":
            continue
        polys.extend(
            pyguymer3.geo.extract_polys(
                record.geometry,
                onlyValid = True,
                   repair = True,
            )
        )
    uk = polys[0]
    for poly in polys:
        if poly.area > uk.area:
            uk = poly
    del polys

    # Find the longitudes and latitudes of the exterior ring of the United
    # Kingdom mainland ...
    coords = numpy.array(uk.exterior.coords)                                    # [°]
    del uk

    # **************************************************************************

    print("Finding the centre of the United Kingdom mainland ...")

    # Calculate the Geodesic bounding circle ...
    midLon, midLat, maxDist = pyguymer3.geo.find_middle_of_locs(
        coords[:, 0],
        coords[:, 1],
           conv = args.geodesicConv,                                            # 10 km
          debug = args.debug,
            eps = args.eps,
         method = "GeodesicCircle",
           nAng = args.nAng,
          nIter = args.nIter,
        nRefine = args.nRefine,                                                 # 156.25 m
            pad = args.geodesicConv,                                            # 10 km
    )                                                                           # [°], [°], [m]
    del coords
    fov = pyguymer3.geo.buffer(
        shapely.geometry.point.Point(midLon, midLat),
        maxDist,
        debug = args.debug,
          eps = args.eps,
         fill = -1.0,
         nAng = args.nAng,
        nIter = args.nIter,
         simp = -1.0,
          tol = args.tol,
    )
    print(f"The Geodesic circle which encompasses the United Kingdom mainland is centred at ({midLon:.6f}°, {midLat:.6f}°) and has a radius of {0.001 * maxDist:,.1f} km (inc. padding).")

    # **************************************************************************

    # Create short-hands, ensure output directory exists and initialise list ...
    dName = __file__.removesuffix(".py")
    if not os.path.exists(dName):
        os.mkdir(dName)
    pNames = []
    wName = f"{dName}/all.webp"

    # Loop over grid sizes ...
    for grid in [
        "22x41",
        "44x82",
    ]:
        print(f"Processing {grid} ...")

        # Create short-hands and append PNG name ...
        nx, ny = grid.split("x")
        nx = int(nx)                                                            # [#]
        ny = int(ny)                                                            # [#]
        pName = f"{dName}/{grid}.png"
        pNames.append(pName)

        # Create figure ...
        fg = matplotlib.pyplot.figure(figsize = (7.2, 7.2))

        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
                   add_coastlines = True,
                    add_gridlines = True,
             coastlines_edgecolor = "white",
            coastlines_resolution = args.coastlinesRes,
                coastlines_zorder = 1.75,
                            debug = args.debug,
                             dist = maxDist,
                              eps = args.eps,
                              fov = fov,
                              lat = midLat,
                              lon = midLon,
                            nIter = args.nIter,
                        onlyValid = True,
                           repair = True,
                              tol = args.tol,
        )

        # Loop over tiles ...
        for ix in range(nx):
            for iy in range(ny):
                # Create short-hand and skip if this tile does not exist ...
                tName = f"{args.absPathToRepo}/pyguymer3/data/png/osTerrain/{nx:d}x{ny:d}/maxElev={args.maxElev:d}m/x={ix:d}/y={iy:d}.png"
                if not os.path.exists(tName):
                    continue

                # Create short-hands ...
                left   =                     float(origWidth  *  ix     ) / float(nx)   # [m]
                right  =                     float(origWidth  * (ix + 1)) / float(nx)   # [m]
                bottom = float(origHeight) - float(origHeight * (iy + 1)) / float(ny)   # [m]
                top    = float(origHeight) - float(origHeight *  iy     ) / float(ny)   # [m]

                # Make a correctly oriented Polygon of the border of the tile
                # and skip this tile if it does not overlap with the
                # field-of-view ...
                tile = pyguymer3.geo.en2ll(
                    shapely.geometry.polygon.orient(
                        shapely.geometry.polygon.Polygon(
                            shapely.geometry.polygon.LinearRing(
                                [
                                    (left , top   ),
                                    (right, top   ),
                                    (right, bottom),
                                    (left , bottom),
                                    (left , top   ),
                                ]
                            )
                        )
                    ),
                    debug = args.debug,
                      tol = args.tol,
                )
                if tile.disjoint(fov):
                    continue

                print(f"  Drawing \"{tName}\" ...")

                # Draw tile ...
                # NOTE: I am explicitly setting the regrid shape based off the
                #       size of the tile, as well as a safety factor
                #       (remembering Nyquist).
                # NOTE: As of 5/Dec/2023, the default "zorder" of the gridlines
                #       is 2.0.
                # NOTE: There is an off-by-one error in Cartopy somewhere ... I
                #       *think* that "cartopy.img_transform.mesh_projection()"
                #       shrinks the array by half a pixel at both ends.
                ax.imshow(
                    matplotlib.image.imread(tName),
                           extent = [
                        left,
                        right,
                        bottom,
                        top,
                    ],
                    interpolation = "gaussian",
                           origin = "upper",
                     regrid_shape = (600, 600),
                         resample = False,
                        transform = cartopy.crs.OSGB(),
                           zorder = 1.5,
                )

        # Configure axis ...
        ax.set_title(grid)

        # Configure figure ...
        fg.tight_layout()

        print(f"  Writing \"{pName}\" ...")

        # Save figure ...
        fg.savefig(pName)
        matplotlib.pyplot.close(fg)

        # Optimise PNG ...
        pyguymer3.image.optimise_image(
            pName,
              debug = args.debug,
              strip = True,
            timeout = args.timeout,
        )

    # **************************************************************************

    print(f"Making \"{wName}\" ...")

    # Save 1 fps WEBP ...
    pyguymer3.media.images2webp(
        pNames,
        wName,
        fps = 1.0,
    )
