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
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy_cache").expanduser(),
            }
        )
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
        "--GSHHG-resolution",
        choices = [
            "c",                        # crude
            "l",                        # low
            "i",                        # intermediate
            "h",                        # high
            "f",                        # full
        ],
        default = "f",                  # full
           dest = "gshhgRes",
           help = "the resolution of the GSHHG datasets",
           type = str,
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
    args = parser.parse_args()

    # **************************************************************************

    # Find the Polygon within the United Kingdom which has the largest Euclidean
    # area ...
    polys = []
    for record in cartopy.io.shapereader.Reader(
        cartopy.io.shapereader.natural_earth(
              category = "cultural",
                  name = "admin_0_countries",
            resolution = "110m",
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

    # Initialise list ...
    pNames = []

    # Loop over grid sizes ...
    for grid in [
        "2x1",
        "4x2",
        "8x4",
        "16x8",
        "32x16",
        "64x32",
        "128x64",
    ]:
        print(f"Processing {grid} ...")

        # Create short-hands ...
        nx, ny = grid.split("x")
        ny = int(ny)                                                            # [#]
        nx = int(nx)                                                            # [#]
        pName = f'{__file__.removesuffix(".py")}_{grid}.png'
        pNames.append(pName)

        # Create figure ...
        fg = matplotlib.pyplot.figure(figsize = (7.2, 7.2))

        # Create axis ...
        ax = pyguymer3.geo.add_axis(
            fg,
            add_coastlines = False,
             add_gridlines = True,
                     debug = args.debug,
                      dist = maxDist,
                       eps = args.eps,
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
                # Make a correctly oriented Polygon of the border of the tile
                # and skip this tile if it does not overlap with the
                # field-of-view ...
                tile = shapely.geometry.polygon.orient(
                    shapely.geometry.polygon.Polygon(
                        shapely.geometry.polygon.LinearRing(
                            [
                                (-180.0 + 360.0 * float(ix    ) / float(nx), +90.0 - 180.0 * float(iy    ) / float(ny)),
                                (-180.0 + 360.0 * float(ix + 1) / float(nx), +90.0 - 180.0 * float(iy    ) / float(ny)),
                                (-180.0 + 360.0 * float(ix + 1) / float(nx), +90.0 - 180.0 * float(iy + 1) / float(ny)),
                                (-180.0 + 360.0 * float(ix    ) / float(nx), +90.0 - 180.0 * float(iy + 1) / float(ny)),
                                (-180.0 + 360.0 * float(ix    ) / float(nx), +90.0 - 180.0 * float(iy    ) / float(ny)),
                            ]
                        )
                    )
                )
                if tile.disjoint(fov):
                    continue

                print(f"  Drawing \"{args.absPathToRepo}/pyguymer3/data/png/gshhg/{nx:d}x{ny:d}/res={args.gshhgRes}/x={ix:d}/y={iy:d}.png\" ...")

                # Draw tile ...
                # NOTE: As of 5/Dec/2023, the default "zorder" of the gridlines
                #       is 2.0.
                ax.imshow(
                    matplotlib.image.imread(f"{args.absPathToRepo}/pyguymer3/data/png/gshhg/{nx:d}x{ny:d}/res={args.gshhgRes}/x={ix:d}/y={iy:d}.png"),
                           extent = [
                        -180.0 + 360.0 * float(ix    ) / float(nx),
                        -180.0 + 360.0 * float(ix + 1) / float(nx),
                         +90.0 - 180.0 * float(iy + 1) / float(ny),
                         +90.0 - 180.0 * float(iy    ) / float(ny),
                    ],
                    interpolation = "none",
                           origin = "upper",
                         resample = False,
                        transform = cartopy.crs.PlateCarree(),
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

    print(f'Making \"{__file__.removesuffix(".py")}.webp\" ...')

    # Save 1fps WEBP ...
    pyguymer3.media.images2webp(
        pNames,
        f'{__file__.removesuffix(".py")}.webp',
        fps = 1.0,
    )

    # Loop over maximum sizes ...
    # NOTE: By inspection, the PNG frames are 2,160 px tall/wide.
    for maxSize in [
         512,
        1024,
        2048,
    ]:
        print(f'Making \"{__file__.removesuffix(".py")}{maxSize:04d}px.webp\" ...')

        # Save 1fps WEBP ...
        pyguymer3.media.images2webp(
            pNames,
            f'{__file__.removesuffix(".py")}{maxSize:04d}px.webp',
                     fps = 1.0,
            screenHeight = maxSize,
             screenWidth = maxSize,
        )
