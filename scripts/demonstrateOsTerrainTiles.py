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
            description = "Demonstrate the \"OS Terrain 50\" dataset tiles.",
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

    # Create short-hand ...
    # NOTE: See "pyguymer3/data/png/README.md".
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
    dName = f'{__file__.removesuffix(".py")}/maxElev={args.maxElev:d}m'
    if not os.path.exists(dName):
        os.makedirs(dName)
    pNames = []
    wName = f"{dName}/all.webp"

    # Loop over grid sizes ...
    for grid in [
        "22x41",
        "44x82",
    ]:
        print(f"Processing {grid} ...")

        # Create short-hand and append PNG name ...
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

        # Add "OS Terrain 50" tiles ...
        pyguymer3.geo.add_OSterrain_tiles(
            ax,
            fov,
                    debug = args.debug,
                     grid = grid,
            interpolation = "gaussian",
                  maxElev = args.maxElev,
             regrid_shape = (
                round(2.0 * fg.get_figwidth() * fg.get_dpi()),
                round(2.0 * fg.get_figheight() * fg.get_dpi()),
            ),                                                                  # [px], [px]
                 resample = False,
                  timeout = args.timeout,
                      tol = args.tol,
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
