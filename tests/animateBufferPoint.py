#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import json
    import os
    import pathlib
    import platform
    import shutil

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
        import geojson
    except:
        raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
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
        import PIL
        import PIL.Image
        PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                         # [px]
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
        import pyguymer3.media
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Animate a point being buffered.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
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
        "--ffmpeg-path",
        default = shutil.which("ffmpeg7") if platform.system() == "Darwin" else shutil.which("ffmpeg"),
           dest = "ffmpegPath",
           help = "the path to the \"ffmpeg\" binary",
           type = str,
    )
    parser.add_argument(
        "--ffprobe-path",
        default = shutil.which("ffprobe7") if platform.system() == "Darwin" else shutil.which("ffprobe"),
           dest = "ffprobePath",
           help = "the path to the \"ffprobe\" binary",
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

    # Check that "ffmpeg" is installed ...
    assert args.ffmpegPath is not None, "\"ffmpeg\" is not installed"

    # Check that "ffprobe" is installed ...
    assert args.ffprobePath is not None, "\"ffprobe\" is not installed"

    # **************************************************************************

    # Configure functions ...
    dist = 2.0e6                                                                # [m]
    fill = 1.0                                                                  # [°]
    fillSpace = "EuclideanSpace"
    simp = -1.0                                                                 # [°]

    # Make output directory ...
    if not os.path.exists("animateBufferPoint"):
        os.mkdir("animateBufferPoint")

    # **************************************************************************

    # Loop over latitude ...
    for lat in range(-90, +92, 2):                          # NOTE: 16,471 images.
        # Loop over longitude ...
        for lon in range(-180, +182, 2):                    # NOTE: 16,471 images.
            # Determine file names ...
            fname = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.png"
            jname = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.geojson"

            # Skip if both outputs already exist ...
            if os.path.exists(fname) and os.path.exists(jname):
                continue

            print(f" > Making \"{jname}\" and \"{fname}\" ...")

            # Create figure ...
            fg = matplotlib.pyplot.figure(
                    dpi = 100,                              # NOTE: Reduce DPI to make test quicker.
                figsize = (9.6, 7.2),
            )

            # Create axis ...
            ax1 = pyguymer3.geo.add_axis(
                fg,
                add_coastlines = False,                     # NOTE: Do not draw coastlines so that changes in GSHGG do not change the image.
                 add_gridlines = True,
                         debug = args.debug,
                           eps = args.eps,
                         index = 1,
                         ncols = 2,
                         nIter = args.nIter,
                         nrows = 2,
                           tol = args.tol,
            )

            # Configure axis ...
            pyguymer3.geo.add_map_background(
                ax1,
                     debug = args.debug,
                resolution = "large1024px",                 # NOTE: Reduce size to make test quicker.
            )

            # Create axis ...
            ax2 = pyguymer3.geo.add_axis(
                fg,
                add_coastlines = False,                     # NOTE: Do not draw coastlines so that changes in GSHGG do not change the image.
                 add_gridlines = True,
                         debug = args.debug,
                           eps = args.eps,
                         index = 2,
                           lat = lat,
                           lon = lon,
                         ncols = 2,
                         nIter = args.nIter,
                         nrows = 2,
                           tol = args.tol,
            )

            # Configure axis ...
            pyguymer3.geo.add_map_background(
                ax2,
                     debug = args.debug,
                resolution = "large1024px",                 # NOTE: Reduce size to make test quicker.
            )

            # Create axis ...
            ax3 = fg.add_subplot(
                2,
                2,
                (3, 4),
            )

            # Configure axis ...
            ax3.grid()
            ax3.set_aspect("equal")
            ax3.set_xlabel("Longitude [°]")
            ax3.set_xlim(-180.0, +180.0)
            ax3.set_xticks(range(-180, 225, 45))
            ax3.set_ylabel("Latitude [°]")
            ax3.set_ylim(-90.0, +90.0)
            ax3.set_yticks(range(-90, 135, 45))

            # Create point ...
            point = shapely.geometry.point.Point(lon, lat)

            # Buffer Point ...
            buff = pyguymer3.geo.buffer(
                point,
                dist,
                    debug = args.debug,
                      eps = args.eps,
                     fill = fill,
                fillSpace = fillSpace,
                     nAng = args.nAng,
                    nIter = args.nIter,
                     simp = simp,
                      tol = args.tol,
            )

            # Plot Point thrice ...
            ax1.add_geometries(
                pyguymer3.geo.extract_polys(buff),
                cartopy.crs.PlateCarree(),
                edgecolor = (1.0, 0.0, 0.0, 1.0),
                facecolor = (1.0, 0.0, 0.0, 0.5),
                linewidth = 1.0,
            )
            ax2.add_geometries(
                pyguymer3.geo.extract_polys(buff),
                cartopy.crs.PlateCarree(),
                edgecolor = (1.0, 0.0, 0.0, 1.0),
                facecolor = (1.0, 0.0, 0.0, 0.5),
                linewidth = 1.0,
            )
            for poly in pyguymer3.geo.extract_polys(buff):
                coords = numpy.array(poly.exterior.coords)                      # [°]
                ax3.plot(
                    coords[:, 0],
                    coords[:, 1],
                    color = (1.0, 0.0, 0.0, 1.0),
                )

            # Save GeoJSON ...
            # NOTE: As of 4/Aug/2025, the Python module "geojson" just converts
            #       the object to a Python dictionary and then it just calls the
            #       standard "json.dump()" function to format the Python
            #       dictionary as text. There is no way to specify the precision
            #       of the written string. Fortunately, if you have no shame,
            #       then you can load and then dump the string again, see:
            #         * https://stackoverflow.com/a/29066406
            with open(jname, "wt", encoding = "utf-8") as fObj:
                json.dump(
                    json.loads(
                        geojson.dumps(
                            buff,
                            ensure_ascii = False,
                                  indent = 4,
                               sort_keys = True,
                        ),
                        parse_float = lambda x: round(float(x), 4),             # NOTE: 0.0001° is approximately 11.1 m.
                    ),
                    fObj,
                    ensure_ascii = False,
                          indent = 4,
                       sort_keys = True,
                )

            # Configure figure ...
            fg.suptitle(f"({lon:+.1f}°,{lat:+.1f}°) buffered by {0.001 * dist:,.1f}km")
            fg.tight_layout()

            # Try to save figure ...
            # NOTE: There is a bug in one of Cartopy v0.21.1, MatPlotLib v3.7.1,
            #       NumPy v1.24.1 or SciPy v1.10.1 which means that the
            #       transform of the background image fails for certain
            #       locations.
            try:
                fg.savefig(fname)
                matplotlib.pyplot.close(fg)
            except:
                print("   Failed")
                matplotlib.pyplot.close(fg)
                continue

            # Optimise PNG ...
            pyguymer3.image.optimise_image(
                fname,
                  debug = args.debug,
                  strip = True,
                timeout = args.timeout,
            )

    # **************************************************************************

    # Initialize list ...
    frames = []

    # Loop over latitude ...
    for lat in range(-90, +92, 2):
        # Loop over longitude ...
        for lon in range(-180, +182, 2):
            # Determine file name and skip if it is missing ...
            frame = f"animateBufferPoint/lon={lon:+04d},lat={lat:+03d}.png"
            if not os.path.exists(frame):
                print(f"WARNING: \"{frame}\" is missing.")
                continue

            # Append it to the list ...
            frames.append(frame)

    print(" > Making \"animateBufferPoint.mp4\" ...")

    # Save 60fps MP4 ...
    vname = pyguymer3.media.images2mp4(
        frames,
              debug = args.debug,
         ffmpegPath = args.ffmpegPath,
        ffprobePath = args.ffprobePath,
                fps = 60.0,
            timeout = 3600.0,
    )
    shutil.move(vname, "animateBufferPoint.mp4")
