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
        import PIL.ImageDraw
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None
    try:
        import shapely
        import shapely.ops
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
            description = "Rasterize the GSHHG datasets.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Find the absolute path to the repository ...
    absPathToRepo = os.path.dirname(os.path.dirname(__file__))

    # Create short-hands ...
    bName = f"{absPathToRepo}/scripts/globe.bin"
    zName = f"{absPathToRepo}/scripts/globe.zip"
    url = "https://www.ngdc.noaa.gov/mgg/topo/DATATILES/elev/globe.zip"
    nx = 43200                                                                  # [px]
    ny = 21600                                                                  # [px]

    # Create short-hand ...
    # NOTE: See "pyguymer3/data/png/README.md".
    tileSize = 600                                                              # [px]

    # **************************************************************************

    # Check if the ZIP file does not exist yet ...
    if not os.path.exists(zName):
        print(f"Downloading \"{zName}\" ...")

        # Start session ...
        with pyguymer3.start_session() as sess:
            # Download the ZIP file ...
            assert pyguymer3.download_file(
                sess,
                url,
                zName,
                  debug = args.debug,
                timeout = args.timeout,
                 verify = True,
            ), f"failed to download \"{url}\" to \"{zName}\""

    # **************************************************************************

    # Check if the BIN file does not exist yet ...
    if not os.path.exists(bName):
        print(f"Making \"{bName}\" ...")

        # Define constant ...
        bins = [
            "all10/a11g",
            "all10/b10g",
            "all10/c10g",
            "all10/d10g",
            "all10/e10g",
            "all10/f10g",
            "all10/g10g",
            "all10/h10g",
            "all10/i10g",
            "all10/j10g",
            "all10/k10g",
            "all10/l10g",
            "all10/m10g",
            "all10/n10g",
            "all10/o10g",
            "all10/p10g",
        ]

        # Make map ...
        elev = numpy.zeros(
            (ny, nx),
            dtype = numpy.int16,
        )                                                                       # [m]

        # Load dataset ...
        with zipfile.ZipFile(zName, "r") as fObj:
            # Initialize index ...
            iy = 0                                                              # [px]

            # Loop over y-axis ...
            for i in range(4):
                # Initialize index ...
                ix = 0                                                          # [px]

                # Loop over x-axis ...
                for j in range(4):
                    # Define tile size ...
                    if i in [0, 3]:
                        nrows = 4800                                            # [px]
                    else:
                        nrows = 6000                                            # [px]
                    ncols = 10800                                               # [px]

                    # Load tile ...
                    tile = numpy.frombuffer(
                        fObj.read(bins[j + i * 4]),
                        dtype = numpy.int16,
                    ).reshape(nrows, ncols)                                     # [m]

                    # Fill map ...
                    elev[iy:iy + tile.shape[0], ix:ix + tile.shape[1]] = tile[:, :] # [m]

                    # Increment index ...
                    ix += ncols                                                 # [px]

                # Increment index ...
                iy += nrows                                                     # [px]

        # Save BIN ...
        elev.tofile(bName)
        del elev

    # **************************************************************************
