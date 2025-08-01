#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import os
    import zipfile

    # Import special modules ...
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

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.geo
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # Import local modules ...
    import generateNeTilesSrc as funcs

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Rasterize the NE datasets and save them as tiles.",
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
        "--elevation-band-interval",
        default = 250,
           dest = "elevBandInt",
           help = "the interval of the elevation bands to shade (in metres)",
           type = int,
    )
    parser.add_argument(
        "--maximum-elevation-interval",
        default = 1000,
           dest = "maxElevInt",
           help = "the elevation interval to make tiles for (in metres)",
           type = int,
    )
    parser.add_argument(
        "--resolutions",
        choices = [
             "10m",
             "50m",
            "110m",
        ],
        default = [
            "10m",
        ],
           dest = "ress",
           help = "the resolutions of the NE datasets",
          nargs = "+",
           type = str,
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Create short-hands ...
    bName = f"{args.absPathToRepo}/scripts/globe.bin"
    zName = f"{args.absPathToRepo}/scripts/globe.zip"
    url = "https://www.ngdc.noaa.gov/mgg/topo/DATATILES/elev/globe.zip"

    # Create short-hands ...
    # NOTE: See "pyguymer3/data/png/README.md".
    nx = 43200                                                                  # [px]
    ny = 21600                                                                  # [px]
    tileSize = 300                                                              # [px]
    nTilesX = nx // tileSize                                                    # [#]
    nTilesY = ny // tileSize                                                    # [#]

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
                    del tile

                    # Increment index ...
                    ix += ncols                                                 # [px]

                # Increment index ...
                iy += nrows                                                     # [px]

        # Save BIN ...
        elev.tofile(bName)
        del elev

    # **************************************************************************

    # Loop over maximum elevations ...
    for maxElev in range(args.maxElevInt, 9000, args.maxElevInt):
        print(f"Processing maximum elevation {maxElev:,d} m ...")

        # **********************************************************************

        # Load data and replace sea with 0 m elevation ...
        arr = numpy.fromfile(
            bName,
            dtype = numpy.int16,
        ).reshape(ny, nx)                                                       # [m]
        numpy.place(arr, arr == -500, 0)                                        # [m]
        if maxElev > arr.max():
            print("  Stopping as the maximum elevation has been reached.")
            break

        # Start ~infinite loop ...
        for shrinkLevel in range(1, 100):
            # Create short-hands and stop looping if this shrink level is too
            # small ...
            shrinkFactor = pow(2, shrinkLevel)
            if nTilesX % shrinkFactor != 0 or nTilesY % shrinkFactor != 0:
                break
            nShrunkenTilesX = nTilesX // shrinkFactor                           # [#]
            nShrunkenTilesY = nTilesY // shrinkFactor                           # [#]
            if nShrunkenTilesX == 0 or nShrunkenTilesY == 0:
                break

            print(f"  Processing shrink level {shrinkLevel:,d} ...")

            # Create shrunken data ...
            # NOTE: The documentation of "numpy.mean()" says "float64
            #       intermediate and return values are used for integer inputs".
            shrunkenArr = numpy.zeros(
                (ny // shrinkFactor, nx // shrinkFactor),
                dtype = numpy.float64,
            )                                                                   # [m]
            for iy in range(ny // shrinkFactor):
                for ix in range(nx // shrinkFactor):
                    shrunkenArr[iy, ix] = arr[iy * shrinkFactor:(iy + 1) * shrinkFactor, ix * shrinkFactor:(ix + 1) * shrinkFactor].mean()  # [m]
            shrunkenArr = shrunkenArr.astype(numpy.int16)

            # ******************************************************************

            # Loop over resolutions ...
            for res in args.ress:
                print(f"    Processing resolution {res} ...")

                # Create the PIL image and drawing object ...
                img = PIL.Image.new(
                    color = matplotlib.colors.CSS4_COLORS["lightblue"],
                     mode = "RGB",
                     size = (nx // shrinkFactor, ny // shrinkFactor),
                )
                draw = PIL.ImageDraw.Draw(img)

                # **************************************************************

                # Draw layers below elevation data ...
                funcs.drawBathymetry(img, draw, res)
                funcs.drawAntarcticIceShelves(img, draw, res)
                funcs.drawReefs(img, draw, res)
                funcs.drawLand(img, draw, res)
                funcs.drawMinorIslands(img, draw, res)

                # **************************************************************

                # Create suitable colour map ...
                cmap = matplotlib.colors.LinearSegmentedColormap.from_list(
                    "elevation",
                    [
                        matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["olivedrab"]),
                        matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightgrey"]),
                    ],
                    N = maxElev // args.elevBandInt,
                )

                # Loop over pixels ...
                for iy in range(ny // shrinkFactor):
                    for ix in range(nx // shrinkFactor):
                        # Find out which elevation band this pixel is (and skip
                        # it if it is too low) ...
                        band = min(shrunkenArr[iy, ix] // args.elevBandInt, maxElev // args.elevBandInt)
                        if band < 1:
                            continue

                        # Paint the pixel ...
                        r, g, b, _ = cmap(band - 1, bytes = True)
                        assert isinstance(r, numpy.uint8)
                        assert isinstance(g, numpy.uint8)
                        assert isinstance(b, numpy.uint8)
                        img.putpixel(
                            (ix, iy),
                            (int(r), int(g), int(b)),
                        )
                del cmap

                # **************************************************************

                # Draw layers above elevation data ...
                funcs.drawGlaciatedAreas(img, draw, res)
                funcs.drawLakes(img, draw, res)
                funcs.drawPlayas(img, draw, res)
                funcs.drawRivers(img, draw, res)
                del draw

                # ******************************************************************

                print("    Converting PIL image in to NumPy array ...")

                # Convert PIL image to NumPy array ...
                imgArr = numpy.array(img).reshape((ny // shrinkFactor, nx // shrinkFactor, 3))
                del img

                # **************************************************************

                # Loop over shrunken x tiles ...
                for iShrunkenTileX in range(nShrunkenTilesX):
                    # Loop over shrunken y tiles ...
                    for iShrunkenTileY in range(nShrunkenTilesY):
                        # Create short-hands, make sure that the directory exists
                        # and skip this tile if it already exists ...
                        dName = f"{args.absPathToRepo}/pyguymer3/data/png/ne/{nShrunkenTilesX:d}x{nShrunkenTilesY:d}/maxElev={maxElev:d}m/elevInt={args.elevBandInt:d}m/res={res}/x={iShrunkenTileX:d}"
                        pName = f"{dName}/y={iShrunkenTileY:d}.png"
                        if not os.path.exists(dName):
                            os.makedirs(dName)
                        if os.path.exists(pName):
                            print(f"    Not making \"{pName}\".")
                            continue

                        print(f"    Making \"{pName}\" ...")

                        # Make PNG source and write it ...
                        src = pyguymer3.image.makePng(
                            imgArr[iShrunkenTileY * tileSize:(iShrunkenTileY + 1) * tileSize, iShrunkenTileX * tileSize:(iShrunkenTileX + 1) * tileSize, :],
                                 debug = args.debug,
                                levels = [9,],
                             memLevels = [9,],
                            strategies = None,
                                wbitss = [15,],
                        )
                        with open(pName, "wb") as fObj:
                            fObj.write(src)
                        del src
                del imgArr
            del shrunkenArr

        # **********************************************************************

        print("  Processing original size ...")

        # Loop over resolutions ...
        for res in args.ress:
            print(f"    Processing resolution {res} ...")

            # Create the PIL image and drawing object ...
            img = PIL.Image.new(
                color = matplotlib.colors.CSS4_COLORS["lightblue"],
                 mode = "RGB",
                 size = (nx, ny),
            )
            draw = PIL.ImageDraw.Draw(img)

            # ******************************************************************

            # Draw layers below elevation data ...
            funcs.drawBathymetry(img, draw, res)
            funcs.drawAntarcticIceShelves(img, draw, res)
            funcs.drawReefs(img, draw, res)
            funcs.drawLand(img, draw, res)
            funcs.drawMinorIslands(img, draw, res)

            # ******************************************************************

            # Create suitable colour map ...
            cmap = matplotlib.colors.LinearSegmentedColormap.from_list(
                "elevation",
                [
                    matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["olivedrab"]),
                    matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightgrey"]),
                ],
                N = maxElev // args.elevBandInt,
            )

            # Loop over pixels ...
            for iy in range(ny):
                for ix in range(nx):
                    # Find out which elevation band this pixel is (and skip it
                    # if it is too low) ...
                    band = min(arr[iy, ix] // args.elevBandInt, maxElev // args.elevBandInt)
                    if band < 1:
                        continue

                    # Paint the pixel ...
                    r, g, b, _ = cmap(band - 1, bytes = True)
                    assert isinstance(r, numpy.uint8)
                    assert isinstance(g, numpy.uint8)
                    assert isinstance(b, numpy.uint8)
                    img.putpixel(
                        (ix, iy),
                        (int(r), int(g), int(b)),
                    )
            del cmap

            # ******************************************************************

            # Draw layers above elevation data ...
            funcs.drawGlaciatedAreas(img, draw, res)
            funcs.drawLakes(img, draw, res)
            funcs.drawPlayas(img, draw, res)
            funcs.drawRivers(img, draw, res)
            del draw

            # ******************************************************************

            print("    Converting PIL image in to NumPy array ...")

            # Convert PIL image to NumPy array ...
            imgArr = numpy.array(img).reshape((ny, nx, 3))
            del img

            # ******************************************************************

            # Loop over x tiles ...
            for iTileX in range(nTilesX):
                # Loop over y tiles ...
                for iTileY in range(nTilesY):
                    # Create short-hands, make sure that the directory exists
                    # and skip this tile if it already exists ...
                    dName = f"{args.absPathToRepo}/pyguymer3/data/png/ne/{nTilesX:d}x{nTilesY:d}/maxElev={maxElev:d}m/elevInt={args.elevBandInt:d}m/res={res}/x={iTileX:d}"
                    pName = f"{dName}/y={iTileY:d}.png"
                    if not os.path.exists(dName):
                        os.makedirs(dName)
                    if os.path.exists(pName):
                        print(f"    Not making \"{pName}\".")
                        continue

                    print(f"    Making \"{pName}\" ...")

                    # Make PNG source and write it ...
                    src = pyguymer3.image.makePng(
                        imgArr[iTileY * tileSize:(iTileY + 1) * tileSize, iTileX * tileSize:(iTileX + 1) * tileSize, :],
                             debug = args.debug,
                            levels = [9,],
                         memLevels = [9,],
                        strategies = None,
                            wbitss = [15,],
                    )
                    with open(pName, "wb") as fObj:
                        fObj.write(src)
                    del src
            del imgArr
        del arr
