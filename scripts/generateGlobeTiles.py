#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import json
    import os

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Save the GLOBE dataset as tiles.",
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

    # Load colour tables and create short-hand ...
    with open(f"{absPathToRepo}/pyguymer3/data/json/colourTables.json", "rt", encoding = "utf-8") as fObj:
        colourTables = json.load(fObj)
    turbo = numpy.array(colourTables["turbo"]).astype(numpy.uint8)

    # Create short-hands ...
    # NOTE: See "pyguymer3/data/png/README.md".
    nx = 43200                                                                  # [px]
    ny = 21600                                                                  # [px]
    tileSize = 600                                                              # [px]
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
    for maxElev in range(250, 9000, 250):
        print(f"Processing maximum elevation {maxElev:,d} m ...")

        # **********************************************************************

        # Load data and replace sea with 0 m elevation ...
        arr = numpy.fromfile(
            bName,
            dtype = numpy.int16,
        ).reshape(ny, nx, 1)                                                    # [m]
        numpy.place(arr, arr == -500, 0)                                        # [m]

        # Start ~infinite loop ...
        for shrinkLevel in range(1, 10):
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
                (ny // shrinkFactor, nx // shrinkFactor, 1),
                dtype = numpy.float64,
            )                                                                   # [m]
            for iy in range(ny // shrinkFactor):
                for ix in range(nx // shrinkFactor):
                    shrunkenArr[iy, ix, 0] = arr[iy * shrinkFactor:(iy + 1) * shrinkFactor, ix * shrinkFactor:(ix + 1) * shrinkFactor, 0].mean()    # [m]
            shrunkenArr = 255.0 * (shrunkenArr / numpy.float64(maxElev))
            numpy.place(shrunkenArr, shrunkenArr <   0.0,   0.0)
            numpy.place(shrunkenArr, shrunkenArr > 255.0, 255.0)
            shrunkenArr = shrunkenArr.astype(numpy.uint8)

            # ******************************************************************

            # Loop over shrunken x tiles ...
            for iShrunkenTileX in range(nShrunkenTilesX):
                # Loop over shrunken y tiles ...
                for iShrunkenTileY in range(nShrunkenTilesY):
                    # Create short-hands, make sure that the directory exists
                    # and skip this tile if it already exists ...
                    dName = f"{absPathToRepo}/pyguymer3/data/png/globe/{nShrunkenTilesX:d}x{nShrunkenTilesY:d}/maxElev={maxElev:d}m/x={iShrunkenTileX:d}"
                    pName = f"{dName}/y={iShrunkenTileY:d}.png"
                    if not os.path.exists(dName):
                        os.makedirs(dName)
                    if os.path.exists(pName):
                        print(f"    Not making \"{pName}\".")
                        continue

                    print(f"    Making \"{pName}\" ...")

                    # Make PNG source and write it ...
                    src = pyguymer3.image.makePng(
                        shrunkenArr[iShrunkenTileY * tileSize:(iShrunkenTileY + 1) * tileSize, iShrunkenTileX * tileSize:(iShrunkenTileX + 1) * tileSize, :],
                             debug = args.debug,
                            levels = [9,],
                         memLevels = [9,],
                          palUint8 = turbo,
                        strategies = None,
                            wbitss = [15,],
                    )
                    with open(pName, "wb") as fObj:
                        fObj.write(src)
                    del src
            del shrunkenArr
        del arr

        # **********************************************************************

        print("  Processing original size ...")

        # Load data, replace sea with 0 m elevation and scale ...
        arr = numpy.fromfile(
            bName,
            dtype = numpy.int16,
        ).reshape(ny, nx, 1)                                                    # [m]
        numpy.place(arr, arr == -500, 0)                                        # [m]
        arr = 255.0 * (arr.astype(numpy.float32) / numpy.float32(maxElev))
        numpy.place(arr, arr <   0.0,   0.0)
        numpy.place(arr, arr > 255.0, 255.0)
        arr = arr.astype(numpy.uint8)

        # **********************************************************************

        # Loop over x tiles ...
        for iTileX in range(nTilesX):
            # Loop over y tiles ...
            for iTileY in range(nTilesY):
                # Create short-hands, make sure that the directory exists and
                # skip this tile if it already exists ...
                dName = f"{absPathToRepo}/pyguymer3/data/png/globe/{nTilesX:d}x{nTilesY:d}/maxElev={maxElev:d}m/x={iTileX:d}"
                pName = f"{dName}/y={iTileY:d}.png"
                if not os.path.exists(dName):
                    os.makedirs(dName)
                if os.path.exists(pName):
                    print(f"    Not making \"{pName}\".")
                    continue

                print(f"    Making \"{pName}\" ...")

                # Make PNG source and write it ...
                src = pyguymer3.image.makePng(
                    arr[iTileY * tileSize:(iTileY + 1) * tileSize, iTileX * tileSize:(iTileX + 1) * tileSize, :],
                         debug = args.debug,
                        levels = [9,],
                     memLevels = [9,],
                      palUint8 = turbo,
                    strategies = None,
                        wbitss = [15,],
                )
                with open(pName, "wb") as fObj:
                    fObj.write(src)
                del src
        del arr
