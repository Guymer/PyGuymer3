#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import io
    import json
    import os
    import re
    import zipfile

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
    try:
        import rsl
    except:
        raise Exception("\"rsl\" is not installed; run \"pip install --user git+https://github.com/Guymer/rsl.git\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Save the OS Terrain 50 dataset as tiles.",
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
        "--maximum-elevation-interval",
        default = 250,
           dest = "maxElevInt",
           help = "the elevation interval to make tiles for (in metres)",
           type = int,
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
    bName = f"{args.absPathToRepo}/scripts/osTerrain.bin"
    zName = f"{args.absPathToRepo}/scripts/osTerrain.zip"
    url = "https://www.ordnancesurvey.co.uk/products/os-terrain-50"

    # Load colour tables and create short-hand ...
    with open(f"{args.absPathToRepo}/pyguymer3/data/json/colourTables.json", "rt", encoding = "utf-8") as fObj:
        colourTables = json.load(fObj)
    turbo = numpy.array(colourTables["turbo"]).astype(numpy.uint8)

    # Create short-hands ...
    # NOTE: See "pyguymer3/data/png/README.md".
    nx = 13200                                                                  # [px]
    ny = 24600                                                                  # [px]
    tileSize = 300                                                              # [px]
    nTilesX = nx // tileSize                                                    # [#]
    nTilesY = ny // tileSize                                                    # [#]

    # **************************************************************************

    # Check if the ZIP file does not exist yet ...
    if not os.path.exists(zName):
        raise Exception(f"you need to download \"{zName}\" file yourself from \"{url}\"") from None

    # **************************************************************************

    # Check if the BIN file does not exist yet ...
    if not os.path.exists(bName):
        print(f"Making \"{bName}\" ...")

        # Find extent of the "OS Terrain 50" dataset ...
        x1, x2, y1, y2 = rsl.findExtent(zName)                                  # [px], [px], [px], [px]

        # Make map ...
        elev = numpy.zeros(
            (ny, nx),
            dtype = numpy.float32,
        )                                                                       # [m]

        # Compile regex to save time ...
        pattern = re.compile(r"data/[a-z]+/[a-z]+[0-9]+_OST50GRID_[0-9]+.zip")

        # Load dataset ...
        with zipfile.ZipFile(zName, "r") as zObj0:
            # Loop over members ...
            for fName in zObj0.namelist():
                # Skip this member if it is not a sub-dataset ...
                if pattern.match(fName) is None:
                    continue

                # Determine sub-dataset key ...
                key = fName.split("/")[-1].split("_")[0].upper()

                # Read sub-dataset into RAM so that it becomes seekable ...
                # NOTE: https://stackoverflow.com/a/12025492
                fObj0 = io.BytesIO(zObj0.read(fName))

                # Load sub-dataset ...
                with zipfile.ZipFile(fObj0, "r") as zObj1:
                    # Read ASCII dataset into RAM so that it becomes seekable ...
                    # NOTE: https://stackoverflow.com/a/12025492
                    fObj1 = io.BytesIO(zObj1.read(f"{key}.asc"))

                    # Load header and contents of ASCII dataset ...
                    hdr = rsl.loadASCIIheader(fObj1)
                    cont = rsl.loadASCIIcontents(fObj1, hdr["length"])          # [m]

                    # Determine indexes (from the upper-left corner not the
                    # lower-left corner) ...
                    ix1 = hdr["xllcorner"] // hdr["cellsize"]
                    ix2 = hdr["xllcorner"] // hdr["cellsize"] + hdr["ncols"]
                    iy1 = ny - (hdr["yllcorner"] // hdr["cellsize"] + hdr["nrows"])
                    iy2 = ny - (hdr["yllcorner"] // hdr["cellsize"])

                    # Populate array ...
                    elev[iy1:iy2, ix1:ix2] = cont[:, :]                         # [m]

        # Save BIN ...
        elev.tofile(bName)
        del elev

    # **************************************************************************

    # Loop over maximum elevations ...
    for maxElev in range(args.maxElevInt, 9000, args.maxElevInt):
        print(f"Processing maximum elevation {maxElev:,d} m ...")

        # **********************************************************************

        # Load data ...
        arr = numpy.fromfile(
            bName,
            dtype = numpy.float32,
        ).reshape(ny, nx, 1)                                                    # [m]
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
            shrunkenArr = numpy.zeros(
                (ny // shrinkFactor, nx // shrinkFactor, 1),
                dtype = numpy.float32,
            )                                                                   # [m]
            for iy in range(ny // shrinkFactor):
                for ix in range(nx // shrinkFactor):
                    shrunkenArr[iy, ix, 0] = arr[iy * shrinkFactor:(iy + 1) * shrinkFactor, ix * shrinkFactor:(ix + 1) * shrinkFactor, 0].mean()    # [m]
            shrunkenArr = 255.0 * (shrunkenArr / numpy.float32(maxElev))
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
                    dName = f"{args.absPathToRepo}/pyguymer3/data/png/osTerrain/{nShrunkenTilesX:d}x{nShrunkenTilesY:d}/maxElev={maxElev:d}m/x={iShrunkenTileX:d}"
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

        # Load data and scale ...
        arr = numpy.fromfile(
            bName,
            dtype = numpy.float32,
        ).reshape(ny, nx, 1)                                                    # [m]
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
                dName = f"{args.absPathToRepo}/pyguymer3/data/png/osTerrain/{nTilesX:d}x{nTilesY:d}/maxElev={maxElev:d}m/x={iTileX:d}"
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
