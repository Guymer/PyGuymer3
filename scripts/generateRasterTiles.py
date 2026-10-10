#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.13/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import multiprocessing
    import os
    import zipfile

    # Import special modules ...
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
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Save raster datasets as tiles.",
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
        "--number-of-children",
        default = os.process_cpu_count() - 1,
           dest = "nChild",
           help = "the number of child \"multiprocessing\" processes to use when making the tiles",
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
    # NOTE: See "pyguymer3/data/png/README.md".
    nx = 21600                                                                  # [px]
    ny = 10800                                                                  # [px]
    tileSize = 300                                                              # [px]
    nTilesX = nx // tileSize                                                    # [#]
    nTilesY = ny // tileSize                                                    # [#]

    # Define the raster datasets ...
    rasters = {
        "cross-blend-hypso" : "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/HYP_HR_SR_OB_DR.zip",
               "gray-earth" : "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/GRAY_HR_SR_OB_DR.zip",
          "natural-earth-1" : "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/NE1_HR_LC_SR_W_DR.zip",
          "natural-earth-2" : "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/NE2_HR_LC_SR_W_DR.zip",
            "shaded-relief" : "https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/10m/raster/SR_HR.zip",
    }

    # **************************************************************************

    # Start session ...
    with pyguymer3.start_session() as sess:
        # Loop over raster datasets ...
        for name, url in rasters.items():
            # Create short-hand and skip if it exists ...
            zName = f"{args.absPathToRepo}/scripts/{name}.zip"
            if os.path.exists(zName):
                continue

            print(f"Downloading \"{zName}\" ...")

            # Download the ZIP file ...
            if not pyguymer3.download_file(
                sess,
                url,
                zName,
                  debug = args.debug,
                timeout = args.timeout,
                 verify = True,
            ):
                raise Exception(f"failed to download \"{url}\"") from None

    # **************************************************************************

    # Loop over raster datasets ...
    for name in rasters.keys():
        # Create short-hands and skip if it exists ...
        tName = f"{args.absPathToRepo}/scripts/{name}.tif"
        zName = f"{args.absPathToRepo}/scripts/{name}.zip"
        if os.path.exists(tName):
            continue

        # Open ZIP file ...
        with zipfile.ZipFile(
            zName,
            mode = "r",
        ) as zObj:
            # Loop over contents ...
            for zInfo in zObj.infolist():
                # Skip item if it is not a TIF image ...
                if not zInfo.filename.lower().endswith(".tif"):
                    continue

                print(f"Extracting \"{tName}\" ...")

                # Open the input compressed TIF image ...
                with zObj.open(
                    zInfo,
                    mode = "r",
                ) as fObjIn:
                    # Open the output uncompressed TIF image ...
                    with open(
                        tName,
                        mode = "wb",
                    ) as fObjOut:
                        # Extract the TIF image ...
                        fObjOut.write(fObjIn.read())

                # Stop looping over contents ...
                break

    # **************************************************************************

    # Loop over raster datasets ...
    for name in rasters.keys():
        print(f"Processing \"{name}\" ...")

        # Create short-hand ...
        tName = f"{args.absPathToRepo}/scripts/{name}.tif"

        # Load image ...
        with PIL.Image.open(
            tName,
            mode = "r",
        ) as iObj:
            img = iObj.convert("RGB")

        # **********************************************************************

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

            print(f"  Processing shrink level {shrinkLevel:,d}, which is a shrink factor of {shrinkFactor:d}×, and results in ({nShrunkenTilesX:,d} × {nShrunkenTilesY:,d}) tiles and ({nx // shrinkFactor:,d} × {ny // shrinkFactor:,d}) pixels ...")

            # Shrink image and convert to NumPy array ...
            shrunkenImg = img.reduce(shrinkFactor)
            shrunkenArr = numpy.array(shrunkenImg)
            shrunkenImg.close()
            del shrunkenImg

            # Create a pool of workers ...
            with multiprocessing.Pool(args.nChild) as pObj:
                # Initialize list ...
                results = []

                # Loop over shrunken x tiles ...
                for iShrunkenTileX in range(nShrunkenTilesX):
                    # Loop over shrunken y tiles ...
                    for iShrunkenTileY in range(nShrunkenTilesY):
                        # Create short-hands, make sure that the directory
                        # exists and skip this tile if it already exists ...
                        dName = f"{args.absPathToRepo}/pyguymer3/data/png/raster/{nShrunkenTilesX:d}x{nShrunkenTilesY:d}/{name}/x={iShrunkenTileX:d}"
                        pName = f"{dName}/y={iShrunkenTileY:d}.png"
                        if not os.path.exists(dName):
                            os.makedirs(dName)
                        if os.path.exists(pName):
                            if args.debug:
                                print(f"    Not making \"{pName}\".")
                            continue

                        print(f"    Adding job to make \"{pName}\" to the worker pool ...")

                        # Add job to make the PNG to the worker pool ...
                        results.append(
                            pObj.apply_async(
                                pyguymer3.image.save_array_as_PNG,
                                (
                                    shrunkenArr[iShrunkenTileY * tileSize:(iShrunkenTileY + 1) * tileSize, iShrunkenTileX * tileSize:(iShrunkenTileX + 1) * tileSize, :],
                                    pName,
                                ),
                                {
                                    "debug" : args.debug,
                                },
                            )
                        )

                # Create short-hands ...
                nResults = len(results)                                         # [#]
                start = pyguymer3.now()

                print("  Waiting for child \"multiprocessing\" processes to finish ...", end = "\r")

                # Loop over results ...
                for iResult, result in enumerate(results):
                    # Get result ...
                    _ = result.get(args.timeout)

                    # Print progress ...
                    # NOTE: The progress string needs padding with extra spaces
                    #       so that the line is fully overwritten when it
                    #       inevitably gets shorter (as the remaining time gets
                    #       shorter). Assume that the longest it will ever be
                    #       is "???.???% (~??h ??m ??.?s still to go)" (which is
                    #       37 characters).
                    fraction = float(iResult + 1) / float(nResults)
                    durationSoFar = pyguymer3.now() - start
                    totalDuration = durationSoFar / fraction
                    remaining = (totalDuration - durationSoFar).total_seconds() # [s]
                    progress = f"{100.0 * fraction:.3f}% (~{pyguymer3.convert_seconds_to_pretty_time(remaining)} still to go)"
                    print(f"  Waiting for child \"multiprocessing\" processes to finish ... {progress:37s}", end = "\r")

                    # Check result ...
                    if not result.successful():
                        # Clear the line and cry ...
                        print()
                        raise Exception("\"multiprocessing.Pool().apply_async()\" was not successful") from None

                # Clear the line ...
                print()

                # Close the pool of worker processes and wait for all of the
                # tasks to finish ...
                # NOTE: The "__exit__()" call of the context manager for
                #       "multiprocessing.Pool()" calls "terminate()" instead of
                #       "join()", so I must manage the end of the pool of worker
                #       processes myself.
                pObj.close()
                pObj.join()

            # Clean up ...
            del shrunkenArr

        # **********************************************************************

        print(f"  Processing original size and results in ({nTilesX:,d} × {nTilesY:,d}) tiles and ({nx:,d} × {ny:,d}) pixels ...")

        # Convert image to NumPy array ...
        arr = numpy.array(img)

        # Create a pool of workers ...
        with multiprocessing.Pool(args.nChild) as pObj:
            # Initialize list ...
            results = []

            # Loop over x tiles ...
            for iTileX in range(nTilesX):
                # Loop over y tiles ...
                for iTileY in range(nTilesY):
                    # Create short-hands, make sure that the directory exists
                    # and skip this tile if it already exists ...
                    dName = f"{args.absPathToRepo}/pyguymer3/data/png/raster/{nTilesX:d}x{nTilesY:d}/{name}/x={iTileX:d}"
                    pName = f"{dName}/y={iTileY:d}.png"
                    if not os.path.exists(dName):
                        os.makedirs(dName)
                    if os.path.exists(pName):
                        if args.debug:
                            print(f"    Not making \"{pName}\".")
                        continue

                    print(f"    Adding job to make \"{pName}\" to the worker pool ...")

                    # Add job to make the PNG to the worker pool ...
                    results.append(
                        pObj.apply_async(
                            pyguymer3.image.save_array_as_PNG,
                            (
                                arr[iTileY * tileSize:(iTileY + 1) * tileSize, iTileX * tileSize:(iTileX + 1) * tileSize, :],
                                pName,
                            ),
                            {
                                "debug" : args.debug,
                            },
                        )
                    )

            # Create short-hands ...
            nResults = len(results)                                             # [#]
            start = pyguymer3.now()

            print("  Waiting for child \"multiprocessing\" processes to finish ...", end = "\r")

            # Loop over results ...
            for iResult, result in enumerate(results):
                # Get result ...
                _ = result.get(args.timeout)

                # Print progress ...
                # NOTE: The progress string needs padding with extra spaces so
                #       that the line is fully overwritten when it inevitably
                #       gets shorter (as the remaining time gets shorter).
                #       Assume that the longest it will ever be is
                #       "???.???% (~??h ??m ??.?s still to go)" (which is 37
                #       characters).
                fraction = float(iResult + 1) / float(nResults)
                durationSoFar = pyguymer3.now() - start
                totalDuration = durationSoFar / fraction
                remaining = (totalDuration - durationSoFar).total_seconds()     # [s]
                progress = f"{100.0 * fraction:.3f}% (~{pyguymer3.convert_seconds_to_pretty_time(remaining)} still to go)"
                print(f"  Waiting for child \"multiprocessing\" processes to finish ... {progress:37s}", end = "\r")

                # Check result ...
                if not result.successful():
                    # Clear the line and cry ...
                    print()
                    raise Exception("\"multiprocessing.Pool().apply_async()\" was not successful") from None

            # Clear the line ...
            print()

            # Close the pool of worker processes and wait for all of the tasks
            # to finish ...
            # NOTE: The "__exit__()" call of the context manager for
            #       "multiprocessing.Pool()" calls "terminate()" instead of
            #       "join()", so I must manage the end of the pool of worker
            #       processes myself.
            pObj.close()
            pObj.join()

        # Clean up ..
        del arr

        # **********************************************************************

        # Clean up ..
        img.close()
        del img
