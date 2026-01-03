#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import multiprocessing
    import os
    import zipfile

    # Import special modules ...
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
           help = "the interval of the elevation bands to shade for the tiles (in metres)",
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
        "--number-of-children",
        default = os.cpu_count() - 1,   # TODO: Once I ditch Python 3.11 and
                                        #       Python 3.12 then I can use
                                        #       "os.process_cpu_count()" instead.
           dest = "nChild",
           help = "the number of child \"multiprocessing\" processes to use when making the tiles",
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
             "50m",
            "110m",
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

    print(f"Loading \"{bName}\" ...")

    # Load data and replace sea with 0 m elevation ...
    arr = numpy.fromfile(
        bName,
        dtype = numpy.int16,
    ).reshape(ny, nx)                                                           # [m]
    numpy.place(arr, arr == -500, 0)                                            # [m]

    # Loop over maximum elevations ...
    for maxElev in range(args.maxElevInt, 9000, args.maxElevInt):
        print(f"Processing maximum elevation {maxElev:,d} m ...")

        # **********************************************************************

        # Stop looping if this elevation would be pointless ...
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

            # Loop over all to-be-generated tiles skip this shrink level if all
            # already exist ...
            allExist = True
            for res in args.ress:
                for iShrunkenTileX in range(nShrunkenTilesX):
                    for iShrunkenTileY in range(nShrunkenTilesY):
                        pName = f"{args.absPathToRepo}/pyguymer3/data/png/ne/{nShrunkenTilesX:d}x{nShrunkenTilesY:d}/maxElev={maxElev:d}m/elevInt={args.elevBandInt:d}m/res={res}/x={iShrunkenTileX:d}/y={iShrunkenTileY:d}.png"
                        if not os.path.exists(pName):
                            allExist = False
                            break
                    if not allExist:
                        break
                if not allExist:
                    break
            if allExist:
                print(f"  Skipping shrink level {shrinkLevel:,d} ({nShrunkenTilesX:d}x{nShrunkenTilesY:d}) as all tiles already exist.")
                continue

            print(f"  Processing shrink level {shrinkLevel:,d} ({nShrunkenTilesX:d}x{nShrunkenTilesY:d}) ...")

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
                # Loop over all to-be-generated tiles skip this resolution if
                # all already exist ...
                allExist = True
                for iShrunkenTileX in range(nShrunkenTilesX):
                    for iShrunkenTileY in range(nShrunkenTilesY):
                        pName = f"{args.absPathToRepo}/pyguymer3/data/png/ne/{nShrunkenTilesX:d}x{nShrunkenTilesY:d}/maxElev={maxElev:d}m/elevInt={args.elevBandInt:d}m/res={res}/x={iShrunkenTileX:d}/y={iShrunkenTileY:d}.png"
                        if not os.path.exists(pName):
                            allExist = False
                            break
                    if not allExist:
                        break
                if allExist:
                    print(f"    Skipping resolution {res} as all tiles already exist.")
                    continue

                print(f"    Processing resolution {res} ...")

                # Create the PIL image and drawing object ...
                img = PIL.Image.new(
                    color = matplotlib.colors.CSS4_COLORS["lightblue"],
                     mode = "RGB",
                     size = (nx // shrinkFactor, ny // shrinkFactor),
                )
                draw = PIL.ImageDraw.Draw(img)

                # **************************************************************

                print("      Drawing layers under elevation data ...")

                # Draw layers below elevation data ...
                funcs.drawBathymetry(img, draw, res)
                funcs.drawAntarcticIceShelves(img, draw, res)
                funcs.drawReefs(img, draw, res)
                funcs.drawLand(img, draw, res)
                funcs.drawMinorIslands(img, draw, res)

                # **************************************************************

                print("      Drawing elevation data ...")

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

                print("      Drawing layers above elevation data ...")

                # Draw layers above elevation data ...
                funcs.drawGlaciatedAreas(img, draw, res)
                funcs.drawLakes(img, draw, res)
                funcs.drawPlayas(img, draw, res)
                funcs.drawRivers(img, draw, res)
                del draw

                # ******************************************************************

                print("      Converting PIL image in to NumPy array ...")

                # Convert PIL image to NumPy array ...
                imgArr = numpy.array(img).reshape((ny // shrinkFactor, nx // shrinkFactor, 3))
                del img

                # **************************************************************

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
                            dName = f"{args.absPathToRepo}/pyguymer3/data/png/ne/{nShrunkenTilesX:d}x{nShrunkenTilesY:d}/maxElev={maxElev:d}m/elevInt={args.elevBandInt:d}m/res={res}/x={iShrunkenTileX:d}"
                            pName = f"{dName}/y={iShrunkenTileY:d}.png"
                            if not os.path.exists(dName):
                                os.makedirs(dName)
                            if os.path.exists(pName):
                                if args.debug:
                                    print(f"        Not making \"{pName}\".")
                                continue

                            print(f"        Adding job to make \"{pName}\" to the worker pool ...")

                            # Add job to make the PNG to the worker pool ...
                            results.append(
                                pObj.apply_async(
                                    pyguymer3.image.save_array_as_PNG,
                                    (
                                        imgArr[iShrunkenTileY * tileSize:(iShrunkenTileY + 1) * tileSize, iShrunkenTileX * tileSize:(iShrunkenTileX + 1) * tileSize, :],
                                        pName,
                                    ),
                                    {
                                        "debug" : args.debug,
                                    },
                                )
                            )

                    # Create short-hands ...
                    nResults = len(results)                                     # [#]
                    start = pyguymer3.now()

                    print("      Waiting for child \"multiprocessing\" processes to finish ...", end = "\r")

                    # Loop over results ...
                    for iResult, result in enumerate(results):
                        # Get result ...
                        _ = result.get(args.timeout)

                        # Print progress ...
                        # NOTE: The progress string needs padding with extra
                        #       spaces so that the line is fully overwritten
                        #       when it inevitably gets shorter (as the
                        #       remaining time gets shorter). Assume that the
                        #       longest it will ever be is
                        #       "???.???% (~??h ??m ??.?s still to go)" (which
                        #       is 37 characters).
                        fraction = float(iResult + 1) / float(nResults)
                        durationSoFar = pyguymer3.now() - start
                        totalDuration = durationSoFar / fraction
                        remaining = (totalDuration - durationSoFar).total_seconds() # [s]
                        progress = f"{100.0 * fraction:.3f}% (~{pyguymer3.convert_seconds_to_pretty_time(remaining)} still to go)"
                        print(f"      Waiting for child \"multiprocessing\" processes to finish ... {progress:37s}", end = "\r")

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
                    #       "multiprocessing.Pool()" calls "terminate()" instead
                    #       of "join()", so I must manage the end of the pool of
                    #       worker processes myself.
                    pObj.close()
                    pObj.join()
                del imgArr
            del shrunkenArr

        # **********************************************************************

        # Loop over all to-be-generated tiles skip this maximum elevation if all
        # already exist ...
        allExist = True
        for res in args.ress:
            for iTileX in range(nTilesX):
                for iTileY in range(nTilesY):
                    pName = f"{args.absPathToRepo}/pyguymer3/data/png/ne/{nTilesX:d}x{nTilesY:d}/maxElev={maxElev:d}m/elevInt={args.elevBandInt:d}m/res={res}/x={iTileX:d}/y={iTileY:d}.png"
                    if not os.path.exists(pName):
                        allExist = False
                        break
                if not allExist:
                    break
            if not allExist:
                break
        if allExist:
            print(f"  Skipping original size ({nTilesX:d}x{nTilesY:d}) as all tiles already exist.")
            continue

        print(f"  Processing original size ({nTilesX:d}x{nTilesY:d}) ...")

        # Loop over resolutions ...
        for res in args.ress:
            # Loop over all to-be-generated tiles skip this resolution if all
            # already exist ...
            allExist = True
            for iTileX in range(nTilesX):
                for iTileY in range(nTilesY):
                    pName = f"{args.absPathToRepo}/pyguymer3/data/png/ne/{nTilesX:d}x{nTilesY:d}/maxElev={maxElev:d}m/elevInt={args.elevBandInt:d}m/res={res}/x={iTileX:d}/y={iTileY:d}.png"
                    if not os.path.exists(pName):
                        allExist = False
                        break
                if not allExist:
                    break
            if allExist:
                print(f"    Skipping resolution {res} as all tiles already exist.")
                continue

            print(f"    Processing resolution {res} ...")

            # Create the PIL image and drawing object ...
            img = PIL.Image.new(
                color = matplotlib.colors.CSS4_COLORS["lightblue"],
                 mode = "RGB",
                 size = (nx, ny),
            )
            draw = PIL.ImageDraw.Draw(img)

            # ******************************************************************

            print("      Drawing layers under elevation data ...")

            # Draw layers below elevation data ...
            funcs.drawBathymetry(img, draw, res)
            funcs.drawAntarcticIceShelves(img, draw, res)
            funcs.drawReefs(img, draw, res)
            funcs.drawLand(img, draw, res)
            funcs.drawMinorIslands(img, draw, res)

            # ******************************************************************

            print("      Drawing elevation data ...")

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

            print("      Drawing layers above elevation data ...")

            # Draw layers above elevation data ...
            funcs.drawGlaciatedAreas(img, draw, res)
            funcs.drawLakes(img, draw, res)
            funcs.drawPlayas(img, draw, res)
            funcs.drawRivers(img, draw, res)
            del draw

            # ******************************************************************

            print("      Converting PIL image in to NumPy array ...")

            # Convert PIL image to NumPy array ...
            imgArr = numpy.array(img).reshape((ny, nx, 3))
            del img

            # ******************************************************************

            # Create a pool of workers ...
            with multiprocessing.Pool(args.nChild) as pObj:
                # Initialize list ...
                results = []

                # Loop over x tiles ...
                for iTileX in range(nTilesX):
                    # Loop over y tiles ...
                    for iTileY in range(nTilesY):
                        # Create short-hands, make sure that the directory
                        # exists and skip this tile if it already exists ...
                        dName = f"{args.absPathToRepo}/pyguymer3/data/png/ne/{nTilesX:d}x{nTilesY:d}/maxElev={maxElev:d}m/elevInt={args.elevBandInt:d}m/res={res}/x={iTileX:d}"
                        pName = f"{dName}/y={iTileY:d}.png"
                        if not os.path.exists(dName):
                            os.makedirs(dName)
                        if os.path.exists(pName):
                            if args.debug:
                                print(f"        Not making \"{pName}\".")
                            continue

                        print(f"        Adding job to make \"{pName}\" to the worker pool ...")

                        # Add job to make the PNG to the worker pool ...
                        results.append(
                            pObj.apply_async(
                                pyguymer3.image.save_array_as_PNG,
                                (
                                    imgArr[iTileY * tileSize:(iTileY + 1) * tileSize, iTileX * tileSize:(iTileX + 1) * tileSize, :],
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

                print("      Waiting for child \"multiprocessing\" processes to finish ...", end = "\r")

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
                    print(f"      Waiting for child \"multiprocessing\" processes to finish ... {progress:37s}", end = "\r")

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
            del imgArr
