#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import multiprocessing
    import os

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
        PIL.Image.MAX_IMAGE_PIXELS = 4 * 1024 * 1024 * 1024                     # [px] (this is more than normal so that I can get 256x128 tiles)
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

    # Create suitable colour map for the bathymetry ...
    bathymetryCmap = matplotlib.colors.LinearSegmentedColormap.from_list(
        "bathymetry",
        [
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["lightblue"]),
            matplotlib.colors.to_rgba(matplotlib.colors.CSS4_COLORS["darkblue"]),
        ]
    )
    bathymetryColors = [0, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]

    # Define colours and create palette ...
    colours = [
        "lightblue",                    # background, lakes, rivers, bathymetry == 0 m
        "aliceblue",                    # Antarctic ice shelves
        "aquamarine",                   # reefs
        "darkkhaki",                    # land, minor islands
        "snow",                         # glaciated areas
        "khaki",                        # playas
        "red",                          # bathymetry == 200 m
        "red",                          # bathymetry == 1,000 m
        "red",                          # bathymetry == 2,000 m
        "red",                          # bathymetry == 3,000 m
        "red",                          # bathymetry == 4,000 m
        "red",                          # bathymetry == 5,000 m
        "red",                          # bathymetry == 6,000 m
        "red",                          # bathymetry == 7,000 m
        "red",                          # bathymetry == 8,000 m
        "red",                          # bathymetry == 9,000 m
        "red",                          # bathymetry == 10,000 m
    ]
    pal = numpy.zeros(
        (len(colours), 3),
        dtype = numpy.uint8,
    )
    for i, colour in enumerate(colours):
        asHex = matplotlib.colors.CSS4_COLORS[colour][1:]
        pal[i, 0] = int(asHex[ :2], 16)
        pal[i, 1] = int(asHex[2:4], 16)
        pal[i, 2] = int(asHex[4: ], 16)

    # Overwrite bathymetry colours ...
    for i, depth in enumerate(
        [
              200,
             1000,
             2000,
             3000,
             4000,
             5000,
             6000,
             7000,
             8000,
             9000,
            10000,
        ]
    ):
        pal[6 + i, 0], pal[6 + i, 1], pal[6 + i, 2], _ = bathymetryCmap(float(depth) / 10000.0, bytes = True)
    del bathymetryCmap

    # Create short-hand ...
    # NOTE: See "pyguymer3/data/png/README.md".
    tileSize = 300                                                              # [px]

    # Start ~infinite loop ...
    for zoomLevel in range(100):
        # Create short-hands and stop looping if this zoom level is too large ...
        nTilesY = pow(2, zoomLevel)                                             # [#]
        nTilesX = 2 * nTilesY                                                   # [#]
        nx = nTilesX * tileSize                                                 # [px]
        ny = nTilesY * tileSize                                                 # [px]
        if (nx * ny) > PIL.Image.MAX_IMAGE_PIXELS:
            break

        print(f"Processing zoom level {zoomLevel:,d} ({nTilesX:d}x{nTilesY:d}) ...")

        # **********************************************************************

        # Loop over resolutions ...
        for res in args.ress:
            print(f"  Processing resolution \"{res}\" ...")

            # ******************************************************************

            # Create the PIL image and drawing object ...
            img = PIL.Image.new(
                color = 0,              # lightblue
                 mode = "L",
                 size = (nx, ny),
            )
            draw = PIL.ImageDraw.Draw(img)

            # ******************************************************************

            print("    Drawing layers under elevation data ...")

            # Draw layers below elevation data ...
            funcs.drawBathymetry(img, res, bathymetryColors, maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS)
            funcs.drawAntarcticIceShelves(img, res, 1, maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS)
            funcs.drawReefs(img, res, 2, maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS)
            funcs.drawLand(img, res, 3, maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS)
            funcs.drawMinorIslands(img, res, 3, maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS)

            # ******************************************************************

            print("    Drawing layers above elevation data ...")

            # Draw layers above elevation data ...
            funcs.drawGlaciatedAreas(img, res, 4, maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS)
            funcs.drawLakes(img, res, 0, maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS)
            funcs.drawPlayas(img, res, 5, maxImagePixels = PIL.Image.MAX_IMAGE_PIXELS)
            funcs.drawRivers(img, draw, res, 0)
            del draw

            # ******************************************************************

            print("    Converting PIL image in to NumPy array ...")

            # Convert PIL image to NumPy array ...
            arr = numpy.array(img).reshape((ny, nx, 1))
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
                        dName = f"{args.absPathToRepo}/pyguymer3/data/png/ne/{nTilesX:d}x{nTilesY:d}/res={res}/x={iTileX:d}"
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
                                    "palUint8" : pal,
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
            del arr
