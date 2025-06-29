#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import json
    import os

    # Import special modules ...
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

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # **************************************************************************

    # Set flags for makePng() function calls ...
    choices = "all"
    debug = False
    dpi = 300
    levels = [9,]
    memLevels = [9,]
    strategies = None
    wbitss = [15,]

    # Create short-hand and make output directory ...
    dName = os.path.basename(__file__).removesuffix(".py")
    if not os.path.exists(dName):
        os.mkdir(dName)

    # **************************************************************************

    # Set image size ...
    nx = 2048                                                                   # [px]
    ny = 1024                                                                   # [px]

    # Load colour tables ...
    with open(f"{pyguymer3.__path__[0]}/data/json/colourTables.json", "rt", encoding = "utf-8") as fObj:
        colourTables = json.load(fObj)

    # Make an example image ...
    arrG = numpy.array(
        PIL.Image.effect_mandelbrot(
            (nx, ny),
            (-0.7436, 0.1306, -0.7426, 0.1316),
            100,
        )
    ).reshape((ny, nx, 1))

    # Initialize arrays ...
    arrT = numpy.zeros(
        (ny, nx, 3),
        dtype = numpy.uint8,
    )
    palT = numpy.zeros(
        (256, 3),
        dtype = numpy.uint8,
    )

    # Loop over x-axis ...
    for ix in range(nx):
        # Loop over y-axis ...
        for iy in range(ny):
            # Populate array ...
            arrT[iy, ix, 0] = numpy.uint8(colourTables["turbo"][arrG[iy, ix, 0]][0])
            arrT[iy, ix, 1] = numpy.uint8(colourTables["turbo"][arrG[iy, ix, 0]][1])
            arrT[iy, ix, 2] = numpy.uint8(colourTables["turbo"][arrG[iy, ix, 0]][2])

    # Loop over levels ...
    for lvl in range(256):
        # Populate array ...
        palT[lvl, 0] = numpy.uint8(colourTables["turbo"][lvl][0])
        palT[lvl, 1] = numpy.uint8(colourTables["turbo"][lvl][1])
        palT[lvl, 2] = numpy.uint8(colourTables["turbo"][lvl][2])

    # Save array as PNG ...
    print(f"Making \"{dName}/arrG.png\" ...")
    with open(f"{dName}/arrG.png", "wb") as fObj:
        fObj.write(
            pyguymer3.image.makePng(
                arrG,
                   choices = choices,
                     debug = debug,
                       dpi = dpi,
                    levels = levels,
                 memLevels = memLevels,
                  palUint8 = None,
                strategies = strategies,
                    wbitss = wbitss,
            )
        )

    # Save array as PNG ...
    print(f"Making \"{dName}/arrP.png\" ...")
    with open(f"{dName}/arrP.png", "wb") as fObj:
        fObj.write(
            pyguymer3.image.makePng(
                arrG,
                   choices = choices,
                     debug = debug,
                       dpi = dpi,
                    levels = levels,
                 memLevels = memLevels,
                  palUint8 = palT,
                strategies = strategies,
                    wbitss = wbitss,
            )
        )

    # Save array as PNG ...
    print(f"Making \"{dName}/arrT.png\" ...")
    with open(f"{dName}/arrT.png", "wb") as fObj:
        fObj.write(
            pyguymer3.image.makePng(
                arrT,
                   choices = choices,
                     debug = debug,
                       dpi = dpi,
                    levels = levels,
                 memLevels = memLevels,
                  palUint8 = None,
                strategies = strategies,
                    wbitss = wbitss,
            )
        )
