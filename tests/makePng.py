#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import io
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

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Demonstrate saving images as PNGs.",
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

    # Set desired maximum image size ...
    nx = 2048                                                                   # [px]
    ny = 1024                                                                   # [px]

    # Create short-hand and make output directory ...
    dName = os.path.basename(__file__).removesuffix(".py")
    if not os.path.exists(dName):
        os.mkdir(dName)

    # **************************************************************************

    # Make the example "mandelbrot" if it is missing ...
    if not os.path.exists(f"{dName}/mandelbrot.bin"):
        print("Making example \"mandelbrot\" ...")
        with PIL.Image.effect_mandelbrot(
            (4 * nx, 4 * ny),
            (-0.7436, 0.1306, -0.7426, 0.1316),
            100,
        ) as iObj:
            img = iObj.convert("L")
        img.thumbnail(
            (nx, ny),
            resample = PIL.Image.Resampling.LANCZOS,
        )
        print(f" > The example \"mandelbrot\" is {img.size[0]:,d} px × {img.size[1]:,d} px.")
        arr = numpy.array(img)
        assert arr.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arr.dtype}\")"
        assert arr.ndim == 2, f"the NumPy array is not 2D (\"{arr.ndim:d}\")"
        with open(f"{dName}/mandelbrot.bin", "wb") as fObj:
            fObj.write(arr.tobytes())

    # Load the binary array of the example "mandelbrot" ...
    arrMandelbrot = numpy.fromfile(
        f"{dName}/mandelbrot.bin",
        dtype = numpy.uint8,
    ).reshape((ny, nx))

    # Make a crude image of the example "mandelbrot" without any compression or
    # metadata ...
    if not os.path.exists(f"{dName}/mandelbrot.pgm"):
        print("Converting example \"mandelbrot\" to a PGM ...")
        pyguymer3.image.save_array_as_PGM(
            arrMandelbrot,
            f"{dName}/mandelbrot.pgm",
        )

    # Make the example "photo" if it is missing ...
    # NOTE: I wanted a "photo" which was in the public domain which I could use
    #       to test my new function. I went to the Wikimedia Commons category
    #       for "Public Domain" here:
    #         * https://commons.wikimedia.org/wiki/Category:Public_domain
    #       I then went to the "UK" sub-category here:
    #         * https://commons.wikimedia.org/wiki/Category:PD-UK
    #       I wanted one which was high-resolution (so that I could downsample
    #       it and remove either compression or scanning artefacts) and which
    #       was clearly in full colour (not just in sepia).
    if not os.path.exists(f"{dName}/photo.bin"):
        print("Downloading example \"photo\" ...")
        with pyguymer3.start_session() as sess:
            resp = pyguymer3.download_stream(
                sess,
                "https://upload.wikimedia.org/wikipedia/commons/a/a6/%22I_hear_they_want_more_Bovril._My_place_is_at_the_front%22_LCCN2003668493.jpg",
                timeout = args.timeout,
                 verify = True,
            )
            assert resp, "failed to download example \"photo\""
            with io.BytesIO(resp) as bObj:
                with PIL.Image.open(bObj) as iObj:
                    img = iObj.convert("RGB")
        img.thumbnail(
            (nx, ny),
            resample = PIL.Image.Resampling.LANCZOS,
        )
        print(f" > The example \"photo\" is {img.size[0]:,d} px × {img.size[1]:,d} px.")
        arr = numpy.array(img)
        assert arr.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arr.dtype}\")"
        assert arr.ndim == 3, f"the NumPy array is not 3D (\"{arr.ndim:d}\")"
        assert arr.shape[2] == 3, f"the NumPy array does not have 3 colour channels (\"{arr.shape[2]:d}\")"
        with open(f"{dName}/photo.bin", "wb") as fObj:
            fObj.write(arr.tobytes())

    # Load the binary array of the example "photo" ...
    arrPhoto = numpy.fromfile(
        f"{dName}/photo.bin",
        dtype = numpy.uint8,
    ).reshape((ny, 782, 3))

    # Make a crude image of the example "photo" without any compression or
    # metadata ...
    if not os.path.exists(f"{dName}/photo.ppm"):
        print("Converting example \"photo\" to a PPM ...")
        pyguymer3.image.save_array_as_PPM(
            arrPhoto,
            f"{dName}/photo.ppm",
        )

    # Make the example "testcard" if it is missing ...
    # NOTE: I wanted a "testcard" which was in the public domain which I could
    #       use to test my new function. I went to the Wikimedia page for
    #       testcards and eventually came across this one.
    if not os.path.exists(f"{dName}/testcard.bin"):
        print("Downloading example \"testcard\" ...")
        with pyguymer3.start_session() as sess:
            resp = pyguymer3.download_stream(
                sess,
                "https://upload.wikimedia.org/wikipedia/commons/thumb/b/bc/ETP-1_recreation.svg/2560px-ETP-1_recreation.svg.png",
                timeout = args.timeout,
                 verify = True,
             )
            assert resp, "failed to download example \"testcard\""
            with io.BytesIO(resp) as bObj:
                with PIL.Image.open(bObj) as iObj:
                    img = iObj.convert("RGB")
        img.thumbnail(
            (nx, ny),
            resample = PIL.Image.Resampling.LANCZOS,
        )
        print(f" > The example \"testcard\" is {img.size[0]:,d} px × {img.size[1]:,d} px.")
        arr = numpy.array(img)
        assert arr.dtype == "uint8", f"the NumPy array is not 8-bit (\"{arr.dtype}\")"
        assert arr.ndim == 3, f"the NumPy array is not 3D (\"{arr.ndim:d}\")"
        assert arr.shape[2] == 3, f"the NumPy array does not have 3 colour channels (\"{arr.shape[2]:d}\")"
        with open(f"{dName}/testcard.bin", "wb") as fObj:
            fObj.write(arr.tobytes())

    # Load the binary array of the example "testcard" ...
    arrTestcard = numpy.fromfile(
        f"{dName}/testcard.bin",
        dtype = numpy.uint8,
    ).reshape((ny, 1365, 3))

    # Make a crude image of the example "testcard" without any compression or
    # metadata ...
    if not os.path.exists(f"{dName}/testcard.ppm"):
        print("Converting example \"testcard\" to a PPM ...")
        pyguymer3.image.save_array_as_PPM(
            arrTestcard,
            f"{dName}/testcard.ppm",
        )

    # **************************************************************************

    # Set flags for makePng() function calls ...
    # NOTE: This is an educated guess as to what the best solution will be: I
    #       think that I know for the compression level, the memory level and
    #       the window size but not the compression strategy - so I want
    #       makePng() to try them all for me.
    levels = [9,]
    memLevels = [9,]
    strategies = None
    wbitss = [15,]

    # **************************************************************************

    # Load colour tables and create short-hand ...
    with open(
        f"{pyguymer3.__path__[0]}/data/json/colourTables.json",
        "rt",
        encoding = "utf-8",
    ) as fObj:
        colourTables = json.load(fObj)
    palT = numpy.array(colourTables["turbo"]).astype(numpy.uint8)
    del colourTables

    # **************************************************************************
    # *                                                                        *
    # *                      Process example "mandelbrot"                      *
    # *                                                                        *
    # **************************************************************************

    # Initialize arrays ...
    arrG = arrMandelbrot.copy()
    arrT = numpy.zeros(
        (ny, nx, 3),
        dtype = numpy.uint8,
    )

    # Loop over x-axis ...
    for ix in range(nx):
        # Loop over y-axis ...
        for iy in range(ny):
            # Create short-hand and populate array ...
            lvl = arrG[iy, ix]
            arrT[iy, ix, 0] = numpy.uint8(palT[lvl, 0])
            arrT[iy, ix, 1] = numpy.uint8(palT[lvl, 1])
            arrT[iy, ix, 2] = numpy.uint8(palT[lvl, 2])

    # Save array as PNG ...
    if not os.path.exists(f"{dName}/mandelbrot-greyscale.png"):
        print(f"Making \"{dName}/mandelbrot-greyscale.png\" ...")
        with open(f"{dName}/mandelbrot-greyscale.png", "wb") as fObj:
            fObj.write(
                pyguymer3.image.makePng(
                    arrG.reshape((ny, nx, 1)),
                         debug = args.debug,
                        levels = levels,
                     memLevels = memLevels,
                      palUint8 = None,
                    strategies = strategies,
                        wbitss = wbitss,
                )
            )

    # Save array as PNG ...
    if not os.path.exists(f"{dName}/mandelbrot-paletted.png"):
        print(f"Making \"{dName}/mandelbrot-paletted.png\" ...")
        with open(f"{dName}/mandelbrot-paletted.png", "wb") as fObj:
            fObj.write(
                pyguymer3.image.makePng(
                    arrG.reshape((ny, nx, 1)),
                         debug = args.debug,
                        levels = levels,
                     memLevels = memLevels,
                      palUint8 = palT,
                    strategies = strategies,
                        wbitss = wbitss,
                )
            )

    # Save array as PNG ...
    if not os.path.exists(f"{dName}/mandelbrot-truecolour.png"):
        print(f"Making \"{dName}/mandelbrot-truecolour.png\" ...")
        with open(f"{dName}/mandelbrot-truecolour.png", "wb") as fObj:
            fObj.write(
                pyguymer3.image.makePng(
                    arrT,
                         debug = args.debug,
                        levels = levels,
                     memLevels = memLevels,
                      palUint8 = None,
                    strategies = strategies,
                        wbitss = wbitss,
                )
            )

    # **************************************************************************
    # *                                                                        *
    # *                        Process example "photo"                         *
    # *                                                                        *
    # **************************************************************************

    # Initialize array ...
    arrT = arrPhoto.copy()

    # Save array as PNG ...
    if not os.path.exists(f"{dName}/photo-truecolour.png"):
        print(f"Making \"{dName}/photo-truecolour.png\" ...")
        with open(f"{dName}/photo-truecolour.png", "wb") as fObj:
            fObj.write(
                pyguymer3.image.makePng(
                    arrT,
                         debug = args.debug,
                        levels = levels,
                     memLevels = memLevels,
                      palUint8 = None,
                    strategies = strategies,
                        wbitss = wbitss,
                )
            )

    # **************************************************************************
    # *                                                                        *
    # *                       Process example "testcard"                       *
    # *                                                                        *
    # **************************************************************************

    # Initialize array ...
    arrT = arrTestcard.copy()

    # Save array as PNG ...
    if not os.path.exists(f"{dName}/testcard-truecolour.png"):
        print(f"Making \"{dName}/testcard-truecolour.png\" ...")
        with open(f"{dName}/testcard-truecolour.png", "wb") as fObj:
            fObj.write(
                pyguymer3.image.makePng(
                    arrT,
                         debug = args.debug,
                        levels = levels,
                     memLevels = memLevels,
                      palUint8 = None,
                    strategies = strategies,
                        wbitss = wbitss,
                )
            )
