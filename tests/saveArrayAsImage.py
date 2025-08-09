#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “save_array_as_image()”.

    # Import standard modules ...
    import argparse
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

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Demonstrate saving an array as an image.",
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
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Create short-hand and make output directory ...
    dName = f'{args.absPathToRepo}/tests/{os.path.basename(__file__).removesuffix(".py")}'
    if not os.path.exists(dName):
        os.mkdir(dName)

    # **************************************************************************

    # Set image size ...
    nx, ny = 16, 32                                                             # [px], [px]

    # Create array ...
    arr = numpy.zeros((ny, nx), dtype = numpy.float64)
    for ix in range(nx):
        for iy in range(ny):
            arr[iy, ix] = 0.5 * float(ix * iy)

    # Save array as PNGs ...
    pyguymer3.image.save_array_as_image(
        arr,
        f"{dName}/saveArrayAsImage0.png",
          debug = args.debug,
        timeout = args.timeout,
    )
    pyguymer3.image.save_array_as_image(
        arr,
        f"{dName}/saveArrayAsImage1.png",
          debug = args.debug,
          scale = True,
        timeout = args.timeout,
    )
    pyguymer3.image.save_array_as_image(
        arr,
        f"{dName}/saveArrayAsImage2.png",
          debug = args.debug,
         pc_bot = 5.0,
         pc_top = 5.0,
          scale = True,
        timeout = args.timeout,
    )
    pyguymer3.image.save_array_as_image(
        arr,
        f"{dName}/saveArrayAsImage3.png",
             ct = "turbo",
          debug = args.debug,
         pc_bot = 5.0,
         pc_top = 5.0,
          scale = True,
        timeout = args.timeout,
    )

    # Save array as PPMs ...
    pyguymer3.image.save_array_as_image(
        arr,
        f"{dName}/saveArrayAsImage0.ppm",
          debug = args.debug,
           form = "ppm",
        timeout = args.timeout,
    )
    pyguymer3.image.save_array_as_image(
        arr,
        f"{dName}/saveArrayAsImage1.ppm",
          debug = args.debug,
           form = "ppm",
          scale = True,
        timeout = args.timeout,
    )
    pyguymer3.image.save_array_as_image(
        arr,
        f"{dName}/saveArrayAsImage2.ppm",
          debug = args.debug,
           form = "ppm",
         pc_bot = 5.0,
         pc_top = 5.0,
          scale = True,
        timeout = args.timeout,
    )
    pyguymer3.image.save_array_as_image(
        arr,
        f"{dName}/saveArrayAsImage3.ppm",
          debug = args.debug,
             ct = "turbo",
           form = "ppm",
         pc_bot = 5.0,
         pc_top = 5.0,
          scale = True,
        timeout = args.timeout,
    )
