#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    """
    A wrapper to my Python sub-module to allow some of the functions to be
    called.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import argparse
    import shutil
    import sys

    # Import sub-functions ...
    from .image2gif import image2gif
    from .image2jpg import image2jpg
    from .image2png import image2png
    from .image2webp import image2webp
    from .manuallyOptimisePng import manuallyOptimisePng
    from .optimise_image import optimise_image

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "This is a wrapper to allow users to call some of the functions within \"pyguymer3.image\" without having to write a trivial Python script.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--chunksize",
        default = 1048576,
           help = "the size of the chunks of any files which are read in (in bytes)",
           type = int,
    )
    parser.add_argument(
        "--convert-input-image-to-GIF",
        action = "store_true",
          dest = "image2gif",
          help = "convert the input image (\"--input-image\") to an output GIF image (\"--output-image\")",
    )
    parser.add_argument(
        "--convert-input-image-to-JPG",
        action = "store_true",
          dest = "image2jpg",
          help = "convert the input image (\"--input-image\") to an output JPG image (\"--output-image\")",
    )
    parser.add_argument(
        "--convert-input-image-to-PNG",
        action = "store_true",
          dest = "image2png",
          help = "convert the input image (\"--input-image\") to an output PNG image (\"--output-image\")",
    )
    parser.add_argument(
        "--convert-input-image-to-WEBP",
        action = "store_true",
          dest = "image2webp",
          help = "convert the input image (\"--input-image\") to an output WEBP image (\"--output-image\")",
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--dont-strip",
        action = "store_true",
          dest = "dontStrip",
          help = "don't strip metadata from output image",
    )
    parser.add_argument(
        "--dpi",
        default = None,
           help = "the DPI of the optimised output PNG image",
           type = int,
    )
    parser.add_argument(
        "--exiftool-path",
        default = shutil.which("exiftool"),
           dest = "exiftoolPath",
           help = "the path to the \"exiftool\" binary",
           type = str,
    )
    parser.add_argument(
        "--gifsicle-path",
        default = shutil.which("gifsicle"),
           dest = "gifsiclePath",
           help = "the path to the \"gifsicle\" binary",
           type = str,
    )
    parser.add_argument(
        "--input-image",
        dest = "inputImage",
        help = "the input image",
        type = str,
    )
    parser.add_argument(
        "--jpegtran-path",
        default = shutil.which("jpegtran"),
           dest = "jpegtranPath",
           help = "the path to the \"jpegtran\" binary",
           type = str,
    )
    parser.add_argument(
        "--lossless",
        action = "store_true",
          help = "write lossless output WEBP image",
    )
    parser.add_argument(
        "--method",
        default = 6,
           help = "the method used to write the output WEBP image",
           type = int,
    )
    parser.add_argument(
        "--mode",
        default = "RGB",
           help = "the mode of the output image",
           type = str,
    )
    parser.add_argument(
        "--optimise-input-image",
        action = "store_true",
          dest = "optimiseImage",
          help = "optimise the input image (\"--input-image\") and replace it if space can be saved",
    )
    parser.add_argument(
        "--optimise-input-PNG-image",
        action = "store_true",
          dest = "manuallyOptimisePng",
          help = "optimise the input PNG image (\"--input-image\") and replace it if space can be saved",
    )
    parser.add_argument(
        "--optipng-path",
        default = shutil.which("optipng"),
           dest = "optipngPath",
           help = "the path to the \"optipng\" binary",
           type = str,
    )
    parser.add_argument(
        "--output-image",
        dest = "outputImage",
        help = "the output image",
        type = str,
    )
    parser.add_argument(
        "--progressive",
        action = "store_true",
          help = "write progressive output JPG image",
    )
    parser.add_argument(
        "--quality",
        default = 95,
           help = "the quality of the output JPG/WEBP image",
           type = int,
    )
    parser.add_argument(
        "--screen-width",
        default = -1,
           dest = "screenWidth",
           help = "resize the input image to fit inside a screen of this width",
           type = int,
    )
    parser.add_argument(
        "--screen-height",
        default = -1,
           dest = "screenHeight",
           help = "resize the input image to fit inside a screen of this height",
           type = int,
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # Put more effort into reducing the file size if the user asked for lossless
    # image ...
    if args.lossless:
        args.quality = 100

    # **************************************************************************

    # Check what needs to be done ...
    if args.image2gif:
        image2gif(
            args.inputImage,
            args.outputImage,
               chunksize = args.chunksize,
                   debug = args.debug,
            exiftoolPath = args.exiftoolPath,
            gifsiclePath = args.gifsiclePath,
            jpegtranPath = args.jpegtranPath,
                    mode = args.mode,
                optimise = True,
             optipngPath = args.optipngPath,
            screenHeight = args.screenHeight,
             screenWidth = args.screenWidth,
                   strip = not args.dontStrip,
                 timeout = args.timeout,
        )
        sys.exit(0)
    if args.image2jpg:
        image2jpg(
            args.inputImage,
            args.outputImage,
               chunksize = args.chunksize,
                   debug = args.debug,
                    exif = None,
            exiftoolPath = args.exiftoolPath,
            gifsiclePath = args.gifsiclePath,
            jpegtranPath = args.jpegtranPath,
                    mode = args.mode,
                optimise = True,
             optipngPath = args.optipngPath,
             progressive = args.progressive,
                 quality = args.quality,
            screenHeight = args.screenHeight,
             screenWidth = args.screenWidth,
                   strip = not args.dontStrip,
                 timeout = args.timeout,
        )
        sys.exit(0)
    if args.image2png:
        image2png(
            args.inputImage,
            args.outputImage,
               chunksize = args.chunksize,
                   debug = args.debug,
                    exif = None,
            exiftoolPath = args.exiftoolPath,
            gifsiclePath = args.gifsiclePath,
            jpegtranPath = args.jpegtranPath,
                    mode = args.mode,
                optimise = True,
             optipngPath = args.optipngPath,
            screenHeight = args.screenHeight,
             screenWidth = args.screenWidth,
                   strip = not args.dontStrip,
                 timeout = args.timeout,
        )
        sys.exit(0)
    if args.image2webp:
        image2webp(
            args.inputImage,
            args.outputImage,
                    exif = None,
                lossless = args.lossless,
                  method = args.method,
                    mode = args.mode,
                 quality = args.quality,
            screenHeight = args.screenHeight,
             screenWidth = args.screenWidth,
        )
        sys.exit(0)
    if args.manuallyOptimisePng:
        manuallyOptimisePng(
            args.inputImage,
            calcAdaptive = True,
             calcAverage = True,
                calcNone = True,
               calcPaeth = True,
                 calcSub = True,
                  calcUp = True,
                   debug = args.debug,
                     dpi = args.dpi,
                 modTime = None,
        )
        sys.exit(0)
    if args.optimiseImage:
        optimise_image(
            args.inputImage,
               chunksize = args.chunksize,
                   debug = args.debug,
            exiftoolPath = args.exiftoolPath,
            gifsiclePath = args.gifsiclePath,
            jpegtranPath = args.jpegtranPath,
             optipngPath = args.optipngPath,
                    pool = None,
                   strip = not args.dontStrip,
                 timeout = args.timeout,
        )
        sys.exit(0)

    # Print catch ...
    print("The see the options for this wrapper, run it again with \"--help\".")
