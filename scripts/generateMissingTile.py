#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Import standard modules ...
    import argparse
    import os

    # Import special modules ...
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

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "Save a tile to be used when desired tiles are missing.",
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

    # Create short-hands ...
    # NOTE: See "pyguymer3/data/png/README.md".
    tileSize = 300                                                              # [px]
    blockSize = tileSize // 10                                                  # [px]
    assert tileSize % blockSize == 0

    # **************************************************************************

    # Create empty tile ...
    tile = PIL.Image.new(
        color = (255, 255, 255),
         mode = "RGB",
         size = (
            tileSize,
            tileSize,
        ),
    )

    # Loop over blocks ...
    for iy in range(tileSize // blockSize):
        for ix in range(tileSize // blockSize):
            # Set block colour ...
            color = None
            if iy % 2 == ix % 2:
                color = (255, 0, 255)
            else:
                color = (0, 0, 0)

            # Fill in block ...
            tile.paste(
                color,
                (
                    ix * blockSize,
                    iy * blockSize,
                    (ix + 1) * blockSize,
                    (iy + 1) * blockSize,
                ),
            )

    # Save tile ...
    tile.save(
        f"{args.absPathToRepo}/pyguymer3/data/png/missingTile.png",
        optimize = True,
    )

    # Optimise PNG ...
    pyguymer3.image.optimise_image(
        f"{args.absPathToRepo}/pyguymer3/data/png/missingTile.png",
          debug = args.debug,
          strip = True,
        timeout = args.timeout,
    )
