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
    from .images2gif import images2gif                                          # TODO
    from .images2mp4 import images2mp4                                          # TODO
    from .images2pdf import images2pdf                                          # TODO
    from .images2webp import images2webp                                        # TODO
    from .optimise_FLAC import optimise_FLAC
    from .optimise_MP4 import optimise_MP4
    from .print_FLAC_blocks import print_FLAC_blocks
    from .print_MP4_atoms import print_MP4_atoms

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "This is a wrapper to allow users to call some of the functions within \"pyguymer3.media\" without having to write a trivial Python script.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--input-media",
        dest = "inputMedia",
        help = "the input media",
        type = str,
    )
    parser.add_argument(
        "--metaflac-path",
        default = shutil.which("metaflac"),
           dest = "metaflacPath",
           help = "the path to the \"metaflac\" binary",
           type = str,
    )
    parser.add_argument(
        "--mp4file-path",
        default = shutil.which("mp4file"),
           dest = "mp4filePath",
           help = "the path to the \"mp4file\" binary",
           type = str,
    )
    parser.add_argument(
        "--optimise-input-FLAC-media",
        action = "store_true",
          dest = "optimiseFlac",
          help = "optimise the input FLAC media (\"--input-media\") and replace it if space can be saved",
    )
    parser.add_argument(
        "--optimise-input-MP4-media",
        action = "store_true",
          dest = "optimiseMp4",
          help = "optimise the input MP4 media (\"--input-media\") and replace it if space can be saved",
    )
    parser.add_argument(
        "--print-FLAC-blocks",
        action = "store_true",
          dest = "printFlacBlocks",
          help = "print the blocks in the input FLAC media (\"--input-media\")",
    )
    parser.add_argument(
        "--print-MP4-atoms",
        action = "store_true",
          dest = "printMp4Atoms",
          help = "print the atoms in the input MP4 media (\"--input-media\")",
    )
    parser.add_argument(
        "--timeout",
        default = 60.0,
           help = "the timeout for any requests/subprocess calls (in seconds)",
           type = float,
    )
    args = parser.parse_args()

    # **************************************************************************

    # Check what needs to be done ...
    if args.optimiseFlac:
        optimise_FLAC(
            args.inputMedia,
               chunksize = args.chunksize,
                   debug = args.debug,
            metaflacPath = args.metaflacPath,
                 timeout = args.timeout,
            )
        sys.exit(0)
    if args.optimiseMp4:
        optimise_MP4(
            args.inputMedia,
                  debug = args.debug,
            mp4filePath = args.mp4filePath,
                timeout = args.timeout,
        )
        sys.exit(0)
    if args.printFlacBlocks:
        print_FLAC_blocks(
            args.inputMedia,
        )
        sys.exit(0)
    if args.printMp4Atoms:
        print_MP4_atoms(
            args.inputMedia,
        )
        sys.exit(0)

    # Print catch ...
    print("The see the options for this wrapper, run it again with \"--help\".")
