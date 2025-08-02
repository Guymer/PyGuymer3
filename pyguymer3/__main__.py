#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    """
    A wrapper to my Python module to allow some of the functions to be called.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import argparse
    import sys

    # Import sub-functions ...
    from .generate_password import generate_password
    from .generate_random_stub import generate_random_stub
    from .sha256 import sha256
    from .sha256_of_GZ import sha256_of_GZ
    from .sha256_of_MP4 import sha256_of_MP4
    from .sha512 import sha512
    from .sha512_of_GZ import sha512_of_GZ
    from .sha512_of_MP4 import sha512_of_MP4

    # **************************************************************************

    # Create argument parser and parse the arguments ...
    parser = argparse.ArgumentParser(
           allow_abbrev = False,
            description = "This is a wrapper to allow users to call some of the functions within \"pyguymer3\" without having to write a trivial Python script.",
        formatter_class = argparse.ArgumentDefaultsHelpFormatter,
    )
    parser.add_argument(
        "--chunksize",
        default = 1048576,
           help = "the size of the chunks of any files which are read in (in bytes)",
           type = int,
    )
    parser.add_argument(
        "--debug",
        action = "store_true",
          help = "print debug messages",
    )
    parser.add_argument(
        "--generate-password",
        action = "store_true",
          dest = "generatePassword",
          help = "generate a random password",
    )
    parser.add_argument(
        "--generate-stub",
        action = "store_true",
          dest = "generateStub",
          help = "generate a random stub",
    )
    parser.add_argument(
        "--ignore-modification-time",
        action = "store_true",
          dest = "ignoreModificationTime",
          help = "ignore the modification time in the input file's metadata if the input is either a GZ file or a MP4 file",
    )
    parser.add_argument(
        "--input-file",
        dest = "inputFile",
        help = "the input file",
        type = str,
    )
    parser.add_argument(
        "--sha-256-of-input-file",
        action = "store_true",
          dest = "sha256",
          help = "calculate the SHA-256 hexdigest of the input file (\"--input-file\")",
    )
    parser.add_argument(
        "--sha-256-of-input-GZ",
        action = "store_true",
          dest = "sha256ofGz",
          help = "calculate the SHA-256 hexdigest of the input file (\"--input-file\") assuming that it is a GZ file",
    )
    parser.add_argument(
        "--sha-256-of-input-MP4",
        action = "store_true",
          dest = "sha256ofMp4",
          help = "calculate the SHA-256 hexdigest of the input file (\"--input-file\") assuming that it is a MP4 file",
    )
    parser.add_argument(
        "--sha-512-of-input-file",
        action = "store_true",
          dest = "sha512",
          help = "calculate the SHA-512 hexdigest of the input file (\"--input-file\")",
    )
    parser.add_argument(
        "--sha-512-of-input-GZ",
        action = "store_true",
          dest = "sha512ofGz",
          help = "calculate the SHA-512 hexdigest of the input file (\"--input-file\") assuming that it is a GZ file",
    )
    parser.add_argument(
        "--sha-512-of-input-MP4",
        action = "store_true",
          dest = "sha512ofMp4",
          help = "calculate the SHA-512 hexdigest of the input file (\"--input-file\") assuming that it is a MP4 file",
    )
    args = parser.parse_args()

    # **************************************************************************

    # Check what needs to be done ...
    if args.generatePassword:
        print(
            generate_password()
        )
        sys.exit(0)
    if args.generateStub:
        print(
            generate_random_stub()
        )
        sys.exit(0)
    if args.sha256:
        print(
            sha256(
                args.inputFile,
                chunksize = args.chunksize,
            )
        )
        sys.exit(0)
    if args.sha256ofGz:
        print(
            sha256_of_GZ(
                args.inputFile,
                             chunksize = args.chunksize,
                ignoreModificationTime = args.ignoreModificationTime,
            )
        )
        sys.exit(0)
    if args.sha256ofMp4:
        print(
            sha256_of_MP4(
                args.inputFile,
                             chunksize = args.chunksize,
                ignoreModificationTime = args.ignoreModificationTime,
            )
        )
        sys.exit(0)
    if args.sha512:
        print(
            sha512(
                args.inputFile,
                chunksize = args.chunksize,
            )
        )
        sys.exit(0)
    if args.sha512ofGz:
        print(
            sha512_of_GZ(
                args.inputFile,
                             chunksize = args.chunksize,
                ignoreModificationTime = args.ignoreModificationTime,
            )
        )
        sys.exit(0)
    if args.sha512ofMp4:
        print(
            sha512_of_MP4(
                args.inputFile,
                             chunksize = args.chunksize,
                ignoreModificationTime = args.ignoreModificationTime,
            )
        )
        sys.exit(0)

    # Print catch ...
    print("The see the options for this wrapper, run it again with \"--help\".")
