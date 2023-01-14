#!/usr/bin/env python3

# Define function ...
def jpegtran(fname1, kwArgCheck = None, chunksize = 1048576, debug = False):
    """
    "jpegtran" does not modify, but it does touch, the image even if it cannot
    make it smaller, therefore it is NOT safe to keep on running "jpegtran" on
    the same JPG over and over again.
    """

    # Import standard modules ...
    import os
    import shutil
    import subprocess
    import tempfile

    # Import sub-functions ...
    from ..sha512 import sha512

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check that "jpegtran" is installed ...
    if shutil.which("jpegtran") is None:
        raise Exception("\"jpegtran\" is not installed") from None

    # Check that the image exists ...
    if not os.path.exists(fname1):
        raise Exception(f"\"{fname1}\" does not exist") from None

    # Create temporary directory ...
    with tempfile.TemporaryDirectory(prefix = "jpegtran.") as tname:
        # Create temporary name ...
        fname2 = f"{tname}/image.jpg"

        # Optimise JP[E]G ...
        subprocess.run(
            [
                "jpegtran",
                "-copy", "all",
                "-optimise",
                "-outfile", fname2,
                "-perfect",
                fname1
            ],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
              stdout = subprocess.DEVNULL,
        )

        # Find the two sizes and don't replace the original if the new one is
        # larger, or equal ...
        if os.path.getsize(fname2) >= os.path.getsize(fname1):
            if debug:
                print(f"INFO: Skipping because \"{fname2}\" is larger than, or equal to, \"{fname1}\"")
            return

        # Find the two hashes and don't replace the original if the new one is
        # the same ...
        if sha512(fname1, chunksize = chunksize) == sha512(fname2, chunksize = chunksize):
            if debug:
                print(f"INFO: Skipping because \"{fname2}\" is the same as \"{fname1}\"")
            return

        # Replace the original ...
        shutil.move(fname2, fname1)
