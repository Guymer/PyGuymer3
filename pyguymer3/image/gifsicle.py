#!/usr/bin/env python3

# Define function ...
def gifsicle(
    fname1,
    /,
    *,
       chunksize = 1048576,
           debug = __debug__,
    gifsiclePath = None,
         timeout = 60.0,
):
    """
    "gifsicle" does modify, and it does touch, the image even if it cannot make
    it smaller, therefore it is NOT safe to keep on running "gifsicle" on the
    same GIF over and over again.

    In my own testing (August 2020) I have found that "gifsicle" switches
    between two different images when it is run repeatadly (try finding the MD5
    hash of the images) and that the only differences between these identically
    sized images is the order of the colours in the colour map (try running
    "gifsicle --color-info file.gif" yourself after each call and then "diff"
    the output).
    """

    # Import standard modules ...
    import os
    import shutil
    import subprocess
    import tempfile

    # Import sub-functions ...
    from ..sha512 import sha512

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if gifsiclePath is None:
        gifsiclePath = shutil.which("gifsicle")
    assert gifsiclePath is not None, "\"gifsicle\" is not installed"

    # Check that the image exists ...
    if not os.path.exists(fname1):
        raise Exception(f"\"{fname1}\" does not exist") from None

    # Create temporary directory ...
    with tempfile.TemporaryDirectory(prefix = "gifsicle.") as tname:
        # Create temporary name ...
        fname2 = f"{tname}/image.gif"

        # Optimise GIF ...
        subprocess.run(
            [
                gifsiclePath,
                "--unoptimize",
                "--optimize=3",
                "--output", fname2,
                fname1,
            ],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
              stdout = subprocess.DEVNULL,
             timeout = timeout,
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
