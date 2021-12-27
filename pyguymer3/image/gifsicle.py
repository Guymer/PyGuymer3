def gifsicle(fname1, kwArgCheck = None, debug = False):
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

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check that "gifsicle" is installed ...
    if shutil.which("gifsicle") is None:
        raise Exception("\"gifsicle\" is not installed") from None

    # Check that the image exists ...
    if not os.path.exists(fname1):
        raise Exception(f"\"{fname1}\" does not exist") from None

    # Create temporary directory ...
    with tempfile.TemporaryDirectory(prefix = "gifsicle.") as tname:
        # Create temporary name ...
        fname2 = os.path.join(tname, "image.gif")

        # Optimise GIF ...
        subprocess.run(
            [
                "gifsicle",
                "--unoptimize",
                "--optimize=3",
                "--output", fname2,
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
        if sha512(fname1) == sha512(fname2):
            if debug:
                print(f"INFO: Skipping because \"{fname2}\" is the same as \"{fname1}\"")
            return

        # Replace the original ...
        shutil.move(fname2, fname1)
