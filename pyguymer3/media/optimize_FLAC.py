def optimize_FLAC(fname1, kwArgCheck = None, debug = False):
    """
    "metaflac" does not modify, but it does touch, the FLAC even if it cannot
    make it smaller, therefore it is NOT safe to keep on running "metaflac" on
    the same FLAC over and over again.
    """

    # Import standard modules ...
    import os
    import shutil
    import subprocess
    import tempfile

    # Import sub-functions ...
    from .does_FLAC_have_padding import does_FLAC_have_padding
    from ..sha512 import sha512

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check that "metaflac" is installed ...
    if shutil.which("metaflac") is None:
        raise Exception("\"metaflac\" is not installed") from None

    # Check that the FLAC exists ...
    if not os.path.exists(fname1):
        raise Exception(f"\"{fname1}\" does not exist") from None

    # Skip this FLAC if it does not have any padding ...
    if not does_FLAC_have_padding(fname1):
        if debug:
            print(f"INFO: Skipping because \"{fname1}\" does not have any padding")
        return

    # Create temporary directory ...
    with tempfile.TemporaryDirectory(prefix = "optimize_FLAC.") as tname:
        # Copy the FLAC into the temporary directory ...
        shutil.copy(fname1, tname)

        # Deduce the FLAC name in the temporary directory ...
        fname2 = os.path.join(tname, os.path.basename(fname1))

        # Optimise FLAC ...
        subprocess.run(
            [
                "metaflac",
                "--dont-use-padding",
                "--remove",
                "--block-type=PADDING",
                fname2
            ],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
              stdout = subprocess.DEVNULL,
        )

        # Find the two hashes and don't replace the original if the new one is
        # the same ...
        if sha512(fname1) == sha512(fname2):
            if debug:
                print(f"INFO: Skipping because \"{fname2}\" is the same as \"{fname1}\"")
            return

        # Replace the original ...
        shutil.move(fname2, fname1)
