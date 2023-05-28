#!/usr/bin/env python3

# Define function ...
def optimize_MP4(fname1, /, *, debug = False):
    """
    "mp4file" does modify, and it does touch, the MP4 even if it cannot optimize
    it, therefore it is NOT safe to keep on running "mp4file" on the same MP4
    over and over again.
    """

    # Import standard modules ...
    import os
    import shutil
    import subprocess
    import tempfile

    # Import sub-functions ...
    from .does_MP4_have_free import does_MP4_have_free
    from .is_moov_at_beginning_of_MP4 import is_moov_at_beginning_of_MP4
    from ..sha512_of_MP4 import sha512_of_MP4

    # Check that "mp4file" is installed ...
    if shutil.which("mp4file") is None:
        raise Exception("\"mp4file\" is not installed") from None

    # Check that the MP4 exists ...
    if not os.path.exists(fname1):
        raise Exception(f"\"{fname1}\" does not exist") from None

    # Skip this MP4 if it does not have any "free" atoms and if the "moov" atom
    # is before the "mdat" atom ...
    if not does_MP4_have_free(fname1) and is_moov_at_beginning_of_MP4(fname1):
        if debug:
            print(f"INFO: Skipping because \"{fname1}\" does not have any \"free\" atoms and the \"moov\" atom is before the \"mdat\" atom")
        return

    # Create temporary directory ...
    with tempfile.TemporaryDirectory(prefix = "optimize_MP4.") as tname:
        # Copy the MP4 into the temporary directory ...
        shutil.copy(fname1, tname)

        # Deduce the MP4 name in the temporary directory ...
        fname2 = f"{tname}/{os.path.basename(fname1)}"

        # Optimize MP4 ...
        subprocess.run(
            [
                "mp4file",
                "--optimize",
                fname2
            ],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
              stdout = subprocess.DEVNULL,
        )

        # Find the two hashes and don't replace the original if the new one is
        # the same ...
        if sha512_of_MP4(fname1, ignoreModificationTime = True) == sha512_of_MP4(fname2, ignoreModificationTime = True):
            if debug:
                print(f"INFO: Skipping because \"{fname2}\" is the same as \"{fname1}\"")
            return

        # Replace the original ...
        shutil.move(fname2, fname1)
