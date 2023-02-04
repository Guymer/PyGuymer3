#!/usr/bin/env python3

# Define function ...
def exiftool(fname, /):
    """
    "exiftool" does not modify, and it does not touch, the image even if it
    cannot strip anything, therefore it is safe to keep on running "exiftool" on
    the same image over and over again.
    """

    # Import standard modules ...
    import os
    import shutil
    import subprocess

    # Check that "exiftool" is installed ...
    if shutil.which("exiftool") is None:
        raise Exception("\"exiftool\" is not installed") from None

    # Check that the image exists ...
    if not os.path.exists(fname):
        raise Exception(f"\"{fname}\" does not exist") from None

    # Extract file extension ...
    ext = os.path.splitext(fname)[1]

    # Strip all metadata depending on the file extension ...
    # NOTE: As of 1/Sep/2022, "exiftool" cannot write WEBP images.
    if ext.lower() not in [".webp"]:
        # Strip all metadata ...
        subprocess.run(
            [
                "exiftool",
                "-overwrite_original",
                "-all=",
                fname
            ],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
              stdout = subprocess.DEVNULL,
        )
