def optipng(fname):
    """
    "optipng" does not modify, and it does not touch, the image even if it
    cannot make it smaller, therefore it is safe to keep on running "optipng" on
    the same PNG over and over again.
    """

    # Import standard modules ...
    import os
    import shutil

    # Check that "optipng" is installed ...
    if shutil.which("optipng") is None:
        raise Exception("\"optipng\" is not installed") from None

    # Check that the image exists ...
    if not os.path.exists(fname):
        raise Exception(f"\"{fname}\" does not exist") from None

    # Optimise PNG ...
    subprocess.run(
        [
            "optipng",
            fname
        ],
           check = True,
        encoding = "utf-8",
          stderr = subprocess.DEVNULL,
          stdout = subprocess.DEVNULL,
    )
