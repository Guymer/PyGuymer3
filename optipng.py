def optipng(fname):
    """
    "optipng" does not modify, and it does not touch, the image if it cannot
    make it smaller, therefore it is safe to keep on running "optipng" on the
    same PNG over and over again.
    """

    # Import standard modules ...
    import os
    import shutil
    import subprocess

    # Check that "optipng" is installed ...
    if shutil.which("optipng") is None:
        raise Exception("\"optipng\" is not installed")

    # Check that the image exists ...
    if not os.path.exists(fname):
        raise Exception("\"{:s}\" does not exist".format(fname))

    # Optimise PNG ...
    subprocess.check_call(
        [
            "optipng",
            fname
        ],
        encoding = "utf-8",
        stderr = open(os.devnull, "wt"),
        stdout = open(os.devnull, "wt")
    )
