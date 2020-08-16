def exiftool(fname):
    """
    "exiftool" does not modify, and it does not touch, the image if it cannot
    strip anything, therefore it is safe to keep on running "exiftool" on the
    same image over and over again.
    """

    # Import standard modules ...
    import os
    import shutil
    import subprocess

    # Check that "exiftool" is installed ...
    if shutil.which("exiftool") is None:
        raise Exception("\"exiftool\" is not installed")

    # Check that the image exists ...
    if not os.path.exists(fname):
        raise Exception("\"{:s}\" does not exist".format(fname))

    # Strip all metadata ...
    try:
        subprocess.check_call(
            [
                "exiftool",
                "-overwrite_original",
                "-all=",
                fname
            ],
            encoding = "utf-8",
            stdout = open(os.devnull, "wt"),
            stderr = open(os.devnull, "wt")
        )
    except subprocess.CalledProcessError:
        raise Exception("\"exiftool\" failed")
