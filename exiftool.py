def exiftool(fname):
    # Import modules ...
    import os
    import subprocess

    # Check that "exiftool" is installed ...
    try:
        subprocess.check_call(
            [
                "type",
                "exiftool"
            ],
            encoding = "utf-8",
            stdout = open(os.devnull, "wt"),
            stderr = open(os.devnull, "wt")
        )
    except subprocess.CalledProcessError:
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
