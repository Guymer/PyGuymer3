def optipng(fname):
    # Import modules ...
    import os
    import subprocess

    # Check that "optipng" is installed ...
    try:
        subprocess.check_call(
            [
                "type",
                "optipng"
            ],
            encoding = "utf-8",
            stdout = open(os.devnull, "wt"),
            stderr = open(os.devnull, "wt")
        )
    except subprocess.CalledProcessError:
        raise Exception("\"optipng\" is not installed")

    # Check that the PNG exists ...
    if not os.path.exists(fname):
        raise Exception("\"{0:s}\" does not exist".format(fname))

    # Optimise PNG ...
    try:
        subprocess.check_call(
            [
                "optipng",
                fname
            ],
            encoding = "utf-8",
            stdout = open(os.devnull, "wt"),
            stderr = open(os.devnull, "wt")
        )
    except subprocess.CalledProcessError:
        raise Exception("\"optipng\" failed")
