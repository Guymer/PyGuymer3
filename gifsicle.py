def gifsicle(fname):
    # Import modules ...
    import os
    import subprocess

    # Check that "gifsicle" is installed ...
    try:
        subprocess.check_call(
            [
                "type",
                "gifsicle"
            ],
            encoding = "utf-8",
            stdout = open(os.devnull, "wt"),
            stderr = open(os.devnull, "wt")
        )
    except subprocess.CalledProcessError:
        raise Exception("\"gifsicle\" is not installed")

    # Check that the image exists ...
    if not os.path.exists(fname):
        raise Exception("\"{:s}\" does not exist".format(fname))

    # Optimise GIF ...
    try:
        subprocess.check_call(
            [
                "gifsicle",
                "--batch",
                "--unoptimize",
                "--optimize=3",
                fname
            ],
            encoding = "utf-8",
            stdout = open(os.devnull, "wt"),
            stderr = open(os.devnull, "wt")
        )
    except subprocess.CalledProcessError:
        raise Exception("\"gifsicle\" failed")
