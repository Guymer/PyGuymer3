def gifsicle(fname):
    """
    "gifsicle" does modify, and it does touch, the image if it cannot make
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
