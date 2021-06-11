def dot2png(dot, png, kwArgCheck = None, debug = False, strip = False):
    # Import standard modules ...
    import os
    import shutil
    import subprocess

    # Load sub-functions ...
    from .optimize_image import optimize_image

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check that "dot" is installed ...
    if shutil.which("dot") is None:
        raise Exception("\"dot\" is not installed") from None

    # Create image ...
    subprocess.check_call(
        ["dot", "-Tpng", dot, "-o", png],
        encoding = "utf-8",
        stderr = open(os.devnull, "wt"),
        stdout = open(os.devnull, "wt")
    )

    # Optimize PNG ...
    optimize_image(png, debug = debug, strip = strip)
