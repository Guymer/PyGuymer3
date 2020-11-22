def xz(fname, threads = 0):
    # Import standard modules ...
    import os
    import shutil
    import subprocess

    # Check that "xz" is installed ...
    if shutil.which("xz") is None:
        raise Exception("\"xz\" is not installed")

    # Check inputs ...
    if not isinstance(threads, int):
        raise Exception("\"threads\" is not an integer")

    # Compress file ...
    subprocess.check_call(
        [
            "xz",
            "--compress",
            "-9e",
            "--check=sha256",
            "--format=xz",
            "--threads={:d}".format(threads),
            fname
        ],
        encoding = "utf-8",
        stderr = open(os.devnull, "wt"),
        stdout = open(os.devnull, "wt")
    )
