def xz(fname, kwArgCheck = None, cwd = None, stderr = None, stdout = None, threads = 0):
    """
    Compress a file using "xz" (with SHA-256 integrity checks).

    Arguments:
    fname -- the name of the file to compress

    Keyword arguments:
    cwd -- the child working directory (default None)
    stderr -- the destination of STDERR (default None)
    stdout -- the destination of STDOUT (default None)
    threads -- the number of threads to use (default 0)
    """

    # Import standard modules ...
    import shutil
    import subprocess

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check that "xz" is installed ...
    if shutil.which("xz") is None:
        raise Exception("\"xz\" is not installed") from None

    # Check inputs ...
    if not isinstance(threads, int):
        raise Exception("\"threads\" is not an integer") from None

    # Compress file ...
    subprocess.run(
        [
            "xz",
            "--compress",
            "-9e",
            "--check=sha256",
            "--format=xz",
            f"--threads={threads:d}",
            fname
        ],
           check = True,
             cwd = cwd,
        encoding = "utf-8",
          stderr = stderr,
          stdout = stdout,
    )
