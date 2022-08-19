def xz(fname, kwArgCheck = None, cwd = None, stderr = None, stdout = None, threads = 0):
    """Compress a file using "xz" (with SHA-256 integrity checks).

    Parameters
    ----------
    fname : str
        the name of the file to compress
    cwd : str, optional
        the child working directory (default None)
    stderr : subprocess.PIPE, subprocess.DEVNULL, io.TextIOWrapper, optional
        the destination of STDERR (default None)
    stdout : subprocess.PIPE, subprocess.DEVNULL, io.TextIOWrapper, optional
        the destination of STDOUT (default None)
    threads : int, optional
        the number of threads to use (default 0)

    Note
    ----
    I still need to provide justification for using this function over https://docs.python.org/3/library/lzma.html
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
