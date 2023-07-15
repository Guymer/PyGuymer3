#!/usr/bin/env python3

# Define function ...
def xz(fname, /, *, cwd = None, stderr = None, stdout = None, threads = 0, timeout = 60.0):
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
    timeout : int, optional
        the timeout for any subprocess calls

    Notes
    -----
    I still need to provide justification for using this function over https://docs.python.org/3.11/library/lzma.html

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import shutil
    import subprocess

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
         timeout = timeout,
    )
