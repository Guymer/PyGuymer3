#!/usr/bin/env python3

# Define function ...
def xz(
    fname,
    /,
    *,
        cwd = None,
     stderr = None,
     stdout = None,
    threads = 0,
    timeout = 60.0,
     xzPath = None,
):
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
    timeout : float, optional
        the timeout for any requests/subprocess calls
    xzPath : str, optional
        the path to the "xz" binary (if not provided then Python will attempt to
        find the binary itself)

    Notes
    -----
    I still need to provide justification for using this function over https://docs.python.org/3.12/library/lzma.html

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import shutil
    import subprocess

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if xzPath is None:
        xzPath = shutil.which("xz")
    assert xzPath is not None, "\"xz\" is not installed"

    # Check inputs ...
    if not isinstance(threads, int):
        raise Exception("\"threads\" is not an integer") from None

    # Compress file ...
    subprocess.run(
        [
            xzPath,
            "--compress",
            "-9e",
            "--check=sha256",
            "--format=xz",
            f"--threads={threads:d}",
            fname,
        ],
           check = True,
             cwd = cwd,
        encoding = "utf-8",
          stderr = stderr,
          stdout = stdout,
         timeout = timeout,
    )
