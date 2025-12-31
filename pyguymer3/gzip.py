#!/usr/bin/env python3

# Define function ...
def gzip(
    fname,
    /,
    *,
         cwd = None,
    gzipPath = None,
      stderr = None,
      stdout = None,
     timeout = 60.0,
):
    """Compress a file using "gzip".

    Parameters
    ----------
    fname : str
        the name of the file to compress
    cwd : str, optional
        the child working directory (default None)
    gzipPath : None or str, optional
        the path to the "gzip" binary (if not provided then Python will attempt
        to find the binary itself)
    stderr : subprocess.PIPE, subprocess.DEVNULL, io.TextIOWrapper, optional
        the destination of STDERR (default None)
    stdout : subprocess.PIPE, subprocess.DEVNULL, io.TextIOWrapper, optional
        the destination of STDOUT (default None)
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Notes
    -----
    I still need to provide justification for using this function over https://docs.python.org/3.12/library/gzip.html

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
    if gzipPath is None:
        gzipPath = shutil.which("gzip")
    assert gzipPath is not None, "\"gzip\" is not installed"

    # Compress file ...
    subprocess.run(
        [
            gzipPath,
            "-9",
            fname,
        ],
           check = True,
             cwd = cwd,
        encoding = "utf-8",
          stderr = stderr,
          stdout = stdout,
         timeout = timeout,
    )
