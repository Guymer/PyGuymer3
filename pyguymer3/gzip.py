#!/usr/bin/env python3

# Define function ...
def gzip(fname, /, *, cwd = None, stderr = None, stdout = None, timeout = 60.0):
    """Compress a file using "gzip".

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
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Notes
    -----
    I still need to provide justification for using this function over https://docs.python.org/3.11/library/gzip.html

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import shutil
    import subprocess

    # Check that "gzip" is installed ...
    if shutil.which("gzip") is None:
        raise Exception("\"gzip\" is not installed") from None

    # Compress file ...
    subprocess.run(
        [
            "gzip",
            "-9",
            fname
        ],
           check = True,
             cwd = cwd,
        encoding = "utf-8",
          stderr = stderr,
          stdout = stdout,
         timeout = timeout,
    )
