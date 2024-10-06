#!/usr/bin/env python3

# Define function ...
def tar(
    tarName,
    fnames,
    /,
    *,
        cwd = None,
     stderr = None,
     stdout = None,
    tarPath = None,
    timeout = 60.0,
):
    """Create a PAX formatted TAR file (without any frills or size limits).

    Parameters
    ----------
    tarName : str
        the name of the TAR file to create
    fnames : list of str
        the list of files to put in the TAR file
    cwd : str, optional
        the child working directory (default None)
    stderr : subprocess.PIPE, subprocess.DEVNULL, io.TextIOWrapper, optional
        the destination of STDERR (default None)
    stdout : subprocess.PIPE, subprocess.DEVNULL, io.TextIOWrapper, optional
        the destination of STDOUT (default None)
    tarPath : str, optional
        the path to the "tar" binary (if not provided then Python will attempt
        to find the binary itself)
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Notes
    -----
    I still need to provide justification for using this function over https://docs.python.org/3.12/library/tarfile.html

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import shutil
    import subprocess
    import tempfile

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if tarPath is None:
        tarPath = shutil.which("tar")
    assert tarPath is not None, "\"tar\" is not installed"

    # Check inputs ...
    if not isinstance(fnames, list):
        raise Exception("\"fnames\" is not a list") from None
    if len(fnames) == 0:
        raise Exception("\"fnames\" is empty") from None

    # Create temporary directory ...
    with tempfile.TemporaryDirectory(prefix = "tar.") as tname:
        # Deduce temporary name ...
        tmpName = f"{tname}/fnames.txt"

        # Make list of files to archive ...
        with open(tmpName, "wt", encoding = "utf-8") as fObj:
            for fname in fnames:
                fObj.write(f"{fname}\n")

        # Make archive ...
        subprocess.run(
            [
                tarPath,
                "--create",
                "--file", tarName,
                "--files-from", tmpName,
                "--format", "pax",
                "--no-acls",
                "--no-fflags",
                "--no-xattrs",
            ],
               check = True,
                 cwd = cwd,
            encoding = "utf-8",
              stderr = stderr,
              stdout = stdout,
             timeout = timeout,
        )
