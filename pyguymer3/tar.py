def tar(tarName, fnames, kwArgCheck = None, cwd = None, stderr = None, stdout = None):
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

    Note
    ----
    I still need to provide justification for using this function over https://docs.python.org/3/library/tarfile.html
    """

    # Import standard modules ...
    import shutil
    import subprocess
    import tempfile

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check that "tar" is installed ...
    if shutil.which("tar") is None:
        raise Exception("\"tar\" is not installed") from None

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
        with open(tmpName, "wt", encoding = "utf-8") as fobj:
            for fname in fnames:
                fobj.write(f"{fname}\n")

        # Make archive ...
        subprocess.run(
            [
                "tar",
                "--create",
                "--file", tarName,
                "--files-from", tmpName,
                "--format", "pax",
                "--no-acls",
                "--no-fflags",
                "--no-xattrs"
            ],
               check = True,
                 cwd = cwd,
            encoding = "utf-8",
              stderr = stderr,
              stdout = stdout,
        )
