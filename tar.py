def tar(tarName, fnames, cwd = None):
    """
    Create a PAX formatted TAR file (without any frills or size limits).

    Arguments:
    tarName -- the name of the TAR file to create
    fnames -- the list of files to put in the TAR file

    Keyword arguments:
    cwd -- the child working directory (default None)
    """

    # Import standard modules ...
    import os
    import shutil
    import subprocess
    import tempfile

    # Check that "tar" is installed ...
    if shutil.which("tar") is None:
        raise Exception("\"tar\" is not installed")

    # Check inputs ...
    if not isinstance(fnames, list):
        raise Exception("\"fnames\" is not a list")
    if len(fnames) == 0:
        raise Exception("\"fnames\" is empty")

    # Create temporary directory ...
    with tempfile.TemporaryDirectory(prefix = "tar.") as tname:
        # Deduce temporary name ...
        tmpName = os.path.join(tname, "fnames.txt")

        # Make list of files to archive ...
        with open(tmpName, "wt") as fobj:
            for fname in fnames:
                fobj.write("{:s}\n".format(fname))

        # Make archive ...
        subprocess.check_call(
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
            cwd = cwd,
            encoding = "utf-8",
            stderr = open(os.devnull, "wt"),
            stdout = open(os.devnull, "wt")
        )
