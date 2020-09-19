def tar(tarName, fnames, cwd = None):
    # Import standard modules ...
    import os
    import shutil
    import subprocess
    import tempfile

    # Check that "tar" is installed ...
    if shutil.which("tar") is None:
        raise Exception("\"tar\" is not installed")

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
