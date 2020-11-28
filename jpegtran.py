def jpegtran(fname1, debug = False):
    """
    "jpegtran" does not modify, but it does touch, the image if it cannot make
    it smaller, therefore it is NOT safe to keep on running "jpegtran" on the
    same JPG over and over again.
    """

    # Import standard modules ...
    import os
    import shutil
    import subprocess
    import tempfile

    # Load sub-functions ...
    from .sha512 import sha512

    # Check that "jpegtran" is installed ...
    if shutil.which("jpegtran") is None:
        raise Exception("\"jpegtran\" is not installed") from None

    # Check that the image exists ...
    if not os.path.exists(fname1):
        raise Exception("\"{0:s}\" does not exist".format(fname1)) from None

    # Create temporary directory ...
    with tempfile.TemporaryDirectory(prefix = "jpegtran.") as tname:
        # Create temporary name ...
        fname2 = os.path.join(tname, "image.jpg")

        # Optimise JP[E]G ...
        subprocess.check_call(
            [
                "jpegtran",
                "-copy", "all",
                "-optimise",
                "-outfile", fname2,
                "-perfect",
                fname1
            ],
            encoding = "utf-8",
            stderr = open(os.devnull, "wt"),
            stdout = open(os.devnull, "wt")
        )

        # Find the two sizes and don't replace the original if the new one is
        # larger, or equal ...
        if os.path.getsize(fname2) >= os.path.getsize(fname1):
            if debug:
                print("INFO: Skipping because \"{:s}\" is larger than, or equal to, \"{:s}\"".format(fname2, fname1))
            return

        # Find the two hashes and don't replace the original if the new one is
        # the same ...
        if sha512(fname1).hexdigest() == sha512(fname2).hexdigest():
            if debug:
                print("INFO: Skipping because \"{:s}\" is the same as \"{:s}\"".format(fname2, fname1))
            return

        # Replace the original ...
        shutil.move(fname2, fname1)
