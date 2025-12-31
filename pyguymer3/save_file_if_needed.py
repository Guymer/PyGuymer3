#!/usr/bin/env python3

# Define function ...
def save_file_if_needed(
    fname,
    fcontent,
    /,
    *,
         debug = __debug__,
      gitFiles = None,
    gitMessage = "regenerated",
       gitPath = None,
       timeout = 60.0,
):
    """Save a file. If the file already exists, then only overwrite it if the
    content is different.

    Parameters
    ----------
    fname : str
        the file name
    fcontent : bytes or str
        the file content
    debug : bool, optional
        print debug messages
    gitFiles : list of str, optional
        a list of file names known to Git, if the file ends up being saved and
        the file is not already known to Git then it will be added to Git
    gitMessage : str, optional
        the Git commit message, if the file ends up being saved and then commit
        it to Git
    gitPath : None or str, optional
        the path to the "git" binary (if not provided then Python will attempt
        to find the binary itself)
    timeout : float, optional
        the timeout for any requests/subprocess calls

    Returns
    -------
    needsSaving : bool
        did the file need saving

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os
    import shutil
    import subprocess

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if gitPath is None:
        gitPath = shutil.which("git")
    assert gitPath is not None, "\"git\" is not installed"

    # Check that the content is one of the two types allowed in Python 3 and set
    # the file access mode and the encoding ...
    match fcontent:
        case bytes():
            mode = "b"
            encoding = None
        case str():
            mode = "t"
            encoding = "utf-8"
        case _:
            # Crash ...
            raise TypeError(f"\"fcontent\" is an unexpected type ({repr(type(fcontent))})") from None

    # Initialize trigger ...
    needsSaving = False

    # Check if the file exists ...
    if os.path.exists(fname):
        # Open the file ...
        with open(fname, f"r{mode}", encoding = encoding) as fObj:
            # Check if the content is the same ...
            if fObj.read() != fcontent:
                # Set trigger ...
                needsSaving = True
    else:
        # Set trigger ...
        needsSaving = True

        # Create short-hand for the parent directory ...
        dname = os.path.dirname(fname)

        # Check that there is a parent directory in the provided file name path ...
        if dname != "":
            # Check if the parent directory does not exist ...
            if not os.path.exists(dname):
                # Make the parent directory ...
                os.makedirs(dname)

    # Check if the file needs saving ...
    if needsSaving:
        if debug:
            print(f"INFO: Saving \"{fname}\" ...")

        # Open the file ...
        with open(fname, f"w{mode}", encoding = encoding) as fObj:
            # Save the file ...
            fObj.write(fcontent)

        # Check if the user provided a list of files known to Git ...
        if gitFiles is not None:
            # Check if the file is not known to Git ...
            if fname not in gitFiles:
                if debug:
                    print(f"INFO: Adding \"{fname}\" to Git ...")

                # Add the file to Git ...
                subprocess.run(
                    [
                        gitPath,
                        "add",
                        "--intent-to-add",
                        fname,
                    ],
                       check = True,
                    encoding = "utf-8",
                      stderr = subprocess.DEVNULL,
                      stdout = subprocess.DEVNULL,
                     timeout = timeout,
                )

            # Check if the user provided a commit message ...
            if gitMessage is not None:
                if debug:
                    print(f"INFO: Committing \"{fname}\" to Git ...")

                # Commit the file to Git ...
                subprocess.run(
                    [
                        gitPath,
                        "commit",
                        fname,
                        "-m", gitMessage,
                    ],
                       check = True,
                    encoding = "utf-8",
                      stderr = subprocess.DEVNULL,
                      stdout = subprocess.DEVNULL,
                     timeout = timeout,
                )

    # Return answer ...
    return needsSaving
