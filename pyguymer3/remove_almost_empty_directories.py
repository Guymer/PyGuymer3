#!/usr/bin/env python3

# Define function ...
def remove_almost_empty_directories(
    path,
    /,
    *,
             debug = __debug__,
    ignorableFiles = (
        ".directory",
        ".DS_Store",
        "._.DS_Store",
        "cover.jpg",
        "cover.png",
        "Thumbs.db",
    ),
            remove = False,
):
    """Remove directories which are almost empty.

    This function removes directories which are *almost* empty, based on a tuple
    of ignorable file names.

    Parameters
    ----------
    path : str
        the directory to search
    debug : bool, optional
        print debug messages
    ignorableFiles : tuple of str, default=(".directory", ".DS_Store", "._.DS_Store", "Thumbs.db")
        the tuple of file names which can safely be ignored
    remove : bool, optional
        remove almost empty directories

    Returns
    -------
    ans : int
        the number of removed directories

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os

    # Initialize counter ...
    i = 0                                                                       # [#]

    # Loop over all the contents of the passed directory ...
    for root, dnames, fnames in os.walk(path):
        # Skip this directory if there are sub-directories ...
        if len(dnames):
            continue

        # Make a list of all of the non-ignorable files in this directory ...
        nonIgnorableFiles = fnames.copy()
        for ignorableFile in ignorableFiles:
            if ignorableFile in nonIgnorableFiles:
                nonIgnorableFiles.remove(ignorableFile)

        # Skip this directory if there are non-ignorable files ...
        if len(nonIgnorableFiles):
            continue

        # Increment counter ...
        i += 1                                                                  # [#]

        # Remove all ignorable files ...
        for ignorableFile in ignorableFiles:
            if ignorableFile in fnames:
                if debug:
                    print(f"Removing \"{root}/{ignorableFile}\" ...")
                if remove:
                    os.remove(f"{root}/{ignorableFile}")

        # Remove directory ...
        if debug:
            print(f"Removing \"{root}\" ...")
        if remove:
            os.rmdir(root)

    # Return counter ...
    return i
