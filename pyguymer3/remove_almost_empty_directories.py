def remove_almost_empty_directories(path, kwArgCheck = None, debug = False, ignorableFiles = (".directory", ".DS_Store", "._.DS_Store", "Thumbs.db"), remove = False):
    """Remove directories which are almost empty.

    This function removes directories which are *almost* empty, based on a tuple
    of ignorable file names.

    Parameters
    ----------
    path : str
        the directory to search
    debug : bool, default=True
        print debug messages
    ignorableFiles : tuple of str, default=(".directory", ".DS_Store", "._.DS_Store", "Thumbs.db")
        the tuple of file names which can safely be ignored
    remove : bool, default=False
        remove almost empty directories

    Returns
    -------
    ans : int
        the number of removed directories
    """

    # Import standard modules ...
    import os

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

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
