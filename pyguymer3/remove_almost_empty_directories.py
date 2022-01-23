def remove_almost_empty_directories(path, kwArgCheck = None, debug = True, ignorableFiles = (".directory", ".DS_Store", "._.DS_Store", "Thumbs.db"), remove = False):
    """Remove directories which are almost empty.

    Parameters
    ----------
    path : str
        the directory to search
    debug : bool, optional
        print debug messages, default True
    ignorableFiles : tuple of str, optional
        the tuple of files which can safely be ignored, default (".directory", ".DS_Store", "._.DS_Store", "Thumbs.db")
    remove : bool, optional
        remove almost empty directories, default False
    """

    # Import standard modules ...
    import os

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize counter ...
    i = 0

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
        i += 1

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
