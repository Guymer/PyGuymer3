def return_file_list(path, kwArgCheck = None, debug = False, follow_symlinks = True, return_symlinks = True):
    """Return a recursive list of file names in a directory.

    This function returns a sorted list of file names recursively in a directory.

    Parameters
    ----------
    path : str
        the directory to search
    debug : bool, default=False
        print debug messages
    follow_symlinks : bool, default=True
        follow symbolic links
    return_symlinks : bool, default=True
        include symbolic links in the returned list

    Returns
    -------
    ans : list of str
            the sorted list of file names
    """

    # Import standard modules ...
    import os

    # Import sub-functions ...
    from .make_path_safe import make_path_safe

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Create empty list ...
    contents = []

    # Check it exists ...
    if os.path.exists(path):
        # Loop over folder contents ...
        for child in os.listdir(path):
            # Make file name ...
            item = os.path.join(path, child)

            # Check if the user wants to perform debugging ...
            if debug:
                # Test if this part is illegal and print the full path for
                # identification ...
                if not child.startswith(".") and child != make_path_safe(child):
                    print(f"WARNING: \"{item}\" is illegal")

            # Check if it might need searching ...
            if os.path.isdir(item):
                # Check that the directory is list-able ...
                if os.access(item, os.X_OK):
                    # Check if the directory is allowed to be followed ...
                    if follow_symlinks or not os.path.islink(item):
                        # Recursively run this function again and add to list ...
                        contents += return_file_list(item, debug = debug, follow_symlinks = follow_symlinks, return_symlinks = return_symlinks)
                    elif debug:
                        print(f"WARNING: \"{item}\" cannot be followed")
                elif debug:
                    print(f"WARNING: \"{item}\" cannot be listed")

            # Check if it should be added to the list ...
            if os.path.isfile(item):
                # Check if it is allowed to be added to the list ...
                if return_symlinks or not os.path.islink(item):
                    # Add to list ...
                    contents.append(item)
    elif debug:
        print(f"WARNING: \"{path}\" does not exist")

    # Return sorted list ...
    return sorted(contents)
