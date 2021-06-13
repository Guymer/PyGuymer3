def return_folder_size(path, kwArgCheck = None, debug = False, follow_symlinks = True, return_symlinks = True):
    """
    Return the total size of all files in a directory.

    Arguments:
    path -- the directory to search

    Keyword Arguments:
    debug -- print debug messages (default False)
    follow_symlinks -- follow symbolic links (default True)
    return_symlinks -- include symbolic links in the returned list (default True)
    """

    # Import standard modules ...
    import os

    # Load sub-functions ...
    from .make_path_safe import make_path_safe
    from .return_folder_size import return_folder_size

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize total ...
    size = 0                                                                    # [B]

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
                    print("WARNING: \"{:s}\" is illegal".format(item))

            # Check if it might need searching ...
            if os.path.isdir(item):
                # Check that the directory is list-able ...
                if os.access(item, os.X_OK):
                    # Check if the directory is allowed to be followed ...
                    if follow_symlinks or not os.path.islink(item):
                        # Recursively run this function again and increment
                        # total ...
                        size += return_folder_size(item, debug = debug, follow_symlinks = follow_symlinks, return_symlinks = return_symlinks)   # [#]
                    elif debug:
                        print("WARNING: \"{:s}\" cannot be followed".format(item))
                elif debug:
                    print("WARNING: \"{:s}\" cannot be listed".format(item))

            # Check if it should be added to the list ...
            if os.path.isfile(item):
                # Check if it is allowed to be added to the list ...
                if return_symlinks or not os.path.islink(item):
                    # Increment total ...
                    size += os.path.getsize(item)                               # [B]
    elif debug:
        print("WARNING: \"{:s}\" does not exist".format(path))

    # Return total ...
    return size
