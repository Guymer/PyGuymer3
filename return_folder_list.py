def return_folder_list(path, debug = False, follow_symlinks = True):
    """
    Return a recursive list of folders in a directory.

    Arguments:
    path -- the directory to search

    Keyword Arguments:
    debug -- print debug messages (default False)
    follow_symlinks -- follow symbolic links (default True)
    """

    # Import standard modules ...
    import os

    # Load sub-functions ...
    from .make_path_safe import make_path_safe
    from .return_folder_list import return_folder_list

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
                    print("WARNING: \"{:s}\" is illegal".format(item))

            # Check if it might need searching ...
            if os.path.isdir(item):
                # Check that the directory is list-able ...
                if os.access(item, os.X_OK):
                    # Check if the directory is allowed to be followed ...
                    if follow_symlinks or not os.path.islink(item):
                        # Recursively run this function again and add to list ...
                        contents += return_folder_list(item, debug = debug, follow_symlinks = follow_symlinks)
                    elif debug:
                        print("WARNING: \"{:s}\" cannot be followed".format(item))
                elif debug:
                    print("WARNING: \"{:s}\" cannot be listed".format(item))

            # Check if it should be added to the list ...
            if os.path.isdir(item):
                # Add to list ...
                contents.append(item)
    elif debug:
        print("WARNING: \"{:s}\" does not exist".format(path))

    # Return sorted list ...
    contents.sort()
    return contents
