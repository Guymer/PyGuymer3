def return_folder_size(path, kwArgCheck = None, debug = False, follow_symlinks = True, return_symlinks = True):
    """Return the total size of all files in a directory.

    This function returns the total size of all files recursively in a directory.

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
    ans : int
        the total size

    Notes
    -----
    Copyright 2018 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os

    # Import sub-functions ...
    from .make_path_safe import make_path_safe

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize total ...
    size = 0                                                                    # [B]

    # Check it exists ...
    if os.path.exists(path):
        # Loop over folder contents ...
        for entry in os.scandir(path):
            # Check if the user wants to perform debugging ...
            if debug:
                # Test if this part is illegal and print the full path for
                # identification ...
                if not entry.name.startswith(".") and entry.name != make_path_safe(entry.name):
                    print(f"WARNING: \"{entry.path}\" is illegal")

            # Check if it might need following ...
            if entry.is_dir(follow_symlinks = follow_symlinks):
                # Check that the directory is list-able ...
                if os.access(entry, os.X_OK, follow_symlinks = follow_symlinks):
                    # Recursively run this function again and add to the total ...
                    size += return_folder_size(entry.path, debug = debug, follow_symlinks = follow_symlinks, return_symlinks = return_symlinks)
                elif debug:
                    print(f"WARNING: \"{entry.path}\" cannot be listed")

            # Check if it might need adding to the total ...
            if entry.is_file(follow_symlinks = return_symlinks):
                # Add to the total ...
                size += os.path.getsize(entry)                                  # [B]
    elif debug:
        print(f"WARNING: \"{path}\" does not exist")

    # Return total ...
    return size
