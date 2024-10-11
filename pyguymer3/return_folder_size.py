#!/usr/bin/env python3

# Define function ...
def return_folder_size(
    path,
    /,
    *,
        allowHidden = False,
              debug = __debug__,
    follow_symlinks = True,
    return_symlinks = True,
):
    """Return the total size of all files in a directory.

    This function returns the total size of all files recursively in a directory.

    Parameters
    ----------
    path : str
        the directory to search
    allowHidden : bool, optional
        allow hidden files
    debug : bool, optional
        print debug messages
    follow_symlinks : bool, optional
        follow symbolic links
    return_symlinks : bool, optional
        include symbolic links in the returned list

    Returns
    -------
    ans : int
        the total size

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os

    # Import sub-functions ...
    from .make_path_safe import make_path_safe

    # Initialize total ...
    size = 0                                                                    # [B]

    # Check it exists ...
    if os.path.exists(path):
        # Open folder ...
        with os.scandir(path) as scanObj:
            # Loop over folder contents ...
            for entry in scanObj:
                # Check if the user wants to perform debugging ...
                if debug:
                    # Test if this part is hidden and print the full path for
                    # identification ...
                    if not allowHidden and entry.name.startswith("."):
                        print(f"WARNING: \"{entry.path}\" is hidden")

                    # Test if this part is illegal and print the full path for
                    # identification ...
                    if not entry.name.startswith(".") and entry.name != make_path_safe(entry.name, allowHidden = allowHidden):
                        print(f"WARNING: \"{entry.path}\" is illegal")

                # Check if it might need following ...
                if entry.is_dir(follow_symlinks = follow_symlinks):
                    # Check that the directory is list-able ...
                    # NOTE: On 20/Aug/2022 this was (incorrectly, in my opinion)
                    #       returning False on regular folders on FreeBSD (but
                    #       not MacOS) when passed "follow_symlinks = False".
                    if os.access(entry, os.X_OK):
                        # Recursively run this function again and add to the
                        # total ...
                        size += return_folder_size(
                            entry.path,
                                allowHidden = allowHidden,
                                      debug = debug,
                            follow_symlinks = follow_symlinks,
                            return_symlinks = return_symlinks,
                        )                                                       # [B]
                    elif debug:
                        print(f"WARNING: \"{entry.path}\" cannot be listed")

                # Check if it might need adding to the total ...
                if entry.is_file(follow_symlinks = return_symlinks):
                    # Add to the total ...
                    size += os.path.getsize(entry)                              # [B]
    elif debug:
        print(f"WARNING: \"{path}\" does not exist")

    # Return total ...
    return size
