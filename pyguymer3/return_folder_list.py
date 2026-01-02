#!/usr/bin/env python3

# Define function ...
def return_folder_list(
    path,
    /,
    *,
        allowHidden = False,
              debug = __debug__,
          ensureNFC = True,
    follow_symlinks = True,
          recursive = True,
    return_symlinks = True,
):
    """Return a recursive list of folder names in a directory.

    This function returns a sorted list of folder names recursively in a
    directory.

    Parameters
    ----------
    path : str
        the directory to search
    allowHidden : bool, optional
        allow hidden files
    debug : bool, optional
        print debug messages
    ensureNFC : bool, optional
        ensure that the Unicode encoding is NFC
    follow_symlinks : bool, optional
        follow symbolic links
    recursive : bool, optional
        recursively call this function on sub-folders
    return_symlinks : bool, optional
        include symbolic links in the returned list

    Returns
    -------
    ans : list of str
        the sorted list of folder names

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

    # Create empty list ...
    contents = []

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
                    if not entry.name.startswith(".") and entry.name != make_path_safe(entry.name, allowHidden = allowHidden, ensureNFC = ensureNFC):
                        print(f"WARNING: \"{entry.path}\" is illegal")

                # Check if it might need following ...
                if recursive and entry.is_dir(follow_symlinks = follow_symlinks):
                    # Check that the directory is list-able ...
                    # NOTE: On 20/Aug/2022 this was (incorrectly, in my opinion)
                    #       returning False on regular folders on FreeBSD (but
                    #       not MacOS) when passed "follow_symlinks = False".
                    if os.access(entry, os.X_OK):
                        # Recursively run this function again and add to the
                        # list ...
                        contents += return_folder_list(
                            entry.path,
                                allowHidden = allowHidden,
                                      debug = debug,
                                  ensureNFC = ensureNFC,
                            follow_symlinks = follow_symlinks,
                            return_symlinks = return_symlinks,
                                  recursive = recursive,
                        )
                    elif debug:
                        print(f"WARNING: \"{entry.path}\" cannot be listed")

                # Check if it should be added to the list ...
                if entry.is_dir(follow_symlinks = return_symlinks):
                    # Add to the list ...
                    contents.append(entry.path)
    elif debug:
        print(f"WARNING: \"{path}\" does not exist")

    # Return sorted list ...
    return sorted(contents)
