#!/usr/bin/env python3

# Define function ...
def perms(
    path,
    /,
    *,
        allowHidden = False,
              debug = __debug__,
          ensureNFC = True,
          filePerms = None,
        folderPerms = None,
    follow_symlinks = True,
              group = None,
     return_dsstore = True,
    return_symlinks = False,
              skips = None,
               user = None,
):
    """Set permissions within a path.

    This function sets the file/folder mode and group/user owner of all files
    and folders within the path.

    Parameters
    ----------
    path : str
        the path to set permissions within
    allowHidden : bool, optional
        allow hidden files
    debug : bool, optional
        print debug messages
    ensureNFC : bool, optional
        ensure that the Unicode encoding is NFC
    filePerms : int, default=None
        desired file mode (recommended to supply integer in octal to improve clarity)
    folderPerms : int, default=None
        desired folder mode (recommended to supply integer in octal to improve clarity)
    group : str, default=None
        desired group owner
    user : str, default=None
        desired user owner
    follow_symlinks : bool, optional
        follow symbolic links
    return_dsstore : bool, optional
        include ".DS_Store" files in the returned list
    return_symlinks : bool, optional
        set permissions on symbolic links
    skips : list of str, default=[]
        a list of strings which, if any are present in a file/folder name, result in the file/folder being skipped

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
    import stat

    # Import sub-functions ...
    from .return_file_list import return_file_list
    from .return_folder_list import return_folder_list
    from .stat import stat as myStat    # NOTE: To avoid name clash.

    # Populate default values ...
    if skips is None:
        skips = []

    # Fetch lists ...
    flist = return_file_list(
        path,
            allowHidden = allowHidden,
                  debug = debug,
              ensureNFC = ensureNFC,
        follow_symlinks = follow_symlinks,
         return_dsstore = return_dsstore,
        return_symlinks = return_symlinks,
    )
    dlist = return_folder_list(
        path,
            allowHidden = allowHidden,
                  debug = debug,
              ensureNFC = ensureNFC,
        follow_symlinks = follow_symlinks,
        return_symlinks = return_symlinks,
    )

    # Loop over files and folders ...
    for name in flist + dlist:
        # Skip if it needs to be skipped ...
        toSkip = False
        for skip in skips:
            if skip in name:
                toSkip = True
                break
        if toSkip:
            continue

        # Fetch information ...
        info = myStat(name, follow_symlinks = follow_symlinks)

        # Set group owner if it is wrong ...
        if group is not None:
            if info["group"] != group:
                if debug:
                    print(f'INFO: Changing group owner of \"{name}\" to \"{group}\" (it is \"{info["group"]}\").')
                shutil.chown(name, group = group)

        # Set user owner if it is wrong ...
        if user is not None:
            if info["user"] != user:
                if debug:
                    print(f'INFO: Changing user owner of \"{name}\" to \"{user}\" (it is \"{info["user"]}\").')
                shutil.chown(name, user = user)

        # Set file permissions if it is wrong ...
        if stat.S_ISREG(info["st_mode"]):
            if filePerms is not None:
                if stat.S_IMODE(info["st_mode"]) != filePerms:
                    if debug:
                        print(f'INFO: Changing file permissions of \"{name}\" to \"{oct(filePerms)}\" (it is \"{oct(stat.S_IMODE(info["st_mode"]))}\").')
                    os.chmod(
                        name,
                        filePerms,
                        follow_symlinks = follow_symlinks,
                    )

        # Set folder permissions if it is wrong ...
        if stat.S_ISDIR(info["st_mode"]):
            if folderPerms is not None:
                if stat.S_IMODE(info["st_mode"]) != folderPerms:
                    if debug:
                        print(f'INFO: Changing folder permissions of \"{name}\" to \"{oct(folderPerms)}\" (it is \"{oct(stat.S_IMODE(info["st_mode"]))}\").')
                    os.chmod(
                        name,
                        folderPerms,
                        follow_symlinks = follow_symlinks,
                    )
