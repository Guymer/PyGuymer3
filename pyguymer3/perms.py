def perms(path, kwArgCheck = None, debug = False, filePerms = None, folderPerms = None, group = None, user = None, follow_symlinks = True, return_symlinks = False):
    """Set permissions within a path.

    This function sets the file/folder mode and group/user owner of all files
    and folders within the path.

    Parameters
    ----------
    path : str
        the path to set permissions within
    debug : bool, default=False
        print debug messages
    filePerms : int, default=None
        desired file mode (recommended to supply integer in octal to improve clarity)
    folderPerms : int, default=None
        desired folder mode (recommended to supply integer in octal to improve clarity)
    group : str, default=None
        desired group owner
    user : str, default=None
        desired user owner
    follow_symlinks : bool, default=True
        follow symbolic links
    return_symlinks : bool, default=False
        set permissions on symbolic links
    """

    # Import standard modules ...
    import os
    import shutil
    import stat

    # Import sub-functions ...
    from .return_file_list import return_file_list
    from .return_folder_list import return_folder_list
    from .stat import stat as myStat    # NOTE: To avoid name clash.

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Fetch lists ...
    flist = return_file_list(path, follow_symlinks = follow_symlinks, return_symlinks = return_symlinks)
    dlist = return_folder_list(path, follow_symlinks = follow_symlinks, return_symlinks = return_symlinks)

    # Loop over files and folders ...
    for name in flist + dlist:
        # Fetch information ...
        info = myStat(name)

        # Set group owner if it is wrong ...
        if group is not None:
            if info["group"] != group:
                if debug:
                    print(f'INFO: Changing group owner of \"{name}\" to \"{group}\" (is is \"{info["group"]}\").')
                shutil.chown(name, group = group)

        # Set user owner if it is wrong ...
        if user is not None:
            if info["user"] != user:
                if debug:
                    print(f'INFO: Changing user owner of \"{name}\" to \"{user}\" (is is \"{info["user"]}\").')
                shutil.chown(name, user = user)

        # Set file permissions if it is wrong ...
        if stat.S_ISREG(info["st_mode"]):
            if filePerms is not None:
                if stat.S_IMODE(info["st_mode"]) != filePerms:
                    if debug:
                        print(f'INFO: Changing file permissions of \"{name}\" to \"{oct(filePerms)}\" (is is \"{oct(stat.S_IMODE(info["st_mode"]))}\").')
                    os.chmod(name, filePerms)

        # Set folder permissions if it is wrong ...
        if stat.S_ISDIR(info["st_mode"]):
            if folderPerms is not None:
                if stat.S_IMODE(info["st_mode"]) != folderPerms:
                    if debug:
                        print(f'INFO: Changing folder permissions of \"{name}\" to \"{oct(folderPerms)}\" (is is \"{oct(stat.S_IMODE(info["st_mode"]))}\").')
                    os.chmod(name, folderPerms)
