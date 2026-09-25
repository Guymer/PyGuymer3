#!/usr/bin/env python3

# Define function ...
def stat(
    fname,
    /,
    *,
    follow_symlinks = True,
):
    """
    This function aims to mimic os.stat() but instead of returning a stat_result
    object with attributes it returns a dictionary with keys (for more user-
    friendly serialization to JSON). Influenced by an answer on StackOverflow
    [1]_.

    References
    ----------
    .. [1] Stackoverflow answer, https://stackoverflow.com/a/58684090
    """

    # Import standard modules ...
    import grp
    import os
    import pwd
    import stat

    # Stat the file ...
    info = os.stat(
        fname,
        follow_symlinks = follow_symlinks,
    )

    # Create a dictionary from the stat_result object ...
    ans = {}
    for key in dir(info):
        if key.startswith("st_"):
            ans[key] = getattr(info, key)

    # Add helpful short-hands for user, group and mode information ...
    if "st_uid" in ans:
        ans["user"] = pwd.getpwuid(ans["st_uid"]).pw_name
    if "st_gid" in ans:
        ans["group"] = grp.getgrgid(ans["st_gid"]).gr_name
    if "st_mode" in ans:
        ans["mode"] = stat.filemode(ans["st_mode"])

    # Return answer ...
    return ans
