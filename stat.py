# NOTE: https://stackoverflow.com/a/58684090

def stat(fname, follow_symlinks = True):
    """
    This function aims to mimic os.stat() but instead of returning a stat_result
    object with attributes it returns a dictionary with keys (for more user-
    friendly serialization to JSON).
    """

    # Import standard modules ...
    import os

    # Stat the file ...
    info = os.stat(fname, follow_symlinks = follow_symlinks)

    # Create a dictionary from the stat_result object ...
    ans = {}
    for key in dir(info):
        if key.startswith("st_"):
            ans[key] = getattr(info, key)

    # Return answer ...
    return ans
