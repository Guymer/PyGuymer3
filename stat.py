# NOTE: See https://stackoverflow.com/a/58684090

def stat(fname):
    """
    This function aims to mimic os.stat() but instead of returning a stat_result
    object it returns a dictionary.
    """

    # Import standard modules ...
    import os

    # Stat the file ...
    info = os.stat(fname)

    # Create a dictionary from the stat_result object ...
    ans = {}
    for key in dir(info):
        if key.startswith("st_"):
            ans[key] = getattr(info, key)

    # Return answer ...
    return ans
