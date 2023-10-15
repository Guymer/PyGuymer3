#!/usr/bin/env python3

# Define function ...
def return_dict_of_bluray_playlists(dname, /, *, cwd = None, debug = False, size_threshold = 1073741824, time_threshold = 60.0, timeout = 60.0):
    # Import standard modules ...
    import glob
    import os

    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe

    # Create short-hand ...
    fname = f"bluray:{dname}"

    # Initialize dictionary ...
    ans = {}

    # Loop over playlist files ...
    for mpls in glob.glob(f"{dname}/BDMV/PLAYLIST/*.mpls"):
        # Extract playlist number ...
        playlist = int(os.path.splitext(os.path.basename(mpls))[0])

        # Make sure that this fname/playlist combination is in the global
        # dictionary ...
        if fname not in __ffprobe__:
            __ffprobe__[fname] = {}
        if playlist not in __ffprobe__[fname]:
            if debug:
                print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
            __ffprobe__[fname][playlist] = ffprobe(fname, cwd = cwd, playlist = playlist, timeout = timeout)

        # Append information if this playlist is worthwhile (by default,
        # "worthwhile" is defined as ≥1 GiB and/or ≥1 minute) ...
        if "duration" in __ffprobe__[fname][playlist]["format"]:
            if float(__ffprobe__[fname][playlist]["format"]["duration"]) >= time_threshold:
                ans[f"{playlist:d}"] = __ffprobe__[fname][playlist]["format"]
        if "size" in __ffprobe__[fname][playlist]["format"]:
            if int(__ffprobe__[fname][playlist]["format"]["size"]) >= size_threshold:
                ans[f"{playlist:d}"] = __ffprobe__[fname][playlist]["format"]

    # Return dictionary ...
    return ans
