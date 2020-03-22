def return_dict_of_bluray_playlists(dname, size_threshold = 1073741824, time_threshold = 60.0):
    # Import modules ...
    import glob
    import os

    # Load sub-functions ...
    from . import __ffprobe__
    from .ffprobe import ffprobe

    # Create short-hand ...
    fname = "bluray:{:s}".format(dname)

    # Initialize dictionary ...
    ans = {}

    # Loop over playlist files ...
    for mpls in glob.glob(os.path.join(dname, "BDMV", "PLAYLIST", "*.mpls")):
        # Extract playlist number ...
        playlist = int(os.path.basename(mpls).split(".")[0])

        # Make sure that this fname/playlist combination is in the global
        # dictionary ...
        if fname not in __ffprobe__:
            __ffprobe__[fname] = {}
        if playlist not in __ffprobe__[fname]:
            __ffprobe__[fname][playlist] = ffprobe(fname, playlist)

        # Append information if this playlist is worthwhile (by default,
        # "worthwhile" is defined as >=1 GiB and/or >=1 minute) ...
        if "duration" in __ffprobe__[fname][playlist]["format"]:
            if float(__ffprobe__[fname][playlist]["format"]["duration"]) >= time_threshold:
                ans["{:d}".format(playlist)] = __ffprobe__[fname][playlist]["format"]
        if "size" in __ffprobe__[fname][playlist]["format"]:
            if int(__ffprobe__[fname][playlist]["format"]["size"]) >= size_threshold:
                ans["{:d}".format(playlist)] = __ffprobe__[fname][playlist]["format"]

    # Return dictionary ...
    return ans
