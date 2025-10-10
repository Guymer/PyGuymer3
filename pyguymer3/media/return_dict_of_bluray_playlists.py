#!/usr/bin/env python3

# Define function ...
def return_dict_of_bluray_playlists(
    dname,
    /,
    *,
         cacheDir = "~/.cache/pyguymer3",
            debug = __debug__,
        ensureNFC = True,
      ffprobePath = None,
    sizeThreshold = 1073741824,
          timeout = 60.0,
    timeThreshold = 60.0,
):
    """
    This function uses the list of MPLS files to obtain all of the possible
    playlists in a Blu-ray, then it calls "ffprobe" on each integer to determine
    the duration and size.
    """


    # Import standard modules ...
    import glob
    import os

    # Import global (subclassed) dictionary ...
    from .__ffprobe__ import __ffprobe__

    # **************************************************************************

    # Configure global (subclassed) dictionary ...
    # NOTE: If I blindly set "__ffprobe__.ffprobePath" to "ffprobePath" each
    #       time then I would clobber any previous calls to "shutil.which()"
    #       performed by the global (subclassed) dictionary itself.
    __ffprobe__.cacheDir = cacheDir
    __ffprobe__.debug = debug
    __ffprobe__.ensureNFC = ensureNFC
    if ffprobePath is not None:
        __ffprobe__.ffprobePath = ffprobePath
    __ffprobe__.timeout = timeout                                               # [s]

    # **************************************************************************

    # Create short-hand ...
    fname = f"bluray:{dname}"

    # Initialize dictionary ...
    ans = {}

    # Loop over playlist files ...
    for mpls in glob.glob(f"{dname}/BDMV/PLAYLIST/*.mpls"):
        # Extract playlist number ...
        playlist = int(os.path.basename(mpls).removesuffix(".mpls"))

        # Append information if this playlist is worthwhile (by default,
        # "worthwhile" is defined as ≥1 GiB and/or ≥1 minute) ...
        if "duration" in __ffprobe__[f"{fname}:{playlist:d}"]["format"]:
            if float(__ffprobe__[f"{fname}:{playlist:d}"]["format"]["duration"]) >= timeThreshold:
                ans[f"{playlist:d}"] = __ffprobe__[f"{fname}:{playlist:d}"]["format"]
                continue
        if "size" in __ffprobe__[f"{fname}:{playlist:d}"]["format"]:
            if int(__ffprobe__[f"{fname}:{playlist:d}"]["format"]["size"]) >= sizeThreshold:
                ans[f"{playlist:d}"] = __ffprobe__[f"{fname}:{playlist:d}"]["format"]
                continue

    # Return dictionary ...
    return ans
