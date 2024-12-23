#!/usr/bin/env python3

# Define function ...
def return_dict_of_bluray_playlists(
    dname,
    /,
    *,
              cwd = None,
            debug = __debug__,
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
    import shutil

    # Import sub-functions ...
    from .__ffprobe__ import __ffprobe__
    from .ffprobe import ffprobe

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if ffprobePath is None:
        ffprobePath = shutil.which("ffprobe")
    assert ffprobePath is not None, "\"ffprobe\" is not installed"

    # **************************************************************************

    # Create short-hand ...
    fname = f"bluray:{dname}"

    # Initialize dictionary ...
    ans = {}

    # Loop over playlist files ...
    for mpls in glob.glob(f"{dname}/BDMV/PLAYLIST/*.mpls"):
        # Extract playlist number ...
        playlist = int(os.path.basename(mpls).removesuffix(".mpls"))

        # Make sure that this fname/playlist combination is in the global
        # dictionary ...
        if fname not in __ffprobe__:
            __ffprobe__[fname] = {}
        if playlist not in __ffprobe__[fname]:
            if debug:
                print(f"INFO: Running ffprobe(\"{fname}\", {playlist:d}) ...")
            __ffprobe__[fname][playlist] = ffprobe(
            fname,
                    cwd = cwd,
            ffprobePath = ffprobePath,
               playlist = playlist,
                timeout = timeout,
        )

        # Append information if this playlist is worthwhile (by default,
        # "worthwhile" is defined as ≥1 GiB and/or ≥1 minute) ...
        if "duration" in __ffprobe__[fname][playlist]["format"]:
            if float(__ffprobe__[fname][playlist]["format"]["duration"]) >= timeThreshold:
                ans[f"{playlist:d}"] = __ffprobe__[fname][playlist]["format"]
                continue
        if "size" in __ffprobe__[fname][playlist]["format"]:
            if int(__ffprobe__[fname][playlist]["format"]["size"]) >= sizeThreshold:
                ans[f"{playlist:d}"] = __ffprobe__[fname][playlist]["format"]
                continue

    # Return dictionary ...
    return ans
