#!/usr/bin/env python3

# Define function ...
def return_dict_of_bluray_playlistsToStreams(dname, /, *, time_threshold = 60.0):
    """
    This function uses the list of MPLS files to obtain all of the possible
    playlists in a Blu-ray, then it calls "parse_MPLS_file" on each file to
    determine the duration.
    """

    # Import standard modules ...
    import glob
    import os

    # Import sub-functions ...
    from .parse_MPLS_file import parse_MPLS_file

    # Initialize dictionary ...
    ans = {}

    # Loop over playlist files ...
    for mpls in glob.glob(f"{dname}/BDMV/PLAYLIST/*.mpls"):
        # Create short-hand ...
        iPlaylist = int(os.path.basename(mpls).removesuffix(".mpls"))

        # Initialize list and parse the playlist file ...
        ans[f"{iPlaylist:d}"] = []
        info = parse_MPLS_file(dname, iPlaylist)

        # Loop over playlist items ...
        for playItem in info["PlayList"]["PlayItems"]:
            # Populate dictionary ...
            # NOTE: According to https://github.com/lw/BluRay/wiki/PlayItem, the
            #       "times" are in "number of samples" assuming a frequency of
            #       45 kHz.
            ans[f"{iPlaylist:d}"].append(
                (
                    f'{int(playItem["ClipInformationFileName"]):d}',
                    float(playItem["OUTTime"] - playItem["INTime"]) / 4.5e4,    # [s],
                )
            )

    # Loop over playlists ...
    for iPlaylist in list(ans.keys()):
        # Initialize total duration ...
        totDuration = 0.0                                                       # [s]

        # Loop over streams ...
        for (_, duration) in ans[iPlaylist]:
            # Increment total duration ...
            totDuration += duration                                             # [s]

        # Delete information if this playlist is not worthwhile (by default,
        # "worthwhile" is defined as ≥1 minute) ...
        if totDuration < time_threshold:
            del ans[iPlaylist]
            continue

    # Return answer ...
    return ans
