#!/usr/bin/env python3

# Define function ...
def return_dict_of_bluray_playlistsToStreams(dname, /):
    # Import standard modules ...
    import glob
    import os

    # Import sub-functions ...
    from .parse_MPLS_file import parse_MPLS_file

    # Initialize dictionary ...
    playlist2streams = {}

    # Loop over playlist files ...
    for mpls in glob.glob(f"{dname}/BDMV/PLAYLIST/*.mpls"):
        # Create short-hand ...
        iPlaylist = int(os.path.basename(mpls).removesuffix(".mpls"))

        # Initialize list and parse the playlist file ...
        playlist2streams[iPlaylist] = []
        info = parse_MPLS_file(dname, iPlaylist)

        # Loop over playlist items ...
        for playItem in info["PlayList"]["PlayItems"]:
            iStream = int(playItem["ClipInformationFileName"])
            playlist2streams[iPlaylist].append(iStream)

    # Return answer ...
    return playlist2streams
