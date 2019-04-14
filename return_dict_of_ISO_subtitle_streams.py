# -*- coding: utf-8 -*-

def return_dict_of_ISO_subtitle_streams(fname, usr_track = -1):
    # Import modules ...
    import subprocess
    import xml.etree.ElementTree

    # Check input ...
    if usr_track == -1:
        raise Exception("no track was requested")

    # Find track info ...
    proc = subprocess.Popen(
        [
            "lsdvd",
            "-x",
            "-Ox",
            fname
        ],
        stderr = subprocess.PIPE,
        stdout = subprocess.PIPE
    )
    stdout, stderr = proc.communicate()
    if proc.returncode != 0:
        raise Exception("\"lsdvd\" command failed")

    # Clean up ...
    # NOTE: "lsdvd" sometimes returns invalid XML as it does not: escape characters; or remove invalid characters.
    stdout = str(stdout, "utf-8", "ignore").replace("&", "&amp;")

    # Loop over all tracks ...
    for track in xml.etree.ElementTree.fromstring(stdout).findall("track"):
        # Skip if this track is not the chosen one ...
        if int(track.find("ix").text) != int(usr_track):
            continue

        # Create empty dictionary ...
        ans = {}

        # Loop over all subtitle channels in this track ...
        for subp in track.findall("subp"):
            # Append information ...
            ans[subp.find("streamid").text] = {
                "content" : subp.find("content").text,
                "langcode" : subp.find("langcode").text,
                "language" : subp.find("language").text
            }

        # Return dictionary ...
        return ans
