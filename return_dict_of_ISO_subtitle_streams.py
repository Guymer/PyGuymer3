def return_dict_of_ISO_subtitle_streams(fname, usr_track = -1):
    # Import modules ...
    import shutil
    import subprocess
    import lxml.etree

    # Check input ...
    if usr_track == -1:
        raise Exception("no track was requested")

    # Check that "lsdvd" is installed ...
    if shutil.which("lsdvd") is None:
        raise Exception("\"lsdvd\" is not installed")

    # Find track info ...
    proc = subprocess.Popen(
        [
            "lsdvd",
            "-x",
            "-Ox",
            fname
        ],
        encoding = "utf-8",
        stderr = subprocess.PIPE,
        stdout = subprocess.PIPE
    )
    stdout, stderr = proc.communicate()
    if proc.returncode != 0:
        raise Exception("\"lsdvd\" command failed")

    # Loop over all tracks ...
    for track in lxml.etree.fromstring(stdout).findall("track"):
        # Skip if this track is not the chosen one ...
        if int(track.find("ix").text) != int(usr_track):
            continue

        # Initialize dictionary ...
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
