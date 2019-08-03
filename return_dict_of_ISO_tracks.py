def return_dict_of_ISO_tracks(fname):
    # Import modules ...
    import subprocess
    import xml.etree.ElementTree

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

    # Clean up ...
    # NOTE: "lsdvd" sometimes returns invalid XML as it does not:
    #         * escape characters; or
    #         * remove invalid characters.
    stdout = stdout.replace("&", "&amp;")

    # Create empty dictionary ...
    ans = {}

    # Loop over all tracks ...
    for track in xml.etree.ElementTree.fromstring(stdout).findall("track"):
        # Append information ...
        ans[track.find("ix").text] = {
            "length" : float(track.find("length").text)                       # [s]
        }

    # Return dictionary ...
    return ans
