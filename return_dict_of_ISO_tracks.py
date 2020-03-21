def return_dict_of_ISO_tracks(fname):
    # Import modules ...
    import subprocess
    import lxml.etree

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

    # Initialize dictionary ...
    ans = {}

    # Loop over all tracks ...
    for track in lxml.etree.fromstring(stdout).findall("track"):
        # Append information ...
        ans[track.find("ix").text] = {
            "length" : float(track.find("length").text)                         # [s]
        }

    # Return dictionary ...
    return ans
