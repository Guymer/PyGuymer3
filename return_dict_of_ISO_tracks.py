def return_dict_of_ISO_tracks(fname):
    # Import modules ...
    import shutil
    import subprocess
    import lxml.etree

    # Check that "lsdvd" is installed ...
    if shutil.which("lsdvd") is None:
        raise Exception("\"lsdvd\" is not installed")

    # Find track info ...
    stderrout = subprocess.check_output(
        [
            "lsdvd",
            "-x",
            "-Ox",
            fname
        ],
        encoding = "utf-8",
        stderr = subprocess.STDOUT
    )

    # Initialize dictionary ...
    ans = {}

    # Loop over all tracks ...
    for track in lxml.etree.fromstring(stderrout).findall("track"):
        # Append information ...
        ans[track.find("ix").text] = {
            "length" : float(track.find("length").text)                         # [s]
        }

    # Return dictionary ...
    return ans
