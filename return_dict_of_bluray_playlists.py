def return_dict_of_bluray_playlists(dname, threshold = 60.0):
    # Import modules ...
    import glob
    import json
    import os
    import subprocess

    # Create empty dictionary ...
    ans = {}

    # Loop over playlists ...
    for fname in glob.iglob(os.path.join(dname, "BDMV", "PLAYLIST", "*.mpls")):
        # Extract number ...
        playlist = int(os.path.basename(fname).split(".")[0])

        # Find format info ...
        proc = subprocess.Popen(
            [
                "ffprobe",
                "-loglevel", "quiet",
                "-probesize", "3G",
                "-analyzeduration", "1800M",
                "-print_format", "json",
                "-show_format",
                "-playlist", "{0:d}".format(playlist),
                "bluray:{0:s}".format(dname)
            ],
            stderr = subprocess.PIPE,
            stdout = subprocess.PIPE
        )
        stdout, stderr = proc.communicate()
        if proc.returncode != 0:
            print("WARNING: \"ffprobe\" command failed on playlist \"{0:d}\" of \"{1:s}\"".format(playlist, fname))
            continue

        # Append information if this playlist is worthwhile ...
        info = json.loads(stdout)["format"]
        if "duration" in info:
            if float(info["duration"]) >= threshold:
                ans["{0:d}".format(playlist)] = info

    # Return dictionary ...
    return ans
