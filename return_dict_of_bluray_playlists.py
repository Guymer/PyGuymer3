def return_dict_of_bluray_playlists(dname, size_threshold = 1073741824, time_threshold = 60.0):
    # Import modules ...
    import glob
    import json
    import os
    import subprocess

    # Create empty dictionary ...
    ans = {}

    # Loop over playlists ...
    for fname in glob.glob(os.path.join(dname, "BDMV", "PLAYLIST", "*.mpls")):
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
                "-playlist", "{:d}".format(playlist),
                "bluray:{:s}".format(dname)
            ],
            encoding = "utf-8",
            stderr = subprocess.PIPE,
            stdout = subprocess.PIPE
        )
        stdout, stderr = proc.communicate()
        if proc.returncode != 0:
            print("WARNING: \"ffprobe\" command failed on playlist \"{:d}\" of \"{:s}\"".format(playlist, fname))
            continue

        # Append information if this playlist is worthwhile (by default,
        # "worthwhile" is defined as >=1 GiB and/or >=1 minute) ...
        info = json.loads(stdout)["format"]
        if "duration" in info:
            if float(info["duration"]) >= time_threshold:
                ans["{:d}".format(playlist)] = info
        if "size" in info:
            if int(info["size"]) >= size_threshold:
                ans["{:d}".format(playlist)] = info

    # Return dictionary ...
    return ans
