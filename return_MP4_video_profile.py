def return_MP4_video_profile(fname):
    # Import modules ...
    import json
    import subprocess

    # Find stream info ...
    proc = subprocess.Popen(
        [
            "ffprobe",
            "-loglevel", "quiet",
            "-probesize", "3G",
            "-analyzeduration", "1800M",
            "-print_format", "json",
            "-show_streams",
            fname
        ],
        stderr = subprocess.PIPE,
        stdout = subprocess.PIPE
    )
    stdout, stderr = proc.communicate()
    if proc.returncode != 0:
        raise Exception("\"ffprobe\" command failed")

    # Loop over streams ...
    for stream in json.loads(stdout)["streams"]:
        # Skip stream if it is not video ...
        if stream["codec_type"].strip().lower() != "video":
            continue

        # Skip stream if it is not H.264 video ...
        if stream["codec_name"].strip().upper() != "H264":
            continue

        # Return profile ...
        return stream["profile"]

    # Return error ...
    return "ERROR"
