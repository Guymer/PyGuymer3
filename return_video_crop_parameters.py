def return_video_crop_parameters(fname, playlist = -1, dt = 2.0):
    # Import standard modules ...
    import shutil
    import subprocess

    # Load sub-functions ...
    from .return_media_duration import return_media_duration
    from .return_video_height import return_video_height
    from .return_video_width import return_video_width

    # Check input ...
    if fname.startswith("bluray:") and playlist < 0:
        raise Exception("a Blu-ray was specified but no playlist was supplied")

    # Check that "ffmpeg" is installed ...
    if shutil.which("ffmpeg") is None:
        raise Exception("\"ffmpeg\" is not installed")

    # Initialize variables ...
    dur = return_media_duration(fname, playlist)                                # [s]
    w = return_video_width(fname, playlist)                                     # [px]
    h = return_video_height(fname, playlist)                                    # [px]
    x1 = w                                                                      # [px]
    x2 = 0                                                                      # [px]
    y1 = h                                                                      # [px]
    y2 = 0                                                                      # [px]

    # Loop over fractions ...
    for frac in [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8]:
        # Deduce start time ...
        t = frac * dur - dt / 2.0                                               # [s]

        # Find crop parameters ...
        if fname.startswith("bluray:"):
            stderrout = subprocess.check_output(
                [
                    "ffmpeg",
                    "-hide_banner",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-playlist", "{0:d}".format(playlist),
                    "-ss", "{0:.3f}".format(t),
                    "-i", fname,
                    "-an",
                    "-sn",
                    "-t", "{:f}".format(dt),
                    "-vf", "cropdetect",
                    "-y",
                    "-f", "null",
                    "/dev/null"
                ],
                encoding = "utf-8",
                stderr = subprocess.STDOUT
            )
        else:
            # Try to analyze it properly first, if it fails then attempt to
            # load it as a raw M-JPEG stream ...
            try:
                stderrout = subprocess.check_output(
                    [
                        "ffmpeg",
                        "-hide_banner",
                        "-probesize", "3G",
                        "-analyzeduration", "1800M",
                        "-ss", "{0:.3f}".format(t),
                        "-i", fname,
                        "-an",
                        "-sn",
                        "-t", "{:f}".format(dt),
                        "-vf", "cropdetect",
                        "-y",
                        "-f", "null",
                        "/dev/null"
                    ],
                    encoding = "utf-8",
                    stderr = subprocess.STDOUT
                )
            except:
                stderrout = subprocess.check_output(
                    [
                        "ffmpeg",
                        "-hide_banner",
                        "-probesize", "3G",
                        "-analyzeduration", "1800M",
                        "-ss", "{0:.3f}".format(t),
                        "-f", "mjpeg",
                        "-i", fname,
                        "-an",
                        "-sn",
                        "-t", "{:f}".format(dt),
                        "-vf", "cropdetect",
                        "-y",
                        "-f", "null",
                        "/dev/null"
                    ],
                    encoding = "utf-8",
                    stderr = subprocess.STDOUT
                )

        # Loop over lines ...
        for line in stderrout.splitlines():
            # Skip irrelevant lines ...
            if not line.startswith("[Parsed_cropdetect"):
                continue

            # Extract information and loop over key+value pairs ...
            info = line.strip().split("]")[-1]
            for keyvalue in info.split():
                # Skip irrelevant key+value pairs ...
                if keyvalue.count(":") != 1:
                    continue

                # Extract key and value and update variables ...
                key, value = keyvalue.split(":")
                if key == "x1":
                    x1 = min(x1, int(value))                                    # [px]
                if key == "x2":
                    x2 = max(x2, int(value))                                    # [px]
                if key == "y1":
                    y1 = min(y1, int(value))                                    # [px]
                if key == "y2":
                    y2 = max(y2, int(value))                                    # [px]

    # Check results ...
    if x1 >= x2:
        raise Exception("failed to find cropped width (x1 = {:d}, y1 = {:d}, x2 = {:d}, y2 = {:d})".format(x1, y1, x2, y2))
    if y1 >= y2:
        raise Exception("failed to find cropped height (x1 = {:d}, y1 = {:d}, x2 = {:d}, y2 = {:d})".format(x1, y1, x2, y2))

    # Return top-left corner, bottom-right corner, width, height and FFMPEG crop
    # parameter string ...
    return x1, y1, x2, y2, x2 - x1 + 1, y2 - y1 + 1, "{:d}:{:d}:{:d}:{:d}".format(w, h, x1 - 1, y1 - 1)
