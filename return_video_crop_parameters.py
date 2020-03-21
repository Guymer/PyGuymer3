def return_video_crop_parameters(fname, playlist = -1):
    # Import modules ...
    import subprocess

    # Load sub-functions ...
    from .return_media_duration import return_media_duration
    from .return_video_height import return_video_height
    from .return_video_width import return_video_width

    # Check input ...
    if fname.startswith("bluray:") and playlist < 0:
        raise Exception("a Blu-ray was specified but no playlist was supplied")

    # Initialize variables ...
    dur = return_media_duration(fname, playlist)                                # [s]
    w = return_video_width(fname, playlist)                                     # [px]
    h = return_video_height(fname, playlist)                                    # [px]
    x1 = w                                                                      # [px]
    x2 = 0                                                                      # [px]
    y1 = h                                                                      # [px]
    y2 = 0                                                                      # [px]

    # Loop over times ...
    for i in range(7):
        # Deduce time ...
        t = 0.1 * float(i + 2) * dur - 1.0                                      # [s]

        # Find crop parameters ...
        if fname.startswith("bluray:"):
            proc = subprocess.Popen(
                [
                    "ffmpeg",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-playlist", "{0:d}".format(playlist),
                    "-ss", "{0:.3f}".format(t),
                    "-i", fname,
                    "-an",
                    "-sn",
                    "-t", "2.0",
                    "-vf", "cropdetect",
                    "-y",
                    "-f", "null",
                    "/dev/null"
                ],
                encoding = "utf-8",
                stderr = subprocess.PIPE,
                stdout = subprocess.PIPE
            )
            stdout, stderr = proc.communicate()
            if proc.returncode != 0:
                raise Exception("\"ffmpeg\" command failed")
        else:
            proc = subprocess.Popen(
                [
                    "ffmpeg",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-ss", "{0:.3f}".format(t),
                    "-i", fname,
                    "-an",
                    "-sn",
                    "-t", "2.0",
                    "-vf", "cropdetect",
                    "-y",
                    "-f", "null",
                    "/dev/null"
                ],
                encoding = "utf-8",
                stderr = subprocess.PIPE,
                stdout = subprocess.PIPE
            )
            stdout, stderr = proc.communicate()
            if proc.returncode != 0:
                # HACK: Fallback and attempt to load it as a raw M-JPEG stream.
                proc = subprocess.Popen(
                    [
                        "ffmpeg",
                        "-probesize", "3G",
                        "-analyzeduration", "1800M",
                        "-ss", "{0:.3f}".format(t),
                        "-f", "mjpeg",
                        "-i", fname,
                        "-an",
                        "-sn",
                        "-t", "2.0",
                        "-vf", "cropdetect",
                        "-y",
                        "-f", "null",
                        "/dev/null"
                    ],
                    encoding = "utf-8",
                    stderr = subprocess.PIPE,
                    stdout = subprocess.PIPE
                )
                stdout, stderr = proc.communicate()
                if proc.returncode != 0:
                    raise Exception("\"ffmpeg\" command failed")

        # Loop over lines ...
        for line in stderr.split("\n"):
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
    #if x1 >= x2:
        #raise Exception(u"failed to find cropped width")
    #if y1 >= y2:
        #raise Exception(u"failed to find cropped height")

    # Return largest extent (and cropped width and height) ...
    return x1, y1, x2, y2, x2 - x1 + 1, y2 - y1 + 1
