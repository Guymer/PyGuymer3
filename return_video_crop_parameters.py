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
    inW = return_video_width(fname, playlist)                                   # [px]
    inH = return_video_height(fname, playlist)                                  # [px]
    outX = 0                                                                    # [px]
    outY = 0                                                                    # [px]

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

            # Extract the information part of the line and make a dictionary of
            # all of the key+value pairs ...
            db = {}
            info = line.strip().split("]")[-1]
            for keyvalue in info.strip().split():
                if keyvalue.count(":") == 1:
                    key, value = keyvalue.split(":")
                elif keyvalue.count("=") == 1:
                    key, value = keyvalue.split("=")
                else:
                    raise Exception("an unexpected string format was encountered (\"{:s}\")".format(keyvalue))
                db[key] = value

            # Update variables ...
            outX = max(outX, int(db["x"]))                                      # [px]
            outY = max(outY, int(db["y"]))                                      # [px]

    # Update variables ...
    outW = inW - 2 * outX                                                       # [px]
    outH = inH - 2 * outY                                                       # [px]
    cropParams = "{:d}:{:d}:{:d}:{:d}".format(outW, outH, outX, outY)

    # Check results ...
    if outW > inW or outW <= 0:
        raise Exception("failed to find cropped width (inW = {:d}, inH = {:d}, outX = {:d}, outY = {:d}, outW = {:d}, outH = {:d})".format(inW, inH, outX, outY, outW, outH))
    if outH > inH or outH <= 0:
        raise Exception("failed to find cropped height (inW = {:d}, inH = {:d}, outX = {:d}, outY = {:d}, outW = {:d}, outH = {:d})".format(inW, inH, outX, outY, outW, outH))

    # Return top-left corner, width, height and FFMPEG crop parameter string ...
    return outX, outY, outW, outH, cropParams
