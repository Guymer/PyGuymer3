def return_subtitle_extent(fname, kwArgCheck = None, playlist = -1, subtitle = 0):
    # Import standard modules ...
    import re
    import shutil
    import subprocess

    # Import sub-functions ...
    from .return_media_duration import return_media_duration
    from .return_video_frame_rate import return_video_frame_rate
    from .return_video_height import return_video_height
    from .return_video_width import return_video_width

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check input ...
    if fname.startswith("bluray:") and playlist < 0:
        raise Exception("a Blu-ray was specified but no playlist was supplied") from None

    # Check that "ffmpeg" is installed ...
    if shutil.which("ffmpeg") is None:
        raise Exception("\"ffmpeg\" is not installed") from None

    # Find out information about video ...
    duration = return_media_duration(fname, playlist = playlist)                # [s]
    fps = return_video_frame_rate(fname, playlist = playlist)                   # [Hz]
    height = return_video_height(fname, playlist = playlist)                    # [px]
    width = return_video_width(fname, playlist = playlist)                      # [px]

    # Find stream info ...
    if fname.startswith("bluray:"):
        stderrout = subprocess.check_output(
            [
                "ffmpeg",
                "-hide_banner",
                "-probesize", "3G",
                "-analyzeduration", "1800M",
                "-f", "lavfi",
                "-i", "color=color=black:size={0:d}x{1:d}:rate={2:f}:duration={3:f},format=yuv420p".format(width, height, fps, duration),
                "-playlist", "{0:d}".format(playlist),
                "-i", fname,
                "-filter_complex", "[0:v:0][1:s:{0:d}]overlay,cropdetect".format(subtitle),
                "-an",
                "-sn",
                "-vn",
                "-y",
                "-f", "null",
                "/dev/null"
            ],
            encoding = "utf-8",
            stderr = subprocess.STDOUT
        )
    else:
        # Try to analyze it properly first, if it fails then attempt to load it
        # as a raw M-JPEG stream ...
        try:
            stderrout = subprocess.check_output(
                [
                    "ffmpeg",
                    "-hide_banner",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-f", "lavfi",
                    "-i", "color=color=black:size={0:d}x{1:d}:rate={2:f}:duration={3:f},format=yuv420p".format(width, height, fps, duration),
                    "-i", fname,
                    "-filter_complex", "[0:v:0][1:s:{0:d}]overlay,cropdetect".format(subtitle),
                    "-an",
                    "-sn",
                    "-vn",
                    "-y",
                    "-f", "null",
                    "/dev/null"
                ],
                encoding = "utf-8",
                  stderr = subprocess.STDOUT,
            )
        except:
            stderrout = subprocess.check_output(
                [
                    "ffmpeg",
                    "-hide_banner",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-f", "lavfi",
                    "-i", "color=color=black:size={0:d}x{1:d}:rate={2:f}:duration={3:f},format=yuv420p".format(width, height, fps, duration),
                    "-f", "mjpeg",
                    "-i", fname,
                    "-filter_complex", "[0:v:0][1:s:{0:d}]overlay,cropdetect".format(subtitle),
                    "-an",
                    "-sn",
                    "-vn",
                    "-y",
                    "-f", "null",
                    "/dev/null"
                ],
                encoding = "utf-8",
                  stderr = subprocess.STDOUT,
            )

    # Initialize values ...
    y1 = height                                                                 # [px]
    y2 = 0                                                                      # [px]

    # Loop over matches ...
    for match in re.findall(r"crop=[0-9]+:[0-9]+:[0-9]+:[0-9]+", stderrout):
        # Extract information ...
        h = int(match.split("=")[1].split(":")[1])                              # [px]
        y = int(match.split("=")[1].split(":")[3])                              # [px]

        # Update values ...
        y1 = min(y1, y)                                                         # [px]
        y2 = max(y2, y + h)                                                     # [px]

    # Return answer ...
    return y1, y2
