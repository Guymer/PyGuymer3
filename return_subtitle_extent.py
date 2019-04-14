# -*- coding: utf-8 -*-

def return_subtitle_extent(fname, playlist = None, subtitle = 0):
    # Check input ...
    if fname.startswith("bluray:") and playlist is None:
        raise Exception("a Blu-ray was specified but no playlist was supplied")

    # Import modules ...
    import re
    import subprocess

    # Load sub-functions ...
    from .return_media_duration import return_media_duration
    from .return_video_frame_rate import return_video_frame_rate
    from .return_video_height import return_video_height
    from .return_video_width import return_video_width

    # Find out information about video ...
    duration = return_media_duration(fname, playlist = playlist)                # [s]
    fps = return_video_frame_rate(fname, playlist = playlist)                   # [Hz]
    height = return_video_height(fname, playlist = playlist)                    # [px]
    width = return_video_width(fname, playlist = playlist)                      # [px]

    # Find stream info ...
    if fname.startswith("bluray:"):
        proc = subprocess.Popen(
            [
                "ffmpeg",
                "-hide_banner",
                "-f", "lavfi",
                "-i", "color=color=black:size={0:d}x{1:d}:rate={2:f}:duration={3:f},format=yuv420p".format(width, height, fps, duration),
                "-probesize", "3G",
                "-analyzeduration", "1800M",
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
                "-hide_banner",
                "-f", "lavfi",
                "-i", "color=color=black:size={0:d}x{1:d}:rate={2:f}:duration={3:f},format=yuv420p".format(width, height, fps, duration),
                "-probesize", "3G",
                "-analyzeduration", "1800M",
                "-i", fname,
                "-filter_complex", "[0:v:0][1:s:{0:d}]overlay,cropdetect".format(subtitle),
                "-an",
                "-sn",
                "-vn",
                "-y",
                "-f", "null",
                "/dev/null"
            ],
            stderr = subprocess.PIPE,
            stdout = subprocess.PIPE
        )
        stdout, stderr = proc.communicate()
        if proc.returncode != 0:
            # HACK: Fallback and attempt to load it as a raw M-JPEG stream.
            proc = subprocess.Popen(
                [
                    "ffmpeg",
                    "-hide_banner",
                    "-f", "lavfi",
                    "-i", "color=color=black:size={0:d}x{1:d}:rate={2:f}:duration={3:f},format=yuv420p".format(width, height, fps, duration),
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
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
                stderr = subprocess.PIPE,
                stdout = subprocess.PIPE
            )
            stdout, stderr = proc.communicate()
            if proc.returncode != 0:
                raise Exception("\"ffmpeg\" command failed")

    # Initialize values ...
    y1 = height                                                                 # [px]
    y2 = 0                                                                      # [px]

    # Loop over matches ...
    for match in re.findall(r"crop=[0-9]+:[0-9]+:[0-9]+:[0-9]+", stderr):
        # Extract information ...
        h = int(match.split("=")[1].split(":")[1])                            # [px]
        y = int(match.split("=")[1].split(":")[3])                            # [px]

        # Update values ...
        y1 = min(y1, y)                                                         # [px]
        y2 = max(y2, y + h)                                                     # [px]

    # Return answer ...
    return y1, y2
