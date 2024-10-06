#!/usr/bin/env python3

# Define function ...
def return_subtitle_extent(
    fname,
    /,
    *,
           cwd = None,
         debug = __debug__,
    ffmpegPath = None,
      playlist = -1,
      subtitle = 0,
       timeout = 60.0,
):
    # Import standard modules ...
    import re
    import shutil
    import subprocess

    # Import sub-functions ...
    from .return_media_duration import return_media_duration
    from .return_video_frame_rate import return_video_frame_rate
    from .return_video_height import return_video_height
    from .return_video_width import return_video_width

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if ffmpegPath is None:
        ffmpegPath = shutil.which("ffmpeg")
    assert ffmpegPath is not None, "\"ffmpeg\" is not installed"

    # Check input ...
    if fname.startswith("bluray:") and playlist < 0:
        raise Exception("a Blu-ray was specified but no playlist was supplied") from None

    # Find out information about video ...
    duration = return_media_duration(
        fname,
             cwd = cwd,
           debug = debug,
        playlist = playlist,
         timeout = timeout,
    )                                                                           # [s]
    fps = return_video_frame_rate(
        fname,
             cwd = cwd,
           debug = debug,
        playlist = playlist,
         timeout = timeout,
    )                                                                           # [Hz]
    width = return_video_width(
        fname,
             cwd = cwd,
           debug = debug,
        playlist = playlist,
         timeout = timeout,
    )                                                                           # [px]
    height = return_video_height(
        fname,
             cwd = cwd,
           debug = debug,
        playlist = playlist,
         timeout = timeout,
    )                                                                           # [px]

    # Check if it is a Blu-ray ...
    if fname.startswith("bluray:"):
        # Find stream info ...
        resp = subprocess.run(
            [
                ffmpegPath,
                "-hide_banner",
                "-probesize", "3G",
                "-analyzeduration", "1800M",
                "-f", "lavfi",
                "-i", f"color=color=black:size={width:d}x{height:d}:rate={fps:f}:duration={duration:f},format=yuv420p",
                "-playlist", f"{playlist:d}",
                "-i", fname,
                "-filter_complex", f"[0:v:0][1:s:{subtitle:d}]overlay,cropdetect",
                "-an",
                "-sn",
                "-vn",
                "-y",
                "-f", "null",
                "/dev/null",
            ],
               check = True,
                 cwd = cwd,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
              stdout = subprocess.PIPE,
             timeout = None,
        )
    else:
        # Attempt to survey the file ...
        try:
            # Find stream info ...
            resp = subprocess.run(
                [
                    ffmpegPath,
                    "-hide_banner",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-f", "lavfi",
                    "-i", f"color=color=black:size={width:d}x{height:d}:rate={fps:f}:duration={duration:f},format=yuv420p",
                    "-i", fname,
                    "-filter_complex", f"[0:v:0][1:s:{subtitle:d}]overlay,cropdetect",
                    "-an",
                    "-sn",
                    "-vn",
                    "-y",
                    "-f", "null",
                    "/dev/null",
                ],
                   check = True,
                     cwd = cwd,
                encoding = "utf-8",
                  stderr = subprocess.STDOUT,
                  stdout = subprocess.PIPE,
                 timeout = None,
            )
        except subprocess.CalledProcessError:
            # Fallback and attempt to find stream info as a raw M-JPEG stream ...
            resp = subprocess.run(
                [
                    ffmpegPath,
                    "-hide_banner",
                    "-probesize", "3G",
                    "-analyzeduration", "1800M",
                    "-f", "lavfi",
                    "-i", f"color=color=black:size={width:d}x{height:d}:rate={fps:f}:duration={duration:f},format=yuv420p",
                    "-f", "mjpeg",
                    "-i", fname,
                    "-filter_complex", f"[0:v:0][1:s:{subtitle:d}]overlay,cropdetect",
                    "-an",
                    "-sn",
                    "-vn",
                    "-y",
                    "-f", "null",
                    "/dev/null",
                ],
                   check = True,
                     cwd = cwd,
                encoding = "utf-8",
                  stderr = subprocess.STDOUT,
                  stdout = subprocess.PIPE,
                 timeout = None,
            )

    # Initialize values ...
    y1 = height                                                                 # [px]
    y2 = 0                                                                      # [px]

    # Loop over matches ...
    for match in re.findall(r"crop=[0-9]+:[0-9]+:[0-9]+:[0-9]+", resp.stdout):
        # Extract information ...
        h = int(match.split("=")[1].split(":")[1])                              # [px]
        y = int(match.split("=")[1].split(":")[3])                              # [px]

        # Update values ...
        y1 = min(y1, y)                                                         # [px]
        y2 = max(y2, y + h)                                                     # [px]

    # Return answer ...
    return y1, y2
