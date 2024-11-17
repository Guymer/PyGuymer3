#!/usr/bin/env python3

# Define function ...
def return_video_crop_parameters(
    fname,
    /,
    *,
            cwd = None,
          debug = __debug__,
             dt = 2.0,
     ffmpegPath = None,
    ffprobePath = None,
       playlist = -1,
        timeout = 60.0,
):
    # Import standard modules ...
    import shutil
    import subprocess

    # Import sub-functions ...
    from .return_media_duration import return_media_duration
    from .return_video_height import return_video_height
    from .return_video_width import return_video_width

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if ffmpegPath is None:
        ffmpegPath = shutil.which("ffmpeg")
    if ffprobePath is None:
        ffprobePath = shutil.which("ffprobe")
    assert ffmpegPath is not None, "\"ffmpeg\" is not installed"
    assert ffprobePath is not None, "\"ffprobe\" is not installed"

    # Check input ...
    if fname.startswith("bluray:") and playlist < 0:
        raise Exception("a Blu-ray was specified but no playlist was supplied") from None

    # Initialize variables ...
    dur = return_media_duration(
        fname,
                cwd = cwd,
              debug = debug,
        ffprobePath = ffprobePath,
           playlist = playlist,
            timeout = timeout,
    )                                                                           # [s]
    inW = return_video_width(
        fname,
                cwd = cwd,
              debug = debug,
        ffprobePath = ffprobePath,
           playlist = playlist,
            timeout = timeout,
    )                                                                           # [px]
    inH = return_video_height(
        fname,
                cwd = cwd,
              debug = debug,
        ffprobePath = ffprobePath,
           playlist = playlist,
            timeout = timeout,
    )                                                                           # [px]
    outX = 0                                                                    # [px]
    outY = 0                                                                    # [px]

    # Loop over fractions ...
    for frac in [0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8]:
        # Deduce start time ...
        t = frac * dur - dt / 2.0                                               # [s]

        # Check if it is a Blu-ray ...
        if fname.startswith("bluray:"):
            # Find crop parameters ...
            resp = subprocess.run(
                [
                    ffmpegPath,
                    "-hide_banner",
                    "-probesize", "1G",
                    "-analyzeduration", "1800M",
                    "-playlist", f"{playlist:d}",
                    "-ss", f"{t:.3f}",
                    "-i", fname,
                    "-an",
                    "-sn",
                    "-t", f"{dt:f}",
                    "-vf", "cropdetect",
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
                # Find crop parameters ...
                resp = subprocess.run(
                    [
                        ffmpegPath,
                        "-hide_banner",
                        "-probesize", "1G",
                        "-analyzeduration", "1800M",
                        "-ss", f"{t:.3f}",
                        "-i", fname,
                        "-an",
                        "-sn",
                        "-t", f"{dt:f}",
                        "-vf", "cropdetect",
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
                # Fallback and attempt to find crop parameters as a raw M-JPEG
                # stream ...
                resp = subprocess.run(
                    [
                        ffmpegPath,
                        "-hide_banner",
                        "-probesize", "1G",
                        "-analyzeduration", "1800M",
                        "-ss", f"{t:.3f}",
                        "-f", "mjpeg",
                        "-i", fname,
                        "-an",
                        "-sn",
                        "-t", f"{dt:f}",
                        "-vf", "cropdetect",
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

        # Loop over lines ...
        for line in resp.stdout.splitlines():
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
                    raise Exception(f"an unexpected string format was encountered (\"{keyvalue}\")") from None
                db[key] = value

            # Update variables ...
            outX = max(outX, int(db["x"]))                                      # [px]
            outY = max(outY, int(db["y"]))                                      # [px]

    # Update variables ...
    outW = inW - 2 * outX                                                       # [px]
    outH = inH - 2 * outY                                                       # [px]
    cropParams = f"{outW:d}:{outH:d}:{outX:d}:{outY:d}"

    # Check results ...
    if outW > inW or outW <= 0:
        raise Exception(f"failed to find cropped width (inW = {inW:d}, inH = {inH:d}, outX = {outX:d}, outY = {outY:d}, outW = {outW:d}, outH = {outH:d})") from None
    if outH > inH or outH <= 0:
        raise Exception(f"failed to find cropped height (inW = {inW:d}, inH = {inH:d}, outX = {outX:d}, outY = {outY:d}, outW = {outW:d}, outH = {outH:d})") from None

    # Return top-left corner, width, height and FFMPEG crop parameter string ...
    return outX, outY, outW, outH, cropParams
