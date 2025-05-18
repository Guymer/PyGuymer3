#!/usr/bin/env python3

# Define function ...
def images2mp4(
    frames,
    /,
    *,
             crf = -1.0,
             cwd = None,
           debug = __debug__,
       ensureNFC = True,
      ffmpegPath = None,
     ffprobePath = None,
            form = "mp4",
             fps = 25.0,
           level = "ERROR",
     mp4filePath = None,
         pkgPath = None,
        portPath = None,
         profile = "ERROR",
    screenHeight = -1,
     screenWidth = -1,
         timeout = 60.0,
      zypperPath = None,
):
    """Convert a sequence of images to a MP4 video.

    This function makes a MP4 video from a list of file paths. The user is able
    to set the format and the framerate, as well as optionally downscaling the
    images to fit withing a screen size.

    Parameters
    ----------
    frames : list of str
        the list of strings which are the paths to the images
    crf : float, optional
        the CRF to be passed to libx264, default -1.0 (which means choose one
        using the function :func:`return_x264_crf`)
    debug : bool, optional
        print debug messages
    ffmpegPath : str, optional
        the path to the "ffmpeg" binary (if not provided then Python will
        attempt to find the binary itself)
    ffprobePath : str, optional
        the path to the "ffprobe" binary (if not provided then Python will
        attempt to find the binary itself)
    form : str, optional
        the format to be passed to ffmpeg, default "mp4" (the only two sensible
        options are "ipod" and "mp4")
    fps : float, optional
        the framerate, default 25.0
    level : str, optional
        the level to be passed to libx264, default "ERROR" (which means choose
        one using the function :func:`return_x264_level`)
    mp4filePath : str, optional
        the path to the "mp4file" binary (if not provided then Python will
        attempt to find the binary itself)
    pkgPath : str, optional
        the path to the "pkg" binary (if not provided then Python will
        attempt to find the binary itself)
    portPath : str, optional
        the path to the "port" binary (if not provided then Python will
        attempt to find the binary itself)
    profile : str, optional
        the profile to be passed to libx264, default "ERROR" (which means choose
        one using the function :func:`return_x264_profile`)
    screenHeight : int, optional
        the height of the screen to downscale the input images to fit within,
        default -1 (integers less than 100 imply no downscaling)
    screenWidth : int, optional
        the width of the screen to downscale the input images to fit within,
        default -1 (integers less than 100 imply no downscaling)
    timeout : float, optional
        the timeout for any requests/subprocess calls
    zypperPath : str, optional
        the path to the "zypper" binary (if not provided then Python will
        attempt to find the binary itself)

    Returns
    -------
    path : str
        the path to the MP4 in a temporary directory (to be copied/moved by the
        user themselves)

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import os
    import platform
    import shutil
    import subprocess
    import tempfile

    # Load sub-functions ...
    from .optimize_MP4 import optimize_MP4
    from .return_video_bit_depth import return_video_bit_depth
    from .return_x264_crf import return_x264_crf
    from .return_x264_level import return_x264_level
    from .return_x264_profile import return_x264_profile
    from ..find_program_version import find_program_version
    from ..image import return_image_size

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if ffmpegPath is None:
        ffmpegPath = shutil.which("ffmpeg")
    if ffprobePath is None:
        ffprobePath = shutil.which("ffprobe")
    assert ffmpegPath is not None, "\"ffmpeg\" is not installed"
    assert ffprobePath is not None, "\"ffprobe\" is not installed"

    # **************************************************************************

    # Check if the user wants to scale the input images down to fit within a
    # screen size ...
    if screenWidth >= 100 and screenHeight >= 100:
        # Check input ...
        if screenWidth % 2 != 0 or screenHeight % 2 != 0:
            raise Exception("the dimensions of the screen must be even") from None

        # Set aspect ratio ...
        screenRatio = float(screenWidth) / float(screenHeight)                  # [px/px]
        if debug:
            print(f"INFO: The input images will be downscaled to fit within {screenWidth:,d}x{screenHeight:,d} ({screenRatio:.5f}:1).")

    # **************************************************************************

    # Set package names based on OS ...
    # NOTE: This is a bit of a hack. The package name is required to find the
    #       version number of the software and the package name changes
    #       depending on the package manager used.
    ffmpeg = "ffmpeg"
    libx264 = "libx264"
    if platform.system() == "Darwin":
        libx264 = "x264"

    # Find the extension of the input images (assuming that they are all the
    # same extension) ...
    ext = os.path.splitext(frames[0])[1].lower()

    # Find the dimensions (and aspect ratio) of the input images (assuming that
    # they are all the same dimensions) ...
    inputWidth, inputHeight = return_image_size(frames[0], compressed = False)  # [px], [px]
    inputRatio = float(inputWidth) / float(inputHeight)                         # [px/px]
    if debug:
        print(f"INFO: The input images are {inputWidth:,d}x{inputHeight:,d} ({inputRatio:.5f}:1).")

    # Find the dimensions (and aspect ratio) of the cropped input images ...
    # NOTE: x264 requires that the dimensions are multiples of 2.
    cropWidth = 2 * (inputWidth // 2)                                           # [px]
    cropHeight = 2 * (inputHeight // 2)                                         # [px]
    cropRatio = float(cropWidth) / float(cropHeight)                            # [px/px]
    if debug:
        print(f"INFO: The cropped input images are {cropWidth:,d}x{cropHeight:,d} ({cropRatio:.5f}:1).")

    # Check if the user wants to scale the input images down to fit within a
    # screen size ...
    if screenWidth >= 100 and screenHeight >= 100:
        # Check if the cropped input images are wider/taller than the screen
        # size ...
        if cropRatio > screenRatio:
            # Find the dimensions of the output video ...
            outputWidth = screenWidth                                           # [px]
            outputHeight = 2 * (round(float(screenWidth) / cropRatio) // 2)     # [px]
        else:
            # Find the dimensions of the output video ...
            outputWidth = 2 * (round(float(screenHeight) * cropRatio) // 2)     # [px]
            outputHeight = screenHeight                                         # [px]

        # Find the aspect ratio of the output video ...
        outputRatio = float(outputWidth) / float(outputHeight)                  # [px/px]
    else:
        # Find the dimensions (and aspect ratio) of the output video ...
        outputWidth = cropWidth                                                 # [px]
        outputHeight = cropHeight                                               # [px]
        outputRatio = cropRatio                                                 # [px/px]
    if debug:
        print(f"INFO: The output video will be {outputWidth:,d}x{outputHeight:,d} ({outputRatio:.5f}:1).")

    # Find CRF, level and profile of the output video (if required) ...
    if crf < 0.0:
        crf = return_x264_crf(outputWidth, outputHeight)
    if level == "ERROR":
        level = return_x264_level(outputWidth, outputHeight)
    if profile == "ERROR":
        profile = return_x264_profile(outputWidth, outputHeight)

    # Create secure output directory ...
    tmpname = tempfile.mkdtemp(prefix = "images2mp4.")

    # Make symbolic links to the input images for ease ...
    for i, frame in enumerate(frames):
        os.symlink(os.path.abspath(frame), f"{tmpname}/frame{i:06d}{ext}")

    # Determine output video filter parameters ...
    filterParams = []
    if inputWidth != cropWidth or inputHeight != cropHeight:
        filterParams += [
            f"crop={cropWidth:d}:{cropHeight:d}:{(inputWidth - cropWidth) // 2:d}:{(inputHeight - cropHeight) // 2:d}",
        ]
    if cropWidth != outputWidth or cropHeight != outputHeight:
        filterParams += [
            f"scale={outputWidth:d}:{outputHeight:d}",
        ]

    # Convert the input images to the output video ...
    # NOTE: Audio and subtitle streams are explicitly disabled just to be safe.
    cmd = [
        ffmpegPath,
        "-hide_banner",
        "-probesize", "1G",
        "-analyzeduration", "1800M",
        "-f", "image2",
        "-framerate", f"{fps:.1f}",
        "-i", f"{tmpname}/frame%06d{ext}",
        "-pix_fmt", "yuv420p",
        "-an",
        "-sn",
        "-c:v", "libx264",
        "-profile:v", profile,
        "-preset", "veryslow",
        "-level", level,
        "-crf", f"{crf:.1f}",
    ]
    if len(filterParams) > 0:
        cmd += [
            "-vf", ",".join(filterParams),
        ]
    cmd += [
        "-f", form,
        "-map_chapters", "-1",
        "-map_metadata", "-1",
        "-metadata", f"comment=Converted to a {form.upper()} using ffmpeg (version {find_program_version(ffmpeg, pkgPath = pkgPath, portPath = portPath, timeout = timeout, zypperPath = zypperPath)}) which used libx264 (version {find_program_version(libx264, pkgPath = pkgPath, portPath = portPath, timeout = timeout, zypperPath = zypperPath)}) using a CRF of {crf:.1f} for libx264 (which adhered to the {profile} profile and level {level}).",
        "-threads", f"{max(1, (os.cpu_count() or 1) - 1):d}",
        f"{tmpname}/video.mp4",
    ]
    if debug:
        print(f'INFO: {" ".join(cmd)}')
    with open(f"{tmpname}/ffmpeg.err", "wt", encoding = "utf-8") as fObjErr:
        with open(f"{tmpname}/ffmpeg.out", "wt", encoding = "utf-8") as fObjOut:
            subprocess.run(
                cmd,
                   check = True,
                     cwd = cwd,
                encoding = "utf-8",
                  stderr = fObjErr,
                  stdout = fObjOut,
                 timeout = None,
            )

    # Check libx264 bit-depth ...
    if return_video_bit_depth(
        f"{tmpname}/video.mp4",
                cwd = cwd,
              debug = debug,
          ensureNFC = ensureNFC,
        ffprobePath = ffprobePath,
           playlist = -1,
            timeout = timeout,
    ) != 8:
        raise Exception(f"successfully converted the input images to a not-8-bit MP4; see \"{tmpname}\" for clues") from None

    # Optimize output video ...
    optimize_MP4(
        f"{tmpname}/video.mp4",
              debug = debug,
        mp4filePath = mp4filePath,
            timeout = timeout,
    )

    # Return path to output video ...
    return f"{tmpname}/video.mp4"
