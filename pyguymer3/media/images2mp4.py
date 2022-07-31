def images2mp4(frames, kwArgCheck = None, crf = -1.0, debug = False, form = "mp4", fps = 25.0, level = "ERROR", profile = "ERROR", screenHeight = -1, screenWidth = -1):
    # Import standard modules ...
    import multiprocessing
    import os
    import platform
    import shutil
    import subprocess
    import tempfile

    # Import special modules ...
    try:
        import PIL
        import PIL.Image
    except:
        raise Exception("\"PIL\" is not installed; run \"pip install --user Pillow\"") from None

    # Load sub-functions ...
    from .optimize_MP4 import optimize_MP4
    from .return_video_bit_depth import return_video_bit_depth
    from .return_x264_crf import return_x264_crf
    from .return_x264_level import return_x264_level
    from .return_x264_profile import return_x264_profile
    from ..find_program_version import find_program_version

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Configure PIL to open images up to 1 GiP ...
    PIL.Image.MAX_IMAGE_PIXELS = 1024 * 1024 * 1024                             # [px]

    # Check that "ffmpeg" is installed ...
    if shutil.which("ffmpeg") is None:
        raise Exception("\"ffmpeg\" is not installed") from None

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
    inputWidth, inputHeight = PIL.Image.open(frames[0]).size                    # [px], [px]
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
            outputHeight = 2 * (int(float(screenWidth) / cropRatio) // 2)       # [px]
        else:
            # Find the dimensions of the output video ...
            outputWidth = 2 * (int(float(screenHeight) * cropRatio) // 2)       # [px]
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
        "ffmpeg",
        "-hide_banner",
        "-probesize", "3G",
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
        "-metadata", f"comment=Converted to a {form.upper()} using ffmpeg (version {find_program_version(ffmpeg)}) which used libx264 (version {find_program_version(libx264)}) using a CRF of {crf:.1f} for libx264 (which adhered to the {profile} profile and level {level}).",
        "-threads", f"{multiprocessing.cpu_count() - 1:d}",
        f"{tmpname}/video.mp4",
    ]
    if debug:
        print(f'INFO: {" ".join(cmd)}')
    with open(f"{tmpname}/ffmpeg.err", "wt", encoding = "utf-8") as fobjErr:
        with open(f"{tmpname}/ffmpeg.out", "wt", encoding = "utf-8") as fobjOut:
            subprocess.run(
                cmd,
                   check = True,
                encoding = "utf-8",
                  stderr = fobjErr,
                  stdout = fobjOut,
            )

    # Check libx264 bit-depth ...
    if return_video_bit_depth(f"{tmpname}/video.mp4") != 8:
        raise Exception(f"successfully converted the input images to a not-8-bit MP4; see \"{tmpname}\" for clues") from None

    # Optimize output video ...
    optimize_MP4(f"{tmpname}/video.mp4", debug = debug)

    # Return path to output video ...
    return f"{tmpname}/video.mp4"
