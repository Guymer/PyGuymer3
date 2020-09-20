def does_FLAC_have_padding(fname):
    # Import standard modules ...
    import shutil
    import subprocess

    # Check that "ffmpeg" is installed ...
    if shutil.which("metaflac") is None:
        raise Exception("\"metaflac\" is not installed")

    # List all PADDING blocks ...
    stderrout = subprocess.check_output(
        [
            "metaflac",
            "--list",
            "--block-type=PADDING",
            fname
        ],
        encoding = "utf-8",
        stderr = subprocess.STDOUT
    )

    # Loop over lines ...
    for line in stderrout.splitlines():
        # Check if it is PADDING ...
        if "type: 1 (PADDING)" in line:
            # Return answer ...
            return True

    # Return answer ...
    return False
