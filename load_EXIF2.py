def load_EXIF2(fname):
    # Import standard modules ...
    import json
    import shutil
    import subprocess

    # Check that "exiftool" is installed ...
    if shutil.which("exiftool") is None:
        raise Exception("\"exiftool\" is not installed")

    # Run "exiftool" and load it as JSON ...
    out = subprocess.check_output(
        [
            "exiftool",
            "-json",
            "-dateFormat", "%Y:%m:%d %H:%M:%S.%f",
            "--printConv",
            fname
        ],
        encoding = "utf-8"
    )
    ans = json.loads(out)[0]

    # Return answer ...
    return ans
