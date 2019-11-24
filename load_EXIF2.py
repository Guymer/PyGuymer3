def load_EXIF2(fname):
    # NOTE: This function uses the binary "exiftool".

    # Import modules ...
    import json
    import subprocess

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
