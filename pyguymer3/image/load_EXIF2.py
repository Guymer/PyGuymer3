def load_EXIF2(fname):
    # Import standard modules ...
    import json
    import shutil
    import subprocess

    # Check that "exiftool" is installed ...
    if shutil.which("exiftool") is None:
        raise Exception("\"exiftool\" is not installed") from None

    # Run "exiftool" and load it as JSON ...
    ans = json.loads(
        subprocess.check_output(
            [
                "exiftool",
                "-api", "largefilesupport=1",
                "-json",
                "-coordFormat", "%+.12f",
                "-dateFormat", "%Y-%m-%dT%H:%M:%S.%.6f",                        # should be the same as datetime.isoformat(sep = "T", timespec = "microseconds")
                "-groupNames",
                "-struct",
                "--printConv",
                fname
            ],
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
        )
    )[0]

    # Return answer ...
    return ans
