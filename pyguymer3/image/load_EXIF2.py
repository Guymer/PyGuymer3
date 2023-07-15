#!/usr/bin/env python3

# Define function ...
def load_EXIF2(fname, /, *, timeout = 60.0):
    # Import standard modules ...
    import json
    import shutil
    import subprocess

    # Check that "exiftool" is installed ...
    if shutil.which("exiftool") is None:
        raise Exception("\"exiftool\" is not installed") from None

    # Run "exiftool" and load it as JSON ...
    # NOTE: Don't merge standard out and standard error together as the result
    #       will probably not be valid JSON if standard error is not empty.
    ans = json.loads(
        subprocess.run(
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
               check = True,
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
              stdout = subprocess.PIPE,
             timeout = timeout,
        ).stdout
    )[0]

    # Return answer ...
    return ans
