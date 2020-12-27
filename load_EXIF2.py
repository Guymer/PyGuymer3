def load_EXIF2(fname):
    # Import standard modules ...
    import json
    import os
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
                "-json",
                "-coordFormat", "%+.12f",
                "-dateFormat", "%Y-%m-%dT%H:%M:%S.%.6f",                        # should be the same as datetime.isoformat()
                "-groupNames",
                "-struct",
                "--printConv",
                fname
            ],
            encoding = "utf-8",
            stderr = open(os.devnull, "wt")
        )
    )[0]

    # Return answer ...
    return ans
