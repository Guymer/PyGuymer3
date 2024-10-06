#!/usr/bin/env python3

# Define function ...
def load_EXIF2(
    fname,
    /,
    *,
      compressed = False,
    exiftoolPath = None,
         timeout = 60.0,
):
    # Import standard modules ...
    import json
    import shutil
    import subprocess

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if exiftoolPath is None:
        exiftoolPath = shutil.which("exiftool")
    assert exiftoolPath is not None, "\"exiftool\" is not installed"

    # Create "exiftool" command ...
    cmd = [
        exiftoolPath,
        "-api", "largefilesupport=1",
        "-json",
    ]
    if compressed:
        cmd += [
            "-zip",
        ]
    cmd += [
        "-coordFormat", "%+.12f",
        "-dateFormat", "%Y-%m-%dT%H:%M:%S.%.6f",                                # should be the same as datetime.isoformat(sep = "T", timespec = "microseconds")
        "-groupNames",
        "-struct",
        "--printConv",
        fname,
    ]

    # Run "exiftool" and load it as JSON ...
    # NOTE: Don't merge standard out and standard error together as the result
    #       will probably not be valid JSON if standard error is not empty.
    ans = json.loads(
        subprocess.run(
            cmd,
               check = True,
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
              stdout = subprocess.PIPE,
             timeout = timeout,
        ).stdout
    )[0]

    # Return answer ...
    return ans
