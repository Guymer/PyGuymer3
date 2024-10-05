#!/usr/bin/env python3

# Define function ...
def load_GPS_EXIF2(
    fname,
    /,
    *,
    compressed = False,
       timeout = 60.0,
):
    # Import standard modules ...
    import datetime
    import json
    import math
    import shutil
    import subprocess

    # Check that "exiftool" is installed ...
    if shutil.which("exiftool") is None:
        raise Exception("\"exiftool\" is not installed") from None

    # Create "exiftool" command ...
    cmd = [
        "exiftool",
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
        "-GPSDateTime",
        "-GPSAltitude",
        "-GPSLongitude",
        "-GPSLatitude",
        "-GPSHPositioningError",
        fname,
    ]

    # Run "exiftool" and load it as JSON ...
    # NOTE: Don't merge standard out and standard error together as the result
    #       will probably not be valid JSON if standard error is not empty.
    dat = json.loads(
        subprocess.run(
            cmd,
               check = True,
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
              stdout = subprocess.PIPE,
             timeout = timeout,
        ).stdout
    )[0]

    # Create default dictionary answer ...
    ans = {}

    # Populate dictionary ...
    if "Composite:GPSLongitude" in dat:
        ans["lon"] = float(dat["Composite:GPSLongitude"])                       # [°]
    if "Composite:GPSLatitude" in dat:
        ans["lat"] = float(dat["Composite:GPSLatitude"])                        # [°]
    if "Composite:GPSAltitude" in dat:
        ans["alt"] = float(dat["Composite:GPSAltitude"])                        # [m]
    if "Composite:GPSHPositioningError" in dat:
        ans["loc_err"] = float(dat["Composite:GPSHPositioningError"])           # [m]
    if "Composite:GPSDateTime" in dat:
        date, time = dat["Composite:GPSDateTime"].removesuffix("Z").split(" ")
        tmp1 = date.split(":")
        tmp2 = time.split(":")
        ye = int(tmp1[0])                                                       # [year]
        mo = int(tmp1[1])                                                       # [month]
        da = int(tmp1[2])                                                       # [day]
        hr = int(tmp2[0])                                                       # [hour]
        mi = int(tmp2[1])                                                       # [minute]
        se = int(math.floor(float(tmp2[2])))                                    # [s]
        us = int(1.0e6 * (float(tmp2[2]) - se))                                 # [μs]
        if hr > 23:
            # HACK: This particular gem is due to my Motorola Moto G3 smartphone.
            hr = hr % 24                                                        # [hour]
        ans["datetime"] = datetime.datetime(
                   year = ye,
                  month = mo,
                    day = da,
                   hour = hr,
                 minute = mi,
                 second = se,
            microsecond = us,
                 tzinfo = datetime.UTC,
        )

    # Check that there is location information ...
    if "lon" in ans and "lat" in ans:
        # Check that there is date/time information ...
        if "datetime" in ans:
            # Check that there is altitude information ...
            if "alt" in ans:
                # Make a pretty string ...
                ans["pretty"] = f'GPS fix returned ({ans["lon"]:.6f}°, {ans["lat"]:.6f}°, {ans["alt"]:.1f}m ASL) at \"{ans["datetime"].isoformat(sep = " ", timespec = "microseconds")}\".'
            else:
                # Make a pretty string ...
                ans["pretty"] = f'GPS fix returned ({ans["lon"]:.6f}°, {ans["lat"]:.6f}°) at \"{ans["datetime"].isoformat(sep = " ", timespec = "microseconds")}\".'
        else:
            # Check that there is altitude information ...
            if "alt" in ans:
                # Make a pretty string ...
                ans["pretty"] = f'GPS fix returned ({ans["lon"]:.6f}°, {ans["lat"]:.6f}°, {ans["alt"]:.1f}m ASL).'
            else:
                # Make a pretty string ...
                ans["pretty"] = f'GPS fix returned ({ans["lon"]:.6f}°, {ans["lat"]:.6f}°).'

    # Return answer ...
    if not ans:
        return False
    return ans
