def load_GPS_EXIF2(fname):
    # Import standard modules ...
    import datetime
    import json
    import math
    import os
    import shutil
    import subprocess

    # Check that "exiftool" is installed ...
    if shutil.which("exiftool") is None:
        raise Exception("\"exiftool\" is not installed") from None

    # Create default dictionary answer ...
    ans = {}

    # Run "exiftool" and load it as JSON ...
    dat = json.loads(
        subprocess.check_output(
            [
                "exiftool",
                "-json",
                "-coordFormat", "%+.12f",
                "-dateFormat", "%Y-%m-%dT%H:%M:%S.%.6f",                        # should be the same as datetime.isoformat()
                "-groupNames",
                "-struct",
                "--printConv",
                "-GPSDateTime",
                "-GPSAltitude",
                "-GPSLongitude",
                "-GPSLatitude",
                "-GPSHPositioningError",
                fname
            ],
            encoding = "utf-8",
            stderr = open(os.devnull, "wt")
        )
    )[0]

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
        date, time = dat["Composite:GPSDateTime"][:-1].split(" ")
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
            tzinfo = datetime.timezone.utc
        )

    # Check that there is location information ...
    if "lon" in ans and "lat" in ans:
        # Check that there is date/time information ...
        if "datetime" in ans:
            # Check that there is altitude information ...
            if "alt" in ans:
                # Make a pretty string ...
                ans["pretty"] = "GPS fix returned ({0:.6f}°, {1:.6f}°, {2:.1f}m ASL) at \"{3:s}\".".format(
                    ans["lon"],
                    ans["lat"],
                    ans["alt"],
                    ans["datetime"].isoformat(" "),
                )
            else:
                # Make a pretty string ...
                ans["pretty"] = "GPS fix returned ({0:.6f}°, {1:.6f}°) at \"{2:s}\".".format(
                    ans["lon"],
                    ans["lat"],
                    ans["datetime"].isoformat(" "),
                )
        else:
            # Check that there is altitude information ...
            if "alt" in ans:
                # Make a pretty string ...
                ans["pretty"] = "GPS fix returned ({0:.6f}°, {1:.6f}°, {2:.1f}m ASL).".format(
                    ans["lon"],
                    ans["lat"],
                    ans["alt"]
                )
            else:
                # Make a pretty string ...
                ans["pretty"] = "GPS fix returned ({0:.6f}°, {1:.6f}°).".format(
                    ans["lon"],
                    ans["lat"]
                )

    # Return answer ...
    if ans == {}:
        return False
    return ans
