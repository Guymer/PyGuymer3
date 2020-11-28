def load_GPS_EXIF2(fname):
    # Import standard modules ...
    import datetime
    import json
    import math
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
                "-dateFormat", "%Y:%m:%d %H:%M:%S.%f",
                "--printConv",
                "-GPSDateTime",
                "-GPSAltitude",
                "-GPSLongitude",
                "-GPSLatitude",
                "-GPSHPositioningError",
                fname
            ],
            encoding = "utf-8",
            stderr = subprocess.STDOUT
        )
    )[0]

    # Populate dictionary ...
    if "GPSLongitude" in dat:
        ans["lon"] = float(dat["GPSLongitude"])                                 # [deg]
    if "GPSLatitude" in dat:
        ans["lat"] = float(dat["GPSLatitude"])                                  # [deg]
    if "GPSAltitude" in dat:
        ans["alt"] = float(dat["GPSAltitude"])                                  # [m]
    if "GPSHPositioningError" in dat:
        ans["loc_err"] = float(dat["GPSHPositioningError"])                     # [m]
    if "GPSDateTime" in dat:
        date, time = dat["GPSDateTime"][:-1].split(" ")
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
