#!/usr/bin/env python3

# Define function ...
def load_GPS_EXIF1(
    fName,
    /,
    *,
    cacheDir = "~/.cache/pyguymer3",
       debug = __debug__,
):
    # NOTE: The following web pages were helpful:
    #       * https://gist.github.com/snakeye/fdc372dbf11370fe29eb
    #       * https://sno.phy.queensu.ca/~phil/exiftool/TagNames/GPS.html

    # Import standard modules ...
    import datetime
    import math

    # Import global (subclassed) dictionary ...
    from .__exifread__ import __exifread__

    # **************************************************************************

    # Configure global (subclassed) dictionary ...
    __exifread__.cacheDir = cacheDir
    __exifread__.debug = debug

    # **************************************************************************

    # Create default dictionary answer ...
    ans = {}

    # Check that the required tags are preset ...
    if "GPS GPSLongitude" in __exifread__[fName] and "GPS GPSLongitudeRef" in __exifread__[fName]:
        # Extract longitude ...
        d = __exifread__[fName]["GPS GPSLongitude"][0]                          # [°]
        m = __exifread__[fName]["GPS GPSLongitude"][1]                          # [min]
        s = __exifread__[fName]["GPS GPSLongitude"][2]                          # [sec]
        ans["lon"] = d + (m / 60.0) + (s / 3600.0)                              # [°]
        if __exifread__[fName]["GPS GPSLongitudeRef"] == "W":
            ans["lon"] = 0.0 - ans["lon"]                                       # [°]
        elif __exifread__[fName]["GPS GPSLongitudeRef"] != "E":
            raise Exception("the longitude reference is unexpected", __exifread__[fName]["GPS GPSLongitudeRef"]) from None

    # Check that the required tags are preset ...
    if "GPS GPSLatitude" in __exifread__[fName] and "GPS GPSLatitudeRef" in __exifread__[fName]:
        # Extract latitude ...
        d = __exifread__[fName]["GPS GPSLatitude"][0]                           # [°]
        m = __exifread__[fName]["GPS GPSLatitude"][1]                           # [min]
        s = __exifread__[fName]["GPS GPSLatitude"][2]                           # [sec]
        ans["lat"] = d + (m / 60.0) + (s / 3600.0)                              # [°]
        if __exifread__[fName]["GPS GPSLatitudeRef"] == "S":
            ans["lat"] = 0.0 - ans["lat"]                                       # [°]
        elif __exifread__[fName]["GPS GPSLatitudeRef"] != "N":
            raise Exception("the latitude reference is unexpected", __exifread__[fName]["GPS GPSLatitudeRef"]) from None

    # Check that the required tags are preset ...
    if "GPS GPSAltitude" in __exifread__[fName] and "GPS GPSAltitudeRef" in __exifread__[fName]:
        # Extract altitude ...
        ans["alt"] = __exifread__[fName]["GPS GPSAltitude"]                     # [m]
        if __exifread__[fName]["GPS GPSAltitudeRef"] == 1:
            ans["alt"] = 0.0 - ans["alt"]                                       # [m]
        elif __exifread__[fName]["GPS GPSAltitudeRef"] != 0:
            raise Exception("the altitude reference is unexpected", __exifread__[fName]["GPS GPSAltitudeRef"]) from None

    # Check that the required tags are preset ...
    if "GPS GPSDate" in __exifread__[fName] and "GPS GPSTimeStamp" in __exifread__[fName]:
        # Extract date/time and merge into one (TZ-aware) object (
        # correcting mistakes that shouldn't exist) ...
        tmp1 = __exifread__[fName]["GPS GPSDate"].split("-")
        ye = int(tmp1[0])                                                       # [year]
        mo = int(tmp1[1])                                                       # [month]
        da = int(tmp1[2])                                                       # [day]
        hr = __exifread__[fName]["GPS GPSTimeStamp"][0]                         # [hour]
        mi = __exifread__[fName]["GPS GPSTimeStamp"][1]                         # [minute]
        tmp2 = __exifread__[fName]["GPS GPSTimeStamp"][2]                       # [s]
        se = int(math.floor(tmp2))                                              # [s]
        us = int(1.0e6 * (tmp2 - se))                                           # [μs]
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

    # Check that the required tags are preset ...
    if "GPS GPSMapDatum" in __exifread__[fName]:
        # Extract map datum ...
        ans["datum"] = __exifread__[fName]["GPS GPSMapDatum"]

    # Check that the required tags are preset ...
    if "GPS GPSMeasureMode" in __exifread__[fName]:
        # Extract measure mode ...
        if __exifread__[fName]["GPS GPSMeasureMode"] == "2":
            ans["mode"] = "2D"
        elif __exifread__[fName]["GPS GPSMeasureMode"] == "3":
            ans["mode"] = "3D"
        else:
            raise Exception("the mode is unexpected", __exifread__[fName]["GPS GPSMeasureMode"]) from None

    # Check that the required tags are preset ...
    if "GPS GPSDOP" in __exifread__[fName]:
        # Extract dilution of precision ...
        ans["dop"] = __exifread__[fName]["GPS GPSDOP"]                          # [ratio]

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
