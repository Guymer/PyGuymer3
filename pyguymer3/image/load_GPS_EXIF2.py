#!/usr/bin/env python3

# Define function ...
def load_GPS_EXIF2(
    fName,
    /,
    *,
        cacheDir = "~/.cache/pyguymer3",
      compressed = False,
           debug = __debug__,
       ensureNFC = True,
    exiftoolPath = None,
         timeout = 60.0,
):
    # Import standard modules ...
    import datetime
    import math

    # Import global (subclassed) dictionary ...
    from .__exiftool__ import __exiftool__

    # **************************************************************************

    # Configure global (subclassed) dictionary ...
    # NOTE: If I blindly set "__exiftool__.exiftoolPath" to "exiftoolPath" each
    #       time then I would clobber any previous calls to "shutil.which()"
    #       performed by the global (subclassed) dictionary itself.
    __exiftool__.cacheDir = cacheDir
    __exiftool__.compressed = compressed
    __exiftool__.debug = debug
    __exiftool__.ensureNFC = ensureNFC
    if exiftoolPath is not None:
        __exiftool__.exiftoolPath = exiftoolPath
    __exiftool__.timeout = timeout                                              # [s]

    # **************************************************************************

    # Create default dictionary answer ...
    ans = {}

    # Populate dictionary ...
    if "Composite:GPSLongitude" in __exiftool__[fName]:
        ans["lon"] = float(__exiftool__[fName]["Composite:GPSLongitude"])       # [°]
    if "Composite:GPSLatitude" in __exiftool__[fName]:
        ans["lat"] = float(__exiftool__[fName]["Composite:GPSLatitude"])        # [°]
    if "Composite:GPSAltitude" in __exiftool__[fName]:
        ans["alt"] = float(__exiftool__[fName]["Composite:GPSAltitude"])        # [m]
    if "Composite:GPSHPositioningError" in __exiftool__[fName]:
        ans["loc_err"] = float(__exiftool__[fName]["Composite:GPSHPositioningError"])   # [m]
    if "Composite:GPSDateTime" in __exiftool__[fName]:
        date, time = __exiftool__[fName]["Composite:GPSDateTime"].removesuffix("Z").split(" ")
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
