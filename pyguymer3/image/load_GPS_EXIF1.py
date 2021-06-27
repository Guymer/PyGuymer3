def load_GPS_EXIF1(fname):
    # NOTE: Earth's mean radius is 6,371,009 m.
    # NOTE: The following web pages were helpful:
    #       * https://gist.github.com/snakeye/fdc372dbf11370fe29eb
    #       * https://sno.phy.queensu.ca/~phil/exiftool/TagNames/GPS.html

    # Import standard modules ...
    import datetime
    import math

    # Import special modules ...
    try:
        import exifread
    except:
        raise Exception("\"exifread\" is not installed; run \"pip install --user ExifRead\"") from None

    # Create default dictionary answer ...
    ans = {}

    # Open RAW file read-only ...
    with open(fname, "rb") as fobj:
        # Load EXIF tags ...
        tags = exifread.process_file(fobj, details = False)

        # Check that there are EXIF tags ...
        gps = False
        for key in tags:
            if key.startswith("GPS "):
                gps = True
                break

        # Check that there are EXIF tags ...
        if gps:
            # Check that the required tags are preset ...
            if "GPS GPSLongitude" in tags and "GPS GPSLongitudeRef" in tags:
                # Extract longitude ...
                d = float(tags["GPS GPSLongitude"].values[0].num) / float(tags["GPS GPSLongitude"].values[0].den)           # [°]
                m = float(tags["GPS GPSLongitude"].values[1].num) / float(tags["GPS GPSLongitude"].values[1].den)           # [min]
                s = float(tags["GPS GPSLongitude"].values[2].num) / float(tags["GPS GPSLongitude"].values[2].den)           # [sec]
                ans["lon"] = d + (m / 60.0) + (s / 3600.0)                                                                  # [°]
                if tags["GPS GPSLongitudeRef"].values[0] == "W":
                    ans["lon"] = 0.0 - ans["lon"]                                                                           # [°]
                elif tags["GPS GPSLongitudeRef"].values[0] != "E":
                    raise Exception("the longitude reference is unexpected", tags["GPS GPSLongitudeRef"].values) from None

                # Deduce longitude precision ...
                ans["lon_prec"] = 0.0                                                                                       # [°]
                if tags["GPS GPSLongitude"].values[0].den != 1:
                    ans["lon_prec"] += 1.0 / float(tags["GPS GPSLongitude"].values[0].den)                                  # [°]
                if tags["GPS GPSLongitude"].values[1].den != 1:
                    ans["lon_prec"] += 1.0 / float(tags["GPS GPSLongitude"].values[1].den) / 60.0                           # [°]
                if tags["GPS GPSLongitude"].values[2].den != 1:
                    ans["lon_prec"] += 1.0 / float(tags["GPS GPSLongitude"].values[2].den) / 3600.0                         # [°]

            # Check that the required tags are preset ...
            if "GPS GPSLatitude" in tags and "GPS GPSLatitudeRef" in tags:
                # Extract latitude ...
                d = float(tags["GPS GPSLatitude"].values[0].num) / float(tags["GPS GPSLatitude"].values[0].den)             # [°]
                m = float(tags["GPS GPSLatitude"].values[1].num) / float(tags["GPS GPSLatitude"].values[1].den)             # [min]
                s = float(tags["GPS GPSLatitude"].values[2].num) / float(tags["GPS GPSLatitude"].values[2].den)             # [sec]
                ans["lat"] = d + (m / 60.0) + (s / 3600.0)                                                                  # [°]
                if tags["GPS GPSLatitudeRef"].values[0] == "S":
                    ans["lat"] = 0.0 - ans["lat"]                                                                           # [°]
                elif tags["GPS GPSLatitudeRef"].values[0] != "N":
                    raise Exception("the latitude reference is unexpected", tags["GPS GPSLatitudeRef"].values) from None

                # Deduce latitude precision ...
                ans["lat_prec"] = 0.0                                                                                       # [°]
                if tags["GPS GPSLatitude"].values[0].den != 1:
                    ans["lat_prec"] += 1.0 / float(tags["GPS GPSLatitude"].values[0].den)                                   # [°]
                if tags["GPS GPSLatitude"].values[1].den != 1:
                    ans["lat_prec"] += 1.0 / float(tags["GPS GPSLatitude"].values[1].den) / 60.0                            # [°]
                if tags["GPS GPSLatitude"].values[2].den != 1:
                    ans["lat_prec"] += 1.0 / float(tags["GPS GPSLatitude"].values[2].den) / 3600.0                          # [°]

            # Check that the required tags are preset ...
            if "GPS GPSAltitude" in tags and "GPS GPSAltitudeRef" in tags:
                # Extract altitude ...
                ans["alt"] = float(tags["GPS GPSAltitude"].values[0].num) / float(tags["GPS GPSAltitude"].values[0].den)    # [m]
                if tags["GPS GPSAltitudeRef"].values[0] == 1:
                    ans["alt"] = 0.0 - ans["alt"]                                                                           # [m]
                elif tags["GPS GPSAltitudeRef"].values[0] != 0:
                    raise Exception("the altitude reference is unexpected", tags["GPS GPSAltitudeRef"].values) from None

                # Deduce altitude precision ...
                ans["alt_prec"] = 0.0                                                                                       # [m]
                if tags["GPS GPSAltitude"].values[0].den != 1:
                    ans["alt_prec"] += 1.0 / float(tags["GPS GPSAltitude"].values[0].den)                                   # [m]

            # Check that the required tags are preset ...
            if "GPS GPSDate" in tags and "GPS GPSTimeStamp" in tags:
                # Extract date/time and merge into one (TZ-aware) object (
                # correcting mistakes that shouldn't exist) ...
                tmp1 = tags["GPS GPSDate"].values.split(":")
                ye = int(tmp1[0])                                                                                           # [year]
                mo = int(tmp1[1])                                                                                           # [month]
                da = int(tmp1[2])                                                                                           # [day]
                hr = int(tags["GPS GPSTimeStamp"].values[0].num)                                                            # [hour]
                mi = int(tags["GPS GPSTimeStamp"].values[1].num)                                                            # [minute]
                tmp2 = float(tags["GPS GPSTimeStamp"].values[2].num) / float(tags["GPS GPSTimeStamp"].values[2].den)        # [s]
                se = int(math.floor(tmp2))                                                                                  # [s]
                us = int(1.0e6 * (tmp2 - se))                                                                               # [μs]
                if hr > 23:
                    # HACK: This particular gem is due to my Motorola Moto G3
                    #       smartphone.
                    hr = hr % 24                                                                                            # [hour]
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

                # Deduce time precision ...
                foo = 0.0                                                                                                   # [s]
                if tags["GPS GPSTimeStamp"].values[0].den != 1:
                    foo += 3600.0 / float(tags["GPS GPSTimeStamp"].values[0].den)                                           # [s]
                if tags["GPS GPSTimeStamp"].values[1].den != 1:
                    foo += 60.0 / float(tags["GPS GPSTimeStamp"].values[1].den)                                             # [s]
                if tags["GPS GPSTimeStamp"].values[2].den != 1:
                    foo += 1.0 / float(tags["GPS GPSTimeStamp"].values[2].den)                                              # [s]
                ans["time_prec"] = datetime.timedelta(seconds = foo)

            # Check that the required tags are preset ...
            if "GPS GPSMapDatum" in tags:
                # Extract map datum ...
                ans["datum"] = tags["GPS GPSMapDatum"].values

            # Check that the required tags are preset ...
            if "GPS GPSMeasureMode" in tags:
                # Extract measure mode ...
                if tags["GPS GPSMeasureMode"].values[0] == "2":
                    ans["mode"] = "2D"
                elif tags["GPS GPSMeasureMode"].values[0] == "3":
                    ans["mode"] = "3D"
                else:
                    raise Exception("the mode is unexpected", tags["GPS GPSMeasureMode"].values) from None

            # Check that the required tags are preset ...
            if "GPS GPSDOP" in tags:
                # Extract dilution of precision ...
                ans["dop"] = float(tags["GPS GPSDOP"].values[0].num) / float(tags["GPS GPSDOP"].values[0].den)              # [ratio]

                # Estimate the location error ...
                # NOTE: The longitude and latitude precisions are added in
                #       quadrature and then multiplied by the dilution of
                #       precision to give an estimate of the error, in degrees.
                #       The error is then divided by 360 to convert it to a
                #       fraction around a circle. Assuming the Earth is a
                #       perfect sphere then the fraction can be converted to a
                #       distance around its circumference.
                ans["loc_err"] = ans["dop"] * math.hypot(ans["lon_prec"], ans["lat_prec"])                                  # [°]
                ans["loc_err"] /= 360.0                                                                                     # [frac]
                ans["loc_err"] *= 2.0 * math.pi * 6371008.8                                                                 # [m]

                # Estimate the time error ...
                # NOTE: The time precision is multiplied by the dilution of
                #       precision to give an estimate of the error
                ans["time_err"] = datetime.timedelta(seconds = ans["dop"] * ans["time_prec"].total_seconds())

            # Check that there is location information ...
            if "lon" in ans and "lat" in ans:
                # Check that there is date/time information ...
                if "datetime" in ans:
                    # Check that there is altitude information ...
                    if "alt" in ans:
                        # Check that there is error information ...
                        if "loc_err" in ans and "time_err" in ans:
                            # Make a pretty string ...
                            ans["pretty"] = "GPS fix returned ({0:.6f}°, {1:.6f}°, {2:.1f}m ASL) ± {3:.1f}m at \"{4:s}\" ± {5:.3f}s.".format(
                                ans["lon"],
                                ans["lat"],
                                ans["alt"],
                                ans["loc_err"],
                                ans["datetime"].isoformat(" "),
                                ans["time_err"].total_seconds()
                            )
                        else:
                            # Make a pretty string ...
                            ans["pretty"] = "GPS fix returned ({0:.6f}°, {1:.6f}°, {2:.1f}m ASL) at \"{3:s}\".".format(
                                ans["lon"],
                                ans["lat"],
                                ans["alt"],
                                ans["datetime"].isoformat(" "),
                            )
                    else:
                        # Check that there is error information ...
                        if "loc_err" in ans and "time_err" in ans:
                            # Make a pretty string ...
                            ans["pretty"] = "GPS fix returned ({0:.6f}°, {1:.6f}°) ± {2:.1f}m at \"{3:s}\" ± {4:.3f}s.".format(
                                ans["lon"],
                                ans["lat"],
                                ans["loc_err"],
                                ans["datetime"].isoformat(" "),
                                ans["time_err"].total_seconds()
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
                        # Check that there is error information ...
                        if "loc_err" in ans:
                            # Make a pretty string ...
                            ans["pretty"] = "GPS fix returned ({0:.6f}°, {1:.6f}°, {2:.1f}m ASL) ± {3:.1f}m.".format(
                                ans["lon"],
                                ans["lat"],
                                ans["alt"],
                                ans["loc_err"]
                            )
                        else:
                            # Make a pretty string ...
                            ans["pretty"] = "GPS fix returned ({0:.6f}°, {1:.6f}°, {2:.1f}m ASL).".format(
                                ans["lon"],
                                ans["lat"],
                                ans["alt"]
                            )
                    else:
                        # Check that there is error information ...
                        if "loc_err" in ans:
                            # Make a pretty string ...
                            ans["pretty"] = "GPS fix returned ({0:.6f}°, {1:.6f}°) ± {2:.1f}m.".format(
                                ans["lon"],
                                ans["lat"],
                                ans["loc_err"]
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
