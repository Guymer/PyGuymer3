#!/usr/bin/env python3

# Define function ...
def EXIF_datetime(
    info,
    /,
    *,
            debug = __debug__,
    gpsSupersedes = True,
           maxDOP = 10.0,
):
    # NOTE: The Wikipedia article on DOP has a handy table on what are good or
    #       bad DOP values:
    #         * https://en.wikipedia.org/wiki/Dilution_of_precision_(navigation)

    # Import standard modules ...
    import datetime

    # Perform some basic checks ...
    assert "EXIF" in info
    assert "DateTimeOriginal" in info["EXIF"]

    # Check that all the date/time information is identical (so it doesn't
    # matter which key we use later) ...
    if "DateTime" in info["EXIF"]:
        assert info["EXIF"]["DateTimeOriginal"] == info["EXIF"]["DateTime"]
    if "DateTimeDigitized" in info["EXIF"]:
        assert info["EXIF"]["DateTimeOriginal"] == info["EXIF"]["DateTimeDigitized"]

    # Check that all the time zone information is identical (so it doesn't
    # matter which key we use later) ...
    if "OffsetTimeOriginal" in info["EXIF"]:
        if "OffsetTime" in info["EXIF"]:
            assert info["EXIF"]["OffsetTimeOriginal"] == info["EXIF"]["OffsetTime"]
        if "OffsetTimeDigitized" in info["EXIF"]:
            assert info["EXIF"]["OffsetTimeOriginal"] == info["EXIF"]["OffsetTimeDigitized"]

    # Check that all the sub-second information is identical (so it doesn't
    # matter which key we use later) ...
    if "SubSecTimeOriginal" in info["EXIF"]:
        if "SubSecTime" in info["EXIF"]:
            assert info["EXIF"]["SubSecTimeOriginal"] == info["EXIF"]["SubSecTime"]
        if "SubSecTimeDigitized" in info["EXIF"]:
            assert info["EXIF"]["SubSecTimeOriginal"] == info["EXIF"]["SubSecTimeDigitized"]

    # If GPS data is present then is the DOP good enough to use the date/time
    # information ...
    goodDOP = False
    if gpsSupersedes and all(
        [
            "GPSDOP" in info["EXIF"],
            "GPSDateStamp" in info["EXIF"],
            "GPSTimeStamp" in info["EXIF"],
        ]
    ):
        goodDOP = bool(float(info["EXIF"]["GPSDOP"]) <= maxDOP)
        if not goodDOP and debug:
            print("DEBUG: There is GPS data but the DOP is not good enough.")

    # Check if the DOP is good enough (which itself can only be true if the user
    # wants to use the GPS data and that there is GPS data present to use) ...
    if goodDOP:
        # Determine date/time that the photo was taken (assuming that the GPS
        # data is in UTC) ...
        ans = datetime.datetime.strptime(
            f'{info["EXIF"]["GPSDateStamp"]} {info["EXIF"]["GPSTimeStamp"]}',
            "%Y:%m:%d %H:%M:%S",
        ).replace(tzinfo = datetime.UTC)
        if ans.microsecond != 0:
            raise Exception("the GPS data has sub-second information, this is not possible given the strptime() string") from None
    else:
        # Determine date/time that the photo was taken (assuming that the EXIF
        # data is in UTC) ...
        ans = datetime.datetime.strptime(
            info["EXIF"]["DateTimeOriginal"],
            "%Y:%m:%d %H:%M:%S",
        ).replace(tzinfo = datetime.UTC)
        if ans.microsecond != 0:
            raise Exception("the EXIF data has sub-second information, this is not possible given the strptime() string") from None

        # Assume that the offset is valid (if it is present) which means that
        # the date/time was in fact in the local time zone rather than in UTC ...
        if "OffsetTimeOriginal" in info["EXIF"]:
            if info["EXIF"]["OffsetTimeOriginal"] != "Z":
                hh, mm = info["EXIF"]["OffsetTimeOriginal"].split(":")
                ans -= datetime.timedelta(
                      hours = int(hh),
                    minutes = int(mm),
                )
        elif "TimeZoneOffset" in info["EXIF"]:
            # NOTE: According to ISO/DIS 12234-2, the two values of the
            #       "TimeZoneOffset" key are:
            #         * Time Zone Offset (in hours) of DateTimeOriginal tag
            #           value relative to Greenwich Mean Time.
            #         * If present, Time Zone Offset (in hours) of DateTime tag
            #           value relative to Greenwich Mean Time.
            #       The Internet Archive has an old copy:
            #         * https://web.archive.org/web/20050109201514/http://www.broomscloset.com/closet/photo/exif/TAG2000-22_DIS12234-2.PDF
            ans -= datetime.timedelta(
                hours = int(info["EXIF"]["TimeZoneOffset"].split(" ")[0]),
            )
        elif debug:
            print("DEBUG: There isn't a \"OffsetTimeOriginal\" key or a \"TimeZoneOffset\" key.")

    # Apply the sub-second offset (if it is present) ...
    if "SubSecTimeOriginal" in info["EXIF"]:
        match info["EXIF"]["SubSecTimeOriginal"]:
            case int():
                ans += datetime.timedelta(
                    milliseconds = info["EXIF"]["SubSecTimeOriginal"],
                )
            case str():
                match len(info["EXIF"]["SubSecTimeOriginal"]):
                    case 1:
                        ans += datetime.timedelta(
                            milliseconds = int(100 * info["EXIF"]["SubSecTimeOriginal"]),
                        )
                    case 2:
                        ans += datetime.timedelta(
                            milliseconds = int(10 * info["EXIF"]["SubSecTimeOriginal"]),
                        )
                    case 3:
                        ans += datetime.timedelta(
                            milliseconds = int(info["EXIF"]["SubSecTimeOriginal"]),
                        )
                    case _:
                        raise Exception(f'\"{info["EXIF"]["SubSecTimeOriginal"]}\" is an unexpected length') from None
            case _:
                raise Exception(f'\"{repr(type(info["EXIF"]["SubSecTimeOriginal"]))}\" is an unexpected type') from None
    elif debug:
        print("DEBUG: There isn't a \"SubSecTimeOriginal\" key.")

    # Return answer ...
    return ans
