#!/usr/bin/env python3

# Define function ...
def EXIF_datetime(
    info,
    /,
    *,
     debug = __debug__,
    maxDOP = 10.0,
    useDOP = False,
    useGPS = True,
):
    # NOTE: The Wikipedia article on DOP has a handy table on what are good or
    #       bad DOP values:
    #         * https://en.wikipedia.org/wiki/Dilution_of_precision_(navigation)

    # NOTE: It is assumed that the dictionary of EXIF data is parsed from
    #       "exiftool". There is a handy table on what all the keys are:
    #         * https://exiftool.org/TagNames/EXIF.html

    # Import standard modules ...
    import datetime

    # Check input ...
    if "EXIF" not in info:
        if debug:
            print("DEBUG: There isn't a \"EXIF\" key.")
        return False

    # Set flag ...
    actuallyUseGPS = False

    # Check if the user wants to use GPS data ...
    if useGPS:
        # Check if the user wants to use GPS DOP data ...
        if useDOP:
            # Check that all the required keys are present ...
            if all(
                [
                    "GPSDOP" in info["EXIF"],
                    "GPSDateStamp" in info["EXIF"],
                    "GPSTimeStamp" in info["EXIF"],
                ]
            ):
                # Set flag ...
                actuallyUseGPS = bool(float(info["EXIF"]["GPSDOP"]) <= maxDOP)
                if not actuallyUseGPS and debug:
                    print("DEBUG: There is GPS data but the DOP is not good enough.")
            elif debug:
                print("DEBUG: There aren't all of the \"EXIF::GPSDateStamp\" key, the \"EXIF::GPSTimeStamp\" key and the \"EXIF::GPSDOP\" key.")
        else:
            # Check that all the required keys are present ...
            if all(
                [
                    "GPSDateStamp" in info["EXIF"],
                    "GPSTimeStamp" in info["EXIF"],
                ]
            ):
                # Set flag ...
                actuallyUseGPS = True
            elif debug:
                print("DEBUG: There aren't all of the \"EXIF::GPSDateStamp\" key and the \"EXIF::GPSTimeStamp\" key.")

    # Check if GPS data is to be used ...
    if actuallyUseGPS:
        # Determine date/time that the photo was taken (assuming that the GPS
        # data is in UTC) ...
        # NOTE: My old Motorola Moto G3 took a photo at "2016:10:24 24:00:11Z"
        #       once, who knows what time that actually was. Either way,
        #       "strptime()" cannot parse it so I must work around it here.
        us = 0                                                                  # [μs]
        if "." in info["EXIF"]["GPSTimeStamp"]:
            us = info["EXIF"]["GPSTimeStamp"].split(".")[1]
            us = int(f"{us:<6s}".replace(" ", "0"))                             # [μs]
        elif debug:
            print("DEBUG: There isn't any subsecond informaton in the \"EXIF::GPSTimeStamp\" key.")
        ans = datetime.datetime.strptime(
            f'{info["EXIF"]["GPSDateStamp"]} {info["EXIF"]["GPSTimeStamp"].split(".")[0]}.{us:06d}'.replace(" 24:", " 00:"),
            "%Y:%m:%d %H:%M:%S.%f",
        ).replace(tzinfo = datetime.UTC)
    else:
        # Check input ...
        if "DateTimeOriginal" not in info["EXIF"]:
            if debug:
                print("DEBUG: There isn't a \"EXIF::DateTimeOriginal\" key.")
            return False

        # Determine date/time that the photo was taken (assuming that the EXIF
        # data is in UTC) ...
        # NOTE: My old Motorola Moto G3 took a photo at "2016:10:24 24:00:11Z"
        #       once, who knows what time that actually was. Either way,
        #       "strptime()" cannot parse it so I must work around it here.
        ans = datetime.datetime.strptime(
            info["EXIF"]["DateTimeOriginal"].replace(" 24:", " 00:"),
            "%Y:%m:%d %H:%M:%S",
        ).replace(tzinfo = datetime.UTC)

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
            ans -= datetime.timedelta(
                hours = int(info["EXIF"]["TimeZoneOffset"].split(" ")[0]),
            )
        elif debug:
            print("DEBUG: There isn't a \"EXIF::OffsetTimeOriginal\" key or a \"EXIF::TimeZoneOffset\" key.")

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
                                microseconds = 100000 * int(info["EXIF"]["SubSecTimeOriginal"]),
                            )
                        case 2:
                            ans += datetime.timedelta(
                                microseconds = 10000 * int(info["EXIF"]["SubSecTimeOriginal"]),
                            )
                        case 3:
                            ans += datetime.timedelta(
                                microseconds = 1000 * int(info["EXIF"]["SubSecTimeOriginal"]),
                            )
                        case 4:
                            ans += datetime.timedelta(
                                microseconds = 100 * int(info["EXIF"]["SubSecTimeOriginal"]),
                            )
                        case 5:
                            ans += datetime.timedelta(
                                microseconds = 10 * int(info["EXIF"]["SubSecTimeOriginal"]),
                            )
                        case 6:
                            ans += datetime.timedelta(
                                microseconds = int(info["EXIF"]["SubSecTimeOriginal"]),
                            )
                        case _:
                            raise Exception(f'\"{info["EXIF"]["SubSecTimeOriginal"]}\" is an unexpected length') from None
                case _:
                    raise Exception(f'\"{repr(type(info["EXIF"]["SubSecTimeOriginal"]))}\" is an unexpected type') from None
        elif debug:
            print("DEBUG: There isn't a \"EXIF::SubSecTimeOriginal\" key.")

    # Return answer ...
    return ans
