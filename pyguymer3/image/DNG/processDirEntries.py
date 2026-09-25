#!/usr/bin/env python3

# Define function ...
def processDirEntries(
    dirEntries,
    fObj,
    endian,
    /,
    *,
     debug = __debug__,
    errors = "strict",
    prefix = None,
    render = True,
):
    """Process the parsed directory entries (i.e., unpack the referenced binary
    data to populate the metadata).

    Parameters
    ----------
    dirEntries : dict
        The parsed directory entries.
    fObj : io.BufferedReader
        The open DNG file object.
    endian : str
        The endianness of the DNG file.
    debug : bool, optional
        Print debug messages.
    errors : str, optional
        How to treat errors when decoding bytes to characters.
    prefix : None or str, optional
        If not None, then all JPEG data streams are saved individually with this
        path prefix.
    render : bool, optional
        Render some of the raw metadata as more useful data types or keys.

    Returns
    -------
    dirEntries : dict
        The processed parsed directory entries.

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import datetime
    import fractions
    import hashlib
    import os
    import struct

    # Import special modules ...
    try:
        import lxml
        import lxml.etree
    except:
        raise Exception("\"lxml\" is not installed; run \"pip install --user lxml\"") from None

    # Import sub-functions ...
    from .parseDirEntries import parseDirEntries

    # **************************************************************************

    # Loop over tag names in the directory entries ...
    for tagName in dirEntries.keys():
        # Catch special cases ...
        if tagName == "XMLPacket" and dirEntries[tagName]["typeName"] == "byte" and dirEntries[tagName]["count"] > 4:
            # Populate the offset, move to it and clean up ...
            ifdValOffset, = struct.unpack(
                f"{endian}1I",
                dirEntries[tagName]["valueOrOffset"],
            )                                                                   # [B]
            fObj.seek(ifdValOffset, os.SEEK_SET)
            del ifdValOffset

            # Create short-hand ...
            ifdValLength = dirEntries[tagName]["count"]                         # [B]

            # Populate the values ...
            dirEntries[tagName] = fObj.read(ifdValLength).decode(
                encoding = "utf-8",
                  errors = errors,
            )

            # Clean up ...
            del ifdValLength

            # Attempt to create a XML tree (thus checking that the string is
            # valid XML) ...
            lxml.etree.XML(dirEntries[tagName])

            # Skip to the next tag name ...
            continue

        # **********************************************************************

        # Match the type name ...
        match dirEntries[tagName]["typeName"]:
            # Catch ASCII ...
            case "ascii":
                # Create short-hand ...
                ifdValLength = dirEntries[tagName]["count"] - 1                 # [B]

                # Check if the value is too long to fit in the "value or offset"
                # field ...
                if ifdValLength + 1 > 4:
                    # Populate the offset, move to it and clean up ...
                    ifdValOffset, = struct.unpack(
                        f"{endian}1I",
                        dirEntries[tagName]["valueOrOffset"],
                    )                                                           # [B]
                    fObj.seek(ifdValOffset, os.SEEK_SET)
                    del ifdValOffset

                    # Populate the value ...
                    dirEntries[tagName] = fObj.read(ifdValLength).decode(
                        encoding = "ascii",
                          errors = errors,
                    )
                else:
                    # Populate the value ...
                    dirEntries[tagName] = dirEntries[tagName]["valueOrOffset"][0:ifdValLength].decode(
                        encoding = "ascii",
                          errors = errors,
                    )

                # Clean up ...
                del ifdValLength

            # Catch 8-bit un-signed integers ...
            case "byte":
                # Create short-hand ...
                ifdValLength = dirEntries[tagName]["count"]                     # [B]

                # Check if the total length of all values is too long to fit in
                # the "value or offset" field ...
                if ifdValLength > 4:
                    # Populate the offset, move to it and clean up ...
                    ifdValOffset, = struct.unpack(
                        f"{endian}1I",
                        dirEntries[tagName]["valueOrOffset"],
                    )                                                           # [B]
                    fObj.seek(ifdValOffset, os.SEEK_SET)
                    del ifdValOffset

                    # Populate the values ...
                    dirEntries[tagName] = struct.unpack(
                        f'{endian}{dirEntries[tagName]["count"]:d}B',
                        fObj.read(ifdValLength),
                    )
                else:
                    # Check if the value is singular or plural ...
                    if dirEntries[tagName]["count"] == 1:
                        # Populate the value ...
                        dirEntries[tagName], = struct.unpack(
                            f"{endian}1B",
                            dirEntries[tagName]["valueOrOffset"][0:ifdValLength],
                        )
                    else:
                        # Populate the values ...
                        dirEntries[tagName] = struct.unpack(
                            f'{endian}{dirEntries[tagName]["count"]:d}B',
                            dirEntries[tagName]["valueOrOffset"][0:ifdValLength],
                        )

                # Clean up ...
                del ifdValLength

            # Catch 8-bit signed integers ...
            case "sbyte":
                # Create short-hand ...
                ifdValLength = dirEntries[tagName]["count"]                     # [B]

                # Check if the total length of all values is too long to fit in
                # the "value or offset" field ...
                if ifdValLength > 4:
                    # Populate the offset, move to it and clean up ...
                    ifdValOffset, = struct.unpack(
                        f"{endian}1I",
                        dirEntries[tagName]["valueOrOffset"],
                    )                                                           # [B]
                    fObj.seek(ifdValOffset, os.SEEK_SET)
                    del ifdValOffset

                    # Populate the values ...
                    dirEntries[tagName] = struct.unpack(
                        f'{endian}{dirEntries[tagName]["count"]:d}b',
                        fObj.read(ifdValLength),
                    )
                else:
                    # Check if the value is singular or plural ...
                    if dirEntries[tagName]["count"] == 1:
                        # Populate the value ...
                        dirEntries[tagName], = struct.unpack(
                            f"{endian}1b",
                            dirEntries[tagName]["valueOrOffset"][0:ifdValLength],
                        )
                    else:
                        # Populate the values ...
                        dirEntries[tagName] = struct.unpack(
                            f'{endian}{dirEntries[tagName]["count"]:d}b',
                            dirEntries[tagName]["valueOrOffset"][0:ifdValLength],
                        )

                # Clean up ...
                del ifdValLength

            # Catch 16-bit un-signed integers ...
            case "short":
                # Create short-hand ...
                ifdValLength = 2 * dirEntries[tagName]["count"]                 # [B]

                # Check if the total length of all values is too long to fit in
                # the "value or offset" field ...
                if ifdValLength > 4:
                    # Populate the offset, move to it and clean up ...
                    ifdValOffset, = struct.unpack(
                        f"{endian}1I",
                        dirEntries[tagName]["valueOrOffset"],
                    )                                                           # [B]
                    fObj.seek(ifdValOffset, os.SEEK_SET)
                    del ifdValOffset

                    # Populate the values ...
                    dirEntries[tagName] = struct.unpack(
                        f'{endian}{dirEntries[tagName]["count"]:d}H',
                        fObj.read(ifdValLength),
                    )
                else:
                    # Check if the value is singular or plural ...
                    if dirEntries[tagName]["count"] == 1:
                        # Populate the value ...
                        dirEntries[tagName], = struct.unpack(
                            f"{endian}1H",
                            dirEntries[tagName]["valueOrOffset"][0:ifdValLength],
                        )
                    else:
                        # Populate the values ...
                        dirEntries[tagName] = struct.unpack(
                            f'{endian}{dirEntries[tagName]["count"]:d}H',
                            dirEntries[tagName]["valueOrOffset"][0:ifdValLength],
                        )

                # Clean up ...
                del ifdValLength

            # Catch 16-bit signed integers ...
            case "sshort":
                # Create short-hand ...
                ifdValLength = 2 * dirEntries[tagName]["count"]                 # [B]

                # Check if the total length of all values is too long to fit in
                # the "value or offset" field ...
                if ifdValLength > 4:
                    # Populate the offset, move to it and clean up ...
                    ifdValOffset, = struct.unpack(
                        f"{endian}1I",
                        dirEntries[tagName]["valueOrOffset"],
                    )                                                           # [B]
                    fObj.seek(ifdValOffset, os.SEEK_SET)
                    del ifdValOffset

                    # Populate the values ...
                    dirEntries[tagName] = struct.unpack(
                        f'{endian}{dirEntries[tagName]["count"]:d}h',
                        fObj.read(ifdValLength),
                    )
                else:
                    # Check if the value is singular or plural ...
                    if dirEntries[tagName]["count"] == 1:
                        # Populate the value ...
                        dirEntries[tagName], = struct.unpack(
                            f"{endian}1h",
                            dirEntries[tagName]["valueOrOffset"][0:ifdValLength],
                        )
                    else:
                        # Populate the values ...
                        dirEntries[tagName] = struct.unpack(
                            f'{endian}{dirEntries[tagName]["count"]:d}h',
                            dirEntries[tagName]["valueOrOffset"][0:ifdValLength],
                        )

                # Clean up ...
                del ifdValLength

            # Catch 32-bit un-signed integers ...
            case "long":
                # Create short-hand ...
                ifdValLength = 4 * dirEntries[tagName]["count"]                 # [B]

                # Check if the total length of all values is too long to fit in
                # the "value or offset" field ...
                if ifdValLength > 4:
                    # Populate the offset, move to it and clean up ...
                    ifdValOffset, = struct.unpack(
                        f"{endian}1I",
                        dirEntries[tagName]["valueOrOffset"],
                    )                                                           # [B]
                    fObj.seek(ifdValOffset, os.SEEK_SET)
                    del ifdValOffset

                    # Populate the values ...
                    dirEntries[tagName] = struct.unpack(
                        f'{endian}{dirEntries[tagName]["count"]:d}I',
                        fObj.read(ifdValLength),
                    )
                else:
                    # Populate the value ...
                    dirEntries[tagName], = struct.unpack(
                        f"{endian}1I",
                        dirEntries[tagName]["valueOrOffset"],
                    )

                # Clean up ...
                del ifdValLength

            # Catch 32-bit signed integers ...
            case "slong":
                # Create short-hand ...
                ifdValLength = 4 * dirEntries[tagName]["count"]                 # [B]

                # Check if the total length of all values is too long to fit in
                # the "value or offset" field ...
                if ifdValLength > 4:
                    # Populate the offset, move to it and clean up ...
                    ifdValOffset, = struct.unpack(
                        f"{endian}1I",
                        dirEntries[tagName]["valueOrOffset"],
                    )                                                           # [B]
                    fObj.seek(ifdValOffset, os.SEEK_SET)
                    del ifdValOffset

                    # Populate the values ...
                    dirEntries[tagName] = struct.unpack(
                        f'{endian}{dirEntries[tagName]["count"]:d}i',
                        fObj.read(ifdValLength),
                    )
                else:
                    # Populate the value ...
                    dirEntries[tagName], = struct.unpack(
                        f"{endian}1i",
                        dirEntries[tagName]["valueOrOffset"],
                    )

                # Clean up ...
                del ifdValLength

            # Catch 32-bit floats ...
            case "float":
                # Create short-hand ...
                ifdValLength = 4 * dirEntries[tagName]["count"]                 # [B]

                # Check if the total length of all values is too long to fit in
                # the "value or offset" field ...
                if ifdValLength > 4:
                    # Populate the offset, move to it and clean up ...
                    ifdValOffset, = struct.unpack(
                        f"{endian}1I",
                        dirEntries[tagName]["valueOrOffset"],
                    )                                                           # [B]
                    fObj.seek(ifdValOffset, os.SEEK_SET)
                    del ifdValOffset

                    # Populate the values ...
                    dirEntries[tagName] = struct.unpack(
                        f'{endian}{dirEntries[tagName]["count"]:d}f',
                        fObj.read(ifdValLength),
                    )
                else:
                    # Populate the value ...
                    dirEntries[tagName], = struct.unpack(
                        f"{endian}1f",
                        dirEntries[tagName]["valueOrOffset"],
                    )

                # Clean up ...
                del ifdValLength

            # Catch 64-bit floats ...
            case "double":
                # Create short-hand ...
                ifdValLength = 8 * dirEntries[tagName]["count"]                 # [B]

                # Populate the offset, move to it and clean up ...
                ifdValOffset, = struct.unpack(
                    f"{endian}1I",
                    dirEntries[tagName]["valueOrOffset"],
                )                                                               # [B]
                fObj.seek(ifdValOffset, os.SEEK_SET)
                del ifdValOffset

                # Populate the values ...
                dirEntries[tagName] = struct.unpack(
                    f'{endian}{dirEntries[tagName]["count"]:d}d',
                    fObj.read(ifdValLength),
                )

                # Clean up ...
                del ifdValLength

            # Catch un-signed rationals ...
            case "rational":
                # Populate the offset, move to it and clean up ...
                ifdValOffset, = struct.unpack(
                    f"{endian}1I",
                    dirEntries[tagName]["valueOrOffset"],
                )                                                               # [B]
                fObj.seek(ifdValOffset, os.SEEK_SET)
                del ifdValOffset

                # Initialize list ...
                tmp = []

                # Loop over values ...
                for _ in range(dirEntries[tagName]["count"]):
                    # Append value to list ...
                    num, den = struct.unpack(
                        f"{endian}2I",
                        fObj.read(8),
                    )
                    tmp.append(
                        fractions.Fraction(
                            num,
                            den,
                        )
                    )
                    del num, den

                # Check if the value is singular or plural ...
                if dirEntries[tagName]["count"] == 1:
                    # Populate the value ...
                    dirEntries[tagName] = tmp[0]
                else:
                    # Populate the values ...
                    dirEntries[tagName] = tuple(tmp)

                # Clean up ...
                del tmp

            # Catch signed rationals ...
            case "srational":
                # Populate the offset, move to it and clean up ...
                ifdValOffset, = struct.unpack(
                    f"{endian}1I",
                    dirEntries[tagName]["valueOrOffset"],
                )                                                               # [B]
                fObj.seek(ifdValOffset, os.SEEK_SET)
                del ifdValOffset

                # Initialize list ...
                tmp = []

                # Loop over values ...
                for _ in range(dirEntries[tagName]["count"]):
                    # Append value to list ...
                    num, den = struct.unpack(
                        f"{endian}2i",
                        fObj.read(8),
                    )
                    tmp.append(
                        fractions.Fraction(
                            num,
                            den,
                        )
                    )
                    del num, den

                # Check if the value is singular or plural ...
                if dirEntries[tagName]["count"] == 1:
                    # Populate the value ...
                    dirEntries[tagName] = tmp[0]
                else:
                    # Populate the values ...
                    dirEntries[tagName] = tuple(tmp)

                # Clean up ...
                del tmp

            # Catch undefined directory entries ...
            case "undefined":
                pass

            # Catch everything else ...
            case _:
                if debug:
                    print(f'DEBUG: {dirEntries[tagName]["typeName"]} has not been implemented yet')

    # **************************************************************************

    # Check if there is EXIF information ...
    if "ExifTag" in dirEntries:
        # Move to the offset ...
        fObj.seek(dirEntries["ExifTag"], os.SEEK_SET)

        # **********************************************************************

        # Parse the EXIF entries ...
        dirEntries["ExifTag"] = parseDirEntries(
            fObj,
            endian,
            debug = debug,
              gps = False,
        )

        # **********************************************************************

        # Process the EXIF entries in to more useful data ...
        dirEntries["ExifTag"] = processDirEntries(
            dirEntries["ExifTag"],
            fObj,
            endian,
             debug = debug,
            errors = errors,
            prefix = prefix,
            render = render,
        )

    # **************************************************************************

    # Check if there is GPS information ...
    if "GPSTag" in dirEntries:
        # Move to the offset ...
        fObj.seek(dirEntries["GPSTag"], os.SEEK_SET)

        # **********************************************************************

        # Parse the GPS entries ...
        dirEntries["GPSTag"] = parseDirEntries(
            fObj,
            endian,
            debug = debug,
              gps = True,
        )

        # **********************************************************************

        # Process the GPS entries in to more useful data ...
        dirEntries["GPSTag"] = processDirEntries(
            dirEntries["GPSTag"],
            fObj,
            endian,
             debug = debug,
            errors = errors,
            prefix = prefix,
            render = render,
        )

    # **************************************************************************

    # Check if the image identifies itself ...
    if "NewSubfileType" in dirEntries:
        # Determine the image type ...
        match dirEntries["NewSubfileType"]:
            case 0:
                imageType = "main"
            case 1:
                imageType = "thumbnail"
            case 65540:
                imageType = "semanticMask"
            case _:
                raise Exception("\"NewSubfileType\" was an unexpected type") from None

        # Initialize the value ...
        dirEntries["binary-image-data-length"] = 0                              # [B]

        # Create hash objects ...
        hObj1 = hashlib.sha256()
        hObj2 = hashlib.sha512()

        # Check if the image is defined by strips ...
        if "StripOffsets" in dirEntries and "StripByteCounts" in dirEntries:
            # Determine if there is a single strip or a tuple of multiple strips ...
            match dirEntries["StripOffsets"]:
                # Catch a single strip ...
                case int():
                    # Move to the offset ...
                    fObj.seek(dirEntries["StripOffsets"], os.SEEK_SET)

                    # Read the image data ...
                    imageData = fObj.read(dirEntries["StripByteCounts"])

                    # Update hash objects with the image data and increment
                    # length ...
                    dirEntries["binary-image-data-length"] += len(imageData)    # [B]
                    hObj1.update(imageData)
                    hObj2.update(imageData)

                    # Write JPEG if the user wants to ...
                    if prefix is not None:
                        with open(
                            f"{prefix}_{imageType}_strip.jpg",
                            mode = "wb",
                        ) as jObj:
                            jObj.write(imageData)

                    # Clean up ...
                    del imageData

                # Catch a tuple of multiple strips ...
                case tuple():
                    # Loop over strips ...
                    for iStrip, (stripOffset, stripByteCount) in enumerate(
                        zip(
                            dirEntries["StripOffsets"],
                            dirEntries["StripByteCounts"],
                            strict = True,
                        )
                    ):
                        # Move to the offset ...
                        fObj.seek(stripOffset, os.SEEK_SET)

                        # Read the image data ...
                        imageData = fObj.read(stripByteCount)

                        # Update hash objects with the image data and increment
                        # length ...
                        dirEntries["binary-image-data-length"] += len(imageData)# [B]
                        hObj1.update(imageData)
                        hObj2.update(imageData)

                        # Write JPEG if the user wants to ...
                        if prefix is not None:
                            with open(
                                f"{prefix}_{imageType}_strip{iStrip:d}.jpg",
                                mode = "wb",
                            ) as jObj:
                                jObj.write(imageData)

                        # Clean up ...
                        del imageData

                # Catch everything else ...
                case _:
                    raise Exception("\"StripOffsets\" was an unexpected type") from None

        # Check if the image is defined by tiles ...
        if "TileOffsets" in dirEntries and "TileByteCounts" in dirEntries:
            # Determine if there is a single tile or a tuple of multiple tiles ...
            match dirEntries["TileOffsets"]:
                # Catch a single tile ...
                case int():
                    # Move to the offset ...
                    fObj.seek(dirEntries["TileOffsets"], os.SEEK_SET)

                    # Read the image data ...
                    imageData = fObj.read(dirEntries["TileByteCounts"])

                    # Update hash objects with the image data and increment
                    # length ...
                    dirEntries["binary-image-data-length"] += len(imageData)    # [B]
                    hObj1.update(imageData)
                    hObj2.update(imageData)

                    # Write JPEG if the user wants to ...
                    if prefix is not None:
                        with open(
                            f"{prefix}_{imageType}_tile.jpg",
                            mode = "wb",
                        ) as jObj:
                            jObj.write(imageData)

                    # Clean up ...
                    del imageData

                # Catch a tuple of multiple tiles ...
                case tuple():
                    # Loop over tiles ...
                    for iTile, (tileOffset, tileByteCount) in enumerate(
                        zip(
                            dirEntries["TileOffsets"],
                            dirEntries["TileByteCounts"],
                            strict = True,
                        )
                    ):
                        # Move to the offset ...
                        fObj.seek(tileOffset, os.SEEK_SET)

                        # Read the image data ...
                        imageData = fObj.read(tileByteCount)

                        # Update hash objects with the image data and increment
                        # length ...
                        dirEntries["binary-image-data-length"] += len(imageData)# [B]
                        hObj1.update(imageData)
                        hObj2.update(imageData)

                        # Write JPEG if the user wants to ...
                        if prefix is not None:
                            with open(
                                f"{prefix}_{imageType}_tile{iTile:d}.jpg",
                                mode = "wb",
                            ) as jObj:
                                jObj.write(imageData)

                        # Clean up ...
                        del imageData

                # Catch everything else ...
                case _:
                    raise Exception("\"TileOffsets\" was an unexpected type") from None

        # Clean up ...
        del imageType

        # Populate the values ...
        dirEntries["binary-image-data-sha256"] = hObj1.hexdigest()
        dirEntries["binary-image-data-sha512"] = hObj2.hexdigest()

        # Clean up ...
        del hObj1, hObj2

    # **************************************************************************

    # Check if the user wants to render fields to be more useful ...
    if render:
        # Check if there is enough information to render the datetime ...
        if "DateTime" in dirEntries:
            # Create short-hands and clean up ...
            yyyymmdd, hhmmdd = dirEntries["DateTime"].split(" ")
            yyyymmdd = yyyymmdd.replace(":", "-")
            dtStr = f"{yyyymmdd}T{hhmmdd}"
            del yyyymmdd, hhmmdd

            # Check if there is enough information for the datetime to have
            # sub-second resolution ...
            if "ExifTag" in dirEntries and "SubSecTime" in dirEntries["ExifTag"]:
                # Update short-hand ...
                dtStr += f'.{dirEntries["ExifTag"]["SubSecTime"]}'

                # Clean up ...
                del dirEntries["ExifTag"]["SubSecTime"]

            # Check if there is enough information for the datetime to be aware ...
            if "ExifTag" in dirEntries and "OffsetTime" in dirEntries["ExifTag"]:
                # Update short-hand ...
                dtStr += dirEntries["ExifTag"]["OffsetTime"]

                # Clean up ...
                del dirEntries["ExifTag"]["OffsetTime"]

            # Render the datetime and clean up ...
            dirEntries["DateTime"] = datetime.datetime.fromisoformat(dtStr)
            del dtStr

        # Check if there is enough information to render the datetime ...
        if "DateTimeDigitized" in dirEntries:
            # Create short-hands and clean up ...
            yyyymmdd, hhmmdd = dirEntries["DateTimeDigitized"].split(" ")
            yyyymmdd = yyyymmdd.replace(":", "-")
            dtStr = f"{yyyymmdd}T{hhmmdd}"
            del yyyymmdd, hhmmdd

            # Check if there is enough information for the datetime to have
            # sub-second resolution ...
            if "SubSecTimeDigitized" in dirEntries:
                # Update short-hand ...
                dtStr += f'.{dirEntries["SubSecTimeDigitized"]}'

                # Clean up ...
                del dirEntries["SubSecTimeDigitized"]

            # Check if there is enough information for the datetime to be aware ...
            if "OffsetTimeDigitized" in dirEntries:
                # Update short-hand ...
                dtStr += dirEntries["OffsetTimeDigitized"]

                # Clean up ...
                del dirEntries["OffsetTimeDigitized"]

            # Render the datetime and clean up ...
            dirEntries["DateTimeDigitized"] = datetime.datetime.fromisoformat(dtStr)
            del dtStr

        # Check if there is enough information to render the datetime ...
        if "DateTimeOriginal" in dirEntries:
            # Create short-hands and clean up ...
            yyyymmdd, hhmmdd = dirEntries["DateTimeOriginal"].split(" ")
            yyyymmdd = yyyymmdd.replace(":", "-")
            dtStr = f"{yyyymmdd}T{hhmmdd}"
            del yyyymmdd, hhmmdd

            # Check if there is enough information for the datetime to have
            # sub-second resolution ...
            if "SubSecTimeOriginal" in dirEntries:
                # Update short-hand ...
                dtStr += f'.{dirEntries["SubSecTimeOriginal"]}'

                # Clean up ...
                del dirEntries["SubSecTimeOriginal"]

            # Check if there is enough information for the datetime to be aware ...
            if "OffsetTimeOriginal" in dirEntries:
                # Update short-hand ...
                dtStr += dirEntries["OffsetTimeOriginal"]

                # Clean up ...
                del dirEntries["OffsetTimeOriginal"]

            # Render the datetime and clean up ...
            dirEntries["DateTimeOriginal"] = datetime.datetime.fromisoformat(dtStr)
            del dtStr

        # Check if there is enough information to render the GPS datetime ...
        if "GPSDateStamp" in dirEntries and "GPSTimeStamp" in dirEntries:
            # Create short-hands and clean up ...
            dtStr = dirEntries["GPSDateStamp"].replace(":", "-")
            hh, mm, ss = dirEntries["GPSTimeStamp"]
            if hh.is_integer():
                dtStr += f"T{round(hh):02d}"
                del hh
            else:
                raise Exception("non-integer hour fractions not (yet) implemented, please send me this example") from None
            if mm.is_integer():
                dtStr += f":{round(mm):02d}"
                del mm
            else:
                raise Exception("non-integer minute fractions not (yet) implemented, please send me this example") from None
            if ss.is_integer():
                dtStr += f":{round(ss):02d}"
                del ss
            else:
                raise Exception("non-integer second fractions not (yet) implemented, please send me this example") from None

            # Render the datetime and clean ...
            dirEntries["GPSDateTime"] = datetime.datetime.fromisoformat(f"{dtStr}Z")

            # Clean up ...
            del dirEntries["GPSDateStamp"], dirEntries["GPSTimeStamp"]

        # Check if there is enough information to render the GPS latitude ...
        if "GPSLatitude" in dirEntries and "GPSLatitudeRef" in dirEntries:
            # Determine the latitude reference ...
            match dirEntries["GPSLatitudeRef"]:
                # Catch north ...
                case "N":
                    # Render the latitude ...
                    # NOTE: A single "/" character and no ".0" appears to be the
                    #       only way to ensure that the result is still a
                    #       "fractions.Fraction".
                    dirEntries["GPSLatitude"] = (
                        dirEntries["GPSLatitude"][0]
                        + dirEntries["GPSLatitude"][1] / 60
                        + dirEntries["GPSLatitude"][2] / 3600
                    )
                    assert isinstance(dirEntries["GPSLatitude"], fractions.Fraction)

                # Catch south ...
                case "S":
                    # Render the latitude ...
                    # NOTE: A single "/" character and no ".0" appears to be the
                    #       only way to ensure that the result is still a
                    #       "fractions.Fraction".
                    dirEntries["GPSLatitude"] = -(
                        dirEntries["GPSLatitude"][0]
                        + dirEntries["GPSLatitude"][1] / 60
                        + dirEntries["GPSLatitude"][2] / 3600
                    )
                    assert isinstance(dirEntries["GPSLatitude"], fractions.Fraction)

                # Catch everything else ...
                case _:
                    raise Exception("\"GPSLatitudeRef\" was an unexpected type") from None

            # Clean up ...
            del dirEntries["GPSLatitudeRef"]

        # Check if there is enough information to render the GPS longitude ...
        if "GPSLongitude" in dirEntries and "GPSLongitudeRef" in dirEntries:
            # Determine the longitude reference ...
            match dirEntries["GPSLongitudeRef"]:
                # Catch east ...
                case "E":
                    # Render the longitude ...
                    # NOTE: A single "/" character and no ".0" appears to be the
                    #       only way to ensure that the result is still a
                    #       "fractions.Fraction".
                    dirEntries["GPSLongitude"] = (
                        dirEntries["GPSLongitude"][0]
                        + dirEntries["GPSLongitude"][1] / 60
                        + dirEntries["GPSLongitude"][2] / 3600
                    )
                    assert isinstance(dirEntries["GPSLongitude"], fractions.Fraction)

                # Catch west ...
                case "W":
                    # Render the longitude ...
                    # NOTE: A single "/" character and no ".0" appears to be the
                    #       only way to ensure that the result is still a
                    #       "fractions.Fraction".
                    dirEntries["GPSLongitude"] = -(
                        dirEntries["GPSLongitude"][0]
                        + dirEntries["GPSLongitude"][1] / 60
                        + dirEntries["GPSLongitude"][2] / 3600
                    )
                    assert isinstance(dirEntries["GPSLongitude"], fractions.Fraction)

                # Catch everything else ...
                case _:
                    raise Exception("\"GPSLongitudeRef\" was an unexpected type") from None

            # Clean up ...
            del dirEntries["GPSLongitudeRef"]

    # **************************************************************************

    # Return answer ...
    return dirEntries
