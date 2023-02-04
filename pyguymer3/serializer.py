#!/usr/bin/env python3

# Define function ...
def serializer(obj, /, *, evaluate = False):
    """Serialize an object into an intrinsic type

    Serializer for objects not serializable by Python's default :mod:`json`
    code. Converts all non-intrinsic types to intrinsic types.

    Parameters
    ----------
    obj : anything
        object to be serialized
    evaluate : bool, optional
        evaluate exifread.utils.Ratio objects into floats, otherwise return as string of form "X/Y" (default false)

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import datetime

    # Import special modules ...
    try:
        import exifread
    except:
        raise Exception("\"exifread\" is not installed; run \"pip install --user ExifRead\"") from None
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # **************************************************************************

    # Check type ...
    if isinstance(obj, datetime.date):
        # Return serializable answer ...
        return obj.isoformat(sep = "T", timespec = "microseconds")

    # Check type ...
    if isinstance(obj, datetime.time):
        # Return serializable answer ...
        return obj.isoformat(sep = "T", timespec = "microseconds")

    # Check type ...
    if isinstance(obj, datetime.datetime):
        # Return serializable answer ...
        return obj.isoformat(sep = "T", timespec = "microseconds")

    # Check type ...
    if isinstance(obj, datetime.timedelta):
        # Return serializable answer ...
        return obj.total_seconds()

    # **************************************************************************

    # Check type ...
    if isinstance(obj, exifread.utils.Ratio):
        # Check if it is an integer mascarading as a fraction ...
        if obj.den == 1:
            return obj.num

        # Check if the user wants to evaluate fractions ...
        if evaluate:
            # Catch floating-point exceptions ...
            if obj.den == 0:
                # Check sign ...
                if obj.num < 0:
                    # Return serializable answer ...
                    return float("-inf")

                # Return serializable answer ...
                return float("inf")

            # Return serializable answer ...
            return float(obj.num) / float(obj.den)

        # Return serializable answer ...
        return f"{obj.num:d}/{obj.den:d}"

    # **************************************************************************

    # Check type ...
    if isinstance(obj, numpy.int8):
        # Return serializable answer ...
        return int(obj)

    # Check type ...
    if isinstance(obj, numpy.int16):
        # Return serializable answer ...
        return int(obj)

    # Check type ...
    if isinstance(obj, numpy.int32):
        # Return serializable answer ...
        return int(obj)

    # Check type ...
    if isinstance(obj, numpy.int64):
        # Return serializable answer ...
        return int(obj)

    # Check type ...
    if isinstance(obj, numpy.uint8):
        # Return serializable answer ...
        return int(obj)

    # Check type ...
    if isinstance(obj, numpy.uint16):
        # Return serializable answer ...
        return int(obj)

    # Check type ...
    if isinstance(obj, numpy.uint32):
        # Return serializable answer ...
        return int(obj)

    # Check type ...
    if isinstance(obj, numpy.uint64):
        # Return serializable answer ...
        return int(obj)

    # Check type ...
    if isinstance(obj, numpy.float16):
        # Return serializable answer ...
        return float(obj)

    # Check type ...
    if isinstance(obj, numpy.float32):
        # Return serializable answer ...
        return float(obj)

    # Check type ...
    if isinstance(obj, numpy.float64):
        # Return serializable answer ...
        return float(obj)

    # **************************************************************************

    # Catch errors ...
    raise TypeError("\"obj\" is an unexpected type", type(obj)) from None
