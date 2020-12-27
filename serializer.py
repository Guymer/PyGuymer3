def serializer(obj, evaluate = False):
    """
    Serializer for objects not serializable by Python's default json code.

    Arguments:
    obj -- the object to be serialized

    Keyword Arguments:
    evaluate -- should fractions be evaluated and returned as floats or should
        they be returned as strings of the form "X/Y"? (default False)
    """

    # Import standard modules ...
    import datetime

    # Import special modules ...
    try:
        import exifread
    except:
        raise Exception("\"exifread\" is not installed; run \"pip install --user ExifRead\"") from None

    # Check type ...
    if isinstance(obj, datetime.date):
        # Return serializable answer ...
        return obj.isoformat()

    # Check type ...
    if isinstance(obj, datetime.time):
        # Return serializable answer ...
        return obj.isoformat()

    # Check type ...
    if isinstance(obj, datetime.datetime):
        # Return serializable answer ...
        return obj.isoformat()

    # Check type ...
    if isinstance(obj, datetime.timedelta):
        # Return serializable answer ...
        return obj.total_seconds()

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
        return "{:d}/{:d}".format(obj.num, obj.den)

    # Catch errors ...
    raise TypeError("\"obj\" is an unexpected type", type(obj)) from None
