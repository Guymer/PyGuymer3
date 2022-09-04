def getRecordAttribute(record, attribute, kwArgCheck = None, strict = False):
    # Import special modules ...
    try:
        import cartopy
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check argument ...
    if not isinstance(record, cartopy.io.shapereader.Record):
        raise TypeError("\"record\" is not a Record") from None

    # Loop over possible attribute names ...
    for posAttribute in [attribute.upper(), attribute.lower()]:
        # Skip if it is not present ...
        if posAttribute not in record.attributes:
            continue

        # Extract the value (whilst handling Natural Earth's foibles) ...
        value = record.attributes[posAttribute]
        if isinstance(value, str):
            value = value.replace("\x00", " ").strip()

        # Check if the user wants to be strict ...
        if strict:
            # Return answer ...
            return value

        # Skip if it is bad and we want to try more possible attribute names ...
        if value == "-99":
            continue

        # Return answer ...
        return value

    # Loop over possible attribute names ...
    # NOTE: According to the developer of Natural Earth:
    #           "Because Natural Earth has a more fidelity than ISO, and tracks
    #           countries that ISO doesn't, Natural Earth maintains it's own set
    #           of 3-character codes for each admin-0 related feature."
    #       Therefore, when "ISO_A2" or "ISO_A3" are not populated I must fall
    #       back on "ISO_A2_EH" and "ISO_A3_EH" instead, see:
    #         * https://github.com/nvkelso/natural-earth-vector/issues/268
    for posAttribute in [f"{attribute.upper()}_EH", f"{attribute.lower()}_eh"]:
        # Skip if it is not present ...
        if posAttribute not in record.attributes:
            continue

        # Extract the value (whilst handling Natural Earth's foibles) ...
        value = record.attributes[posAttribute]
        if isinstance(value, str):
            value = value.replace("\x00", " ").strip()

        # Check if the user wants to be strict ...
        if strict:
            # Return answer ...
            return value

        # Skip if it is bad and we want to try more possible attribute names ...
        if value == "-99":
            continue

        # Return answer ...
        return value

    # Return answer ...
    return "ERROR"
