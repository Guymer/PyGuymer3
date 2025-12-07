#!/usr/bin/env python3

# Define function ...
def getRecordAttribute(
    record,
    attribute,
    /,
    *,
    strict = False,
):
    """Get an attribute of a Shapefile record

    This function gets an attribute of a Shapefile record, falling back on
    different attributes to work around inconsistencies in records in Shapefiles
    obtained from `Natural Earth <https://www.naturalearthdata.com>`_.

    For example, if the user wants the "ISO_A2" attribute then this function
    (with ``strict = False``) will return the first value it finds (which does
    *not* equal "-99") from:

    * ``record.attributes["ISO_A2"]``
    * ``record.attributes["iso_a2"]``
    * ``record.attributes["ISO_A2_EH"]``
    * ``record.attributes["iso_a2_eh"]``

    Parameters
    ----------
    record : cartopy.io.shapereader.Record
        the record
    attribute : str
        the attribute
    debug : bool, optional
        if ``True`` then return "-99" if that is what the value is; if ``False``
        then continue looping through the possibilities

    Returns
    -------
    value : str
        the value

    Notes
    -----
    According to the developer of Natural Earth [1]_ :

        "Because Natural Earth has a more fidelity than ISO, and tracks
        countries that ISO doesn't, Natural Earth maintains it's own set of
        3-character codes for each admin-0 related feature."

    Therefore, when "ISO_A2" or "ISO_A3" are not populated I must fall back on
    "ISO_A2_EH" and "ISO_A3_EH" instead.

    Copyright 2017 Thomas Guymer [2]_

    References
    ----------
    .. [1] developer comment on issue #268 of Natural Earth, https://github.com/nvkelso/natural-earth-vector/issues/268#issuecomment-778832542
    .. [2] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import pathlib

    # Import special modules ...
    try:
        import cartopy
        cartopy.config.update(
            {
                "cache_dir" : pathlib.PosixPath("~/.local/share/cartopy").expanduser(),
            }
        )
    except:
        raise Exception("\"cartopy\" is not installed; run \"pip install --user Cartopy\"") from None

    # **************************************************************************

    # Check argument ...
    assert isinstance(record, cartopy.io.shapereader.Record), "\"record\" is not a Record"

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
