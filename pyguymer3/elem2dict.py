def elem2dict(elem, kwArgCheck = None, debug = False):
    """Convert a XML element into a Python dictionary

    This function accepts a XML element and recursively turns it in to a Python
    dictionary.

    Parameters
    ----------
    elem : lxml.etree._Element
        the XML element
    debug : bool, optional
        print debug messages

    Returns
    -------
    ans : dict
        the XML element as a Python dictionary

    Notes
    -----
    If present, non-empty text will be mapped to a "$$TEXT$$" dictionary field
    (it is assumed that the XML does not contain any tags called "$$TEXT$$"
    otherwise). If present, non-empty attributes will be mapped to "::KEY::"
    dictionary fields (it is assumed that the XML does not contain any tags
    called "::KEY::" otherwise). If present, child elements will be mapped to
    "KEY" dictionary fields. If multiple child elements have identical tag names
    then the "KEY" dictionary field will be a list of them.
    """

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Initialize dictionary ...
    ans = {}

    # Check if it has text ...
    if elem.text:
        # Check that the text isn't empty ...
        if elem.text.strip():
            ans["$$TEXT$$"] = elem.text
        elif debug:
            print(f"INFO: Skipping empty text in \"{elem.tag}\".")

    # Loop over attributes ...
    for key, value in elem.attrib.iteritems():
        # Check that the attribute isn't empty ...
        if value.strip():
            # Check if the attribute can be dealt with specially ...
            if value.isdigit():
                ans[f"::{key}::"] = int(value)
            elif value.lower() == "true":
                ans[f"::{key}::"] = True
            elif value.lower() == "false":
                ans[f"::{key}::"] = False
            else:
                ans[f"::{key}::"] = value
        elif debug:
            print(f"INFO: Skipping empty \"{key}\" attribute in \"{elem.tag}\".")

    # Loop over children ...
    for child in elem.iterchildren():
        # Convert child to a dictionary ...
        value = elem2dict(child)

        # Check if a field already exists ...
        if child.tag in ans:
            # Check if the field is already a list ...
            if isinstance(ans[child.tag], list):
                # Append to list ...
                ans[child.tag].append(value)
            else:
                # Convert the field to a list ...
                ans[child.tag] = [ans[child.tag], value]
        else:
            # Set field ...
            ans[child.tag] = value

    # Return answer ...
    return ans
