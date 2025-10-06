#!/usr/bin/env python3

# Define function ...
def elem2dict(
    elem,
    /,
    *,
       debug = __debug__,
    simplify = False,
):
    """Convert a XML element into a Python dictionary

    This function accepts a XML element and recursively turns it in to a Python
    dictionary.

    Parameters
    ----------
    elem : lxml.etree._Element
        the XML element
    debug : bool, optional
        print debug messages
    simplify : bool, optional
        Simplify XML elements. If true, then XML elements which only contain
        text (i.e., no attributes and no children) are collapsed to be simply
        `key = "value"` rather than `key = { "$$TEXT$$" : "value" }`.

    Returns
    -------
    ans : dict
        the XML element as a Python dictionary

    Notes
    -----
    If present, non-empty text will be mapped to a "$$TEXT$$" dictionary field
    (it is assumed that the XML does not contain any tags called "$$TEXT$$"
    otherwise).

    If present, non-empty attributes will be mapped to "::KEY::" dictionary
    fields (it is assumed that the XML does not contain any tags called
    "::KEY::" otherwise).

    If present, child elements will be mapped to "KEY" dictionary fields. If
    multiple child elements have identical tag names then the "KEY" dictionary
    field will be a list of them.

    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Initialize dictionary ...
    ans = {}

    # Check if it has text ...
    if elem.text:
        # Check that the text isn't empty ...
        if elem.text.strip():
            # Check if the text can be dealt with specially ...
            if elem.text.strip().lower() == "true":
                ans["$$TEXT$$"] = True
            elif elem.text.strip().lower() == "false":
                ans["$$TEXT$$"] = False
            else:
                ans["$$TEXT$$"] = elem.text
        elif debug:
            print(f"INFO: Skipping empty text in a \"{elem.tag}\" tag.")

    # Loop over attributes ...
    for key, value in elem.attrib.iteritems():
        # Check that the attribute isn't empty ...
        if value.strip():
            # Check if the attribute can be dealt with specially ...
            if value.strip().lower() == "true":
                ans[f"::{key}::"] = True
            elif value.strip().lower() == "false":
                ans[f"::{key}::"] = False
            else:
                ans[f"::{key}::"] = value
        elif debug:
            print(f"INFO: Skipping empty \"{key}\" attribute in a \"{elem.tag}\" tag.")

    # Loop over children ...
    for child in elem.iterchildren():
        # Convert child to a dictionary ...
        value = elem2dict(
            child,
               debug = debug,
            simplify = simplify,
        )

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

    # Simplify dictionary ...
    if simplify:
        if list(ans.keys()) == ["$$TEXT$$",]:
            if debug:
                print(f"INFO: Simplifying a text-only \"{elem.tag}\" tag.")
            ans = ans["$$TEXT$$"]

    # Return answer ...
    return ans
