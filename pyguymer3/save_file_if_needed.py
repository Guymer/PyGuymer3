def save_file_if_needed(fname, fcontent, kwArgCheck = None, debug = False):
    # Import standard modules ...
    import os

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check that the content is one of the two types allowed in Python 3 and set
    # the file access mode ...
    if isinstance(fcontent, bytes):
        mode = "b"
    elif isinstance(fcontent, str):
        mode = "t"
    else:
        raise TypeError("\"fcontent\" is an unexpected type") from None

    # Initizalize trigger ...
    save = False

    # Check if the file does not exist ...
    if not os.path.exists(fname):
        # Set trigger ...
        save = True

        # Create short-hand for the parent directory ...
        dname = os.path.dirname(fname)

        # Check that there is a parent directory in the provided file name path ...
        if dname != "":
            # Check if the parent directory does not exist ...
            if not os.path.exists(dname):
                # Make the parent directory ...
                os.makedirs(dname)
    else:
        # Check the old content ...
        if open(fname, f"r{mode}").read() != fcontent:
            # Set trigger ...
            save = True

    # Save the file if needed ...
    if save:
        if debug:
            print("INFO: Saving \"{:s}\" ...".format(fname))
        open(fname, f"w{mode}").write(fcontent)
