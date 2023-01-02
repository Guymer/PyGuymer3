def sha512(fname, kwArgCheck = None, chunksize = 1048576):
    """
    This function runs hashlib.sha512() on a file using chunks to ensure that
    too much RAM is not used.
    """

    # Import standard modules ...
    import hashlib

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Create hash object ...
    hobj = hashlib.sha512()

    # Open input file as bytes ...
    with open(fname, "rb") as fObj:
        # Start infinite loop ...
        while True:
            # Read a chunk ...
            chunk = fObj.read(chunksize)

            # Stop looping if this chunk is empty ...
            if len(chunk) == 0:
                break

            # Update hash object with chunk ...
            hobj.update(chunk)

    # Return answer ...
    return hobj.hexdigest()
