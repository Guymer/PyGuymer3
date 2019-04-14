# -*- coding: utf-8 -*-

def generate_password():
    # Import modules ...
    import base64
    import re

    # Define constant ...
    test = re.compile(r"[a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9][a-zA-Z0-9]")

    # Open stream ...
    with open("/dev/random", "rb") as devrand:
        # Clear the password ...
        passwd = ""

        # Infinite loop ...
        while True:
            # Obtain a 4 character stub from 3 random bytes ...
            stub = base64.b64encode(devrand.read(3))

            # Check that it is just the 4 characters I want and add it to the password ...
            if test.match(stub) is not None:
                passwd += stub

                # Exit if the password is the correct length ...
                if len(passwd) == 20:
                    return passwd
