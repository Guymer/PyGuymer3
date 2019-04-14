# -*- coding: utf-8 -*-

def generate_random_stub():
    # Import modules ...
    import base64
    import re

    # Define constant ...
    test = re.compile(r"[a-z][a-z][a-z][a-z]")

    # Open stream ...
    with open("/dev/random", "rb") as devrand:
        # Infinite loop ...
        while True:
            # Obtain a 4 character stub from 3 random bytes ...
            stub = base64.b64encode(devrand.read(3))

            # Exit if it is just the 4 characters I want ...
            if test.match(stub) is not None:
                return stub
