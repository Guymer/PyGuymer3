#!/usr/bin/env python3

# Define function ...
def generate_password():
    # Import standard modules ...
    import secrets
    import string

    # Return twenty randomly/securely chosen digits and ASCII letters ...
    return "".join(secrets.choice(string.digits + string.ascii_letters) for i in range(20))
