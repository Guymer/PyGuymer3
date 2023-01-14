#!/usr/bin/env python3

# Define function ...
def generate_random_stub():
    # Import standard modules ...
    import secrets
    import string

    # Return four randomly/securely chosen lower-case ASCII letters ...
    return "".join(secrets.choice(string.ascii_lowercase) for i in range(4))
