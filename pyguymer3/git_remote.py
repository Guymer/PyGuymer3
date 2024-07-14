#!/usr/bin/env python3

# Define function ...
def git_remote(cwd, /, *, name = "origin", timeout = 60.0):
    # Import standard modules ...
    import shutil
    import subprocess

    # Check that "git" is installed ...
    if shutil.which("git") is None:
        raise Exception("\"git\" is not installed") from None

    # Find the remote URL of the repository ...
    resp = subprocess.run(
        ["git", "remote", "get-url", name],
           check = True,
             cwd = cwd,
        encoding = "utf-8",
          stderr = subprocess.STDOUT,
          stdout = subprocess.PIPE,
         timeout = timeout,
    )

    # Return answer ...
    return resp.stdout.strip()
