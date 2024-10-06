#!/usr/bin/env python3

# Define function ...
def git_remote(
    cwd,
    /,
    *,
    gitPath = None,
       name = "origin",
    timeout = 60.0,
):
    # Import standard modules ...
    import shutil
    import subprocess

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if gitPath is None:
        gitPath = shutil.which("git")
    assert gitPath is not None, "\"git\" is not installed"

    # Find the remote URL of the repository ...
    resp = subprocess.run(
        [
            gitPath,
            "remote",
            "get-url",
            name,
        ],
           check = True,
             cwd = cwd,
        encoding = "utf-8",
          stderr = subprocess.STDOUT,
          stdout = subprocess.PIPE,
         timeout = timeout,
    )

    # Return answer ...
    return resp.stdout.strip()
