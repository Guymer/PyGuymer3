#!/usr/bin/env python3

# Define function ...
def git_files(
    cwd,
    /,
    *,
    gitPath = None,
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

    # Find the full paths of all of the files in the repository ...
    resp = subprocess.run(
        [
            gitPath,
            "ls-tree",
            "--full-tree",
            "-r",
            "--name-only",
            "HEAD",
        ],
           check = True,
             cwd = cwd,
        encoding = "utf-8",
          stderr = subprocess.STDOUT,
          stdout = subprocess.PIPE,
         timeout = timeout,
    )

    # Return answer ...
    return sorted(resp.stdout.strip().splitlines())
