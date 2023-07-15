#!/usr/bin/env python3

# Define function ...
def git_files(cwd, /, *, timeout = 60.0):
    # Import standard modules ...
    import shutil
    import subprocess

    # Check that "git" is installed ...
    if shutil.which("git") is None:
        raise Exception("\"git\" is not installed") from None

    # Find the full paths of all of the files in the repository ...
    resp = subprocess.run(
        ["git", "ls-tree", "--full-tree", "-r", "--name-only", "HEAD"],
           check = True,
             cwd = cwd,
        encoding = "utf-8",
          stderr = subprocess.STDOUT,
          stdout = subprocess.PIPE,
         timeout = timeout,
    )

    # Return answer ...
    return sorted(resp.stdout.strip().splitlines())
