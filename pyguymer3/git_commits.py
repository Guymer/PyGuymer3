#!/usr/bin/env python3

# Define function ...
def git_commits(
    cwd,
    /,
    *,
      fname = None,
    timeout = 60.0,
):
    # Import standard modules ...
    import datetime
    import shutil
    import subprocess

    # Check that "git" is installed ...
    if shutil.which("git") is None:
        raise Exception("\"git\" is not installed") from None

    # Check if the user wants the commit for the whole repository or just a
    # single file ...
    if fname is None:
        # Find the UNIX timestamps of all of the commits in the repository ...
        resp = subprocess.run(
            ["git", "log", "--format=format:%ct"],
               check = True,
                 cwd = cwd,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
              stdout = subprocess.PIPE,
             timeout = timeout,
        )
    else:
        # Find the UNIX timestamps of all of the commits for the file ...
        resp = subprocess.run(
            ["git", "log", "--follow", "--format=format:%ct", fname],
               check = True,
                 cwd = cwd,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
              stdout = subprocess.PIPE,
             timeout = timeout,
        )

    # Convert list of UNIX timestamps (as strings) to list of aware datetime
    # objects ...
    commits = []
    for line in resp.stdout.strip().splitlines():
        commits.append(datetime.datetime.fromtimestamp(int(line), tz = datetime.UTC))

    # Return answer ...
    return commits
