def git_commits(kwArgCheck = None, cwd = None, fname = None):
    # Import standard modules ...
    import datetime
    import shutil
    import subprocess

    # Check keyword arguments ...
    if kwArgCheck is not None:
        print(f"WARNING: \"{__name__}\" has been called with an extra positional argument")

    # Check that "git" is installed ...
    if shutil.which("git") is None:
        raise Exception("\"git\" is not installed") from None

    # Check if the user wants the commit for the whole repository or just a
    # single file ...
    if fname is None:
        # Find the UNIX timestamps of all of the commits in the repository ...
        stamps = subprocess.check_output(
            ["git", "log", "--format=format:%ct"],
                 cwd = cwd,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
        ).splitlines()
    else:
        # Find the UNIX timestamps of all of the commits for the file ...
        stamps = subprocess.check_output(
            ["git", "log", "--follow", "--format=format:%ct", fname],
                 cwd = cwd,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
        ).splitlines()

    # Convert list of UNIX timestamps (as strings) to list of aware datetime
    # objects ...
    commits = []
    for stamp in stamps:
        commits.append(datetime.datetime.fromtimestamp(int(stamp), tz = datetime.timezone.utc))

    # Clean up ...
    del stamps

    # Return answer ...
    return commits
