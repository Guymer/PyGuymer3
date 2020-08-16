def find_program_version(prog):
    # Import standard modules ...
    import shutil
    import subprocess

    # Try lots of different package managers ...
    if shutil.which("pkg") is not None:
        # NOTE: It is FreeBSD.

        # Find raw string ...
        proc = subprocess.Popen(
            ["pkg", "info", prog],
            encoding = "utf-8",
            stderr = subprocess.PIPE,
            stdout = subprocess.PIPE
        )
        stdout, stderr = proc.communicate()
        if proc.returncode != 0:
            raise Exception("\"pkg\" command failed")
    elif shutil.which("port") is not None:
        # NOTE: It is MacPorts.

        # Find raw string ...
        proc = subprocess.Popen(
            ["port", "info", "--version", prog],
            encoding = "utf-8",
            stderr = subprocess.PIPE,
            stdout = subprocess.PIPE
        )
        stdout, stderr = proc.communicate()
        if proc.returncode != 0:
            raise Exception("\"port\" command failed")
    elif shutil.which("zypper") is not None:
        # NOTE: It is OpenSUSE.

        # Find raw string ...
        proc = subprocess.Popen(
            ["zypper", "--disable-repositories", "info", prog],
            encoding = "utf-8",
            stderr = subprocess.PIPE,
            stdout = subprocess.PIPE
        )
        stdout, stderr = proc.communicate()
        if proc.returncode != 0:
            raise Exception("\"zypper\" command failed")
    else:
        raise Exception("neither \"pkg\" nor \"port\" nor \"zypper\" have been found")

    # Find clean string ...
    for line in stdout.splitlines():
        if line.strip().lower().startswith("version"):
            return line.strip().lower().split(":")[1].strip()

    # Create final catch-all ...
    raise Exception("failed to extract version number for \"{0:s}\"".format(prog))
