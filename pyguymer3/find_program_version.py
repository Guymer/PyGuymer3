#!/usr/bin/env python3

# Define function ...
def find_program_version(prog, /):
    # Import standard modules ...
    import shutil
    import subprocess

    # Try lots of different package managers ...
    if shutil.which("pkg") is not None:
        # NOTE: It is FreeBSD.

        # Find raw string ...
        resp = subprocess.run(
            ["pkg", "info", prog],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
              stdout = subprocess.PIPE,
        )
    elif shutil.which("port") is not None:
        # NOTE: It is MacPorts.

        # Find raw string ...
        resp = subprocess.run(
            ["port", "info", "--version", prog],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
              stdout = subprocess.PIPE,
        )
    elif shutil.which("zypper") is not None:
        # NOTE: It is OpenSUSE.

        # Find raw string ...
        resp = subprocess.run(
            ["zypper", "--disable-repositories", "info", prog],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
              stdout = subprocess.PIPE,
        )
    else:
        raise Exception("neither \"pkg\" nor \"port\" nor \"zypper\" have been found") from None

    # Find clean string ...
    for line in resp.stdout.splitlines():
        if line.strip().lower().startswith("version"):
            return line.strip().lower().split(":")[1].strip()

    # Create final catch-all ...
    raise Exception(f"failed to extract version number for \"{prog}\"") from None
