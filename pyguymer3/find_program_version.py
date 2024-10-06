#!/usr/bin/env python3

# Define function ...
def find_program_version(
    prog,
    /,
    *,
       pkgPath = None,
      portPath = None,
       timeout = 60.0,
    zypperPath = None,
):
    # Import standard modules ...
    import shutil
    import subprocess

    # **************************************************************************

    # Try to find the paths if the user did not provide them ...
    if pkgPath is None:
        pkgPath = shutil.which("pkg")
    if portPath is None:
        portPath = shutil.which("port")
    if zypperPath is None:
        zypperPath = shutil.which("zypper")

    # Try lots of different package managers ...
    if pkgPath is not None:
        # NOTE: It is FreeBSD.

        # Find raw string ...
        resp = subprocess.run(
            [
                pkgPath,
                "info",
                prog,
            ],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
              stdout = subprocess.PIPE,
             timeout = timeout,
        )
    elif portPath is not None:
        # NOTE: It is MacPorts.

        # Find raw string ...
        resp = subprocess.run(
            [
                portPath,
                "info",
                "--version",
                prog,
            ],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
              stdout = subprocess.PIPE,
             timeout = timeout,
        )
    elif zypperPath is not None:
        # NOTE: It is OpenSUSE.

        # Find raw string ...
        resp = subprocess.run(
            [
                zypperPath,
                "--disable-repositories",
                "info",
                prog,
            ],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.STDOUT,
              stdout = subprocess.PIPE,
             timeout = timeout,
        )
    else:
        raise Exception("neither \"pkg\" nor \"port\" nor \"zypper\" have been found") from None

    # Find clean string ...
    for line in resp.stdout.splitlines():
        if line.strip().lower().startswith("version"):
            return line.strip().lower().split(":")[1].strip()

    # Create final catch-all ...
    raise Exception(f"failed to extract version number for \"{prog}\"") from None
