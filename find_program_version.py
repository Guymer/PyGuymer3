def find_program_version(prog = "python"):
    # Import modules ...
    import os
    import re
    import subprocess

    # Check if it is FreeBSD ...
    proc = subprocess.Popen(["type", "pkg"], stderr = open(os.devnull, "wt"), stdout = open(os.devnull, "wt"))
    proc.wait()
    if proc.returncode == 0:
        # Find raw string ...
        proc = subprocess.Popen(["pkg", "info", prog], stderr = subprocess.PIPE, stdout = subprocess.PIPE)
        stdout, stderr = proc.communicate()
        if proc.returncode != 0:
            raise Exception("\"pkg\" command failed")
    else:
        # Check if it is MacPorts ...
        proc = subprocess.Popen(["type", "port"], stderr = open(os.devnull, "wt"), stdout = open(os.devnull, "wt"))
        proc.wait()
        if proc.returncode == 0:
            # Find raw string ...
            proc = subprocess.Popen(["port", "info", "--version", prog], stderr = subprocess.PIPE, stdout = subprocess.PIPE)
            stdout, stderr = proc.communicate()
            if proc.returncode != 0:
                raise Exception("\"port\" command failed")
        else:
            # Check if it is OpenSUSE ...
            proc = subprocess.Popen(["type", "zypper"], stderr = open(os.devnull, "wt"), stdout = open(os.devnull, "wt"))
            proc.wait()
            if proc.returncode == 0:
                # Find raw string ...
                proc = subprocess.Popen(["zypper", "--disable-repositories", "info", prog], stderr = subprocess.PIPE, stdout = subprocess.PIPE)
                stdout, stderr = proc.communicate()
                if proc.returncode != 0:
                    raise Exception("\"zypper\" command failed")
            else:
                raise Exception("neither \"pkg\" nor \"port\" nor \"zypper\" have been found")

    # Find clean string ...
    for line in stdout.splitlines():
        match = re.match(r"^[Vv][Ee][Rr][Ss][Ii][Oo][Nn].+$", line)
        if match is None:
            continue
        return match.group(0).split(":")[1].strip()

    # Create final catch-all ...
    raise Exception("failed to extract version number for {0:s}".format(prog))
