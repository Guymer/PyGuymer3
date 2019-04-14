# -*- coding: utf-8 -*-

def jpegtran(fname):
    # Import modules ...
    import os
    import subprocess

    # Check that "jpegtran" is installed ...
    try:
        subprocess.check_call(
            [
                "type",
                "jpegtran"
            ],
            stdout = open(os.devnull, "wt"),
            stderr = open(os.devnull, "wt")
        )
    except subprocess.CalledProcessError:
        raise Exception("\"jpegtran\" is not installed")

    # Check that the JP[E]G exists ...
    if not os.path.exists(fname):
        raise Exception("\"{0:s}\" does not exist".format(fname))

    # Optimise JP[E]G ...
    try:
        subprocess.check_call(
            [
                "jpegtran",
                "-copy", "all",
                "-optimise",
                "-outfile", fname,
                "-perfect",
                fname
            ],
            stdout = open(os.devnull, "wt"),
            stderr = open(os.devnull, "wt")
        )
    except subprocess.CalledProcessError:
        raise Exception("\"jpegtran\" failed")
