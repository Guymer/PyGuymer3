#!/usr/bin/env python3

# Define function ...
def optipng(fname, /, *, pool = None, timeout = 60.0):
    """
    "optipng" does not modify, and it does not touch, the image even if it
    cannot make it smaller, therefore it is safe to keep on running "optipng" on
    the same PNG over and over again.
    """

    # Import standard modules ...
    import multiprocessing
    import multiprocessing.pool
    import os
    import shutil
    import subprocess

    # Check that "optipng" is installed ...
    if shutil.which("optipng") is None:
        raise Exception("\"optipng\" is not installed") from None

    # Check that the image exists ...
    if not os.path.exists(fname):
        raise Exception(f"\"{fname}\" does not exist") from None

    # Check if the user wants to be asynchronous ...
    if isinstance(pool, multiprocessing.pool.Pool):
        # Optimise PNG asynchronously ...
        # NOTE: I have to use the "--" argument to "optipng" in case the user
        #       supplies a file name which starts with "-".
        pool.apply_async(
            subprocess.run,
            [
                [
                    "optipng",
                    "-strip", "all",
                    "--",
                    fname,
                ]
            ],
            {
                   "check" : True,
                "encoding" : "utf-8",
                  "stderr" : subprocess.DEVNULL,
                  "stdout" : subprocess.DEVNULL,
                 "timeout" : timeout,
            },
        )
    else:
        # Optimise PNG synchronously ...
        # NOTE: I have to use the "--" argument to "optipng" in case the user
        #       supplies a file name which starts with "-".
        subprocess.run(
            [
                "optipng",
                "-strip", "all",
                "--",
                fname,
            ],
               check = True,
            encoding = "utf-8",
              stderr = subprocess.DEVNULL,
              stdout = subprocess.DEVNULL,
             timeout = timeout,
        )
