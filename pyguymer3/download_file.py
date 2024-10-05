#!/usr/bin/env python3

# Define function ...
def download_file(
    sess,
    url,
    fname,
    /,
    *,
    cookies = None,
    headers = None,
    timeout = 10.0,
     verify = True,
):
    """GET a URL and save the content in a file

    This function performs a HTTP GET operation on a URL and saves the content
    in a file, and sets the Last-Modified time if available.

    Parameters
    ----------
    sess : requests.sessions.Session
        the :mod:`requests` session to use
    url : str
        the URL
    fname : str
        the name of the file to save the content in
    cookies : dict, optional
        the cookie jar
    headers : dict, optional
        extra headers to send
    timeout : float, optional
        the timeout of the GET request
    verify : bool, optional
        verify the server's certificates

    Returns
    -------
    resp : bool
        the success of the download

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import standard modules ...
    import email
    import email.utils
    import os

    # Import sub-functions ...
    from .download import download

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Try to download the file ...
    resp = download(
        sess,
        "get",
        url,
        cookies = cookies,
        headers = headers,
        timeout = timeout,
         verify = verify,
    )

    # Check response ...
    if resp is False:
        return False

    # Save file to disk ...
    dname = os.path.dirname(fname)
    if len(dname) > 0:
        if not os.path.exists(dname):
            os.makedirs(dname)
    with open(fname, "wb") as fObj:
        fObj.write(resp.content)

    # Change modification time if present ...
    if "Last-Modified" in resp.headers:
        modtime = email.utils.mktime_tz(email.utils.parsedate_tz(resp.headers["Last-Modified"]))
        os.utime(fname, (modtime, modtime))

    return True
