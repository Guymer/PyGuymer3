#!/usr/bin/env python3

# Define function ...
def download_header(sess, url, /, *, cookies = None, headers = None, timeout = 10.0, verify = True):
    """HEAD a URL and return the headers

    This function performs a HTTP HEAD operation on a URL and returns the
    headers.

    Parameters
    ----------
    sess : requests.sessions.Session
        the :mod:`requests` session to use
    url : str
        the URL
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
    resp : bool, bytes
        `False` if unsuccessful or a `dict` if successful

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import sub-functions ...
    from .download import download

    # Populate default values ...
    if cookies is None:
        cookies = {}
    if headers is None:
        headers = {}

    # **************************************************************************

    # Try to get the headers ...
    resp = download(
        sess,
        "head",
        url,
        cookies = cookies,
        headers = headers,
        timeout = timeout,
         verify = verify,
    )

    # Check response ...
    if resp is False:
        return False

    return resp.headers
