#!/usr/bin/env python3

# NOTE: The following pages are useful guides and references for Sphinx:
#         * https://www.sphinx-doc.org/en/master/usage/restructuredtext/basics.html
#         * https://github.com/ralsina/rst-cheatsheet/blob/master/rst-cheatsheet.rst
#         * https://sphinx-tutorial.readthedocs.io
#         * https://www.ericholscher.com/blog/2016/jul/1/sphinx-and-rtd-for-writers/

# NOTE: The following pages are useful guides and references for the automation:
#         * https://www.sphinx-doc.org/en/master/usage/extensions/autodoc.html
#         * https://www.sphinx-doc.org/en/master/usage/extensions/autosummary.html

# NOTE: The following pages are useful guides and references for NumPy-style
#       documentation:
#         * https://numpydoc.readthedocs.io/en/latest/format.html#docstring-standard
#       To see examples of documentation syntax see:
#         * https://sphinxcontrib-napoleon.readthedocs.io/en/latest/example_numpy.html

# Import standard modules ...
import os
import sys

# Import special modules ...
import sphinx_rtd_theme

# Expand $PYTHONPATH so that PyGuymer3 can be imported later ...
sys.path.insert(0, os.path.abspath(".."))

# Set project information ...
project = "PyGuymer3"
copyright = "2023, Thomas Guymer"                                               # pylint: disable=W0622
author = "Thomas Guymer"

# Add Sphinx extension modules ...
extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.autosummary",
    "sphinx.ext.intersphinx",
    "sphinx.ext.linkcode",
    "sphinx.ext.napoleon",
]

# Configure Sphinx extension modules ...
autosummary_generate = True
intersphinx_mapping = {
          "h5py" : ("https://docs.h5py.org/en/latest/", None),
    "matplotlib" : ("https://matplotlib.org/stable/", None),
         "numpy" : ("https://numpy.org/doc/stable/", None),
        "python" : ("https://docs.python.org/3", None),
         "scipy" : ("https://docs.scipy.org/doc/scipy/", None),
}
napoleon_google_docstring = False

# Define function ...
def linkcode_resolve(domain, info, /, *, branch = "main", repository = "PyGuymer3"):
    """Resolve the link on GitHub for a piece of code.

    Parameters
    ----------
    domain : str
        the domain of the code
    info : dict
        information about the code
    branch : str, optional
        the branch in the repository on GitHub
    repository : str, optional
        the repository on GitHub

    Returns
    -------
    link : str
        the link

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Skip bad domains ...
    if domain != "py":
        return None

    # Skip domains with missing information ...
    if not info["module"]:
        return None
    if not info["fullname"]:
        return None

    # Return answer ...
    return f'https://github.com/Guymer/{repository}/blob/{branch}/{info["module"].replace(".", "/")}/{info["fullname"]}.py'

# Set the HTML theme ...
html_theme = "sphinx_rtd_theme"
html_theme_path = [sphinx_rtd_theme.get_html_theme_path()]

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files ...
exclude_patterns = ["_build", "Thumbs.db", ".DS_Store"]
