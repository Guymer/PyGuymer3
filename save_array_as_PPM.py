# -*- coding: utf-8 -*-

def save_array_as_PPM(img, fname):
    # Write out PPM ...
    with open(fname, "wb") as fobj:
        fobj.write("P6 {0:d} {1:d} 255 ".format(img.shape[1], img.shape[0]))
        img.tofile(fobj)
