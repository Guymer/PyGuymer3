# -*- coding: utf-8 -*-

# NOTE: This function was obtained from a question on StackOverflow (see
#       http://stackoverflow.com/q/7041172). The licence of this function is
#       unknown but as of February 2017 the author, "wim" (see
#       http://stackoverflow.com/users/674039/wim), says "Code included below,
#       for future readers to use:". I have copied the code and use it for
#       personal use only.

def yuv2rgb(im, version = 'SDTV'):
    """
    Convert array-like YUV image to RGB colourspace

    version:
        - 'SDTV': ITU-R BT.601 version (default)
        - 'HDTV': ITU-R BT.709 version
    """

    # import modules
    import numpy

    # check input
    if not im.dtype == 'uint8':
        raise TypeError('yuv2rgb only implemented for uint8 arrays')

    # clip input to the valid range
    yuv = numpy.zeros(im.shape, dtype = numpy.float64)
    yuv[:, :,  0] = im[:, :,  0].clip(16, 235).astype(yuv.dtype) -  16
    yuv[:, :, 1:] = im[:, :, 1:].clip(16, 240).astype(yuv.dtype) - 128

    # decide what to do
    if version.upper() == 'SDTV':
        A = numpy.array(
            [
                [1.0,                    0.0,  0.701                ],
                [1.0, -0.886 * 0.114 / 0.587, -0.701 * 0.299 / 0.587],
                [1.0,  0.886                ,                    0.0]
            ],
            dtype = numpy.float64
        )
        A[:,  0] *= 255.0 / 219.0
        A[:, 1:] *= 255.0 / 112.0
    elif version.upper() == 'HDTV':
        A = numpy.array(
            [
                [1.164,    0.0,  1.793],
                [1.164, -0.213, -0.533],
                [1.164,  2.112,    0.0]
            ],
            dtype = numpy.float64
        )
    else:
        raise Exception("unrecognised version (choose 'SDTV' or 'HDTV')")

    rgb = numpy.dot(yuv, A.T)
    result = rgb.clip(0, 255).astype('uint8')

    return result
