#!/usr/bin/env python3

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.10/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # This is a test suite for “save_array_as_image()”.

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Import my modules ...
    try:
        import pyguymer3
        import pyguymer3.image
    except:
        raise Exception("\"pyguymer3\" is not installed; you need to have the Python module from https://github.com/Guymer/PyGuymer3 located somewhere in your $PYTHONPATH") from None

    print(f"Testing \"{pyguymer3.__path__[0]}\" ...")

    # Set image size ...
    nx, ny = 16, 32                                                             # [px], [px]

    # Create array ...
    arr = numpy.zeros((ny, nx), dtype = numpy.float64)
    for ix in range(nx):
        for iy in range(ny):
            arr[iy, ix] = 0.5 * float(ix * iy)

    # Save array as PNGs ...
    pyguymer3.image.save_array_as_image(arr, "saveArrayAsImage0.png")
    pyguymer3.image.save_array_as_image(arr, "saveArrayAsImage1.png", scale = True)
    pyguymer3.image.save_array_as_image(arr, "saveArrayAsImage2.png", scale = True, pc_bot = 5.0, pc_top = 5.0)
    pyguymer3.image.save_array_as_image(arr, "saveArrayAsImage3.png", scale = True, pc_bot = 5.0, pc_top = 5.0, ct = "fire")
    pyguymer3.image.save_array_as_image(arr, "saveArrayAsImage4.png", scale = True, pc_bot = 5.0, pc_top = 5.0, ct = "rainbow")
