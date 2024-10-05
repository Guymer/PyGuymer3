#!/usr/bin/env python3

# Define function ...
def checkSudokuBoard(
    board,
    /,
):
    """Check a Sudoku board is valid

    This function reads in a 2D array representing the values in a Sudoku board
    and check that it is valid.

    Parameters
    ----------
    board : numpy.ndarray
        the Sudoku board

    Returns
    -------
    valid : bool
        the validity of the Sudoku board

    Notes
    -----
    Copyright 2017 Thomas Guymer [1]_

    References
    ----------
    .. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
    """

    # Import special modules ...
    try:
        import numpy
    except:
        raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

    # Check arguments ...
    if not isinstance(board, numpy.ndarray):
        raise TypeError("\"board\" is not a NumPy array") from None
    if board.shape != (9, 9):
        raise TypeError("\"board\" is not the correct shape") from None

    # Loop over numbers ...
    for num in range(1, 10):
        # Loop over columns ...
        for iy in range(9):
            # Check that the number is not in the row more than once ...
            if (board[iy, :] == num).sum() > 1:
                # Return answer ...
                return False

        # Loop over rows ...
        for ix in range(9):
            # Check that the number is not in the column more than once ...
            if (board[:, ix] == num).sum() > 1:
                # Return answer ...
                return False

        # Loop over y-squares ...
        for sy in range(3):
            # Loop over x-squares ...
            for sx in range(3):
                # Check that the number is not in the square more than once ...
                if (board[sy * 3:(sy + 1) * 3, sx * 3:(sx + 1) * 3] == num).sum() > 1:
                    # Return answer ...
                    return False

    # Return answer ...
    return True
