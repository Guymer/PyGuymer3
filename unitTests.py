#!/usr/bin/env python3

# Import standard modules ...
import unittest

# Import special modules ...
try:
    import numpy
except:
    raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None

# Import my modules ...
try:
    import pyguymer3
except:
    raise Exception("\"pyguymer3\" is not installed; run \"pip install --user PyGuymer3\"") from None

# Define a test case ...
class MyTestCase(unittest.TestCase):
    """
    Test the module "pyguymer3"
    """

    # Define a test ...
    def test_convertBytesToPrettyBytes(self):
        """
        Test the function "pyguymer3.convert_bytes_to_pretty_bytes()"
        """

        self.assertSequenceEqual(
            pyguymer3.convert_bytes_to_pretty_bytes(16.0 * 1024.0 * 1024.0),
            (16.0, "MiB"),
        )

    # Define a test ...
    def test_convertPrettyBytesToBytes(self):
        """
        Test the function "pyguymer3.convert_pretty_bytes_to_bytes()"
        """

        self.assertEqual(
            pyguymer3.convert_pretty_bytes_to_bytes("16.0 MiB"),
            16.0 * 1024.0 * 1024.0,
        )

    # Define a test ...
    def test_convertSecondsToPrettyTime(self):
        """
        Test the function "pyguymer3.convert_seconds_to_pretty_time()"
        """

        self.assertEqual(
            pyguymer3.convert_seconds_to_pretty_time(7326.311),
            "2h 2m 6.3s",
        )

    # Define a test ...
    def test_findIntegerDivisors(self):
        """
        Test the function "pyguymer3.find_integer_divisors()"
        """

        self.assertSequenceEqual(
            pyguymer3.find_integer_divisors(12),
            [2, 3, 4, 6],
        )

    # Define a test ...
    def test_interpolate(self):
        """
        Test the function "pyguymer3.interpolate()"
        """

        self.assertEqual(
            pyguymer3.interpolate(1.0, 3.0, 2.0, 4.0, 2.0),
            3.0,
        )

    # Define a test ...
    def test_intersection(self):
        """
        Test the function "pyguymer3.intersection()"
        """

        self.assertSequenceEqual(
            pyguymer3.intersection(
                (1.0, 3.0),
                (3.0, 1.0),
                (1.0, 1.0),
                (3.0, 3.0),
            ).tolist(),
            [2.0, 2.0],
        )

    # Define a test ...
    def test_makePathSafe(self):
        """
        Test the function "pyguymer3.make_path_safe()"
        """

        self.assertEqual(
            pyguymer3.make_path_safe(".what do you think of this path?", allowHidden = True),
            ".what do you think of this path",
        )
        self.assertEqual(
            pyguymer3.make_path_safe(".what do you think of this path?", allowHidden = False),
            "what do you think of this path",
        )

    # Define a test ...
    def test_stats(self):
        """
        Test the statistical functions "pyguymer3.mean()", "pyguymer3.var()",
        "pyguymer3.stddev()" and "pyguymer3.stderr()"
        """

        # Create datasets ...
        # NOTE: See the "Worked examples" section of https://en.wikipedia.org/wiki/Student's_t-test
        arr1 = numpy.array(
            [
                30.02,
                29.99,
                30.11,
                29.97,
                30.01,
                29.99,
            ],
            dtype = numpy.float64,
        )
        arr2 = numpy.array(
            [
                29.89,
                29.93,
                29.72,
                29.98,
                30.02,
                29.98,
            ],
            dtype = numpy.float64,
        )

        self.assertAlmostEqual(
            pyguymer3.mean(arr1),
            30.015,
        )
        self.assertAlmostEqual(
            pyguymer3.var(arr1),
            2.058333333333355e-3,
        )
        self.assertAlmostEqual(
            pyguymer3.stddev(arr1),
            4.536885862938757e-2,
        )
        self.assertAlmostEqual(
            pyguymer3.stderr(arr1),
            2.028957039137771e-2,
        )
        self.assertAlmostEqual(
            pyguymer3.mean(arr2),
            29.92,
        )
        self.assertAlmostEqual(
            pyguymer3.var(arr2),
            9.700000000000071e-3,
        )
        self.assertAlmostEqual(
            pyguymer3.stddev(arr2),
            9.848857801796140e-2,
        )
        self.assertAlmostEqual(
            pyguymer3.stderr(arr2),
            4.404543109109064e-2,
        )

# Use the proper idiom in the main module ...
# NOTE: See https://docs.python.org/3.12/library/multiprocessing.html#the-spawn-and-forkserver-start-methods
if __name__ == "__main__":
    # Run the tests ...
    unittest.main()
