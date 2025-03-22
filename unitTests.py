#!/usr/bin/env python3

# Import standard modules ...
import json
import math
import unittest

# Import special modules ...
try:
    import geojson
    geojson.geometry.Geometry.__init__.__defaults__ = (None, False, 12)     # NOTE: See https://github.com/jazzband/geojson/issues/135#issuecomment-596509669
except:
    raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
try:
    import numpy
except:
    raise Exception("\"numpy\" is not installed; run \"pip install --user numpy\"") from None
try:
    import shapely
    import shapely.geometry
except:
    raise Exception("\"shapely\" is not installed; run \"pip install --user Shapely\"") from None

# Import my modules ...
try:
    import pyguymer3
    import pyguymer3.geo
    import pyguymer3.image
    import pyguymer3.media
    import pyguymer3.openstreetmap
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

        # Assert result ...
        self.assertSequenceEqual(
            pyguymer3.convert_bytes_to_pretty_bytes(16.0 * 1024.0 * 1024.0),
            (16.0, "MiB"),
        )

    # Define a test ...
    def test_convertPrettyBytesToBytes(self):
        """
        Test the function "pyguymer3.convert_pretty_bytes_to_bytes()"
        """

        # Assert result ...
        self.assertEqual(
            pyguymer3.convert_pretty_bytes_to_bytes("16.0 MiB"),
            16.0 * 1024.0 * 1024.0,
        )

    # Define a test ...
    def test_convertSecondsToPrettyTime(self):
        """
        Test the function "pyguymer3.convert_seconds_to_pretty_time()"
        """

        # Assert result ...
        self.assertEqual(
            pyguymer3.convert_seconds_to_pretty_time(7326.311),
            "2h 2m 6.3s",
        )

    # Define a test ...
    def test_findIntegerDivisors(self):
        """
        Test the function "pyguymer3.find_integer_divisors()"
        """

        # Assert result ...
        self.assertSequenceEqual(
            pyguymer3.find_integer_divisors(12),
            [2, 3, 4, 6],
        )

    # Define a test ...
    def test_geoBufferArea(self):
        """
        Test the geospatial functions "pyguymer3.geo.buffer()" and
        "pyguymer3.geo.area()"
        """

        # Define buffering distance and calculate the area exactly ...
        dist = 1000.0                                                           # [m]
        exactArea = math.pi * pow(dist, 2)                                      # [m²]

        # Create Point ...
        pnt = shapely.geometry.point.Point(
            -1.0,
            50.5,
        )

        # Buffer Point ...
        buff = pyguymer3.geo.buffer(
            pnt,
            dist,
            debug = False,
             fill = -1.0,
             nAng = 361,
             simp = -1.0,
        )

        # Find area ...
        estimArea = pyguymer3.geo.area(
            buff,
            level = 6,
        )                                                                       # [m²]

        # Assert result ...
        self.assertLessEqual(
            abs(estimArea - exactArea) / exactArea,
            1.0e-4,
        )

    # Define a test ...
    @unittest.skip("test takes too long for GitHub Actions")
    def test_geoBufferAreaMatrix(self):
        """
        Test the geospatial functions "pyguymer3.geo.buffer()" and
        "pyguymer3.geo.area()"
        """

        # Define buffering distance and calculate the area exactly ...
        dist = 1000.0                                                           # [m]
        exactArea = math.pi * pow(dist, 2)                                      # [m²]

        # Loop over longitudes ...
        for lon in range(-180, +180, 5):
            # Loop over latitudes ...
            for lat in range(-90, +90, 5):
                # Create Point ...
                pnt = shapely.geometry.point.Point(
                    float(lon) + 2.5,
                    float(lat) + 2.5,
                )

                # Buffer Point ...
                buff = pyguymer3.geo.buffer(
                    pnt,
                    dist,
                    debug = False,
                     fill = -1.0,
                     nAng = 361,
                     simp = -1.0,
                )

                # Find area ...
                estimArea = pyguymer3.geo.area(
                    buff,
                    level = 6,
                )                                                               # [m²]

                # Tell "unittest" that we are doing sub-tests ...
                with self.subTest(
                    lon = float(lon) + 2.5,
                    lat = float(lat) + 2.5,
                ):
                    # Assert result ...
                    self.assertLessEqual(
                        abs(estimArea - exactArea) / exactArea,
                        1.0e-4,
                    )

    # Define a test ...
    def test_geoBufferBufferArea(self):
        """
        Test the geospatial functions "pyguymer3.geo.buffer()" and
        "pyguymer3.geo.area()"
        """

        # Define buffering distance and calculate the area exactly ...
        dist = 1000.0                                                           # [m]
        exactArea = math.pi * pow(2.0 * dist, 2)                                # [m²]

        # Create Point ...
        pnt = shapely.geometry.point.Point(
            -1.0,
            50.5,
        )

        # Buffer Point twice ...
        buff = pyguymer3.geo.buffer(
            pyguymer3.geo.buffer(
                pnt,
                dist,
                debug = False,
                 fill = -1.0,
                 nAng = 361,
                 simp = -1.0,
            ),
            dist,
            debug = False,
             fill = -1.0,
             nAng = 361,
             simp = -1.0,
        )

        # Find area ...
        estimArea = pyguymer3.geo.area(
            buff,
            level = 6,
        )                                                                       # [m²]

        # Assert result ...
        self.assertLessEqual(
            abs(estimArea - exactArea) / exactArea,
            1.0e-2,
        )

    # Define a test ...
    @unittest.skip("test takes too long for GitHub Actions")
    def test_geoBufferBufferAreaMatrix(self):
        """
        Test the geospatial functions "pyguymer3.geo.buffer()" and
        "pyguymer3.geo.area()"
        """

        # Define buffering distance and calculate the area exactly ...
        dist = 1000.0                                                           # [m]
        exactArea = math.pi * pow(2.0 * dist, 2)                                # [m²]

        # Loop over longitudes ...
        for lon in range(-180, +180, 5):
            # Loop over latitudes ...
            for lat in range(-90, +90, 5):
                # Create Point ...
                pnt = shapely.geometry.point.Point(
                    float(lon) + 2.5,
                    float(lat) + 2.5,
                )

                # Buffer Point twice ...
                buff = pyguymer3.geo.buffer(
                    pyguymer3.geo.buffer(
                        pnt,
                        dist,
                        debug = False,
                         fill = -1.0,
                         nAng = 361,
                         simp = -1.0,
                    ),
                    dist,
                    debug = False,
                     fill = -1.0,
                     nAng = 361,
                     simp = -1.0,
                )

                # Find area ...
                estimArea = pyguymer3.geo.area(
                    buff,
                    level = 6,
                )                                                               # [m²]

                # Tell "unittest" that we are doing sub-tests ...
                with self.subTest(
                    lon = float(lon) + 2.5,
                    lat = float(lat) + 2.5,
                ):
                    # Assert result ...
                    self.assertLessEqual(
                        abs(estimArea - exactArea) / exactArea,
                        1.0e-2,
                    )

    # Define a test ...
    def test_geoFindMiddleOfLocsGeodesicBox(self):
        """
        Test the function "pyguymer3.geo.find_middle_of_locs()"
        """

        # Load data ...
        with open("tests/findMiddleOfLocs/comparison.json", "rt", encoding = "utf-8") as fObj:
            db = json.load(fObj)

        # Load data and convert to NumPy array ...
        with open("tests/findMiddleOfLocs/lons.json", "rt", encoding = "utf-8") as fObj:
            lons = json.load(fObj)                                              # [°]
        lons = numpy.array(lons, dtype = numpy.float64)                         # [°]

        # Load data and convert to NumPy array ...
        with open("tests/findMiddleOfLocs/lats.json", "rt", encoding = "utf-8") as fObj:
            lats = json.load(fObj)                                              # [°]
        lats = numpy.array(lats, dtype = numpy.float64)                         # [°]

        # Calculate the Geodesic bounding box ...
        midLon, midLat, maxDist = pyguymer3.geo.find_middle_of_locs(
            lons,
            lats,
               conv = 10000.0,                                                  # 10 km
              debug = False,
             method = "GeodesicBox",
               nAng = 361,
              nIter = 1000000,
            nRefine = 6,                                                        # 156.25 m
                pad = 10000.0,                                                  # 10 km
        )                                                                       # [°], [°], [m]

        # Assert results ...
        self.assertAlmostEqual(
            midLon,
            db["GeodesicBox"]["lon"],
        )
        self.assertAlmostEqual(
            midLat,
            db["GeodesicBox"]["lat"],
        )
        self.assertAlmostEqual(
            maxDist,
            db["GeodesicBox"]["dist"],
        )

    # Define a test ...
    @unittest.skip("test takes too long for GitHub Actions")
    def test_geoFindMiddleOfLocsGeodesicCircle(self):
        """
        Test the function "pyguymer3.geo.find_middle_of_locs()"
        """

        # Load data ...
        with open("tests/findMiddleOfLocs/comparison.json", "rt", encoding = "utf-8") as fObj:
            db = json.load(fObj)

        # Load data and convert to NumPy array ...
        with open("tests/findMiddleOfLocs/lons.json", "rt", encoding = "utf-8") as fObj:
            lons = json.load(fObj)                                              # [°]
        lons = numpy.array(lons, dtype = numpy.float64)                         # [°]

        # Load data and convert to NumPy array ...
        with open("tests/findMiddleOfLocs/lats.json", "rt", encoding = "utf-8") as fObj:
            lats = json.load(fObj)                                              # [°]
        lats = numpy.array(lats, dtype = numpy.float64)                         # [°]

        # Calculate the Geodesic bounding circle ...
        midLon, midLat, maxDist = pyguymer3.geo.find_middle_of_locs(
            lons,
            lats,
               conv = 10000.0,                                                  # 10 km
              debug = False,
             method = "GeodesicCircle",
               nAng = 361,
              nIter = 1000000,
            nRefine = 6,                                                        # 156.25 m
                pad = 10000.0,                                                  # 10 km
        )                                                                       # [°], [°], [m]

        # Assert results ...
        self.assertAlmostEqual(
            midLon,
            db["GeodesicCircle"]["lon"],
        )
        self.assertAlmostEqual(
            midLat,
            db["GeodesicCircle"]["lat"],
        )
        self.assertAlmostEqual(
            maxDist,
            db["GeodesicCircle"]["dist"],
        )

    # Define a test ...
    def test_geoGreatCircle(self):
        """
        Test the function "pyguymer3.geo.great_circle()"
        """

        # Define pairs of coordinates ...
        coords1 = [
            ( -90.0, +15.0),                                                    # [°], [°]
            (  +1.0, +50.7),                                                    # [°], [°]
            (-122.4, +37.6),                                                    # [°], [°]
            ( +91.0, +15.0),                                                    # [°], [°]
        ]
        coords2 = [
            ( +90.0, +15.0),                                                    # [°], [°]
            (-178.0, -88.0),                                                    # [°], [°]
            (+140.4, +35.8),                                                    # [°], [°]
            ( -91.0, +15.0),                                                    # [°], [°]
        ]

        # Define number of points ...
        npoints = [3, 4, 8, 16, 1000]                                           # [#]

        # Loop over tests ...
        for iCoord, (coord1, coord2) in enumerate(
            zip(
                coords1,
                coords2,
                strict = True,
            )
        ):
            # Loop over number of points ...
            for iPoint, npoint in enumerate(npoints):
                # Load GeoJSON ...
                with open(f"tests/greatCircle/greatCircle{iCoord:d}_{iPoint:d}.geojson", "rt", encoding = "utf-8") as fObj:
                    savedCircle = shapely.geometry.shape(geojson.load(fObj))

                # Find the great circle ...
                calculatedCircle = pyguymer3.geo.great_circle(
                    coord1[0],
                    coord1[1],
                    coord2[0],
                    coord2[1],
                      debug = False,
                    maxdist = None,
                      nIter = None,
                     npoint = npoint,
                )

                # Loop over lines in the great circles ...
                for iLine, (savedLine, calculatedLine) in enumerate(
                    zip(
                        pyguymer3.geo.extract_lines(savedCircle),
                        pyguymer3.geo.extract_lines(calculatedCircle),
                        strict = True,
                    )
                ):
                    # Extract the coordinates from the lines ...
                    savedCoords = numpy.array(savedLine.coords)                 # [°]
                    calculatedCoords = numpy.array(calculatedLine.coords)       # [°]

                    # Tell "unittest" that we are doing sub-tests ...
                    with self.subTest(
                        iCoord = iCoord,
                        iPoint = iPoint,
                         iLine = iLine,
                    ):
                        # Assert result ...
                        self.assertAlmostEqual(
                            numpy.hypot(
                                numpy.diff(calculatedCoords[:, 0]),
                                numpy.diff(calculatedCoords[:, 1]),
                            ).sum(),
                            numpy.hypot(
                                numpy.diff(savedCoords[:, 0]),
                                numpy.diff(savedCoords[:, 1]),
                            ).sum(),
                        )

    # Define a test ...
    def test_interpolate(self):
        """
        Test the function "pyguymer3.interpolate()"
        """

        # Assert result ...
        self.assertEqual(
            pyguymer3.interpolate(1.0, 3.0, 2.0, 4.0, 2.0),
            3.0,
        )

    # Define a test ...
    def test_intersection(self):
        """
        Test the function "pyguymer3.intersection()"
        """

        # Assert result ...
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

        # Assert results ...
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

        # Assert results ...
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
