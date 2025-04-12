#!/usr/bin/env python3

# Import standard modules ...
import json
import math
import os
import shutil
import unittest

# Import special modules ...
try:
    import geojson
    geojson.geometry.Geometry.__init__.__defaults__ = (None, False, 12)         # NOTE: See https://github.com/jazzband/geojson/issues/135#issuecomment-596509669
except:
    raise Exception("\"geojson\" is not installed; run \"pip install --user geojson\"") from None
try:
    import lxml
    import lxml.etree
except:
    raise Exception("\"lxml\" is not installed; run \"pip install --user lxml\"") from None
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

    # Define constants from the environment ...
    # NOTE: As of 12/April/2025, on my MacBook Pro this is the output:
    #           % python3.13 -m unittest unitTests.py
    #           ......s.s..................
    #           ----------------------------------------------------------------------
    #           Ran 27 tests in 41.989s
    #
    #           OK (skipped=2)
    #           % ALLTESTS=1234 python3.13 -m unittest unitTests.py
    #           ...........................
    #           ----------------------------------------------------------------------
    #           Ran 27 tests in 5564.652s
    #
    #           OK
    myAllTests = bool("ALLTESTS" in os.environ)
    myDebug = bool("DEBUG" in os.environ)

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
    def test_elem2dict(self):
        """
        Test the function "pyguymer3.elem2dict()" and "pyguymer3.sha256()"
        """

        # Load input data as binary and parse it as XML ...
        with open("tests/feed.atom", "rb") as fObj:
            src = fObj.read()
        xmlElem = lxml.etree.XML(src)

        # Convert the XML element to a Python dictionary and save it as a JSON ...
        pyDict = pyguymer3.elem2dict(
            xmlElem,
            debug = self.myDebug,
        )
        with open("feed.json", "wt", encoding = "utf-8") as fObj:
            json.dump(
                pyDict,
                fObj,
                ensure_ascii = False,
                      indent = 4,
                   sort_keys = True,
            )

        # Assert result ...
        self.assertEqual(
            pyguymer3.sha256("feed.json"),
            pyguymer3.sha256("tests/feed.json"),
        )

        # Clean up ...
        os.remove("feed.json")

    # Define a test ...
    def test_findInstancesOfAFile(self):
        """
        Test the function "pyguymer3.find_instances_of_a_file()"
        """

        # Assert result ...
        self.assertSequenceEqual(
            pyguymer3.find_instances_of_a_file(
                "pyguymer3",
                "__init__.py",
                debug = self.myDebug,
            ),
            [
                "pyguymer3/__init__.py",
                "pyguymer3/f90/__init__.py",
                "pyguymer3/geo/__init__.py",
                "pyguymer3/geo/bufferSrc/__init__.py",
                "pyguymer3/geo/checkSrc/__init__.py",
                "pyguymer3/geo/cleanSrc/__init__.py",
                "pyguymer3/geo/en2llSrc/__init__.py",
                "pyguymer3/geo/fillinSrc/__init__.py",
                "pyguymer3/geo/find_middle_of_locsSrc/__init__.py",
                "pyguymer3/geo/ll2enSrc/__init__.py",
                "pyguymer3/geo/ll2merSrc/__init__.py",
                "pyguymer3/geo/max_distSrc/__init__.py",
                "pyguymer3/geo/mer2llSrc/__init__.py",
                "pyguymer3/geo/min_distSrc/__init__.py",
                "pyguymer3/image/__init__.py",
                "pyguymer3/media/CLPI/__init__.py",
                "pyguymer3/media/MPLS/__init__.py",
                "pyguymer3/media/__init__.py",
                "pyguymer3/openstreetmap/__init__.py",
            ],
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
            debug = self.myDebug,
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
    @unittest.skipUnless(myAllTests, "test takes too long for GitHub Actions")
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
                    debug = self.myDebug,
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
                debug = self.myDebug,
                 fill = -1.0,
                 nAng = 361,
                 simp = -1.0,
            ),
            dist,
            debug = self.myDebug,
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
    @unittest.skipUnless(myAllTests, "test takes too long for GitHub Actions")
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
                        debug = self.myDebug,
                         fill = -1.0,
                         nAng = 361,
                         simp = -1.0,
                    ),
                    dist,
                    debug = self.myDebug,
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
    def test_geoFindMiddleOfLocsMatrix(self):
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

        # Assert results ...
        self.assertAlmostEqual(
            pyguymer3.geo.max_dist(
                lons,
                lats,
                lons.mean(),
                lats.mean(),
                  eps = None,
                nIter = None,
                space = "EuclideanSpace",
            ),
            27.9289451900862,
        )
        self.assertAlmostEqual(
            pyguymer3.geo.max_dist(
                lons,
                lats,
                lons.mean(),
                lats.mean(),
                nIter = 1000000,
                space = "GeodesicSpace",
            ),
            2964937.2004250353,
        )
        self.assertAlmostEqual(
            pyguymer3.geo.min_dist(
                lons,
                lats,
                lons.mean(),
                lats.mean(),
                  eps = None,
                nIter = None,
                space = "EuclideanSpace",
            ),
            7.703375722764471,
        )
        self.assertAlmostEqual(
            pyguymer3.geo.min_dist(
                lons,
                lats,
                lons.mean(),
                lats.mean(),
                nIter = 1000000,
                space = "GeodesicSpace",
            ),
            746061.0481588101,
        )

        # Loop over methods (and the convergence/padding in their appropriate
        # units) ...
        for convPad, method in [
            (10000.0 / pyguymer3.RESOLUTION_OF_EARTH, "EuclideanBox"   ,),
            (10000.0 / pyguymer3.RESOLUTION_OF_EARTH, "EuclideanCircle",),
            (10000.0                                , "GeodesicBox"    ,),
            (10000.0                                , "GeodesicCircle" ,),
        ]:
            # Calculate the bounding box ...
            midLon, midLat, maxDist = pyguymer3.geo.find_middle_of_locs(
                lons,
                lats,
                   conv = convPad,                                              # ~10 km
                  debug = self.myDebug,
                 method = method,
                   nAng = 361,
                  nIter = 1000000,
                nRefine = 6,                                                    # ~156.25 m
                    pad = convPad,                                              # ~10 km
            )                                                                   # [°], [°], [°] or [m]

            # Tell "unittest" that we are doing sub-tests ...
            with self.subTest(
                  conv = convPad,
                method = method,
                   pad = convPad,
            ):
                # Assert results ...
                self.assertAlmostEqual(
                    midLon,
                    db[method]["lon"],
                )
                self.assertAlmostEqual(
                    midLat,
                    db[method]["lat"],
                )
                self.assertAlmostEqual(
                    maxDist,
                    db[method]["dist"],
                )

    # Define a test ...
    def test_geoGreatCircleMatrix(self):
        """
        Test the function "pyguymer3.geo.find_middle_of_great_circle()",
        "pyguymer3.geo.find_point_on_great_circle()" and
        "pyguymer3.geo.great_circle()"
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
            # Tell "unittest" that we are doing sub-tests ...
            with self.subTest(
                iCoord = iCoord,
            ):
                # Assert result ...
                # NOTE: This is comparing the Euclidean distance from (0°, 0°).
                self.assertAlmostEqual(
                    numpy.hypot(
                        *pyguymer3.geo.find_middle_of_great_circle(
                            coord1[0],
                            coord1[1],
                            coord2[0],
                            coord2[1],
                        )
                    ),
                    numpy.hypot(
                        *pyguymer3.geo.find_point_on_great_circle(
                            0.5,
                            coord1[0],
                            coord1[1],
                            coord2[0],
                            coord2[1],
                        )
                    ),
                )

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
                      debug = self.myDebug,
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
                        # NOTE: This is comparing the sum of all of the
                        #       Euclidean distances between each point, i.e.,
                        #       the Euclidean length of the line.
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
    def test_imageReturnImageSize(self):
        """
        Test the function "pyguymer3.image.return_image_size()"
        """

        # Assert result ...
        self.assertSequenceEqual(
            pyguymer3.image.return_image_size("tests/saveArrayAsImage/saveArrayAsImage0.png"),
            (16, 32),
        )

    # Define a test ...
    def test_imageSaveArrayAsImage(self):
        """
        Test the function "pyguymer3.image.save_array_as_image()" and
        "pyguymer3.sha256()"
        """

        # Set image size ...
        nx, ny = 16, 32                                                         # [px], [px]

        # Create array ...
        arr = numpy.zeros((ny, nx), dtype = numpy.float64)
        for ix in range(nx):
            for iy in range(ny):
                arr[iy, ix] = 0.5 * float(ix * iy)

        # **********************************************************************

        # Save array as PNGs ...
        pyguymer3.image.save_array_as_image(
            arr,
            "saveArrayAsImage0.png",
            debug = self.myDebug,
        )
        pyguymer3.image.save_array_as_image(
            arr,
            "saveArrayAsImage1.png",
            debug = self.myDebug,
            scale = True,
        )
        pyguymer3.image.save_array_as_image(
            arr,
            "saveArrayAsImage2.png",
             debug = self.myDebug,
            pc_bot = 5.0,
            pc_top = 5.0,
             scale = True,
        )
        pyguymer3.image.save_array_as_image(
            arr,
            "saveArrayAsImage3.png",
                ct = "fire",
             debug = self.myDebug,
            pc_bot = 5.0,
            pc_top = 5.0,
             scale = True,
        )
        pyguymer3.image.save_array_as_image(
            arr,
            "saveArrayAsImage4.png",
                ct = "rainbow",
             debug = self.myDebug,
            pc_bot = 5.0,
            pc_top = 5.0,
             scale = True,
        )

        # Loop over output ...
        for png in [
            "saveArrayAsImage0.png",
            "saveArrayAsImage1.png",
            "saveArrayAsImage2.png",
            "saveArrayAsImage3.png",
            "saveArrayAsImage4.png",
        ]:
            # Tell "unittest" that we are doing sub-tests ...
            with self.subTest(
                png = png,
            ):
                # Assert result ...
                self.assertEqual(
                    pyguymer3.sha256(png),
                    pyguymer3.sha256(f"tests/saveArrayAsImage/{png}"),
                )

                # Clean up ...
                os.remove(png)

        # **********************************************************************

        # Save array as PPMs ...
        pyguymer3.image.save_array_as_image(
            arr,
            "saveArrayAsImage0.ppm",
            form = "ppm",
        )
        pyguymer3.image.save_array_as_image(
            arr,
            "saveArrayAsImage1.ppm",
            form = "ppm",
            scale = True,
        )
        pyguymer3.image.save_array_as_image(
            arr,
            "saveArrayAsImage2.ppm",
              form = "ppm",
            pc_bot = 5.0,
            pc_top = 5.0,
             scale = True,
        )
        pyguymer3.image.save_array_as_image(
            arr,
            "saveArrayAsImage3.ppm",
                ct = "fire",
              form = "ppm",
            pc_bot = 5.0,
            pc_top = 5.0,
             scale = True,
        )
        pyguymer3.image.save_array_as_image(
            arr,
            "saveArrayAsImage4.ppm",
                ct = "rainbow",
              form = "ppm",
            pc_bot = 5.0,
            pc_top = 5.0,
             scale = True,
        )

        # Loop over output ...
        for ppm in [
            "saveArrayAsImage0.ppm",
            "saveArrayAsImage1.ppm",
            "saveArrayAsImage2.ppm",
            "saveArrayAsImage3.ppm",
            "saveArrayAsImage4.ppm",
        ]:
            # Tell "unittest" that we are doing sub-tests ...
            with self.subTest(
                ppm = ppm,
            ):
                # Assert result ...
                self.assertEqual(
                    pyguymer3.sha256(ppm),
                    pyguymer3.sha256(f"tests/saveArrayAsImage/{ppm}"),
                )

                # Clean up ...
                os.remove(ppm)

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
    def test_media(self):
        """
        Test the media functions "pyguymer3.media.return_MP4_audio_profile()",
        "pyguymer3.media.return_MP4_video_level()",
        "pyguymer3.media.return_MP4_video_profile()",
        "pyguymer3.media.return_audio_bit_rate()",
        "pyguymer3.media.return_audio_channels()",
        "pyguymer3.media.return_audio_format()",
        "pyguymer3.media.return_audio_sample_rate()",
        "pyguymer3.media.return_media_bit_rate()",
        "pyguymer3.media.return_media_duration()",
        "pyguymer3.media.return_media_format()",
        "pyguymer3.media.return_video_bit_depth()",
        "pyguymer3.media.return_video_bit_rate()",
        "pyguymer3.media.return_video_crop_parameters()",
        "pyguymer3.media.return_video_display_aspect_ratio()",
        "pyguymer3.media.return_video_format()",
        "pyguymer3.media.return_video_frame_rate()",
        "pyguymer3.media.return_video_height()",
        "pyguymer3.media.return_video_pixel_aspect_ratio()",
        "pyguymer3.media.return_video_ratios()",
        "pyguymer3.media.return_video_rotation()",
        "pyguymer3.media.return_video_size()",
        "pyguymer3.media.return_video_source_aspect_ratio()" and
        "pyguymer3.media.return_video_width()"

        It appears that different builds of "ffprobe" round the bit-rate
        differently. I have just had this amazing error from GitHub:

        Traceback (most recent call last):
        File "/home/runner/work/PyGuymer3/PyGuymer3/main/unitTests.py", line 606, in test_media
            self.assertEqual(
            ~~~~~~~~~~~~~~~~^
                pyguymer3.media.return_media_bit_rate("tests/BigBuckBunny.mp4"),
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                533985,
                ^^^^^^^
            )
            ^
        AssertionError: 533986 != 533985

        """

        # Assert results ...
        self.assertEqual(
            pyguymer3.media.return_MP4_audio_profile(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            "LC",
        )
        self.assertEqual(
            pyguymer3.media.return_MP4_video_level(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            40,
        )
        self.assertEqual(
            pyguymer3.media.return_MP4_video_profile(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            "High",
        )
        self.assertAlmostEqual(
            pyguymer3.media.return_audio_bit_rate(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            69631,
            delta = 1,
        )
        self.assertEqual(
            pyguymer3.media.return_audio_channels(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            1,
        )
        self.assertEqual(
            pyguymer3.media.return_audio_format(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            "AAC",
        )
        self.assertEqual(
            pyguymer3.media.return_audio_sample_rate(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            48000,
        )
        self.assertAlmostEqual(
            pyguymer3.media.return_media_bit_rate(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            533985,
            delta = 1,
        )
        self.assertAlmostEqual(
            pyguymer3.media.return_media_duration(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            634.567,
            delta = 1.0 / 60.0,         # The video is 60 fps
        )
        self.assertEqual(
            pyguymer3.media.return_media_format(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            "MP4 (ISO/IEC 14496-12)",
        )
        self.assertEqual(
            pyguymer3.media.return_video_bit_depth(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            8,
        )
        self.assertAlmostEqual(
            pyguymer3.media.return_video_bit_rate(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            452617,
            delta = 1,
        )
        self.assertSequenceEqual(
            pyguymer3.media.return_video_crop_parameters(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            (0, 2, 320, 176, "320:176:0:2"),
        )
        self.assertEqual(
            pyguymer3.media.return_video_display_aspect_ratio(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            "16:9",
        )
        self.assertEqual(
            pyguymer3.media.return_video_format(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            "H.264",
        )
        self.assertEqual(
            pyguymer3.media.return_video_frame_rate(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            60.0,
        )
        self.assertEqual(
            pyguymer3.media.return_video_height(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            180,
        )
        self.assertEqual(
            pyguymer3.media.return_video_pixel_aspect_ratio(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            "1:1",
        )
        self.assertSequenceEqual(
            pyguymer3.media.return_video_ratios(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            ("16:9", "1:1", "16:9"),
        )
        self.assertEqual(
            pyguymer3.media.return_video_rotation(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            0,
        )
        self.assertSequenceEqual(
            pyguymer3.media.return_video_size(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            (320, 180),
        )
        self.assertEqual(
            pyguymer3.media.return_video_source_aspect_ratio(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            "16:9",
        )
        self.assertEqual(
            pyguymer3.media.return_video_width(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
            320,
        )

    # Define a test ...
    def test_mediaDoesMediaHaveAudio(self):
        """
        Test the function "pyguymer3.media.does_media_have_audio()"
        """

        # Assert result ...
        self.assertTrue(
            pyguymer3.media.does_media_have_audio(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
        )

    # Define a test ...
    def test_mediaDoesMediaHaveRtpHints(self):
        """
        Test the function "pyguymer3.media.does_media_have_RTP_hints()"
        """

        # Assert result ...
        self.assertFalse(
            pyguymer3.media.does_media_have_RTP_hints(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
        )

    # Define a test ...
    def test_mediaDoesMediaHaveSubtitle(self):
        """
        Test the function "pyguymer3.media.does_media_have_subtitle()"
        """

        # Assert result ...
        self.assertFalse(
            pyguymer3.media.does_media_have_subtitle(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
        )

    # Define a test ...
    def test_mediaDoesMediaHaveVideo(self):
        """
        Test the function "pyguymer3.media.does_media_have_video()"
        """

        # Assert result ...
        self.assertTrue(
            pyguymer3.media.does_media_have_video(
                "tests/BigBuckBunny.mp4",
                debug = self.myDebug,
            ),
        )

    # Define a test ...
    def test_mediaDoesFlacHavePadding(self):
        """
        Test the function "pyguymer3.media.does_FLAC_have_padding()"
        """

        # Assert result ...
        self.assertFalse(
            pyguymer3.media.does_FLAC_have_padding("tests/BigBuckBunny.flac"),
        )

    # Define a test ...
    def test_mediaDoesMp4HaveFree(self):
        """
        Test the function "pyguymer3.media.does_MP4_have_free()"
        """

        # Assert result ...
        self.assertFalse(
            pyguymer3.media.does_MP4_have_free("tests/BigBuckBunny.mp4"),
        )

    # Define a test ...
    def test_mediaIsMoovAtBeginningOfMp4(self):
        """
        Test the function "pyguymer3.media.is_moov_at_beginning_of_MP4()"
        """

        # Assert result ...
        self.assertTrue(
            pyguymer3.media.is_moov_at_beginning_of_MP4("tests/BigBuckBunny.mp4"),
        )

    # Define a test ...
    def test_mediaX264(self):
        """
        Test the function "pyguymer3.media.return_x264_crf()",
        "pyguymer3.media.return_x264_level()" and
        "pyguymer3.media.return_x264_profile()"
        """

        # Assert results ...
        self.assertEqual(
            pyguymer3.media.return_x264_crf(1920, 800),
            22.0,
        )
        self.assertEqual(
            pyguymer3.media.return_x264_level(1920, 800),
            "4.0",
        )
        self.assertEqual(
            pyguymer3.media.return_x264_profile(1920, 800),
            "high",
        )

    # Define a test ...
    def test_returnFileList(self):
        """
        Test the function "pyguymer3.return_file_list()"
        """

        # Assert result ...
        self.assertSequenceEqual(
            pyguymer3.return_file_list(
                "tests/findMiddleOfLocs",
                debug = self.myDebug,
            ),
            [
                "tests/findMiddleOfLocs/comparison.json",
                "tests/findMiddleOfLocs/comparison.png",
                "tests/findMiddleOfLocs/lats.json",
                "tests/findMiddleOfLocs/locations.png",
                "tests/findMiddleOfLocs/lons.json",
            ],
        )

    # Define a test ...
    def test_returnFolderList(self):
        """
        Test the function "pyguymer3.return_folder_list()"
        """

        # Pretend that the scripts in "tests" have been run ...
        os.makedirs("tests/animateBufferPoint", exist_ok = True)
        os.makedirs("tests/animateExpandPoint", exist_ok = True)

        # Assert result ...
        self.assertSequenceEqual(
            pyguymer3.return_folder_list(
                "tests",
                debug = self.myDebug,
            ),
            [
                "tests/animateBufferPoint",
                "tests/animateExpandPoint",
                "tests/area",
                "tests/buffer",
                "tests/bufferPoint",
                "tests/fillin",
                "tests/findMiddleOfLocs",
                "tests/greatCircle",
                "tests/mapUnderlay",
                "tests/saveArrayAsImage",
            ],
        )

    # Define a test ...
    def test_shaOfGz(self):
        """
        Test the functions "pyguymer3.gzip()", "pyguymer3.sha256()",
        "pyguymer3.sha256_of_GZ()", "pyguymer3.sha512()" and
        "pyguymer3.sha512_of_GZ()"
        """

        # Assert result ...
        self.assertEqual(
            pyguymer3.sha256("tests/Lorem Ipsum.txt.gz"),
            pyguymer3.sha256_of_GZ("tests/Lorem Ipsum.txt.gz", ignoreModificationTime = False),
        )
        self.assertEqual(
            pyguymer3.sha512("tests/Lorem Ipsum.txt.gz"),
            pyguymer3.sha512_of_GZ("tests/Lorem Ipsum.txt.gz", ignoreModificationTime = False),
        )
        self.assertEqual(
            pyguymer3.sha256_of_GZ("tests/Lorem Ipsum.txt.gz", ignoreModificationTime = True),
            "393ac251b9c74617c0dd45b633803957134f8250fc5debe54ffb9fe659e03ac6",
        )
        self.assertEqual(
            pyguymer3.sha256_of_GZ("tests/Lorem Ipsum.txt.gz", ignoreModificationTime = False),
            "8a3b42132f26934e171c6a5202eecf73ba6cbb5a64ff6023536d293fcce54faf",
        )
        self.assertEqual(
            pyguymer3.sha512_of_GZ("tests/Lorem Ipsum.txt.gz", ignoreModificationTime = True),
            "989c6186d7e0cc8104cfe3d445e170d09d19ac29afbbbe5d328f62cbf98ef50b8ab06cdcd811f07f5522a05620d7678e5ba0dd6ec6d9c6febebee7563b4cff42",
        )
        self.assertEqual(
            pyguymer3.sha512_of_GZ("tests/Lorem Ipsum.txt.gz", ignoreModificationTime = False),
            "286f9e328bc285741dcc935d2d56d75bca907e82cc09345ecc161129ad265828aee946848e00c91acd5564a8680637e7edff52a82aa94a327bf7c8666ed7c1e4",
        )

        # Be daring and try to demonstrate the benefits of these functions by
        # making a fresh GZ file which has the same contents but different
        # metadata ...
        # NOTE: The file name cannot change otherwise the compressed file will
        #       change: "pyguymer3.sha256_of_GZ()" currently can only ignore the
        #       modification time not the file name.
        shutil.copy(
            "tests/Lorem Ipsum.txt",
            "Lorem Ipsum.txt",
        )
        pyguymer3.gzip(
            "Lorem Ipsum.txt",
        )
        self.assertNotEqual(
            pyguymer3.sha256_of_GZ("Lorem Ipsum.txt.gz", ignoreModificationTime = False),
            pyguymer3.sha256_of_GZ("tests/Lorem Ipsum.txt.gz", ignoreModificationTime = False),
        )
        self.assertEqual(
            pyguymer3.sha256_of_GZ("Lorem Ipsum.txt.gz", ignoreModificationTime = True),
            pyguymer3.sha256_of_GZ("tests/Lorem Ipsum.txt.gz", ignoreModificationTime = True),
        )
        self.assertNotEqual(
            pyguymer3.sha512_of_GZ("Lorem Ipsum.txt.gz", ignoreModificationTime = False),
            pyguymer3.sha512_of_GZ("tests/Lorem Ipsum.txt.gz", ignoreModificationTime = False),
        )
        self.assertEqual(
            pyguymer3.sha512_of_GZ("Lorem Ipsum.txt.gz", ignoreModificationTime = True),
            pyguymer3.sha512_of_GZ("tests/Lorem Ipsum.txt.gz", ignoreModificationTime = True),
        )
        os.remove("Lorem Ipsum.txt.gz")

    # Define a test ...
    def test_shaOfMp4(self):
        """
        Test the functions "pyguymer3.sha256()", "pyguymer3.sha256_of_MP4()",
        "pyguymer3.sha512()" and "pyguymer3.sha512_of_MP4()"
        """

        # Assert results ...
        self.assertEqual(
            pyguymer3.sha256("tests/BigBuckBunny.mp4"),
            pyguymer3.sha256_of_MP4("tests/BigBuckBunny.mp4", ignoreModificationTime = False),
        )
        self.assertEqual(
            pyguymer3.sha512("tests/BigBuckBunny.mp4"),
            pyguymer3.sha512_of_MP4("tests/BigBuckBunny.mp4", ignoreModificationTime = False),
        )
        self.assertEqual(
            pyguymer3.sha256_of_MP4("tests/BigBuckBunny.mp4", ignoreModificationTime = True),
            "3bf2f25a339896f6440b8b9bd90ba2b32d835620aec6147fd411ed34961c4dda",
        )
        self.assertEqual(
            pyguymer3.sha256_of_MP4("tests/BigBuckBunny.mp4", ignoreModificationTime = False),
            "fe9f981c1c2ff75306bc7c7c9d193e1683af4101221e6a567ba28c66370f1ac1",
        )
        self.assertEqual(
            pyguymer3.sha512_of_MP4("tests/BigBuckBunny.mp4", ignoreModificationTime = True),
            "1bbc3836c4fc0faa611b040582aedfef368daf3f20f095c5c5b1e4b683a9635c81304078ccfcdb9a02cf0bcb4f122a186ee34c61ed46d0814e721cd404aec46a",
        )
        self.assertEqual(
            pyguymer3.sha512_of_MP4("tests/BigBuckBunny.mp4", ignoreModificationTime = False),
            "92fa11caccbe37481ecb61a46693f5cb9cea1d95cb458ad17339153ba057455e4af40f413d79a7121314b338a2ec47874420a635e2caacafbc542a591c584356",
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
