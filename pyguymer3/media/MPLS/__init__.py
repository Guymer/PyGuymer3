#!/usr/bin/env python3

"""
This sub-module is a native Python implementation of a parser for Blu-ray MPLS files.

Notes
-----
It has used an excellent MPLS Wiki [1]_ with a little help from a WikiBook [2]_ too. This project was started because, as of February 2018, ``ffprobe`` [3]_ v3.4 doesn't return the language information for the audio streams in a Blu-ray playlist.

For example, running ``ffprobe -probesize 3G -analyzeduration 1800M -playlist 820 bluray:/path/to/br`` yields::

    ffprobe version 3.4 Copyright (c) 2007-2017 the FFmpeg developers
      built with FreeBSD clang version 4.0.0 (tags/RELEASE_400/final 297347) (based on LLVM 4.0.0)
      configuration: --prefix=/usr/local --mandir=/usr/local/man --datadir=/usr/local/share/ffmpeg --pkgconfigdir=/usr/local/libdata/pkgconfig --enable-shared --enable-pic --enable-gpl --enable-postproc --enable-avfilter --enable-avresample --enable-pthreads --cc=cc --disable-alsa --disable-libopencore-amrnb --disable-libopencore-amrwb --enable-libass --disable-libbs2b --disable-libcaca --disable-libcdio --disable-libcelt --disable-chromaprint --disable-libdc1394 --disable-debug --disable-htmlpages --disable-libdrm --enable-libfdk-aac --disable-ffserver --disable-libflite --enable-fontconfig --enable-libfreetype --enable-frei0r --disable-libfribidi --disable-libgme --disable-libgsm --enable-iconv --disable-libilbc --disable-jack --disable-libkvazaar --disable-ladspa --enable-libmp3lame --enable-libbluray --disable-librsvg --disable-libxml2 --enable-mmx --disable-libmodplug --disable-openal --disable-opencl --enable-libopencv --disable-opengl --disable-libopenh264 --disable-libopenjpeg --enable-optimizations --disable-libopus --disable-libpulse --enable-runtime-cpudetect --disable-librubberband --disable-sdl2 --disable-libsmbclient --disable-libsnappy --disable-sndio --disable-libsoxr --disable-libspeex --enable-sse --disable-libssh --disable-libtesseract --enable-libtheora --disable-libtwolame --enable-libv4l2 --enable-vaapi --enable-vdpau --disable-libvidstab --enable-libvorbis --disable-libvo-amrwbenc --enable-libvpx --disable-libwavpack --disable-libwebp --enable-libx264 --enable-libx265 --disable-libxcb --enable-libxvid --disable-outdev=xv --disable-libzimg --disable-libzmq --disable-libzvbi --disable-gcrypt --enable-gmp --disable-librtmp --enable-gnutls --disable-openssl --enable-version3 --enable-nonfree --disable-libmysofa
      libavutil      55. 78.100 / 55. 78.100
      libavcodec     57.107.100 / 57.107.100
      libavformat    57. 83.100 / 57. 83.100
      libavdevice    57. 10.100 / 57. 10.100
      libavfilter     6.107.100 /  6.107.100
      libavresample   3.  7.  0 /  3.  7.  0
      libswscale      4.  8.100 /  4.  8.100
      libswresample   2.  9.100 /  2.  9.100
      libpostproc    54.  7.100 / 54.  7.100
    [bluray @ 0x80d07e000] 13 usable playlists:
    Input #0, mpegts, from 'bluray:/path/to/br':
      Duration: 01:01:38.57, start: 11.650667, bitrate: 33068 kb/s
      Program 1
        Stream #0:0[0x1011]: Video: h264 (High) (HDMV / 0x564D4448), yuv420p(progressive), 1920x1080 [SAR 1:1 DAR 16:9], 23.98 fps, 23.98 tbr, 90k tbn, 47.95 tbc
        Stream #0:1[0x1100]: Audio: ac3 (AC-3 / 0x332D4341), 48000 Hz, 5.1(side), fltp, 640 kb/s
        Stream #0:2[0x1101]: Audio: truehd (AC-3 / 0x332D4341), 48000 Hz, 7.1, s32 (24 bit)
        Stream #0:3[0x1101]: Audio: ac3 (AC-3 / 0x332D4341), 48000 Hz, 5.1(side), fltp, 640 kb/s
        Stream #0:4[0x1102]: Audio: ac3 (AC-3 / 0x332D4341), 48000 Hz, 5.1(side), fltp, 448 kb/s
        Stream #0:5[0x1103]: Audio: ac3 (AC-3 / 0x332D4341), 48000 Hz, stereo, fltp, 256 kb/s
        Stream #0:6[0x1104]: Audio: ac3 (AC-3 / 0x332D4341), 48000 Hz, 5.1(side), fltp, 448 kb/s
        Stream #0:7[0x1105]: Audio: ac3 (AC-3 / 0x332D4341), 48000 Hz, 5.1(side), fltp, 448 kb/s
        Stream #0:8[0x1106]: Audio: ac3 (AC-3 / 0x332D4341), 48000 Hz, 5.1(side), fltp, 448 kb/s
        Stream #0:9[0x1107]: Audio: ac3 (AC-3 / 0x332D4341), 48000 Hz, 5.1(side), fltp, 448 kb/s
        Stream #0:10[0x1200]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:11[0x1201]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:12[0x1202]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:13[0x1203]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:14[0x1204]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:15[0x1205]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:16[0x1206]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:17[0x1207]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:18[0x1208]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:19[0x1209]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:20[0x120a]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:21[0x120b]: Subtitle: hdmv_pgs_subtitle ([144][0][0][0] / 0x0090), 1920x1080
        Stream #0:22[0x1a00]: Audio: eac3 ([161][0][0][0] / 0x00A1), 48000 Hz, stereo, fltp, 192 kb/s
        Stream #0:23[0x1b00]: Video: h264 (High) (HDMV / 0x564D4448), yuv420p(progressive), 720x480 [SAR 40:33 DAR 20:11], 23.98 fps, 23.98 tbr, 90k tbn, 47.95 tbc

The Blu-ray itself has language information and opening the tiny binary file ``00820.mpls`` in a text editor shows strings such as ``eng`` and ``spa`` amongst all the gibberish. I decided that instead of writing feature requests in both ffmpeg [4]_ and libbluray [5]_ it would be *much* quicker to code up a binary reader for the file and add its data to the data provided by ``ffprobe ...`` . My function ``return_dict_of_media_audio_streams`` now calls this sub-module and adds the language code to each stream.

References
----------
.. [1] MPLS Wiki, https://github.com/lw/BluRay/wiki/MPLS
.. [2] MPLS WikiBook, https://en.wikibooks.org/wiki/User:Bdinfo/mpls
.. [3] ``ffprobe`` documentation, https://www.ffmpeg.org/ffprobe.html
.. [4] ``ffmpeg`` documentation, https://www.ffmpeg.org/
.. [5] ``libbluray`` documentation, https://www.videolan.org/developers/libbluray.html

Examples
--------
The below code will print out the entire dictionary for you.

>>> import pyguymer3
>>> import pyguymer3.media
>>> info = pyguymer3.media.parse_MPLS_file("/path/to/blu-ray", 800)
>>> import json
>>> print(json.dumps(info, ensure_ascii = False, indent = 4, sort_keys = True))

The below code ...

>>> import pyguymer3
>>> import pyguymer3.media
>>> info = pyguymer3.media.parse_MPLS_file("/path/to/blu-ray", 800)
>>> for PlayItem in info["PlayList"]["PlayItems"]:
...     for name in ["PrimaryAudioStreamEntries", "SecondaryAudioStreamEntries"]:
...         for AudioStreamEntry in PlayItem["STNTable"][name]:
...             if "StreamEntry" in AudioStreamEntry and "StreamAttributes" in AudioStreamEntry:
...                 if "RefToStreamPID" in AudioStreamEntry["StreamEntry"] and "LanguageCode" in AudioStreamEntry["StreamAttributes"]:
...                     print(AudioStreamEntry["StreamEntry"]["RefToStreamPID"], AudioStreamEntry["StreamAttributes"]["LanguageCode"])
0x1100 jpn
0x1101 eng
0x1102 fra
0x1103 ita
0x1104 deu
0x1105 spa
0x1106 eng
0x1107 eng
0x1108 eng

"""

# Import sub-functions ...
from .load_AppInfoPlayList import load_AppInfoPlayList
from .load_ExtensionData import load_ExtensionData
from .load_PlayItem import load_PlayItem
from .load_PlayList import load_PlayList
from .load_PlayListMark import load_PlayListMark
from .load_STNTable import load_STNTable
from .load_StreamAttributes import load_StreamAttributes
from .load_StreamEntry import load_StreamEntry
from .load_SubPath import load_SubPath
from .load_SubPlayItem import load_SubPlayItem
from .load_header import load_header
