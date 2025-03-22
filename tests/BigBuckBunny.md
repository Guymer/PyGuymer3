## Big Buck Bunny

[Bug Buck Bunny](https://studio.blender.org/projects/big-buck-bunny/) is an iconic film that was released by [Blender](https://www.blender.org/) as part of their [Open Movie](https://studio.blender.org/films/) project. It is licensed under the [Creative Commons Attribution 3.0 license](https://creativecommons.org/licenses/by/3.0/). You can [watch it on YouTube](https://www.youtube.com/watch?v=aqz-KE-bpKQ). The input/output files for the video, as well as the video itself, [can be downloaded](http://bbb3d.renderfarming.net/download.html).

On 22/March/2025, I downloaded the "native render size 60fps" video from the above website: the resultant file "bbb_sunflower_native_60fps_normal.mp4" was 831,871,244 bytes (793.3 MiB). The result of running `ffprobe7` on that file was:

```txt
ffprobe version 7.1 Copyright (c) 2007-2024 the FFmpeg developers
  built with Apple clang version 16.0.0 (clang-1600.0.26.6)
  configuration: --cc=/usr/bin/clang --datadir=/opt/local/libexec/ffmpeg7/share/data --docdir=/opt/local/libexec/ffmpeg7/share/doc --progs-suffix=7 --prefix=/opt/local/libexec/ffmpeg7 --enable-audiotoolbox --disable-indev=jack --disable-libjack --disable-libopencore-amrnb --disable-libopencore-amrwb --disable-libplacebo --disable-libvmaf --disable-libxcb --disable-libxcb-shm --disable-libxcb-xfixes --disable-metal --enable-opencl --disable-outdev=xv --enable-sdl2 --disable-securetransport --enable-videotoolbox --disable-xlib --enable-avfilter --enable-fontconfig --enable-gnutls --enable-lcms2 --enable-libass --enable-libbluray --enable-libdav1d --enable-libfreetype --enable-libfribidi --enable-libmodplug --enable-libmp3lame --enable-libopenjpeg --enable-libopus --enable-librsvg --enable-libsoxr --enable-libspeex --enable-libtheora --enable-libvorbis --enable-libvpx --enable-libwebp --enable-libzimg --enable-libzvbi --enable-lzma --enable-pthreads --enable-shared --enable-swscale --enable-zlib --enable-libaom --enable-libsvtav1 --arch=arm64 --enable-gpl --enable-libvidstab --enable-libx264 --enable-libx265 --enable-libxvid --enable-postproc --enable-libfdk-aac --enable-nonfree
  libavutil      59. 39.100 / 59. 39.100
  libavcodec     61. 19.100 / 61. 19.100
  libavformat    61.  7.100 / 61.  7.100
  libavdevice    61.  3.100 / 61.  3.100
  libavfilter    10.  4.100 / 10.  4.100
  libswscale      8.  3.100 /  8.  3.100
  libswresample   5.  3.100 /  5.  3.100
  libpostproc    58.  3.100 / 58.  3.100
Input #0, mov,mp4,m4a,3gp,3g2,mj2, from 'bbb_sunflower_native_60fps_normal.mp4':
  Metadata:
    major_brand     : isom
    minor_version   : 1
    compatible_brands: isomavc1
    creation_time   : 2013-12-24T11:10:47.000000Z
    title           : Big Buck Bunny, Sunflower version
    artist          : Blender Foundation 2008, Janus Bager Kristensen 2013
    comment         : Creative Commons Attribution 3.0 - http://bbb3d.renderfarming.net
    genre           : Animation
    composer        : Sacha Goedegebure
  Duration: 00:10:34.57, start: 0.000000, bitrate: 10487 kb/s
  Stream #0:0[0x1](und): Video: h264 (High) (avc1 / 0x31637661), yuv420p(progressive), 4000x2250 [SAR 1:1 DAR 16:9], 10002 kb/s, 60 fps, 60 tbr, 60k tbn (default)
      Metadata:
        creation_time   : 2013-12-24T11:10:47.000000Z
        handler_name    : GPAC ISO Video Handler
        vendor_id       : [0][0][0][0]
  Stream #0:1[0x2](und): Audio: mp3 (mp3float) (mp4a / 0x6134706D), 48000 Hz, stereo, fltp, 160 kb/s (default)
      Metadata:
        creation_time   : 2013-12-24T11:10:49.000000Z
        handler_name    : GPAC ISO Audio Handler
        vendor_id       : [0][0][0][0]
  Stream #0:2[0x3](und): Audio: ac3 (ac-3 / 0x332D6361), 48000 Hz, 5.1(side), fltp, 320 kb/s (default)
      Metadata:
        creation_time   : 2013-12-24T11:10:49.000000Z
        handler_name    : GPAC ISO Audio Handler
        vendor_id       : [0][0][0][0]
      Side data:
        audio service type: main
```

I then converted the video to a much lower size using the following two commands:

```sh
ffmpeg7                                                                         \
    -hide_banner                                                                \
    -probesize 1G                                                               \
    -analyzeduration 1800M                                                      \
    -i bbb_sunflower_native_60fps_normal.mp4                                    \
    -c:a libfdk_aac                                                             \
    -profile:a aac_low                                                          \
    -vbr 3                                                                      \
    -ac 1                                                                       \
    -sn                                                                         \
    -c:v libx264                                                                \
    -profile:v high                                                             \
    -preset veryslow                                                            \
    -level 4.0                                                                  \
    -crf 16.0                                                                   \
    -vf scale=320:180                                                           \
    -f mp4                                                                      \
    -map_chapters -1                                                            \
    -map_metadata -1                                                            \
    -threads 8                                                                  \
    BigBuckBunny.mp4
mp4file                                                                         \
    --optimize                                                                  \
    BigBuckBunny.mp4
```

[The resultant file](BigBuckBunny.mp4) is 42,356,196 bytes (40.4 MiB). The result of running `ffprobe7` on [the file](BigBuckBunny.mp4) is:

```txt
ffprobe version 7.1 Copyright (c) 2007-2024 the FFmpeg developers
  built with Apple clang version 16.0.0 (clang-1600.0.26.6)
  configuration: --cc=/usr/bin/clang --datadir=/opt/local/libexec/ffmpeg7/share/data --docdir=/opt/local/libexec/ffmpeg7/share/doc --progs-suffix=7 --prefix=/opt/local/libexec/ffmpeg7 --enable-audiotoolbox --disable-indev=jack --disable-libjack --disable-libopencore-amrnb --disable-libopencore-amrwb --disable-libplacebo --disable-libvmaf --disable-libxcb --disable-libxcb-shm --disable-libxcb-xfixes --disable-metal --enable-opencl --disable-outdev=xv --enable-sdl2 --disable-securetransport --enable-videotoolbox --disable-xlib --enable-avfilter --enable-fontconfig --enable-gnutls --enable-lcms2 --enable-libass --enable-libbluray --enable-libdav1d --enable-libfreetype --enable-libfribidi --enable-libmodplug --enable-libmp3lame --enable-libopenjpeg --enable-libopus --enable-librsvg --enable-libsoxr --enable-libspeex --enable-libtheora --enable-libvorbis --enable-libvpx --enable-libwebp --enable-libzimg --enable-libzvbi --enable-lzma --enable-pthreads --enable-shared --enable-swscale --enable-zlib --enable-libaom --enable-libsvtav1 --arch=arm64 --enable-gpl --enable-libvidstab --enable-libx264 --enable-libx265 --enable-libxvid --enable-postproc --enable-libfdk-aac --enable-nonfree
  libavutil      59. 39.100 / 59. 39.100
  libavcodec     61. 19.100 / 61. 19.100
  libavformat    61.  7.100 / 61.  7.100
  libavdevice    61.  3.100 / 61.  3.100
  libavfilter    10.  4.100 / 10.  4.100
  libswscale      8.  3.100 /  8.  3.100
  libswresample   5.  3.100 /  5.  3.100
  libpostproc    58.  3.100 / 58.  3.100
Input #0, mov,mp4,m4a,3gp,3g2,mj2, from 'BigBuckBunny.mp4':
  Metadata:
    major_brand     : isom
    minor_version   : 512
    compatible_brands: isomiso2avc1mp41
    encoder         : Lavf61.7.100
  Duration: 00:10:34.57, start: 0.000000, bitrate: 533 kb/s
  Stream #0:0[0x1](und): Video: h264 (High) (avc1 / 0x31637661), yuv420p(progressive), 320x180 [SAR 1:1 DAR 16:9], 452 kb/s, 60 fps, 60 tbr, 15360 tbn (default)
      Metadata:
        handler_name    : VideoHandler
        vendor_id       : [0][0][0][0]
        encoder         : Lavc61.19.100 libx264
  Stream #0:1[0x2](und): Audio: aac (LC) (mp4a / 0x6134706D), 48000 Hz, mono, fltp, 69 kb/s (default)
      Metadata:
        handler_name    : SoundHandler
        vendor_id       : [0][0][0][0]
```

I will use [BigBuckBunny.mp4](BigBuckBunny.mp4) as example input for testing my Python module.
