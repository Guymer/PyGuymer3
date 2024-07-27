#!/usr/bin/env python3

"""
A Python sub-module containing a bunch of random media-related functions that I
have written over the years.

Notes
-----
Copyright 2017 Thomas Guymer [1]_

References
----------
.. [1] PyGuymer3, https://github.com/Guymer/PyGuymer3
"""

# Import sub-functions ...
from .__ffprobe__ import __ffprobe__
from .does_FLAC_have_padding import does_FLAC_have_padding
from .does_media_have_audio import does_media_have_audio
from .does_media_have_RTP_hints import does_media_have_RTP_hints
from .does_media_have_subtitle import does_media_have_subtitle
from .does_media_have_video import does_media_have_video
from .does_MP4_have_free import does_MP4_have_free
from .ffprobe import ffprobe
from .images2gif import images2gif
from .images2mp4 import images2mp4
from .images2pdf import images2pdf
from .images2webp import images2webp
from .is_moov_at_beginning_of_MP4 import is_moov_at_beginning_of_MP4
from .optimize_FLAC import optimize_FLAC
from .optimize_MP4 import optimize_MP4
from .parse_CLPI_file import parse_CLPI_file
from .parse_MPLS_file import parse_MPLS_file
from .print_FLAC_blocks import print_FLAC_blocks
from .print_MP4_atoms import print_MP4_atoms
from .return_audio_bit_rate import return_audio_bit_rate
from .return_audio_channels import return_audio_channels
from .return_audio_format import return_audio_format
from .return_audio_sample_rate import return_audio_sample_rate
from .return_dict_of_bluray_playlists import return_dict_of_bluray_playlists
from .return_dict_of_bluray_playlistsToStreams import return_dict_of_bluray_playlistsToStreams
from .return_dict_of_ISO_audio_streams import return_dict_of_ISO_audio_streams
from .return_dict_of_ISO_subtitle_streams import return_dict_of_ISO_subtitle_streams
from .return_dict_of_ISO_tracks import return_dict_of_ISO_tracks
from .return_dict_of_media_audio_streams import return_dict_of_media_audio_streams
from .return_dict_of_media_subtitle_streams import return_dict_of_media_subtitle_streams
from .return_dict_of_media_video_streams import return_dict_of_media_video_streams
from .return_ISO_palette import return_ISO_palette
from .return_media_bit_rate import return_media_bit_rate
from .return_media_duration import return_media_duration
from .return_media_format import return_media_format
from .return_MP4_audio_profile import return_MP4_audio_profile
from .return_MP4_video_level import return_MP4_video_level
from .return_MP4_video_profile import return_MP4_video_profile
from .return_subtitle_bit_rate import return_subtitle_bit_rate
from .return_subtitle_extent import return_subtitle_extent
from .return_subtitle_format import return_subtitle_format
from .return_video_bit_depth import return_video_bit_depth
from .return_video_bit_rate import return_video_bit_rate
from .return_video_crop_parameters import return_video_crop_parameters
from .return_video_display_aspect_ratio import return_video_display_aspect_ratio
from .return_video_format import return_video_format
from .return_video_frame_rate import return_video_frame_rate
from .return_video_height import return_video_height
from .return_video_pixel_aspect_ratio import return_video_pixel_aspect_ratio
from .return_video_ratios import return_video_ratios
from .return_video_rotation import return_video_rotation
from .return_video_size import return_video_size
from .return_video_source_aspect_ratio import return_video_source_aspect_ratio
from .return_video_width import return_video_width
from .return_x264_crf import return_x264_crf
from .return_x264_level import return_x264_level
from .return_x264_profile import return_x264_profile
from .yuv2rgb import yuv2rgb
