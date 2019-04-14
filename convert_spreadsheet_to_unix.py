# -*- coding: utf-8 -*-

def convert_spreadsheet_to_unix(val):
    return 86400 * (val - 25569)
