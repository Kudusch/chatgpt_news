#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys

fname = sys.argv[1]

with open(fname, 'rb') as f:
    hexdata = f.read().hex()

hexdata = map(''.join, zip(hexdata[::2], hexdata[1::2]))
hexdata = list(hexdata)
new_hexdata = []

# Ä:c4
# Ü:dc
# Ö:d6
# ä:e4
# ö:f6
# ü:fc
# ß:df
# ó:f3
# ś:15b

for i, b in enumerate(hexdata):
    if int(b, 16) >= 128:
        if b == "8a":
            hexdata[i] = "f6"
        elif b == "9f":
            hexdata[i] = "fc"
        elif b == "80":
            hexdata[i] = "c4"
        elif b == "85":
            hexdata[i] = "d6"
        elif b == "86":
            hexdata[i] = "dc"
        elif b == "96":
            hexdata[i] = "fc"
        elif b in ["c4","dc","d6","e4","f6","fc","df", "f3", "15b", "e9"]:
            hexdata[i] = b
        else:
            # hexdata[i] = "00"
            # window = bytes.fromhex("".join(hexdata[i-10:i+10]))
            # if (not b in ["96", "93", "84", "92", "ed"]):
            #     print(str(window))
            #     print(b)
            #     print("---\n")
            hexdata[i] = ""

with open(fname, 'wb') as f:
    f.write(bytes.fromhex("".join(hexdata)))
