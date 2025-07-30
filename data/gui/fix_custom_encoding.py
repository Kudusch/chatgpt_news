#!/usr/bin/env python3
# -*- coding: utf-8 -*-

import sys

fname = sys.argv[1]

with open(fname, 'rb') as f:
    hexdata = f.read().hex()

hexdata = map(''.join, zip(hexdata[::2], hexdata[1::2]))
hexdata = list(hexdata)
new_hexdata = []
for i, b in enumerate(hexdata):
    if int(b, 16) >= 128:
        if b == "8a":
            hexdata[i] = "f6"
        elif b == "9f":
            hexdata[i] = "fc"
        elif b == "9a":
            hexdata[i] = "f6"
        elif b == "80":
            hexdata[i] = "c4"
        elif b == "85":
            hexdata[i] = "d6"
        elif b == "86":
            hexdata[i] = "dc"
        else:
            hexdata[i] = ""

with open(fname, 'wb') as f:
    f.write(bytes.fromhex("".join(hexdata)))
