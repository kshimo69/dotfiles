#!/usr/bin/env python
# -*- coding: utf-8 -*-
# Copyright (C) 2011 Shimomura Kimihiko <kshimo69@gmail.com>

""" This is a reimplementation in python to redfs.rb.
"""

import re
import sys
from optparse import OptionParser


CLASS_RE_STR = r"""\A\s*(?:
    import\a
  | from\s
  | class\s
  | raise\s
)
"""
CLASS_RE = re.compile(CLASS_RE_STR, re.X)
DEF_RE_STR = r"""\A\s*(?:
   import\a
  | from\s
  | class\s
  | raise\s
  | def\s
)
"""
DEF_RE = re.compile(DEF_RE_STR, re.X)


def main():
    parser = OptionParser(usage="usage: %prog [options] FILENAME")
    parser.add_option("-c", "--class", dest="class_only_p",
                      default=False, action="store_true",
                      help="Show only classes and import modules.")
    parser.add_option("-n", "--lineno", dest="show_line_number_p",
                      default=False, action="store_true",
                      help="Prints line number.")
    (options, args) = parser.parse_args()
    if not args:
        parser.print_help()
        sys.exit(1)

    for filename in args:
        parse_file(filename, options)


def parse_file(filename, options):
    re_type = DEF_RE
    if options.class_only_p:
        re_type = CLASS_RE

    print "-- %s --" % filename
    f = open(filename)
    line = f.readline()
    lineno = 1

    while line:
        # print line
        result = re_type.match(line)
        if result:
            if options.show_line_number_p:
                print "%4d:" % lineno,
            print line,
        line = f.readline()
        lineno += 1
    f.close()

if __name__ == '__main__':
    main()
