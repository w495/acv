#! /usr/bin/env python
# -*- coding: utf-8 -*-

import time
import generate.util
import generate.base

# ----------------------------------------------------------------------------

def main():
    generate.base.Base("base.css")
    return "ok";

# ----------------------------------------------------------------------------

if(__name__ == '__main__'):
    print main()

