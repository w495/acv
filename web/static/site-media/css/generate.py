#! /usr/bin/env python
# -*- coding: utf-8 -*-

import time
import generate.util
import generate.base

from generate.gen_config import Use as Use;

# ----------------------------------------------------------------------------

def main():
    Use.bgi_grad        = False
    Use.css3_grad       = True
    Use.bgi_corners     = False
    Use.css3_corners    = True
    generate.base.Base("base.css3.css")
    Use.bgi_grad        = True
    Use.css3_grad       = False
    Use.bgi_corners     = True 
    Use.css3_corners    = False 
    generate.base.Base("base.bgi.css")
    return "ok";

# ----------------------------------------------------------------------------

if(__name__ == '__main__'):
    print main()

