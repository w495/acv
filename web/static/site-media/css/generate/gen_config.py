# -*- coding: utf-8 -*-

USE_CSS3 = True # False True
USE_BGI = not USE_CSS3

def d(cls):
    return cls.__dict__

class Use:
    compilation     = True
    bgi_grad        = USE_BGI
    css3_grad       = USE_CSS3
    bgi_corners     = USE_BGI
    css3_corners    = USE_CSS3

class Metrics:
    gwidth = "%spx"%(1000)
    rheight = "%spx"%(700)
    header_loffset = "%spx"%(23)
    header_roffset = "%spx"%(23)

class Colors:
    green = "";
