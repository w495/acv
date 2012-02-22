# -*- coding: utf-8 -*-

import time
import generate.util

class Gen_css:
    def __init__(self, filename_1 = None, filename_2 = None):
        if(filename_1 and filename_2):
            self.from_file(filename_1, filename_2)
        elif(filename_1 and (None == filename_2)):
            self.from_code(filename_1)
            
    def css(self):
        return "";

    def from_code(self, filename):
        f = open(filename, "wb")
        string = "/* auto gen %s */\n%s\n"%(
            time.ctime(), generate.util.trunc(self.css()))
        f.write(string)
        f.close()

    def procstr(self, string):
        '''
            TODO:
                написать препроцессор для css, который
                1) будет автоматически заменять url(path) -> url(base64)
                2) будет обрабатывать простые шаблоны вида %(var_name)s
        '''
        pass

    def from_file(self, from_filename, to_filename):
        frmstr = open(from_filename, "rb").read()

        string = "/* auto gen %s */\n%s\n"%(
            time.ctime(), self.procstr(frmstr))

        f = open(to_filename, "wb")
        f.write(string)
        f.close()