#!/usr/bin/python

import os

b = [0, 1, 2]
base = 8
level = 15
prog = "./main "
direct = "result8/"
for flag in b:
    tag = str(int(flag))
    fname = " >> " + direct + "DATA" + tag + ".txt"
    for case in range(level):
        grid = str(int(2**case*base))
        # grid = str(int(3*case))
        command = prog + grid + " " + tag + fname
        print(command)
        os.system(command)

# end
