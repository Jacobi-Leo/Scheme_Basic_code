import os
import numpy as np

b = np.linspace(0,2,3)
base = 8
level = 15
prog = "./main "
direct = "result2/"
for item in b:
    tag = str(int(item))
    fname = " >> " + direct + "DATA" + tag + ".txt"
    for case in range(level):
        grid = str(int(2**case*base))
        command = prog + grid + " " + tag + fname
        print(command)
        os.system(command)

