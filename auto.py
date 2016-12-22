import os

b = [0, 1, 2]
base = 8
level = 15
prog = "./main "
direct = "result3/"
for item in b:
    tag = str(int(item))
    fname = " >> " + direct + "DATA" + tag + ".txt"
    for case in range(level):
        grid = str(int(2**case*base))
        command = prog + grid + " " + tag + fname
        print(command)
        os.system(command)