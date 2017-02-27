#!/usr/bin/python

import sys, os

def main(argv):
    exe = argv[0]
    if (len(argv) != 3):
        print 'Usage: {0} filein fileout'.format(exe)
        print '------ Converts an LLVM 3.9 .ll file named <filein> into an LLVM 3.5 .ll file named <fileout> ------'.format(exe)
        return
    filein = argv[1]
    fileout = argv[2]

    f = open(filein,'r')
    fdata = f.read()
    f.close()

    # replace("old data", "new data")
    # THIS IS SUPER JANKY BUT IT WORKS FOR NOW
    print 'Replacing 3.9 IR with 3.5 IR...'
    fdata = fdata.replace("load i32,","load")
    fdata = fdata.replace("load i8,","load")
    fdata = fdata.replace("load i8*,","load")
    fdata = fdata.replace("load i16,","load")
    fdata = fdata.replace("load i32*,","load")
    fdata = fdata.replace("getelementptr [2 x i32],","getelementptr")
    fdata = fdata.replace("getelementptr [4 x i8],","getelementptr")
    fdata = fdata.replace("getelementptr [0 x i8],","getelementptr")
    fdata = fdata.replace("getelementptr [5 x i32],","getelementptr")
    fdata = fdata.replace("getelementptr [3 x i32],","getelementptr")

    f = open(fileout,'w')
    f.write(fdata)
    f.close()

    cmd = 'llvm-as-3.5 ' + fileout
    print cmd
    os.system(cmd)

if __name__ == "__main__":
    main(sys.argv)

