#!/usr/bin/python

import sys, os

def main(argv):
    this = argv[0]
    if (len(argv) != 2):
        print 'Usage: {0} file-to-verify'.format(this)
        return
    f = argv[1]
    _, ext = os.path.splitext(f)

    verifDir = os.getcwd() + '/../verifying-constant-time'
    smackDir = verifDir + '/tools/smack/share/smack'
    if not os.path.isdir(verifDir):
        print 'Error: Cannot find verifying-constant-time directory'
        return
    if not os.path.isdir(smackDir):
        print 'Error: Cannot find smack directory'
        return

    if (ext == '.bc'): 
        linkBc(f)
    elif (ext != '.c'):
        print 'Error: {0} not a valid file extension'.format(ext)
        return

    
   
def linkBc(f):
    print 'Linking {0}'.format(f)
    

if __name__ == "__main__":
    main(sys.argv)
