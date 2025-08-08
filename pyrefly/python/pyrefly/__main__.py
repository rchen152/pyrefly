import os
import subprocess
import sys


if __name__ == '__main__':
    proc = subprocess.run(['pyrefly', *sys.argv[1:]])
    sys.exit(proc.returncode)
