

import sys
import os
sys.path.insert(0, os.path.abspath(os.path.join(os.path.dirname(__file__), '..')))
from src.RSimp.rsimp import RSimp

testFilePath = os.path.abspath(os.path.join(os.path.dirname(__file__), '../tests/test.pas'))

def main():
    rsimp = RSimp()
    rsimp.run(testFilePath, True, True)


if __name__ == '__main__':
    main()