module test

import StdEnv

:: UNIT = UNIT


instance == UNIT where (==) x y = True

Start = (==) UNIT UNIT