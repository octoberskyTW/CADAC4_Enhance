***********************************************************************
************************** CRUISE5 SIMULATION *************************
***********************************************************************
********************* CADAC3 FAMILY OF 5 DOF SIMULATIONS **************
***********************************************************************
***********************************************************************

VERSION 1.0 (18 April 2000)

FEATURES:	Pseudo 5 DOF
			Flat earth
			Trimmed aerodynamics
			Turbojet propulsion
			Altitude hold, terrain following, obstacle avoidance
			Bank-to-turn autopilot
			Waypoint guidance, line guidance
			GPS/INS navigation
			LOS seeker, offset targeting
			Stochastic terrain

EXECUTION:	Compile MODULE.FOR with CADX3.FOR and UTL3.FOR
			Execute with files CADIN.ASC and HEAD.ASC present in curent
			directory
			Plot results of output TRAJ.ASC with KPLOT utility.

REFERENCE:	Zipfel, Peter H, "Modeling and Simulation of Aerospace Vehicle
			Dynamics", AIAA Education Series, American Insitute of
			Aeronautics and Astronautics, 1801 Alexander Bell Drive,
			Reston, VA 20191.

************************************************************************
************************ TEST CASES ************************************
************************************************************************

INCLIMB.ASC:CRUISE5,CIMBING,DIVING AT CONSTANT HEADING
INCRUISE.ASC:CRUISE5,HEADING CHANGES AT CONST ALTITUDE  
INPUT.ASC:CRUISE5 WITH GPS+TF/OA,LOOK-FWD  
INGPS.ASC:GPS+TF/OA,LOOK-FWD,MEDIUM TERRAIN, MONTE CARLO RUN
INIP.ASC:CRUISE5,APPROACHING AN IP WITH HEADING AND DIVE ANGLE

******************************* INCLIMB ********************************

 INCLIMB.ASC:CRUISE5,CIMBING,DIVING AT CONSTANT HEADING  20-APR-2000
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
    0.00    0.00      0.   1000.      0.      0.     0.0     0.0   -50.0     0.0     0.0     0.0      0.      0.
    0.05    0.68    225.   1000.      0.  20013.     0.0     0.0   -50.0     0.0     0.0     0.0    231.      0.
    1.00    0.68    225.    999.      0.  19799.     0.2     0.0   -50.0    -0.7     0.0     0.0    233.      0.
    2.00    0.68    225.    995.      0.  19574.     0.6     0.0   -50.0    -1.1     0.0     0.0    234.      0.
    3.00    0.67    225.    990.      0.  19351.     0.9     0.0   -50.0    -1.2     0.0     0.0    236.      0.
    4.00    0.67    225.    985.      0.  19129.     1.0     0.0   -50.0    -1.2     0.0     0.0    237.      0.
    5.00    0.67    225.    981.      0.  18908.     1.0     0.0   -50.0    -1.1     0.0     0.0    239.      0.
    6.00    0.66    225.    976.      0.  18688.     1.0     0.0   -50.0    -1.1     0.0     0.0    240.      0.
    7.00    0.66    225.    972.      0.  18469.     0.9     0.0   -50.0    -1.1     0.0     0.0    242.      0.
    8.00    0.66    225.    968.      0.  18252.     0.9     0.0   -50.0    -1.1     0.0     0.0    243.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
    9.00    0.65    225.    964.      0.  18035.     0.9     0.0   -50.0    -1.1     0.0     0.0    244.      0.
   10.00    0.65    225.    959.      0.  17819.     1.0     0.0   -50.0    -1.1     0.0     0.0    246.      0.
   11.00    0.65    225.    955.      0.  17605.     1.0     0.0   -50.0    -1.2     0.0     0.0    247.      0.
   12.00    0.65    225.    951.      0.  17392.     1.0     0.0   -50.0    -1.2     0.0     0.0    248.      0.
   13.00    0.64    225.    946.      0.  17179.     1.0     0.0   -50.0    -1.2     0.0     0.0    249.      0.
   14.00    0.64    225.    942.      0.  16968.     1.0     0.0   -50.0    -1.2     0.0     0.0    251.      0.
   15.00    0.64    225.    937.      0.  16757.     1.0     0.0   -50.0    -1.2     0.0     0.0    252.      0.
   16.00    0.63    225.    933.      0.  16548.     1.0     0.0   -50.0    -1.2     0.0     0.0    253.      0.
   17.00    0.63    225.    929.      0.  16340.     1.0     0.0   -50.0    -1.2     0.0     0.0    254.      0.
   18.00    0.63    225.    924.      0.  16133.     1.0     0.0   -50.0    -1.2     0.0     0.0    256.      0.
   19.00    0.63    225.    920.      0.  15926.     1.0     0.0   -50.0    -1.2     0.0     0.0    257.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   20.00    0.62    225.    916.      0.  15721.     1.1     0.0   -50.0    -1.2     0.0     0.0    258.      0.
 Stage   1 criteria #  1 satisfied.  Stage variable #:   2000 Time:   20.050
   21.00    0.62    225.    915.      0.  15517.     6.7     0.0   -50.0     1.6     0.0     0.0    260.      0.
   22.00    0.61    225.    928.      0.  15316.     6.7     0.0   -50.0     5.8     0.0     0.0    263.      0.
   23.00    0.60    225.    956.      0.  15122.     6.7     0.0   -50.0    10.0     0.0     0.0    267.      0.
   24.00    0.59    226.    998.      0.  14934.     6.7     0.0   -50.0    14.1     0.0     0.0   1950.      0.
   25.00    0.59    226.   1053.      0.  14753.     6.0     0.0   -50.0    18.1     0.0     0.0   1945.      0.
   26.00    0.58    226.   1118.      0.  14579.     2.7     0.0   -50.0    20.5     0.0     0.0   1938.      0.
   27.00    0.57    226.   1187.      0.  14409.     0.5     0.0   -50.0    20.7     0.0     0.0   1930.      0.
   28.00    0.57    226.   1253.      0.  14242.    -0.1     0.0   -50.0    19.8     0.0     0.0   1923.      0.
   29.00    0.56    226.   1316.      0.  14076.     0.3     0.0   -50.0    18.8     0.0     0.0   1916.      0.
   30.00    0.56    226.   1375.      0.  13910.     1.0     0.0   -50.0    18.3     0.0     0.0   1910.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   31.00    0.55    226.   1433.      0.  13747.     1.6     0.0   -50.0    18.2     0.0     0.0   1903.      0.
   32.00    0.55    226.   1490.      0.  13586.     1.8     0.0   -50.0    18.3     0.0     0.0   1897.      0.
   33.00    0.54    226.   1547.      0.  13427.     1.8     0.0   -50.0    18.4     0.0     0.0   1891.      0.
   34.00    0.54    226.   1604.      0.  13270.     1.7     0.0   -50.0    18.5     0.0     0.0   1885.      0.
   35.00    0.53    226.   1661.      0.  13116.     1.7     0.0   -50.0    18.4     0.0     0.0   1879.      0.
   36.00    0.52    226.   1716.      0.  12963.     1.8     0.0   -50.0    18.3     0.0     0.0   1873.      0.
   37.00    0.52    226.   1771.      0.  12813.     1.9     0.0   -50.0    18.2     0.0     0.0   1866.      0.
   38.00    0.52    226.   1824.      0.  12664.     2.1     0.0   -50.0    18.1     0.0     0.0   1860.      0.
   39.00    0.51    226.   1877.      0.  12518.     2.2     0.0   -50.0    18.0     0.0     0.0   1854.      0.
   40.00    0.51    226.   1929.      0.  12373.     2.3     0.0   -50.0    17.9     0.0     0.0   1848.      0.
   41.00    0.50    226.   1981.      0.  12231.     2.5     0.0   -50.0    17.9     0.0     0.0   1842.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   42.00    0.50    226.   2032.      0.  12090.     2.6     0.0   -50.0    17.8     0.0     0.0   1837.      0.
   43.00    0.49    226.   2082.      0.  11952.     2.7     0.0   -50.0    17.7     0.0     0.0   1831.      0.
   44.00    0.49    226.   2131.      0.  11815.     2.9     0.0   -50.0    17.6     0.0     0.0   1825.      0.
   45.00    0.48    226.   2180.      0.  11680.     3.0     0.0   -50.0    17.5     0.0     0.0   1819.      0.
   46.00    0.48    226.   2228.      0.  11547.     3.1     0.0   -50.0    17.5     0.0     0.0   1814.      0.
   47.00    0.47    226.   2275.      0.  11416.     3.3     0.0   -50.0    17.4     0.0     0.0   1808.      0.
   48.00    0.47    226.   2321.      0.  11286.     3.4     0.0   -50.0    17.3     0.0     0.0   1802.      0.
   49.00    0.46    226.   2367.      0.  11159.     3.6     0.0   -50.0    17.2     0.0     0.0   1797.      0.
   50.00    0.46    226.   2413.      0.  11033.     3.7     0.0   -50.0    17.2     0.0     0.0   1791.      0.
 Stage   2 criteria #  1 satisfied.  Stage variable #:   2000 Time:   50.050
   51.00    0.46    226.   2454.      0.  10908.    -6.7     0.0   -50.0    13.2     0.0     0.0   1786.      0.
   52.00    0.45    226.   2480.      0.  10778.    -6.7     0.0   -50.0     7.2     0.0     0.0   1783.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   53.00    0.46    226.   2491.      0.  10643.    -6.7     0.0   -50.0     1.1     0.0     0.0   1781.      0.
   54.00    0.46    226.   2486.      0.  10503.    -6.7     0.0   -50.0    -4.9     0.0     0.0   1781.      0.
   55.00    0.47    226.   2465.      0.  10360.    -6.7     0.0   -50.0   -10.9     0.0     0.0   1782.      0.
   56.00    0.48    226.   2428.      0.  10213.    -6.7     0.0   -50.0   -16.8     0.0     0.0   1785.      0.
   57.00    0.49    226.   2374.      0.  10063.    -6.7     0.0   -50.0   -22.7     0.0     0.0   1789.      0.
   58.00    0.51    226.   2303.      0.   9912.    -0.8     0.0   -50.0   -27.2     0.0     0.0   1795.      0.
   59.00    0.52    226.   2225.      0.   9757.     4.7     0.0   -50.0   -27.2     0.0     0.0   1801.      0.
   60.00    0.54    226.   2147.      0.   9596.     6.4     0.0   -50.0   -24.9     0.0     0.0   1808.      0.
   61.00    0.55    226.   2075.      0.   9430.     5.3     0.0   -50.0   -22.4     0.0     0.0   1814.      0.
   62.00    0.56    226.   2007.      0.   9260.     3.3     0.0   -50.0   -20.6     0.0     0.0   1822.      0.
   63.00    0.58    226.   1941.      0.   9085.     1.5     0.0   -50.0   -20.1     0.0     0.0   1829.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   64.00    0.59    226.   1874.      0.   8907.     0.7     0.0   -50.0   -20.4     0.0     0.0   1836.      0.
   65.00    0.60    226.   1804.      0.   8726.     0.6     0.0   -50.0   -20.9     0.0     0.0   1844.      0.
   66.00    0.61    225.   1731.      0.   8542.     0.9     0.0   -50.0   -21.3     0.0     0.0    254.      0.
   67.00    0.62    225.   1656.      0.   8357.     1.2     0.0   -50.0   -21.4     0.0     0.0    252.      0.
   68.00    0.63    225.   1580.      0.   8170.     1.3     0.0   -50.0   -21.3     0.0     0.0    250.      0.
   69.00    0.63    225.   1504.      0.   7982.     1.2     0.0   -50.0   -21.2     0.0     0.0    248.      0.
   70.00    0.64    225.   1427.      0.   7793.     1.1     0.0   -50.0   -21.0     0.0     0.0    246.      0.
   71.00    0.65    225.   1350.      0.   7603.     1.0     0.0   -50.0   -21.0     0.0     0.0    244.      0.
   72.00    0.65    225.   1272.      0.   7413.     0.9     0.0   -50.0   -21.0     0.0     0.0    242.      0.
   73.00    0.66    225.   1193.      0.   7222.     0.9     0.0   -50.0   -20.9     0.0     0.0    240.      0.
   74.00    0.67    225.   1114.      0.   7030.     0.8     0.0   -50.0   -20.9     0.0     0.0    238.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   75.00    0.67    225.   1033.      0.   6839.     0.8     0.0   -50.0   -20.9     0.0     0.0    236.      0.
   76.00    0.68    225.    953.      0.   6647.     0.7     0.0   -50.0   -20.8     0.0     0.0    234.      0.
   77.00    0.68    225.    871.      0.   6457.     0.7     0.0   -50.0   -20.8     0.0     0.0    232.      0.
   78.00    0.69    225.    789.      0.   6267.     0.6     0.0   -50.0   -20.7     0.0     0.0    230.      0.
   79.00    0.70    225.    707.      0.   6078.     0.6     0.0   -50.0   -20.7     0.0     0.0    228.      0.
   80.00    0.70    225.    623.      0.   5891.     0.5     0.0   -50.0   -20.6     0.0     0.0    226.      0.
   81.00    0.71    225.    540.      0.   5706.     0.5     0.0   -50.0   -20.6     0.0     0.0    225.      0.
   82.00    0.71    225.    455.      0.   5523.     0.5     0.0   -50.0   -20.6     0.0     0.0    223.      0.
   83.00    0.72    225.    371.      0.   5344.     0.4     0.0   -50.0   -20.5     0.0     0.0    222.      0.
   84.00    0.72    225.    285.      0.   5168.     0.4     0.0   -50.0   -20.5     0.0     0.0    221.      0.
   85.00    0.72    225.    200.      0.   4997.     0.4     0.0   -50.0   -20.5     0.0     0.0    220.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   86.00    0.73    225.    114.      0.   4831.     0.3     0.0   -50.0   -20.4     0.0     0.0    219.      0.
   87.00    0.73    225.     27.      0.   4671.     0.3     0.0   -50.0   -20.4     0.0     0.0    218.      0.
0  IMPACT OCCURRED IN THE HORIZONTAL PLANE
   87.35    0.73    225.     -4.      0.   4616.     0.3     0.0   -50.0   -20.4     0.0     0.0    218.      0.
 FLT TIME =   87.4     CRIT VAR =   0.00     RUN   1 OF   0 RUNS  IN  GROUP  1

Press any key to continue


******************************* INCRUISE *******************************

 INCRUISE.ASC:CRUISE5,HEADING CHANGES AT CONST ALTITUDE  18-APR-2000
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
    0.00    0.00      0.   1000.      0.      0.     0.0     0.0   -50.0     0.0     0.0     0.0      0.      0.
    0.05    0.68    235.   1000.      0.  20013.     0.1    -0.3   -50.0     0.0    -1.5     1.0    231.      0.
    5.00    0.66    235.   1000.      0.  18899.     2.7   -53.0   -64.8     0.0    -1.5     1.6    240.      0.
   10.00    0.64    235.   1000.      0.  17833.     2.0   -40.8   -79.9     0.0    -0.9     1.3    249.      0.
   15.00    0.63    235.   1000.      0.  16869.     1.2   -16.3   -87.0     0.0    -0.3     1.0    256.      0.
   20.00    0.61    235.   1000.      0.  15981.     1.2    -4.5   -89.2     0.0    -0.1     1.0    262.      0.
   25.00    0.60    234.   1000.      0.  15146.     1.2    -1.1   -89.8     0.0     0.0     1.0    916.      0.
   30.00    0.60    234.   1000.      0.  14345.     1.2    -0.3   -90.0     0.0     0.0     1.0   1056.      0.
 Stage   1 criteria #  1 satisfied.  Stage variable #:   2000 Time:   30.050
   35.00    0.60    234.   1000.      0.  13496.     3.2    52.7   -73.3     0.0     1.5     1.6   1210.      0.
   40.00    0.60    234.   1000.      0.  12532.     3.4    52.5   -55.1     0.0     1.5     1.6   1206.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   45.00    0.60    234.   1000.      0.  11533.     3.4    52.4   -37.0     0.0     1.5     1.6   1206.      0.
   50.00    0.60    234.   1000.      0.  10587.     1.6    25.5   -25.1     0.0     0.4     1.1   1072.      0.
   55.00    0.60    234.   1000.      0.   9707.     1.2     7.4   -21.3     0.0     0.1     1.0   1055.      0.
   60.00    0.60    234.   1000.      0.   8875.     1.2     1.8   -20.3     0.0     0.0     1.0   1054.      0.
   65.00    0.60    234.   1000.      0.   8089.     1.2     0.4   -20.1     0.0     0.0     1.0   1054.      0.
   70.00    0.60    234.   1000.      0.   7358.     1.2     0.1   -20.0     0.0     0.0     1.0   1054.      0.
   75.00    0.60    234.   1000.      0.   6700.     1.2     0.0   -20.0     0.0     0.0     1.0   1054.      0.
   80.00    0.60    234.   1000.      0.   6137.     1.2     0.0   -20.0     0.0     0.0     1.0   1054.      0.
   85.00    0.60    234.   1000.      0.   5698.     1.2     0.0   -20.0     0.0     0.0     1.0   1054.      0.
   90.00    0.60    234.   1000.      0.   5414.     1.2     0.0   -20.0     0.0     0.0     1.0   1054.      0.
   95.00    0.60    234.   1000.      0.   5308.     1.2     0.0   -20.0     0.0     0.0     1.0   1055.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
  100.00    0.60    234.   1000.      0.   5392.     1.2     0.0   -20.0     0.0     0.0     1.0   1055.      0.
 Stage   2 criteria #  1 satisfied.  Stage variable #:   2000 Time:   100.00
  100.00    0.60    234.   1000.      0.   5392.     1.2     0.0   -20.0     0.0     0.0     1.0   1055.      0.
 FLT TIME =   100.     CRIT VAR =   0.00     RUN   1 OF   0 RUNS  IN  GROUP  1

Press any key to continue


		
**************************** INPUT.ASC ********************************************
NOTE: These are stochastic runs; you may get different miss distances.

 INPUT.ASC:CRUISE5 WITH GPS+TF/OA,LOOK-FWD  18-APR-2000
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
    0.00    0.00      0.    150.      0.      0.     0.0     0.0   -60.0     0.0     0.0     0.0      0.      0.
    0.05    0.80 130135.    150.   6091.   6094.     0.2    -6.3   -60.0     0.0     0.0     1.0    195.      0.
    2.00    0.80 130136.    150.   5563.   5566.     0.3     2.9   -60.5    -0.1     0.0     1.0   1993.      0.
 Stage   1 criteria #  1 satisfied.  Stage variable #:   2000 Time:   3.0500
    4.00    0.80 132136.    150.   5022.   5025.     0.5     0.0   -60.5     0.0     0.0     1.1   1993.      0.
    6.00    0.80 132136.    150.   4481.   4484.     0.3     0.0   -60.5     0.0     0.0     1.0   1993.      0.
    8.00    0.80 132136.    150.   3941.   3944.     0.3     0.0   -60.5     0.0     0.0     1.0   1993.      0.
   10.00    0.79 132136.    150.   3401.   3404.     0.3     0.0   -60.5     0.0     0.0     1.0   1993.      0.
 Stage   2 criteria #  1 satisfied.  Stage variable #:   2000 Time:   10.000
   12.00    0.79 132136.    150.   2862.   2864.     0.3     1.0   -60.6     0.0     0.0     1.0   1993.      0.
   14.00    0.79 132136.    150.   2323.   2325.     0.3     0.0   -60.6     0.0     0.0     1.0   1994.      0.
   16.00    0.79 132136.    148.   1783.   1786.     0.1     1.7   -60.6    -1.0     0.0     1.4   1994.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   18.00    0.79 132136.    147.   1245.   1247.     1.1     0.0   -60.6     1.5     0.0     0.6   1994.      0.
   20.00    0.79 332136.    145.    707.    709.     0.6     1.9   -60.6    -2.0     0.0     2.1   1995.      1.
   22.00    0.79 132136.    145.    171.    171.     3.4     8.5   -60.6     2.7     0.0     2.1   1995.      5.
    TIME OF CLOSEST APPROACH T=   22.638        AT WAYPOINT #  1
     DBW=  5.0949      SBWL1= 0.90479      SBWL2= 0.87891      SBWL3= -4.9363
    DBWC=  7.6855     SBWCL1=-0.48829     SBWCL2=-0.19106     SBWCL3= -7.6676
    DWCW=  3.2474     SWCWL1=  1.3931     SWCWL2=  1.0700     SWCWL3=  2.7313
    DBWH=  1.2614      DBWCH= 0.52434      DWCWH=  1.7566

  FVYAWM= 9.89825E-03 TFVYAW=  20.300     FVPITM= 1.72584E-02 TFVPIT=  20.300
  FRPSIM= 1.11620E-02 TFRPSI=  20.350     FRTHTM= 6.38008E-02 TFRTHT=  21.750

********************** nomenclature ********************************************
DWB = Closest distance of vehicle to waypoint - m
SBWL(3) = Coordinates of closest distance of vehicle to waypoint in L-coordinates - m
DBWC = Closest distance of vehicle to computed waypoint - m
SBWCL(3) = Coordinates of closest distance of vehicle to computed waypoint in L-coordinates - m
DWCW = Distance between computed and true waypoint - m
SWCWL(3) = Coordinates of Distance between computed and true waypoint in L-coordinates - m
DBWH = Horizontal closest distance of vehicle to waypoint - m
DBWCH = Horizontal closest distance of vehicle to computed waypoint - m
DWCWH = Horizontal distance between computed and true waypoint - m

FVYAWM = Max yaw half-FOV encountered - rad
TFVYAW = Time at which FVYAWM was encountered - s
FVPITM = Max pitch half-FOV encountered - rad
TFVPIT = Time at which FVPITM was encountered - s
FRPSIM = Max yaw half-FOR encountered - rad
TFRPSI = Time at which FRPSIM was encountered - s
FRTHTM = Max pitch half-FOR encountered - rad
TFRTHT = Time at which FRTHTM was encountered - s
**********************************************************************************

 Stage   3 criteria #  1 satisfied.  Stage variable #:    450 Time:   22.700
   24.00    0.78  32136.    170.  13529.  13530.    -0.9   -13.9   -59.7     1.1     0.0     0.2   1995.      0.
   26.00    0.78  32136.    167.  12996.  12998.     2.9   -69.5   -63.0    -1.5     0.0     2.8   1995.      0.
   28.00    0.79  32136.    151.  12470.  12472.     4.6   -69.9   -75.5    -1.7     0.0     2.9   1995.      0.
   30.00    0.79  32136.    139.  11978.  11981.     5.4   -69.9   -89.5    -0.8     0.0     4.0   1997.      0.
   32.00    0.78  32136.    155.  11550.  11553.    -0.5    41.1   -94.0     4.2     0.0    -0.6   1996.      0.
   34.00    0.79  32136.    167.  11128.  11131.    -0.8   -63.5   -92.9    -1.9     0.0     2.2   1993.      0.
   36.00    0.80  32134.    158.  10700.  10702.     1.5    11.7   -90.1     0.6     0.0     2.6   1835.      0.
   38.00    0.79  32136.    169.  10268.  10270.    -0.3   -26.7   -89.2    -0.2     0.0     0.3   1992.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   40.00    0.76  32136.    164.   9846.   9847.     2.7    -5.1   -86.1     0.7     0.0     2.1   2002.      0.
   42.00    0.76  32136.    178.   9433.   9434.    -1.8   -24.2   -85.0     0.6     0.0     0.2   2004.      0.
   44.00    0.75  32136.    184.   9021.   9021.    -2.7     0.5   -81.7     1.1     0.0    -1.2   2003.      0.
   46.00    0.76  32136.    166.   8609.   8609.     4.4    33.0   -79.8    -2.3     0.0     3.1   2004.      0.
   48.00    0.74  32136.    176.   8192.   8191.    -4.5    -7.3   -77.5     1.4     0.0    -2.3   2008.      0.
   50.00    0.75  32146.    151.   7783.   7782.     3.3    31.4   -75.0    -3.1     0.0     2.1   2010.      0.
   52.00    0.76  32136.    153.   7365.   7364.     2.2    44.9   -72.8     3.3     0.0     3.2   2006.      0.
   54.00    0.75  32136.    189.   6941.   6939.    -1.1   -19.7   -69.2     1.7     0.0     0.7   2005.      0.
   56.00    0.75  32136.    193.   6520.   6517.    -2.3    69.3   -66.9    -1.3     0.0    -0.3   2005.      0.
   58.00    0.74  32136.    161.   6099.   6097.     4.0    61.1   -62.4    -3.5     0.0     3.4   2010.      0.
   60.00    0.75  32136.    146.   5661.   5658.     1.6    58.5   -57.9    -1.2     0.0     2.1   2010.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   62.00    0.75  32136.    143.   5211.   5208.     0.9    34.4   -52.5     0.8     0.0     1.0   2010.      0.
 Stage   4 criteria #  1 satisfied.  Stage variable #:    130 Time:   62.949
   64.00    0.75 133146.    145.   4759.   4750.     1.4    62.3   -48.0    -0.5     0.0     1.3   2009.      0.
   66.00    0.75 133146.    138.   4290.   4281.     1.7    51.0   -42.4    -0.6     0.0     1.5   2009.      0.
   68.00    0.75 133146.    132.   3811.   3801.     1.2    49.0   -36.6    -0.4     0.0     1.2   2010.      0.
   70.00    0.75 133146.    127.   3323.   3314.     2.7    61.5   -31.2    -0.3     0.0     1.5   2012.      0.
   72.00    0.74 133146.    124.   2832.   2823.     2.4    61.7   -25.3    -0.4     0.0     1.6   2015.      0.
   74.00    0.74 133146.    119.   2337.   2328.     1.8    50.9   -19.6    -0.6     0.0     1.6   2016.      0.
   76.00    0.74 133146.    115.   1840.   1831.     1.4    43.2   -14.9    -0.5     0.0     1.2   2017.      0.
   78.00    0.75 133146.    109.   1337.   1327.     0.5    21.4   -11.4    -0.6     0.0     1.2   2014.      0.
   80.00    0.76 333146.    105.    816.    813.     0.9    32.9    -9.4    -0.3     0.0     1.5   2010.      2.
 Stage   5 criteria #  1 satisfied.  Stage variable #:    130 Time:   80.850
   82.00    0.76 333146.    101.    301.    298.     0.7     2.1    -7.4    -0.7     0.0     1.1   2011.      6.
  INPUT.ASC:CRUISE5 WITH GPS+TF/OA,LOOK-FWD
      PARAMETERS AT IMPACT TIME  T=   83.151     TRCOND=   45.000
     DBT=  4.6328      SBTP1= -2.1952      SBTP2= -4.0797     EPSVPX=  6.5879
    DBTC= 0.19923     SBTCP1=-8.72803E-02 SBTCP2= 0.17909     PSIVLX= -6.5510
    DTCT=  4.7519     STCTP1= -2.1079     STCTP2= -4.2588     THTVLX=-0.69789

  FVYAWM= 8.50838E-03 TFVYAW=  79.600     FVPITM= 3.43169E-03 TFVPIT=  82.250
  FRPSIM= 3.45413E-02 TFRPSI=  79.750     FRTHTM= 1.98622E-02 TFRTHT=  80.500

 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   83.20    0.77 133146.     98.     11.     14.     0.7   -15.7    -6.6    -0.7     0.0     1.5   2008.    777.
 FLT TIME =   83.2     CRIT VAR =   0.00     RUN   1 OF   0 RUNS  IN  GROUP  1

Press any key to continue

********************** nomenclature ********************************************
DBT = Closest distance of vehicle to target in target plane: miss distance - m
SBTP1,2 = Miss distance components in target plane and coor. - m
DBTC = Guidance & control miss mag. in target plane - m
SBTCP1,2 = Guidance & control error components in target plane - mDWCW = Distance between computed and true waypoint - m
DTCT = Navigation miss magnitude in target plane - m
STCTP1,2 = Navigation error components in target plane - m
EPSVPX = Angle between normal to plane and vel vector- rad
PSIVLX = Heading angle at impact - deg
THTVLX = Flight path angle at impact - deg

FVYAWM = Max yaw half-FOV encountered - rad
TFVYAW = Time at which FVYAWM was encountered - s
FVPITM = Max pitch half-FOV encountered - rad
TFVPIT = Time at which FVPITM was encountered - s
FRPSIM = Max yaw half-FOR encountered - rad
TFRPSI = Time at which FRPSIM was encountered - s
FRTHTM = Max pitch half-FOR encountered - rad
TFRTHT = Time at which FRTHTM was encountered - s
********************************************************************************

***************************** INGPS.ASC ****************************************

 INGPS.ASC:GPS+TF/OA,LOOK-FWD,MEDIUM TERRAIN, MC 18-APR-2000
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
    0.00    0.00      0.    150.      0.      0.     0.0     0.0   -60.0     0.0     0.0     0.0      0.      0.
    0.05    0.80 130135.    150.   6112.   6094.     0.2     0.9   -60.0     0.0     0.0     1.0    195.      0.
 Stage   1 criteria #  1 satisfied.  Stage variable #:   2000 Time:   3.0500
    5.00    0.80 132136.    150.   4769.   4755.     0.3     0.1   -60.5     0.1     0.0     1.0   1993.      0.
   10.00    0.79 132136.    150.   3418.   3404.     0.3     0.3   -60.5     0.0     0.0     1.0   1993.      0.
   15.00    0.79 132136.    150.   2069.   2055.     0.3     0.0   -60.4     0.0     0.0     1.0   1994.      0.
   20.00    0.79 332136.    145.    718.    709.     0.6   -24.7   -60.5    -2.0     0.0     2.2   1995.      2.
    TIME OF CLOSEST APPROACH T=   22.639        AT WAYPOINT #  1
     DBW=  5.7039      SBWL1=  2.5811      SBWL2=  1.7627      SBWL3= -4.7713
    DBWC=  5.7672     SBWCL1= -2.5775     SBWCL2=  1.7959     SBWCL3= -4.8364
    DWCW=  5.1591     SWCWL1=  5.1586     SWCWL2=-3.31957E-02 SWCWL3= 6.51002E-02
    DBWH=  3.1255      DBWCH=  3.1415      DWCWH=  5.1587

  FVYAWM= 1.66311E-02 TFVYAW=  19.850     FVPITM= 3.44070E-03 TFVPIT=  20.400
  FRPSIM= 2.30400E-02 TFRPSI=  20.350     FRTHTM= 5.73969E-02 TFRTHT=  21.750

 Stage   2 criteria #  1 satisfied.  Stage variable #:    450 Time:   22.700
   25.00    0.78  32136.    173.  13279.  13264.    -0.4   -64.4   -61.2    -0.3     0.0     0.6   1996.      0.
   30.00    0.76  32136.    144.  12005.  11988.     6.7   -69.9   -83.6    -1.0     0.0     4.0   2007.      0.
   35.00    0.74  32136.    160.  10972.  10955.     2.4    10.3   -91.6    -0.7     0.0     1.6   2012.      0.
   40.00    0.76  32136.    166.   9954.   9935.     2.3     6.9   -87.0     0.8     0.0     1.8   2005.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   45.00    0.76  32136.    182.   8917.   8899.    -0.9   -30.6   -82.9    -2.5     0.0     0.9   2001.      0.
   50.00    0.75  32146.    158.   7873.   7854.     3.3    13.9   -76.0    -1.7     0.0     3.3   2008.      0.
   55.00    0.76  32136.    194.   6818.   6800.     0.0    62.7   -70.7     0.5     0.0     1.2   2000.      0.
   60.00    0.75  32136.    144.   5742.   5724.     0.0    19.3   -57.8    -0.9     0.0     1.2   2008.      0.
 Stage   3 criteria #  1 satisfied.  Stage variable #:    130 Time:   63.249
   65.00    0.75 133146.    149.   4596.   4591.     3.1    69.7   -47.5    -0.2     0.0     1.9   2008.      0.
   70.00    0.76 133146.    126.   3387.   3383.     1.8    38.3   -31.7    -1.1     0.0     1.9   2007.      0.
   75.00    0.75 133146.    123.   2135.   2131.     0.8    51.5   -17.6    -0.5     0.0     1.5   2013.      0.
   80.00    0.74 333146.    116.    873.    864.    -3.3    10.2    -9.6    -0.3     0.0     0.1   2015.      2.
  INGPS.ASC:GPS+TF/OA,LOOK-FWD,MEDIUM TERRAIN
      PARAMETERS AT IMPACT TIME  T=   83.497     TRCOND=   545.00
     DBT=  19.233      SBTP1= -3.5516      SBTP2= -18.902     EPSVPX=  9.1724
    DBTC=  20.524     SBTCP1=  1.0569     SBTCP2= -20.497     PSIVLX= -8.6212
    DTCT=  4.8764     STCTP1= -4.6085     STCTP2=  1.5941     THTVLX= -3.1435

  FVYAWM= 2.10198E-02 TFVYAW=  79.750     FVPITM= 7.10608E-03 TFVPIT=  78.750
  FRPSIM= 5.37548E-02 TFRPSI=  81.800     FRTHTM= 6.64968E-02 TFRTHT=  80.800

   83.50    0.72 133146.     96.     21.     19.    -0.2    23.9    -8.6    -3.1     0.0     4.0   2027.      8.
 FLT TIME =   83.5     CRIT VAR =   0.00     RUN   1 OF   3 RUNS  IN  GROUP  1

 Stage   1 criteria #  1 satisfied.  Stage variable #:   2000 Time:   3.0500
    TIME OF CLOSEST APPROACH T=   22.638        AT WAYPOINT #  1
     DBW=  4.6264      SBWL1=-0.63428      SBWL2=-2.34375E-02  SBWL3= -4.5827
    DBWC=  37.602     SBWCL1= -25.252     SBWCL2=  27.835     SBWCL3= -1.2049
    DWCW=  37.331     SWCWL1=  24.618     SWCWL2= -27.859     SWCWL3= -3.3777
    DBWH= 0.63471      DBWCH=  37.583      DWCWH=  37.178

  FVYAWM= 1.04918E-02 TFVYAW=  21.300     FVPITM= 7.22644E-03 TFVPIT=  20.750
  FRPSIM= 1.20487E-02 TFRPSI=  21.300     FRTHTM= 4.66445E-02 TFRTHT=  18.050

 Stage   2 criteria #  1 satisfied.  Stage variable #:    450 Time:   22.700
 Stage   3 criteria #  1 satisfied.  Stage variable #:    130 Time:   63.349
  INGPS.ASC:GPS+TF/OA,LOOK-FWD,MEDIUM TERRAIN
      PARAMETERS AT IMPACT TIME  T=   83.452     TRCOND=   54.000
     DBT=  8.6584      SBTP1=  1.6086      SBTP2=  8.5077     EPSVPX=  5.0119
    DBTC=  1.7595     SBTCP1= 0.85046     SBTCP2=  1.5403     PSIVLX= -4.9097
    DTCT=  7.0085     STCTP1= 0.75818     STCTP2=  6.9674     THTVLX=  1.0082

  FVYAWM= 2.51029E-02 TFVYAW=  78.900     FVPITM= 1.40877E-02 TFVPIT=  82.200
  FRPSIM= 5.92470E-02 TFRPSI=  80.600     FRTHTM= 6.75079E-02 TFRTHT=  82.500

 FLT TIME =   83.5     CRIT VAR =   0.00     RUN   2 OF   3 RUNS  IN  GROUP  1

 Stage   1 criteria #  1 satisfied.  Stage variable #:   2000 Time:   3.0500
    TIME OF CLOSEST APPROACH T=   22.641        AT WAYPOINT #  1
     DBW=  14.897      SBWL1= -12.346      SBWL2= -6.7393      SBWL3= -4.9055
    DBWC=  14.887     SBWCL1=  8.8784     SBWCL2= -11.151     SBWCL3= -4.2948
    DWCW=  21.687     SWCWL1= -21.225     SWCWL2=  4.4121     SWCWL3=-0.61071
    DBWH=  14.066      DBWCH=  14.254      DWCWH=  21.678

  FVYAWM= 3.56854E-02 TFVYAW=  19.650     FVPITM= 1.66574E-02 TFVPIT=  21.700
  FRPSIM= 4.07522E-02 TFRPSI=  19.700     FRTHTM= 5.99236E-02 TFRTHT=  21.750

 Stage   2 criteria #  1 satisfied.  Stage variable #:    450 Time:   22.700
 Stage   3 criteria #  1 satisfied.  Stage variable #:    130 Time:   64.749
  INGPS.ASC:GPS+TF/OA,LOOK-FWD,MEDIUM TERRAIN
      PARAMETERS AT IMPACT TIME  T=   85.077     TRCOND=   45454.
     DBT=  7.3602      SBTP1=  7.1866      SBTP2= -1.5890     EPSVPX=  6.5986
    DBTC= 0.94267     SBTCP1= 0.81155     SBTCP2=-0.47959     PSIVLX= -6.5822
    DTCT=  6.4709     STCTP1=  6.3751     STCTP2= -1.1094     THTVLX=-0.46623

  FVYAWM= 1.58759E-02 TFVYAW=  81.100     FVPITM= 1.82334E-02 TFVPIT=  81.500
  FRPSIM= 4.49576E-02 TFRPSI=  80.500     FRTHTM= 5.29811E-02 TFRTHT=  82.300

 FLT TIME =   85.1     CRIT VAR =   0.00     RUN   3 OF   3 RUNS  IN  GROUP  1

Press any key to continue

********************************** INIP *******************************


 INIP.ASC:CRUISE5,APPROACHING AN IP WITH HEADING AND DIVE ANGLE  20-APR-2000
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
    0.00    0.00      0.    500.      0.      0.     0.0     0.0   -60.0     0.0     0.0     0.0      0.      0.
    0.05    0.68  30135.    500.   6096.   6096.     0.1     0.4   -60.0     0.0     0.0     1.0    236.      0.
    3.00    0.67  30135.    500.   5424.   5424.     0.7     0.5   -60.6     0.0     0.0     1.0    242.      0.
    6.00    0.66  30135.    500.   4752.   4752.     0.8     0.3   -60.7     0.0     0.0     1.0    247.      0.
    9.00    0.65  30135.    500.   4092.   4092.     0.8     0.1   -60.6     0.0     0.0     1.0    253.      0.
   12.00    0.63  30135.    500.   3442.   3442.     0.9     0.1   -60.6     0.0     0.0     1.0    257.      0.
   15.00    0.62  30135.    500.   2803.   2803.     0.9     0.0   -60.6     0.0     0.0     1.0    262.      0.
   18.00    0.61  30135.    500.   2175.   2175.     0.9     0.3   -60.6     0.0     0.0     1.0    267.      0.
   21.00    0.61  30135.    500.   1555.   1555.     1.0     0.3   -60.5     0.0     0.0     1.0    271.      0.
   24.00    0.60  30134.    500.    945.    945.     1.0     0.2   -60.5     0.0     0.0     1.0   1181.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   27.00    0.60  30134.    500.    338.    338.     1.0     0.3   -60.5     0.0     0.0     1.0   1086.      0.
 Stage   1 criteria #  1 satisfied.  Stage variable #:    130 Time:   28.200
   30.00    0.60  30134.    499.  13623.  13623.     5.3   -61.3   -66.2    -0.2     0.0     2.3   1343.      0.
   33.00    0.60  30134.    500.  13040.  13040.     4.9   -63.0   -83.0     0.2     0.0     2.1   1494.      0.
   36.00    0.60  30134.    502.  12528.  12528.    -0.8    31.0   -95.4     0.1     0.0     0.9   1259.      0.
   39.00    0.60  30134.    500.  12042.  12042.     1.2    13.4   -92.9    -0.1     0.0     1.1   1095.      0.
   42.00    0.60  30134.    500.  11556.  11556.     1.1    12.4   -91.0     0.0     0.0     1.0   1112.      0.
   45.00    0.60  30134.    500.  11071.  11071.     1.1    13.1   -89.2     0.0     0.0     1.0   1112.      0.
   48.00    0.60  30134.    500.  10585.  10585.     1.1    14.1   -87.2     0.0     0.0     1.0   1113.      0.
   51.00    0.60  30134.    500.  10098.  10098.     1.1    15.1   -85.0     0.0     0.0     1.0   1114.      0.
   54.00    0.60  30134.    500.   9611.   9611.     1.1    16.4   -82.7     0.0     0.0     1.0   1115.      0.
   57.00    0.60  30134.    500.   9121.   9121.     1.2    17.8   -80.1     0.0     0.0     1.1   1116.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   60.00    0.60  30134.    500.   8629.   8629.     1.2    19.3   -77.3     0.0     0.0     1.1   1118.      0.
   63.00    0.60  30134.    500.   8133.   8133.     1.2    21.1   -74.2     0.0     0.0     1.1   1121.      0.
   66.00    0.60  30134.    500.   7633.   7633.     1.3    23.1   -70.9     0.0     0.0     1.1   1123.      0.
   69.00    0.60  30134.    500.   7127.   7127.     1.3    25.4   -67.1     0.0     0.0     1.1   1127.      0.
   72.00    0.60  30134.    500.   6613.   6613.     1.4    27.8   -63.0     0.0     0.0     1.1   1132.      0.
   75.00    0.60  30134.    500.   6091.   6091.     1.5    30.4   -58.3     0.0     0.0     1.2   1137.      0.
   78.00    0.60  30134.    500.   5557.   5557.     1.6    33.0   -53.2     0.0     0.0     1.2   1143.      0.
   81.00    0.60  30134.    500.   5011.   5011.     1.7    35.3   -47.5     0.0     0.0     1.2   1149.      0.
 Stage   2 criteria #  1 satisfied.  Stage variable #:    130 Time:   81.100
   84.00    0.60  33144.    485.   4474.   4474.     1.5    40.4   -41.4    -2.4     0.0     1.2    844.      0.
   87.00    0.60  33144.    457.   3902.   3902.     1.7    40.4   -35.0    -2.9     0.0     1.2    786.      0.
   90.00    0.60  33144.    423.   3317.   3317.     1.6    42.2   -28.4    -3.6     0.0     1.2    690.      0.
 TIME    VMACH   EVENT   HBE     DBWC    DBT1    ALPHAX  PHIBVX  PSIVLX  THTVLX  ALCOM   ANCOM   FTHALT  NFIX
   93.00    0.60  33144.    379.   2721.   2721.     1.4    41.8   -21.8    -4.7     0.0     1.1    549.      0.
   96.00    0.60  33144.    322.   2116.   2116.     1.1    36.3   -16.0    -5.9     0.0     1.1    377.      0.
   99.00    0.60  33145.    253.   1507.   1507.     0.9    25.5   -11.6    -7.0     0.0     1.0    276.      0.
  102.00    0.60  33145.    174.    895.    895.     0.7    15.0    -8.8    -7.7     0.0     1.0    277.      0.
  105.00    0.60  33145.     90.    280.    280.     0.7     6.6    -7.4    -8.1     0.0     1.0    277.      0.
  INIP.ASC:CRUISE5,APPROACHING AN IP WITH HEADING AND DIVE ANGLE          000
      PARAMETERS AT IMPACT TIME  T=   106.37     TRCOND=   0.0000
     DBT= 0.26574      SBTP1= 0.25881      SBTP2=-6.02706E-02 EPSVPX=  81.786
    DBTC= 0.26942     SBTCP1= 0.26228     SBTCP2=-6.15815E-02 PSIVLX= -7.0410
    DTCT= 3.71334E-03 STCTP1=-3.47424E-03 STCTP2= 1.31094E-03 THTVLX= -8.2136

  FVYAWM=  0.0000     TFVYAW=  0.0000     FVPITM=  0.0000     TFVPIT=  0.0000
  FRPSIM=  0.0000     TFRPSI=  0.0000     FRTHTM=  0.0000     TFRTHT=  0.0000

  106.40    0.61  33145.     49.      8.      8.     0.6     7.6    -7.0    -8.2     0.0     1.2    277.      0.
 FLT TIME =   106.     CRIT VAR =   0.00     RUN   1 OF   0 RUNS  IN  GROUP  1

Press any key to continue
