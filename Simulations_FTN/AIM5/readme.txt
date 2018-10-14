***********************************************************************
**************************** AIM5  SIMULATION *************************
***********************************************************************
********************* CADAC4 FAMILY OF 5 DOF SIMULATIONS **************
***********************************************************************
***********************************************************************

VERSION 1.0 (18 APRIL 2000)

TYPE:		Simple air-to-air missile against constant speed target
			Lock-on before launch

FEATURES:	Pseudo 5 DOF
			Flat earth
			Trimmed aerodynamics
			Rocket propulsion
			Proportional/Integral acceleration autopilot
			LOS seeker

EXECUTION:	Compile MODULE.FOR with CADX3.FOR and UTL3.FOR
			Execute with files CADIN.ASC and HEAD.ASC present in curent
			directory
			Plot results of output TRAJ.ASC with KPLOT utility.

REFERENCE:	Zipfel, Peter H, "Modeling and Simulation of Aerospace Vehicle
			Dynamics", AIAA Education Series, American Insitute of
			Aeronautics and Astronautics, 1801 Alexander Bell Drive,
			Reston, VA 20191, www.aiaa.org.   

************************************************************************
************************ TEST CASES ************************************
************************************************************************
 INHORI.ASC: HORIZONTAL ENGAGEMENT  
 INHORI3.ASC: HORIZONTAL ENGAGEMENT 3 TEST CASES  
 INVERT.ASC: VERTICAL SHOOT-DOWN TEST CASE 
 INVERT3.ASC: VERTICAL SHOOT-DOWN 3 TEST CASES 
 INLENV.ASC: AIM5 LAUNCH ENVELOPE - not shown because of size

Note: you miss distance may vary 
************************** INHORI.ASC **********************************

 INHORI.ASC: HORIZONTAL ENGAGEMENT 3 TEST CASES  18-APR-2000
 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
     0.0  10000.    0.00     0.0     0.0     45.      0.     0.0     0.0      0.
     0.0  10000.    0.91     0.0    -0.1     45.      0.    45.0     0.0   8995.
     0.5   9999.    1.77     1.3    -5.6     47.      0.    38.6    -0.8   8726.
     1.0   9999.    2.86     1.5   -11.5     53.      0.    28.5    -1.5   8335.
     1.5  10004.    3.58     0.8   -14.5     61.      1.    19.1    -1.5   7811.
     2.0  10012.    3.88     0.3   -15.0     70.      1.    11.8    -1.2   7191.
     2.5  10021.    3.95     0.1   -14.5     79.      1.     5.5    -0.9   6520.
     3.0  10027.    4.04     0.0   -13.0     87.      0.     0.3    -0.7   5816.
     3.5  10031.    3.81    -0.1   -12.2     94.      0.    -3.9    -0.5   5107.
     4.0  10031.    3.61     0.0   -10.8     99.      0.    -6.9    -0.3   4425.
 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
     4.5  10030.    3.45     0.1    -9.2    104.      0.    -8.8    -0.3   3767.
     5.0  10026.    3.32     0.3    -7.4    107.     -1.    -9.6    -0.3   3130.
     5.5  10021.    3.21     0.6    -5.6    108.     -1.    -9.5    -0.4   2513.
     6.0  10015.    3.11     1.2    -3.6    109.     -1.    -8.5    -0.8   1911.
     6.5  10008.    3.02     2.0     0.0    110.     -1.    -5.6    -1.6   1324.
     7.0  10003.    2.94     3.2     6.0    109.     -1.     0.3    -2.8    750.
     7.5  10001.    2.84     1.8    10.0    106.      0.     6.2    -2.1    188.
  INHORI.ASC: HORIZONTAL ENGAGEMENT 3 TEST CASES
  PARAMETERS AT TIME OF CLOSEST APPROACH T=   7.6701
     DBT=  1.5651      SBTL1=  1.3379      SBTL2= 0.28418      SBTL3=-0.76074

     7.7  10001.    2.81    -5.7     0.2    106.      0.    83.5   -28.6      2.
 FLT TIME =   7.67     CRIT VAR =   1.57     RUN   1 OF   0 RUNS  IN  GROUP  1

Press any key to continue
*************************************************************************
DBT = Miss distance - m
SBTL1,2,3 = Miss components in local level coordiantes - m
*************************************************************************


************************** INHORI3.ASC **********************************

 INHORI3.ASC: HORIZONTAL ENGAGEMENT 3 TEST CASES  08-NOV-1999
 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
     0.0  10000.    0.00     0.0     0.0      0.      0.     0.0     0.0      0.
     0.0  10000.    0.91     0.1    -0.1     45.      0.    45.0     0.0   8995.
     1.0   9998.    2.85     4.7   -11.3     53.      1.    28.7    -4.9   8335.
     2.0  10040.    3.87     1.0   -15.0     70.      3.    11.9    -3.8   7195.
     3.0  10087.    4.03    -0.1   -13.1     87.      2.     0.3    -2.3   5825.
     4.0  10102.    3.60     0.0   -10.9     99.      0.    -6.9    -1.1   4438.
     5.0  10084.    3.31     1.1    -7.5    106.     -2.    -9.6    -0.9   3147.
     6.0  10048.    3.11     3.8    -3.4    109.     -3.    -8.3    -2.6   1930.
     7.0  10012.    2.92     7.3     5.1    109.     -2.    -0.4    -6.6    771.
  INHORI3.ASC: HORIZONTAL ENGAGEMENT 3 TEST CASES
  PARAMETERS AT TIME OF CLOSEST APPROACH T=   7.6919
     DBT=  1.3142      SBTL1= 0.78655      SBTL2= 0.19189      SBTL3= -1.0352

     7.7  10001.    2.79   -13.9    -4.9    106.     -1.   165.2   -20.3      9.
 FLT TIME =   7.70     CRIT VAR =   1.31     RUN   1 OF   0 RUNS  IN  GROUP  1

 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
     0.0  10000.    0.00     0.0     0.0      0.      0.     0.0     0.0      0.
     0.0  10000.    0.91     0.1     0.0     45.      0.    45.0     0.0   8995.
     1.0   9997.    2.86     4.7    -8.5     50.      1.    33.7    -4.5   8340.
     2.0  10038.    3.96     1.6   -12.3     63.      3.    22.0    -4.1   7238.
     3.0  10094.    4.16     0.4   -13.1     78.      2.    11.6    -3.4   5920.
     4.0  10128.    3.64    -0.1   -14.6     93.      1.     0.5    -2.3   4570.
     5.0  10127.    3.22     0.0   -13.8    107.     -1.    -8.4    -1.1   3314.
     6.0  10095.    2.93     1.0   -11.6    117.     -3.   -14.1    -0.8   2152.
     7.0  10047.    2.73     3.4    -8.5    124.     -4.   -16.0    -2.1   1070.
     8.0  10000.    2.54    17.1    22.0    124.     -1.     9.8   -15.8     47.
  INHORI3.ASC: HORIZONTAL ENGAGEMENT 3 TEST CASES
  PARAMETERS AT TIME OF CLOSEST APPROACH T=   8.0475
     DBT=  2.5944      SBTL1= -2.3312      SBTL2= -1.0752      SBTL3= 0.37500

     8.1  10000.    2.49    22.0    34.5    123.     -1.  -105.3    12.3      4.
 FLT TIME =   8.05     CRIT VAR =   2.59     RUN   1 OF   0 RUNS  IN  GROUP  2

 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
     0.0  10000.    0.00     0.0     0.0      0.      0.     0.0     0.0      0.
     0.0  10000.    0.91     0.1     0.0     45.      0.    45.0     0.0   8995.
     1.0   9997.    2.87     4.7    -5.6     49.      1.    38.6    -4.1   8345.
     2.0  10033.    4.04     2.2    -8.7     56.      3.    33.1    -4.1   7290.
     3.0  10094.    4.34     1.2   -10.0     66.      3.    29.3    -4.1   6075.
     4.0  10150.    3.87     0.7   -13.3     78.      2.    23.1    -4.2   4857.
     5.0  10182.    3.37     0.2   -17.3     94.      1.    13.5    -3.9   3727.
     6.0  10184.    2.84    -0.2   -21.8    113.     -1.     0.2    -2.7   2695.
     7.0  10154.    2.31     0.0   -26.3    135.     -4.   -15.1    -1.4   1792.
     8.0  10102.    1.82     1.9   -30.9    158.     -6.   -30.6    -1.2   1054.
     9.0  10047.    1.39     5.7   -35.0    182.     -7.   -40.5    -2.5    516.
 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
    10.0  10001.    1.12    12.4   -35.0    202.     -7.   -10.8    -6.2    222.
    11.0   9969.    0.96    35.0   -35.0    216.     -4.    44.3   -18.9    301.
    12.0   9955.    0.87    35.0   -35.0    224.     -2.    55.1   -15.9    493.
    12.0   9955.    0.87    35.0   -35.0    224.     -2.    55.1   -15.9    493.
 FLT TIME =   12.0     CRIT VAR =   0.00     RUN   1 OF   0 RUNS  IN  GROUP  3

Press any key to continue


******************************* INVERT.ASC*****************************


 INVERT.ASC: VERTICAL SHOOT-DOWN TEST CASE  05-NOV-1999
 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
     0.0  10000.    0.00     0.0     0.0      0.      0.     0.0     0.0   9000.
     0.0  10000.    0.91     0.0     0.0      0.      0.     0.0   -23.9   9844.
     0.5   9995.    1.77    -1.4     0.0      0.     -3.     0.0   -20.6   9538.
     1.0   9968.    2.88    -5.2     0.0      0.     -6.     0.0   -14.7   9085.
     1.5   9897.    3.67    -8.1     0.0      0.    -11.     0.0    -8.7   8487.
     2.0   9764.    4.06    -8.2     0.0      0.    -16.     0.0    -5.1   7797.
     2.5   9572.    4.22    -7.8     0.0      0.    -20.     0.0    -2.3   7063.
     3.0   9323.    4.37    -7.2     0.0      0.    -25.     0.0     0.3   6300.
     3.5   9030.    4.17    -7.1     0.0      0.    -29.     0.0     2.8   5536.
     4.0   8714.    3.97    -7.0     0.0      0.    -33.     0.0     5.2   4800.
 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
     4.5   8378.    3.79    -6.7     0.0      0.    -36.     0.0     7.4   4090.
     5.0   8028.    3.62    -6.3     0.0      0.    -40.     0.0     9.4   3407.
     5.5   7668.    3.47    -5.8     0.0      0.    -43.     0.0    11.1   2748.
     6.0   7302.    3.33    -5.4     0.0      0.    -45.     0.0    12.6   2112.
     6.5   6934.    3.20    -4.8     0.0      0.    -48.     0.0    13.7   1498.
     7.0   6566.    3.08    -3.5     0.0      0.    -49.     0.0    14.1    903.
     7.5   6201.    2.96     0.8     0.0      0.    -51.     0.0    11.9    326.
  INVERT.ASC: VERTICAL SHOOT-DOWN TEST CASE
  PARAMETERS AT TIME OF CLOSEST APPROACH T=   7.7897
     DBT=  6.3125      SBTL1= -3.8545      SBTL2=-8.71242E-05  SBTL3=  4.9990

     7.8   5995.    2.87    19.9     0.0      0.    -48.     0.0    85.3      6.
 FLT TIME =   7.79     CRIT VAR =   6.31     RUN   1 OF   0 RUNS  IN  GROUP  1

Press any key to continue


***************************** INVERT3.ASC *****************************

 INVERT3.ASC: VERTICAL SHOOT-DOWN 3 TEST CASES  08-NOV-1999
 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
     0.0  10000.    0.00     0.0     0.0      0.      0.     0.0     0.0      0.
     0.0  10000.    0.91     0.0     0.0      0.      0.     0.0   -23.9   9844.
     1.0   9958.    2.88    -8.1     0.0      0.     -8.     0.0    -9.5   9082.
     2.0   9682.    4.03    -8.9     0.0      0.    -21.     0.0     1.2   7779.
     3.0   9141.    4.35    -6.2     0.0      0.    -30.     0.0     5.6   6271.
     4.0   8464.    3.98    -5.0     0.0      0.    -35.     0.0     8.6   4760.
     5.0   7761.    3.65    -3.1     0.0      0.    -39.     0.0    10.1   3351.
     6.0   7061.    3.36     0.2     0.0      0.    -41.     0.0     9.5   2034.
     7.0   6394.    3.10     6.5     0.0      0.    -41.     0.0     4.3    794.
  INVERT3.ASC: VERTICAL SHOOT-DOWN 3 TEST CASES
  PARAMETERS AT TIME OF CLOSEST APPROACH T=   7.6722
     DBT= 0.78751      SBTL1= 0.36230      SBTL2= 4.21958E-08  SBTL3=-0.69922

     7.7   5996.    2.89     4.5     0.0      0.    -36.  -180.0    -8.5      9.
 FLT TIME =   7.68     CRIT VAR =  0.788     RUN   1 OF   0 RUNS  IN  GROUP  1

 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
     0.0  10000.    0.00     0.0     0.0      0.      0.     0.0     0.0      0.
     0.0  10000.    0.91     0.0     0.0      0.      0.     0.0   -23.9   9844.
     1.0   9968.    2.88    -5.2     0.0      0.     -6.     0.0   -14.7   9085.
     2.0   9764.    4.06    -8.2     0.0      0.    -16.     0.0    -5.1   7797.
     3.0   9323.    4.37    -7.2     0.0      0.    -25.     0.0     0.3   6300.
     4.0   8714.    3.97    -7.0     0.0      0.    -33.     0.0     5.2   4800.
     5.0   8028.    3.62    -6.3     0.0      0.    -40.     0.0     9.4   3407.
     6.0   7302.    3.33    -5.4     0.0      0.    -45.     0.0    12.6   2112.
     7.0   6566.    3.08    -3.5     0.0      0.    -49.     0.0    14.1    903.
  INVERT3.ASC: VERTICAL SHOOT-DOWN 3 TEST CASES
  PARAMETERS AT TIME OF CLOSEST APPROACH T=   7.7897
     DBT=  6.3125      SBTL1= -3.8545      SBTL2=-1.91865E-07  SBTL3=  4.9990

     7.8   5995.    2.87    19.9     0.0      0.    -48.     0.0    85.3      6.
 FLT TIME =   7.79     CRIT VAR =   6.31     RUN   1 OF   0 RUNS  IN  GROUP  2

     0.0  10000.    0.00     0.0     0.0      0.      0.     0.0     0.0      0.
 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSISBX  THTSBX  DBT1
     0.0  10000.    0.91     0.0     0.0      0.      0.     0.0   -23.9   9844.
     1.0   9977.    2.89    -2.0     0.0      0.     -4.     0.0   -20.1   9088.
     2.0   9852.    4.10    -6.5     0.0      0.    -10.     0.0   -13.4   7824.
     3.0   9553.    4.43    -6.9     0.0      0.    -17.     0.0    -9.5   6362.
     4.0   9085.    4.02    -8.3     0.0      0.    -26.     0.0    -4.7   4899.
     5.0   8489.    3.63    -9.5     0.0      0.    -36.     0.0     1.1   3544.
     6.0   7781.    3.28   -10.8     0.0      0.    -48.     0.0     8.1   2297.
     7.0   6994.    2.93   -12.7     0.0      0.    -62.     0.0    16.6   1168.
     8.0   6183.    2.51   -22.8     0.0      0.    -83.     0.0    32.7    191.
  INVERT3.ASC: VERTICAL SHOOT-DOWN 3 TEST CASES
  PARAMETERS AT TIME OF CLOSEST APPROACH T=   8.2345
     DBT=  18.353      SBTL1=  17.443      SBTL2= 1.06444E-06  SBTL3= -5.7085

     8.2   6002.    2.23   -35.0     0.0      0.    -95.     0.0   -45.0     19.
 FLT TIME =   8.24     CRIT VAR =   18.4     RUN   1 OF   0 RUNS  IN  GROUP  3

Press any key to continue
