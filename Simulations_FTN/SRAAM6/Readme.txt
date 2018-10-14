***********************************************************************
************************** SRAAM6  SIMULATION *************************
***********************************************************************
*************** CADAC6 FAMILY OF MISSILE 6 DOF SIMULATIONS ************
***********************************************************************
***********************************************************************

VERSION 1.0 (13 APR 2000)

TYPE:		Short Range Air-to-Air Missile

FEATURES:	6 DOF
			Flat earth
			LAR and CIRCLE fight engagements
			Aero tables as function of alpha prime, phi prime and Mach
			Single pulse rocket propulsion
			Acceleration feedback autopilot
			Compensated Pro-Nav guidance
			IIR seeker

EXECUTION:	Compile MODULE.FOR with CADX3.FOR and UTL3.FOR
			Execute with files CADIN.ASC and HEAD.ASC present in curent
			directory
			Plot results of output TRAJ.ASC with KPLOT utility.

REFERENCE:	(1) Zipfel, Peter H, "Modeling and Simulation of Aerospace 
			Vehicle Dynamics", AIAA Education Series, American Insitute 
			of Aeronautics and Astronautics, 1801 Alexander Bell Drive,
			Reston, VA 20191, www.aiaa.org.  

************************************************************************
************************ TEST CASES ************************************
************************************************************************

 INLAR1.ASC:SRAAM6;LAR1 TRAGET CENTERED ENGAGEMENT  13-APR-2000
 INLAR3.ASC:SRAAM6;LAR3 SINGLE CIRCLE FIGHT  13-APR-2000
 INLAR4.ASC:SRAAM6;LAR4 DOUBLE CIRCLE FIGHT  13-APR-2000
 INCRC3.ASC:SRAAM6;CIRCLE3 HEAD-ON CIRCLE ENGAGEMENT  13-APR-2000

 INLAR3WM.ASC:SRAAM6,LAR3 MONTE CARLO SWEEP >> turn on SWEEP in HEAD.ASC

************************** INLAR1.ASC **********************************

 INLAR1.ASC:SRAAM6;LAR1 TRAGET CENTERED ENGAGEMENT  13-APR-2000
 TIME    EVENT   VMACH   PDYNMC  DBT1    HBE     ALPHAX  BETAX   ALCOM   ANCOM   PSIVLX  THTVLX  GNDTCK
     0.0      0.     0.7  21200.      0.   5000.     0.0     0.0      0.      0.      0.      0.      0.
     0.0      0.     0.7  21201.      0.   5000.     1.8     0.0      0.      0.   -140.      0.      0.
     0.5   4631.     1.3  65015.   9789.   4999.     0.3   -16.8      8.      0.   -138.      0.    162.
     1.0   4631.     1.9 139784.   9481.   4998.     0.3   -12.5      7.      0.   -126.      0.    421.
     1.5   4631.     2.6 252795.   9066.   4996.     0.3    -7.0      8.      0.   -121.      0.    782.
     2.0   4631.     3.3 399988.   8545.   4995.     0.3    -6.3      7.      1.   -117.      0.   1249.
     2.5   4631.     3.9 571877.   7918.   4993.     0.2    -3.7      9.      1.   -114.      0.   1823.
     3.0   4630.     3.8 544750.   7245.   4990.     0.2    -6.4     13.      0.   -112.      0.   2445.
     3.5   4630.     3.6 487187.   6605.   4986.     0.2    -6.2     10.      1.   -109.      0.   3037.
     4.0   4630.     3.4 438641.   5999.   4981.     0.4    -5.7      8.      1.   -107.      0.   3596.
 TIME    EVENT   VMACH   PDYNMC  DBT1    HBE     ALPHAX  BETAX   ALCOM   ANCOM   PSIVLX  THTVLX  GNDTCK
     4.5   4630.     3.2 396839.   5423.   4976.     0.4    -5.4      8.      1.   -105.     -1.   4129.
     5.0   4630.     3.1 360608.   4876.   4971.     0.5    -5.0      6.      1.   -103.     -1.   4634.
     5.5   4630.     2.9 328890.   4354.   4965.     0.6    -4.5      6.      1.   -102.     -1.   5118.
     6.0   4630.     2.8 301069.   3856.   4960.     0.7    -4.1      5.      1.   -100.     -1.   5578.
     6.5   4630.     2.7 276448.   3378.   4954.     0.8    -3.7      4.      1.    -99.     -1.   6020.
     7.0   4630.     2.6 254644.   2921.   4948.     0.9    -3.3      3.      1.    -98.     -1.   6444.
     7.5   4630.     2.5 235279.   2482.   4942.     0.9    -3.0      2.      1.    -97.     -1.   6850.
     8.0   4630.     2.4 217923.   2060.   4936.     1.0    -2.7      2.      1.    -96.     -1.   7242.
     8.5   4630.     2.3 202405.   1653.   4930.     1.1    -2.5      2.      1.    -95.     -1.   7617.
     9.0   4630.     2.2 188382.   1258.   4924.     2.4    -2.0      1.      4.    -95.     -1.   7981.
     9.5   4630.     2.1 175355.    869.   4921.     6.8     0.4     -3.      9.    -94.      0.   8330.
 TIME    EVENT   VMACH   PDYNMC  DBT1    HBE     ALPHAX  BETAX   ALCOM   ANCOM   PSIVLX  THTVLX  GNDTCK
    10.0   4630.     2.0 159916.    482.   4928.    13.9     1.2     -9.     16.    -95.      3.   8666.
    10.5   4630.     1.9 136750.     96.   4961.    16.0     6.5    -49.      9.   -101.      8.   8981.
0 INLAR1.ASC:SRAAM6;LAR1 TRAGET CENTERED ENGAGEMENT
 INTERCEPT TIME =   10.62    LIMITER VIOLATIONS TRCOND =   9.000
    DBT=  10.047      SBTT1=  3.3804      SBTT2= -9.4555      SBTT3=-0.33211
    DBT=  10.047      SBTP1=-0.39196      SBTP2=  10.039     PSIPTX= -159.14
   DBTC=  10.047     SBTCP1=-0.39196     SBTCP2=  10.039     THTPTX= -122.08
   DTCT=  0.0000     STCTP1=  0.0000     STCTP2=  0.0000      VMACH=  1.8468
   DVBE=  592.12     PSIVLX= -102.87     THTVLX=  6.7895     PHIBLX= -34.578
   DVBT=  764.71     PSITLX=  27.166     THTTLX=  8.3933     PHITLX=  75.851
    10.6   3330.     1.8 129472.     10.   4971.    -6.2    22.4      0.      0.   -103.      7.   9055.
 FLT TIME =   10.6     CRIT VAR =   10.0     RUN   1 OF   0 RUNS  IN  GROUP  1

Press any key to continue

************************** INLAR3.ASC **********************************

 INLAR3.ASC:SRAAM6;LAR3 SINGLE CIRCLE FIGHT  13-APR-2000
 TIME    EVENT   VMACH   PDYNMC  DBT1    HBE     ALPHAX  BETAX   ALCOM   ANCOM   PSIVLX  THTVLX  GNDTCK
     0.0      0.     0.7  21200.      0.   5000.     0.0     0.0      0.      0.      0.      0.      0.
     0.0      0.     0.7  21201.      0.   5000.    13.3     0.0      0.      0.    -13.      0.      0.
     0.5   4631.     1.3  63132.   3793.   4998.     0.0   -21.7     20.     -2.     -9.     -2.    161.
     1.0   4631.     1.8 125471.   3488.   4990.     0.5   -20.3     10.      1.      9.     -1.    411.
     1.5   4631.     2.5 227484.   3064.   4989.     1.6    -7.2      9.      1.     18.      0.    752.
     2.0   4631.     3.1 367650.   2524.   4995.     0.7    -7.8     12.      1.     23.      1.   1198.
     2.5   4631.     3.7 530933.   1865.   5011.     0.9    -6.2     11.      2.     28.      2.   1750.
     3.0   4630.     3.7 505670.   1152.   5034.     1.3    -6.2     14.      2.     30.      3.   2350.
     3.5   4630.     3.5 449738.    464.   5063.     0.4    -8.2     19.      2.     34.      3.   2921.
0 INLAR3.ASC:SRAAM6;LAR3 SINGLE CIRCLE FIGHT
 INTERCEPT TIME =   3.851    LIMITER VIOLATIONS TRCOND =   9.000
    DBT=  3.9039      SBTT1=-0.25331      SBTT2= -3.8829      SBTT3=-0.31560
    DBT=  3.9039      SBTP1=-0.32247      SBTP2=  3.8906     PSIPTX=  175.29
   DBTC=  3.9039     SBTCP1=-0.32247     SBTCP2=  3.8906     THTPTX= -78.152
   DTCT=  0.0000     STCTP1=  0.0000     STCTP2=  0.0000      VMACH=  3.3277
   DVBE=  1065.5     PSIVLX=  36.256     THTVLX=  3.5421     PHIBLX= -8.6588
   DVBT=  1294.8     PSITLX= -147.48     THTTLX=  11.547     PHITLX= -35.433
     3.9   3330.     3.3 414033.      4.   5084.     5.5    -3.2      2.     22.     36.      4.   3302.
 FLT TIME =   3.85     CRIT VAR =   3.90     RUN   1 OF   0 RUNS  IN  GROUP  1
Press any key to continue

************************** INLAR4.ASC **********************************

 INLAR4.ASC:SRAAM6;LAR4 DOUBLE CIRCLE FIGHT  13-APR-2000
 TIME    EVENT   VMACH   PDYNMC  DBT1    HBE     ALPHAX  BETAX   ALCOM   ANCOM   PSIVLX  THTVLX  GNDTCK
     0.0      0.     0.7  21200.      0.   5000.     0.0     0.0      0.      0.      0.      0.      0.
     0.0      0.     0.7  21201.      0.   5000.    13.3     0.0      0.      0.    -13.      0.      0.
     0.5   4631.     1.3  62808.   2859.   4997.    -0.6   -23.7     36.      5.     -9.     -2.    161.
     1.0   4631.     1.5  88573.   2652.   4986.    -0.2   -48.0     35.     -4.     19.     -2.    397.
     1.5   4631.     1.6 102039.   2373.   4990.    -0.2   -28.6     20.      0.     52.      3.    647.
     2.0   4631.     2.3 196837.   2008.   4998.     0.3   -11.7     19.      5.     65.      1.    958.
     2.5   4631.     2.9 324303.   1528.   5010.     0.8    -9.0     11.     -1.     74.      3.   1377.
     3.0   4630.     2.9 318206.    988.   5035.     0.7    -6.0      1.      2.     77.      3.   1850.
     3.5   4630.     2.8 289495.    456.   5060.     0.8     9.9    -39.     -2.     76.      3.   2306.
0 INLAR4.ASC:SRAAM6;LAR4 DOUBLE CIRCLE FIGHT
 INTERCEPT TIME =   3.941    LIMITER VIOLATIONS TRCOND =   9.000
    DBT=  2.3126      SBTT1= 0.59994      SBTT2= -2.2326      SBTT3= 5.91082E-02
    DBT=  2.3126      SBTP1= 6.35508E-02  SBTP2=  2.3117     PSIPTX= -165.54
   DBTC=  2.3126     SBTCP1= 6.35508E-02 SBTCP2=  2.3117     THTPTX= -111.55
   DTCT=  0.0000     STCTP1=  0.0000     STCTP2=  0.0000      VMACH=  2.4306
   DVBE=  778.20     PSIVLX=  64.995     THTVLX=  5.0024     PHIBLX= -1.4403
   DVBT=  984.36     PSITLX= -145.51     THTTLX=  8.6847     PHITLX=  80.805
     3.9   5330.     2.4 220785.      2.   5088.    -0.2    33.5     -9.      9.     65.      5.   2679.
 FLT TIME =   3.94     CRIT VAR =   2.31     RUN   1 OF   0 RUNS  IN  GROUP  1

Press any key to continue

************************** INLAR4.ASC **********************************

 INCRC3.ASC:SRAAM6;CIRCLE3 HEAD-ON CIRCLE ENGAGEMENT  13-APR-2000
 TIME    EVENT   VMACH   PDYNMC  DBT1    HBE     ALPHAX  BETAX   ALCOM   ANCOM   PSIVLX  THTVLX  GNDTCK
     0.0      0.     0.7  21200.      0.   5000.     0.0     0.0      0.      0.      0.      0.      0.
     0.0      0.     0.7  21201.      0.   5000.     6.4     0.0      0.      0.    -20.      0.      0.
     0.5   4631.     1.3  63704.   5857.   4999.     0.0    23.3    -36.      1.    -24.     -1.    162.
     1.0   4631.     1.6  93495.   5730.   4995.     0.2    43.3    -12.     -1.    -52.     -1.    400.
     1.5   4631.     2.0 158582.   5478.   4995.     0.0    10.4    -18.      0.    -71.      0.    681.
     2.0   4631.     2.7 274499.   5107.   4993.     0.1    11.5    -16.      0.    -81.      0.   1061.
     2.5   4631.     3.3 422316.   4610.   4992.     0.2     7.6    -11.      1.    -87.      0.   1547.
     3.0   4630.     3.3 409962.   4048.   4991.     0.8     1.2      0.      1.    -90.      0.   2084.
     3.5   4630.     3.1 373057.   3501.   4990.     0.8    -0.6      0.      1.    -90.      0.   2600.
     4.0   4630.     3.0 340595.   2974.   4989.     0.8    -0.2      0.      2.    -89.      0.   3091.
 TIME    EVENT   VMACH   PDYNMC  DBT1    HBE     ALPHAX  BETAX   ALCOM   ANCOM   PSIVLX  THTVLX  GNDTCK
     4.5   4630.     2.9 311792.   2462.   4988.     0.8    -0.1      0.      1.    -89.      0.   3562.
     5.0   4630.     2.7 286295.   1967.   4987.     0.9     0.0      0.      1.    -89.      0.   4011.
     5.5   4630.     2.6 263574.   1484.   4985.     0.9     0.2      0.      1.    -89.      0.   4444.
     6.0   4630.     2.5 243363.   1015.   4983.     0.9     0.6     -1.      1.    -90.      0.   4857.
     6.5   4630.     2.4 225244.    557.   4981.     0.9     1.4     -1.      1.    -90.      0.   5256.
     7.0   4630.     2.3 208951.    108.   4978.     1.0     2.7     -4.      1.    -90.      0.   5640.
0 INCRC3.ASC:SRAAM6;CIRCLE3 HEAD-ON CIRCLE ENGAGEMENT
 INTERCEPT TIME =   7.121    LIMITER VIOLATIONS TRCOND =   0.000
    DBT= 3.56638E-02  SBTT1= 1.47792E-02  SBTT2= 2.20596E-02  SBTT3= 2.38087E-02
    DBT= 3.56638E-02  SBTP1= 3.27487E-02  SBTP2=-1.41219E-02 PSIPTX= -155.95
   DBTC= 3.56638E-02 SBTCP1= 3.27487E-02 SBTCP2=-1.41219E-02 THTPTX= -133.36
   DTCT=  0.0000     STCTP1=  0.0000     STCTP2=  0.0000      VMACH=  2.3263
   DVBE=  745.87     PSIVLX= -90.673     THTVLX=-0.45092     PHIBLX=  2.2366
   DVBT=  883.73     PSITLX=  26.967     THTTLX= -1.2669     PHITLX=  65.488
     7.1   5630.     2.3 205258.      0.   4978.     1.0     3.3    -14.      5.    -91.      0.   5731.
 FLT TIME =   7.12     CRIT VAR =  0.357E-01 RUN   1 OF   0 RUNS  IN  GROUP  1

Press any key to continue
