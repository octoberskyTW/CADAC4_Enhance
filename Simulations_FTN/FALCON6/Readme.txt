***********************************************************************
************************* FALCON6  SIMULATION *************************
***********************************************************************
*************** CADAC7 FAMILY OF AIRCRAFT 6 DOF SIMULATIONS ***********
***********************************************************************
***********************************************************************

VERSION 1.0 (13 APR 2000)

TYPE:		F16 Aircraft without flight control system

FEATURES:	6 DOF
			Flat earth
			Aero tables at 0.6 Mach
			Turbojet propulsion

EXECUTION:	Compile MODULE.FOR with CADX3.FOR and UTL3.FOR
			Execute with files CADIN.ASC and HEAD.ASC present in curent
			directory
			Plot results of output TRAJ.ASC with KPLOT utility.

REFERENCE:	(1) Zipfel, Peter H, "Modeling and Simulation of Aerospace Vehicle
			Dynamics", AIAA Education Series, American Insitute of
			Aeronautics and Astronautics, 1801 Alexander Bell Drive,
			Reston, VA 20191, www.aiaa.org. 

		(2) Stevens & Lewis, "Aircraft Control and
		    Simulation" Wiley-Interscience Publication, 1992.

		(3) Nguyen, J.T., et al., "Simulator Study of
		    Stall/Post-Stall Characteristics of a Fighter Airplane with
		    Relaxec Longitudinal Static Stability", NASA Tech. Paper 1538,
		    NASA, Washington, D.C. Dec. 1979.  www.aiaa.org. 

************************************************************************
************************ TEST CASES ************************************
************************************************************************

************************** INPITCH.ASC **********************************

 INPITCH.ASC:FALCON6 PITCH DOUBLET  13-APR-2000
 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSIBLX  THTBLX  PHIBLX  THRUST
     0.0   1524.    0.00    1.00    0.00      0.     -1.      0.      0.      0.      0.
     0.0   1524.    0.55    1.02    0.00      0.     -1.      0.      0.      0.   7436.
     0.5   1522.    0.55   -0.40    0.00      0.     -2.      0.     -2.      0.  11394.
     1.0   1518.    0.55   -2.78    0.00      0.     -4.      0.     -6.      0.  13852.
     1.5   1510.    0.55   -4.03    0.00      0.     -7.      0.    -11.      0.  15347.
     2.0   1497.    0.55   -4.04    0.00      0.    -10.      0.    -14.      0.  16256.
     2.5   1478.    0.55   -3.52    0.00      0.    -13.      0.    -17.      0.  16806.
     3.0   1455.    0.56   -2.87    0.00      0.    -16.      0.    -19.      0.  17135.
     3.5   1427.    0.56   -4.54    0.00      0.    -19.      0.    -23.      0.  17335.
     4.0   1394.    0.56   -8.21    0.00      0.    -24.      0.    -32.      0.  17490.
 TIME    HBE     VMACH   ALPHAX  BETAX   PSIVLX  THTVLX  PSIBLX  THTBLX  PHIBLX  THRUST
     4.5   1350.    0.56  -10.12    0.00      0.    -32.      0.    -42.      0.  17657.
     5.0   1295.    0.56  -10.34    0.00      0.    -40.      0.    -50.      0.  17817.
     5.5   1231.    0.56   -9.08    0.00      0.    -48.      0.    -57.      0.  17943.
     6.0   1158.    0.56   -7.69    0.00      0.    -54.      0.    -62.      0.  18019.
     6.0   1158.    0.56   -7.69    0.00      0.    -54.      0.    -62.      0.  18019.
 FLT TIME =   6.00     CRIT VAR =   0.00     RUN   1 OF   0 RUNS  IN  GROUP  1

Press any key to continue