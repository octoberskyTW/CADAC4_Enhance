C**********************************************************************
C*********************** MODULES FOR AIM5 *****************************
C**********************************************************************
C*** *
C*** * Simple short range air-to-air missile program
C*** * Member of CADAC4-Family of pseudo-5 DoF programs
C*** *
C*** * Calling sequence of Modules:
C*** *   G1   TARGET
C*** *   G2   AIR DATA
C*** *   S1   SEEKER
C*** *   C1   GUIDANCE
C*** *   C2   CONTROL
C*** *   A1   AERO COEF
C*** *   A2   PROPULSION
C*** *   A3   FORCES
C*** *   D1   DYNAMICS
C*** *   D2   ROTATION
C*** *
C*** * MODIFICATION HISTORY
C*** * 000418 Version 1,0 Created by Peter Zipfel
C*** *
C**********************************************************************
C**********************************************************************
      SUBROUTINE A1I
C**********************************************************************
C*** * The initialization module for the Aerodynamic Module A1
C*** * Reserved C(3510) locations are 1200-1299
C*** * This module performs the following functions:
C*** *
C*** * (1) Provides the trimmed aerodynamic tables of a generic air-to
C*** *     air missile AIM5; length= 2.25 m, diameter = 0.15 m,
C*** *	 reference AREA= 0.01767 m^2
C*** *	 c.m. location with thrust-on at 52.4% from nose
C*** *						thrust-off at 48%
C*** *
C*** * MODIFICATION HISTORY
C*** * 991102 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      COMMON/AERO1/ATBLON,ATBLOF,TNTBL
C
      DIMENSION ATBLON(64),ATBLOF(64),TNTBL(64)
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(1202),CY)
      EQUIVALENCE (C(1210),PHIP)
C
C*** THRUST-ON DRAG COEFFICIENT
C
C    MACH NO ACROSS: .7  .9  1.1  1.4  1.8  2.5  3.5  5.
C    ANGLE OF ATTACK DOWN: 0  5  10  15  20  25  30  35
C
      DATA(ATBLON(N),N=1,64)/
     C 0.207,0.317,0.751,0.682,0.575,0.611,0.613,0.585,
     C 0.233,0.344,0.775,0.697,0.597,0.635,0.649,0.630,
     C 0.306,0.412,0.830,0.761,0.723,0.810,0.887,0.857,
     C 0.463,0.584,1.048,1.053,1.173,1.329,1.356,1.270,
     C 0.722,0.887,1.441,1.758,2.077,2.114,2.061,1.950,
     C 1.275,1.567,2.494,3.071,3.291,3.111,3.038,2.964,
     C 2.054,2.639,4.002,4.658,4.692,4.408,4.370,4.307,
     C 3.063,4.059,5.656,6.619,6.347,5.986,6.053,5.977/
C
C*** THRUST-OFF DRAG COEFFICIENT
C
      DATA(ATBLOF(N),N=1,64)/
     C 0.260,0.369,0.830,0.754,0.638,0.657,0.644,0.607,
     C 0.287,0.398,0.858,0.770,0.661,0.681,0.677,0.647,
     C 0.363,0.468,0.916,0.837,0.788,0.847,0.895,0.854,
     C 0.526,0.654,1.153,1.138,1.233,1.349,1.339,1.245,
     C 0.833,0.971,1.577,1.870,2.141,2.116,2.016,1.902,
     C 1.371,1.676,2.669,3.203,3.360,3.105,2.975,2.895,
     C 2.183,2.794,4.235,4.819,4.782,4.401,4.298,4.231,
     C 3.236,4.274,5.956,6.836,6.471,5.996,5.982,5.904/
C
C*** LIFT COEFFICIENT
C
      DATA(TNTBL(N),N=1,64)/
     C 0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     C 0.399,0.405,0.400,0.390,0.404,0.376,0.398,0.421,
     C 0.890,0.913,0.958,1.011,1.132,1.265,1.465,1.355,
     C 1.485,1.602,1.898,2.152,2.604,2.834,2.659,2.345,
     C 2.300,2.644,3.389,4.154,4.608,4.336,3.879,3.557,
     C 3.394,3.986,5.619,6.448,6.416,5.666,5.171,4.962,
     C 4.735,5.847,8.267,8.370,7.846,6.980,6.569,6.387,
     C 6.335,8.048,10.56,10.15,9.081,8.191,7.917,7.719/
C
      CY=0.0
      PHIP=0.
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A1
C**********************************************************************
C*** * Aerodynamic Module A1 of generic air-to-air missile AIM5
C*** * Reserved C(3510) locations are 1200-1299
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates  axial, normal and side force coefficients
C*** *     force coefficients
C*** * (2) Calculates  lift and drag coefficients for diagnositcs
C*** *
C*** * MODIFICATION HISTORY
C*** * 991102 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      COMMON/AERO1/ATBLON,ATBLOF,TNTBL
C
      DIMENSION NXY(2),COFARG(16),ATBLON(8,8),ATBLOF(8,8),TNTBL(8,8)
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(1202),CY)
      EQUIVALENCE (C(1210),PHIP)
C
C PHIP = I Aerodynamic roll angle - rad
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0221),VMACH)
      EQUIVALENCE (C(0908),ALPHA)
      EQUIVALENCE (C(0909),BETA)
      EQUIVALENCE (C(1300),MPROP)
C
C VMACH= O Mach number - ND
C ALPHA= O Angle-of-attack - rad
C BETA= O Sideslip angle - rad
C MPROP= O MPROP=0: Motor stopped burning
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1200),CA)
      EQUIVALENCE (C(1201),CN)
      EQUIVALENCE (C(1202),CY)
C
C CA = O Axial force coefficient - ND
C CN = O Normal force coefficient - ND
C CY = O Side force coefficient - ND
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1209),ALPPX)
      EQUIVALENCE (C(1222),CL)
      EQUIVALENCE (C(1223),CD)
C
C ALPPX = G Total angle of attack - deg
C CL = G Lift coefficient - ND
C CD = G Drag coefficient - ND
C
C*** INITIALIZE ROWS AND COLUMNS OF TABLES
C
      DATA COFARG/.7, .9, 1.1, 1.4, 1.8, 2.5, 3.5, 5.,
     &            0., 5., 10., 15., 20., 25., 30., 35./
      DATA NXY/8,8/
C
C*** CONVERT TO ROLL COORDINATES
C
      ALPP=ACOS(COS(ALPHA)*COS(BETA))
C
      DUM1=TAN(BETA)
      DUM2=SIN(ALPHA)
      IF(ABS(DUM2).LT.1.E-5) DUM2=SIGN(1.E-5,DUM2)
      PHIP=ATAN2(DUM1,DUM2)
C
C*** CONVERT TO DEGREES
C
      ALPPX=ALPP*CRAD
      PHIPX=PHIP*CRAD
C
C*** CALL TABL2 TO FIND CD AT ALPPX AND VMACH
C
      VMACHI=VMACH
      ALPHAI=ABS(ALPPX)
      IF(MPROP.EQ.0) THEN
         CALL TABL2(VMACHI,ALPHAI,COFARG,ATBLOF,NXY,CDT)
      ELSE
         CALL TABL2(VMACHI,ALPHAI,COFARG,ATBLON,NXY,CDT)
      ENDIF
      CD=CDT
C
C*** CALL TABL2 TO FIND CL AT ALPPX AND VMACH
C
      VMACHI=VMACH
      ALPHAI=ABS(ALPPX)
C
      CALL TABL2(VMACHI,ALPHAI,COFARG,TNTBL,NXY,CLT)
      CL=CLT
C
C*** BODY AERO COEFF IN MANEUVER PLANE
C
      CALPH=COS(ALPP)
      SALPH=SIN(ALPP)
      CNM=CL*CALPH+CD*SALPH
      CAM=CD*CALPH-CL*SALPH
C
C*** AERO COEFF IN BODY AXES AND OUTPUT TO A3 MODULE
C
      IF(ALPP.LT.0.0)CNM=-CNM
      CPHIP=COS(PHIP)
      SPHIP=SIN(PHIP)
      CY=-CNM*SPHIP
      CN=CNM*CPHIP
      CA=CAM
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A2
C**********************************************************************
C*** * Propulsion Module A2.
C*** * Reserved C(3510) locations are 1300-1399
C*** * Configuration: Boost rocket motor, ISP = 267 sec
C*** *                Initial mass of vehicle 63.8 kg
C*** * This module performs the following functions:
C*** *
C*** * 1) Calculates vehicle mass
C*** * 2) Calculates the rocket thrust at altitude
C*** *
C*** * MODIFICATION HISTORY
C*** * 991102 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION PROTIM(10),THRUST(10),TMASS(10)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1302),AEXIT)
C
C AEXIT = D Nozzle exit area = m^2
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0212),PPRES)
      EQUIVALENCE (C(2000),T)
C
C PPRES= O Atmospheric pressure - Pa
C T= E Time since launch - s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1310),AMASS)
C
C MPROP = O MPROP=0: Motor stopped burning
C FTHALT = O Thrust at altitude - N
C AMASS = O Missile mass - kg
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1311),FTHSL)
C
C FTHSL = G Sea level thrust - N
C
C*** ROCKET MOTOR DATA
C
      DATA PROTIM/0.,.01,.5,.82,1.,1.5,2.,2.36,3.,3.02/
      DATA THRUST/28075.,28075.,34614.,39231.,30767.,18464.,8847.,
     &7308.,8308.,0./
      DATA TMASS/63.8,63.8,57.4,52.6,50.,44.9,42.,40.7,38.6,38.6/
      DATA NX/10/
C
C*** DETERMINE PROPULSIVE STATUS
C
      IF(T.LT.3.02) THEN
C
C***    DETERMINE MISSILE MASS AND THRUST
C
         CALL TABLE(T,PROTIM,TMASS,NX,AMASS)
         CALL TABLE(T,PROTIM,THRUST,NX,FTHSL)
         FTHALT=FTHSL+(101325.-PPRES)*AEXIT
      ELSE
         MPROP=0
         FTHALT=0.
      ENDIF
      RETURN
      END
C**********************************************************************
      SUBROUTINE A3
C**********************************************************************
C*** * Force Module A3
C*** * Reserved C(3510) locations are 1400-1499
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates specific force in velosity axes
C*** * (2) Calculates T.M. TBV for skid-to-turn and bank-to-turn vehicles
C*** *
C*** * MODIFICATION HISTORY
C*** * 991103 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION FB(3),FSPB(3),TVB(3,3),TBV(3,3),FSPV(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1403),AREA)
      EQUIVALENCE (C(1410),FRACA)
      EQUIVALENCE (C(1411),FRACN)
      EQUIVALENCE (C(1412),FRACY)
C
C AREA = D Reference area for aerodynamic data - m^2
C FRACA = D Fractional increase in axial force coefficient
C FRACN = D Fractional increase in normal force coefficient
C FRACY = D Fractional increase in side force coefficient
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0054),AGRAV)
C
C AGRAV = E Gravitaional acceleration 9.8066 m/s^2
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0220),PDYNMC)
      EQUIVALENCE (C(1200),CA)
      EQUIVALENCE (C(1201),CN)
      EQUIVALENCE (C(1202),CY)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1310),AMASS)
C
C PDYNMC= O Dynamic pressure - Pa
C CA= O Axial force coefficient - ND
C CN= O Normal force coefficient - ND
C CY= O Side force coefficient - ND
C FTHALT= O Thrust at altitude - N
C AMASS= O Missile mass - kg
C
C*** OUTPUTS TO OTHER MODULES
C
      EQUIVALENCE (C(1400),FSPB(1))
      EQUIVALENCE (C(1413),TBV(1,1))
      EQUIVALENCE (C(1423),FSPV(1))
C
C FSPB(3) = O Specific force in body coor - N/kg
C TBV(3,3) = O Trans matrix of body wrt flight path axes
C FSPV(3) = O Specific force in flight path axes - N/kg
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1426),FB(1))
      EQUIVALENCE (C(1437),AL)
      EQUIVALENCE (C(1438),AN)
C
C FB(3) = G Aero and thrust forces in body axes - N
C AL = G Side load factor - g's
C AN = G Normal load factor - g's
C
C*** FRACTIONAL CHANGE IN THE AERODYNAMIC COEFFICIENTS
C
      CA = (1.0 + FRACA)*CA
      CN = (1.0 + FRACN)*CN
      CY = (1.0 + FRACY)*CY
C
C*** AERODYNAMIC AND PROPULSION FORCES IN BODY COOR.
C
      FB(1) = FTHALT-CA*PDYNMC*AREA
      FB(2) =  CY*PDYNMC*AREA
      FB(3) = -CN*PDYNMC*AREA
C
C*** SPECIFIC FORCE IN BODY AXES
C
      CALL MATCON(FSPB,1./AMASS,FB,3,1)
      AL=FSPB(2)/AGRAV
      AN=-FSPB(3)/AGRAV
C
C*** SPECIFIC FORCE IN WIND AXES
C
      CALL A3TRA(TBV)
      CALL MATTRA(TVB,TBV,3,3)
      CALL MATMUL(FSPV,TVB,FSPB,3,3,1)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A3TRA(TBV)
C**********************************************************************
C*** * Calculation of TBV Transformation Matrix
C*** *
C*** * Argument Output
C*** *				TBV(3,3)=T.M. of body wrt velocity coordiantes
C*** *
C*** * MODIFICATION HISTORY
C*** * 991103 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION TBV(3,3)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0908),ALPHA)
      EQUIVALENCE (C(0909),BETA)
C
C ALPHA= O Angle-of-attack - rad
C BETA= O Sideslip angle - rad
C
C*** YAW TO TURN
C
         CALP=COS(ALPHA)
         SALP=SIN(ALPHA)
         CBET=COS(BETA)
         SBET=SIN(BETA)
C
         TBV(1,1)=CALP*CBET
         TBV(1,2)=-CALP*SBET
         TBV(1,3)=-SALP
         TBV(2,1)=SBET
         TBV(2,2)=CBET
         TBV(2,3)=0.
         TBV(3,1)=SALP*CBET
         TBV(3,2)=-SALP*SBET
         TBV(3,3)=CALP
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C1
C**********************************************************************
C*** * Guidance Module C1
C*** * Reserved C(3510) locations are 800-899
C*** *
C*** * MGUID=6 Simple proportional navigation
C*** *
C*** * MODIFICATION HISTORY
C*** * 991102 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION TBL(3,3),TLB(3,3),UT1BB(3),WOELK(3),DUM9(3,3),DUM3(3)
     +,APL(3),GRAV(3),APNL(3),UT1BL(3),APNB(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(0801),GNAV)
C
C MGUID = D MGUID=6: Proportional navigation guidance
C GNAV = D Navigation ratio - ND
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0411),DVBT1K)
      EQUIVALENCE (C(0412),WOELK(1))
      EQUIVALENCE (C(0447),THTSB)
      EQUIVALENCE (C(0454),PSISB)
      EQUIVALENCE (C(1701),TBL(1,1))
C
C AGRAV= E Gravitaional acceleration 9.8066 m/s^2
C DVBT1K= O Closing velocity - m/s
C WOELK= O Inertial LOS rate rad/s
C THTSB= O Seeker elevation angle from body frame - rad
C PSISB= G Seeker azimuth angle from body frame - rad
C TBL= O Trans matrix of body wrt local earth axes
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0802),ALCOM)
      EQUIVALENCE (C(0803),ANCOM)
C
C ALCOM = O Lateral acceleration command - g's
C ANCOM = O Normal accelration command - g's
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0804),APNB(1))
C
C APNB(3) = G Demanded acceleration in body axes - m/s^
C
      IF(MGUID.EQ.6) THEN
C
C*** PROPORTIONAL NAVIGATION
C
         CALL MATCAR(UT1BB,1.,PSISB,THTSB)
         CALL MATTRA(TLB,TBL,3,3)
         CALL MATMUL(UT1BL,TLB,UT1BB,3,3,1)
         CALL MATSKS(DUM9,WOELK)
         CALL MATMUL(DUM3,DUM9,UT1BL,3,3,1)
         DUM1=GNAV*DVBT1K
         CALL MATCON(APL,DUM1,DUM3,3,1)
         CALL VECVEC(GRAV,0.,0.,AGRAV)
         CALL MATSUB(APNL,APL,GRAV,3,1)
         CALL MATMUL(APNB,TBL,APNL,3,3,1)
C
         ANCOM=-APNB(3)/AGRAV
         ALCOM=APNB(2)/AGRAV
      ENDIF
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C2I
C**********************************************************************
C*** * The initialization module for the Control Module C2
C*** * Reserved C(3510) locations are 900-999
C*** * This module performs the following functions:
C*** *
C*** * (1) Sets up integration variables
C*** *
C*** * MAUT =|MAUTL|MAUTP|
C*** *
C*** *       |MAUTL|= 4   Acceleration Hold
C*** *             |MAUTP|= 4 Acceleration Hold
C*** *
C*** * MODIFICATION HISTORY
C*** * 991102 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C NIP = E Number of variables to be integrated
C IPL(100) = E C-locaion of the derivative of the state variable
C IPLV(100) = E C-locaion of the the state variable
C
C*** INITIALIZE STATE VARIABLES
C
      IPL(NIP)=946
      IPLV(NIP)=947
      NIP=NIP+1
      IPL(NIP)=948
      IPLV(NIP)=949
      NIP=NIP+1
      IPL(NIP)=950
      IPLV(NIP)=951
      NIP=NIP+1
      IPL(NIP)=952
      IPLV(NIP)=953
      NIP=NIP+1
      IPL(NIP)=954
      IPLV(NIP)=955
      NIP=NIP+1
      IPL(NIP)=956
      IPLV(NIP)=957
      NIP=NIP+1
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C2
C**********************************************************************
C*** * Proportional-integral autopilot Module C2
C*** * Reserved C(3510) locations are 900-999
C*** * This module performs the following functions:
C*** *
C*** * (1)
C*** *
C*** * MAUT =|MAUTL|MAUTP|
C*** *
C*** *       |MAUTL|= 4   Acceleration Hold
C*** *             |MAUTP|= 4 Acceleration Hold
C*** *
C*** * MODIFICATION HISTORY
C*** * 991102 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION FSPB(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0900),MAUT)
      EQUIVALENCE (C(0901),TR)
      EQUIVALENCE (C(0902),TA)
      EQUIVALENCE (C(0903),GACP)
      EQUIVALENCE (C(0910),ANLIM)
      EQUIVALENCE (C(0911),ALLIM)
      EQUIVALENCE (C(0912),CNALP)
      EQUIVALENCE (C(0977),ALPLIM)
      EQUIVALENCE (C(0979),BETLIM)
C
C MAUT = D MAUT=44 Acceleration autopilot in both planes
C TR = D/G Rate loop time const (if > 0: simple rate loop)
C TA = D Ratio of prop/integral gains.
C GACP = D Root locus gain of accel loop - rad/s2
C ANLIM = D Pitch acceleration limiter - g's
C ALLIM = D Yaw acceleration limiter - g's
C CNALP = D Normal force derivative - 1/rad
C ALPLIM = D Alpha limiter - rad
C BETLIM = D Beta limiter - rad
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0219),DVBA)
      EQUIVALENCE (C(0220),PDYNMC)
      EQUIVALENCE (C(0802),ALCOM)
      EQUIVALENCE (C(0803),ANCOM)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1400),FSPB(1))
      EQUIVALENCE (C(1403),AREA)
      EQUIVALENCE (C(1310),AMASS)
C
C DVBA= O Speed of missile wrt air - m/s
C PDYNMC= O Dynamic pressure - Pa
C ALCOM= O Lateral acceleration command - g's
C ANCOM= O Normal accelration command - g's
C FTHALT= O Thrust at altitude - N
C FSPB= O Specific force in body coor - N/kg
C AREA= D Reference area for aerodynamic data - m^2
C AMASS= O Missile mass - kg
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0908),ALPHA)
      EQUIVALENCE (C(0909),BETA)
      EQUIVALENCE (C(0927),ALPHAX)
      EQUIVALENCE (C(0928),BETAX)
C
C ALPHA = O Angle-of-attack - rad
C BETA = O Sideslip angle - rad
C ALPHAX = O Angle-of-attack - deg
C BETAX = O Sideslip angle - deg
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0946),XID)
      EQUIVALENCE (C(0947),XI)
      EQUIVALENCE (C(0948),YID)
      EQUIVALENCE (C(0949),YI)
      EQUIVALENCE (C(0950),RATEPD)
      EQUIVALENCE (C(0951),RATEP)
      EQUIVALENCE (C(0952),RATEYD)
      EQUIVALENCE (C(0953),RATEY)
      EQUIVALENCE (C(0954),ALPD)
      EQUIVALENCE (C(0955),ALP)
      EQUIVALENCE (C(0956),BETD)
      EQUIVALENCE (C(0957),BET)
C
C XI = S Integral feedback, pitch loop - rad/s
C YI = S Integral feedback, yaw loop - rad/s
C RATEP = S pitch rate - rad/s
C RATEY = S yawrate- rad/s
C ALP = S Angle-of-attack state variable - rad
C BET = S Sideslip angle state variable - rad
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0929),RATEPC)
      EQUIVALENCE (C(0930),RATEYC)
      EQUIVALENCE (C(0995),GR)
      EQUIVALENCE (C(0996),TIP)
C
C RATEPC = G Internal pitch rate command - m/s
C RATEYC = G Internal yaw rate command - m/s
C GR = G Proportional gain - rad/m*s
C TIP = G Time incidence lag, pitch loop - s
C
      MAUTL=INT(MAUT/10.)
      MAUTP=MAUT-MAUTL*10.
C
C*** PITCH ACCELERATION HOLD
C
      IF(MAUTP.EQ.4) THEN
         IF(ANCOM.GT.ANLIM) ANCOM=ANLIM
         IF(ANCOM.LT.-ANLIM) ANCOM=-ANLIM
         ABECZ=-ANCOM*AGRAV
         EP=(ABECZ-FSPB(3))
         TIP=DVBA*AMASS/(PDYNMC*AREA*CNALP+FTHALT)
         GR=GACP*TIP*TR/DVBA
         GI=GR/TA
         XID=GI*EP
         RATEPC=-(EP*GR+XI)
         RATEPD=(RATEPC-RATEP)/TR
         ALPD=(TIP*RATEP-ALP)/TIP
         ALPHA=ALP
         IF(ALPHA.GT.ALPLIM) ALPHA=ALPLIM
         IF(ALPHA.LT.-ALPLIM) ALPHA=-ALPLIM
      ENDIF
C
C*** YAW ACCELERATION HOLD
C
      IF(MAUTL.EQ.4) THEN
         IF(ALCOM.GT.ALLIM) ALCOM=ALLIM
         IF(ALCOM.LT.-ALLIM) ALCOM=-ALLIM
         ABECY=ALCOM*AGRAV
         EY=(ABECY-FSPB(2))
         TIY=TIP
         GR=GACP*TIY*TR/DVBA
         GI=GR/TA
         YID=GI*EY
         RATEYC=(EY*GR+YI)
         RATEYD=(RATEYC-RATEY)/TR
         BETD=-(TIY*RATEY+BET)/TIY
         BETA=BET
         IF(BETA.GT.BETLIM) BETA=BETLIM
         IF(BETA.LT.-BETLIM) BETA=-BETLIM
      ENDIF
      ALPHAX=CRAD*ALPHA
      BETAX=CRAD*BETA
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE D1I
C**********************************************************************
C*** * The initialization module for the Dynamic Module D1
C*** * Reserved C(3510) locations are 1600-1699
C*** * This module performs the following functions:
C*** *
C*** * (1) Initializes state variable integrators
C*** * (2) Initializes missile velocity vector
C*** * (3) Initializes missile poition  vector
C*** *
C*** * Velocity Initialization: Flight path angle (DVBE,PSIVLX,THTVLX)
C*** * Position Initialization: Position coordinates SBEL(3)
C*** *
C*** * MODIFICATION HISTORY
C*** * 991108 Updated by Peter Zipfel
C*** * 000209 Changed initialization, PZi
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100),SBEL(3),VBEL(3),TVL(3,3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1649),SBEL(1))
      EQUIVALENCE (C(1665),PSIVLX)
      EQUIVALENCE (C(1666),THTVLX)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INITIALIZATIONS
C
      EQUIVALENCE (C(1615),HBE)
      EQUIVALENCE (C(1627),TVL(1,1))
      EQUIVALENCE (C(1641),DVBE)
      EQUIVALENCE (C(1643),PSIVL)
      EQUIVALENCE (C(1645),THTVL)
      EQUIVALENCE (C(1652),VBEL(1))
C
C*** INITIALIZE SCALAR STATE VARIABLE INTEGRATION: DVBE, PSIVL, THTVL
C
      IPL(NIP)=1640
      IPLV(NIP)=1641
      NIP=NIP+1
      IPL(NIP)=1642
      IPLV(NIP)=1643
      NIP=NIP+1
      IPL(NIP)=1644
      IPLV(NIP)=1645
      NIP=NIP+1
C
C*** INIPITIALIZE VECTOR STATE VARIABLE INIPTEGRATIONIP: SBEL(3)
C
      ILOC=1646
      DO I=0,2
         IPL(NIP)=ILOC+I
         IPLV(NIP)=ILOC+I+3
         NIP=NIP+1
      ENDDO
C
C*** INITIALIZE MISSILE VELOCITY VECTOR
C
	PSIVL=PSIVLX/CRAD
	THTVL=THTVLX/CRAD
      CALL MATCAR(VBEL,DVBE,PSIVL,THTVL)
C
      HBE=-SBEL(3)
C
C*** INITIALIZE TVL T.M.
C
      CALL MAT2TR(TVL,PSIVL,THTVL)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE D1
C**********************************************************************
C*** * Dynamic Module D1
C*** * Reserved C(3510) locations are 1600-1699
C*** * This module performs the following functions:
C*** *
C*** * (1) Integraates Newton's equations in velocity coordinates
C*** * (2) Integrates velocity to obtain position
C*** * (3) Calculates angular velocity of velocity frame wrt earth frame
C*** * (4) Concatinates event flags into EVENT
C*** *
C*** * MODIFICATION HISTORY
C*** * 991103 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION SBEL(3),SBELD(3),VBEL(3),FSPV(3),TVL(3,3),WVEV(3)
     &,SBT1L(3),ST1EL(3),VT1EL(3),VBT1L(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(0400),MSEEK)
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(0900),MAUT)
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1310),AMASS)
      EQUIVALENCE (C(1423),FSPV(1))
C
C VT1EL= D Constant target velocity vector - m/s
C ST1EL= O Target position from ref. point E - m
C MSEEK= D =0:No seeker, =1:Seeker lock-on
C MGUID= D MGUID=6: Proportional navigation guidance
C MAUT= D MAUT=44 Acceleration autopilot in both planes
C MPROP= O MPROP=0: Motor stopped burning
C AMASS= O Missile mass - kg
C FSPV= O Specific force in flight path axes - N/kg
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1640),DVBED)
      EQUIVALENCE (C(1641),DVBE)
      EQUIVALENCE (C(1642),PSIVLD)
      EQUIVALENCE (C(1643),PSIVL)
      EQUIVALENCE (C(1644),THTVLD)
      EQUIVALENCE (C(1645),THTVL)
      EQUIVALENCE (C(1646),SBELD(1))
      EQUIVALENCE (C(1649),SBEL(1))
C
C DVBE = S Vehicle speed - m/s
C PSIVL = S Heading angle - rad
C THTVL = S Flight path angle - rad
C SBEL(3) = S Displacement of vehicle wrt to point E - m/s	
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1608),WVEV(1))
      EQUIVALENCE (C(1615),HBE)
      EQUIVALENCE (C(1627),TVL(1,1))
      EQUIVALENCE (C(1652),VBEL(1))
      EQUIVALENCE (C(1671),DBT1)
      EQUIVALENCE (C(1672),SBT1L(1))
      EQUIVALENCE (C(1675),VBT1L(1))
C
C WVEV(3) = O angular velocity of flight path wrt earth - rad/s
C HBE = O altitude of vehicle above S.L.
C TVL(3,3) = O T.M. of velocity to local level coordinates - ND
C VBEL(3) = O Velocity of vehicle wrt earth in L-coord - m
C DBT1 = O Distance between vehicle and target - m
C SBT1L(3) = O Displacement of vehicle wrt target - m
C VBT1L(3) = O Velocity of vehicle wrt target - m/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1601),EVENT)
      EQUIVALENCE (C(1665),PSIVLX)
      EQUIVALENCE (C(1666),THTVLX)
C
C EVENT = G EVENT=|MSEEK|MGUID|MAUTL|MAUTP|MPROP|
C PSIVLX = I/G Heading angle from north - deg
C THTVLX = I/G Flight path angle - deg
C
C*** EQUATIONS OF MOTION
C
C*** VELOCITY
      DVBED=FSPV(1)-SIN(THTVL)*AGRAV
      PSIVLD=FSPV(2)/(DVBE*COS(THTVL))
      THTVLD=-(FSPV(3)+COS(THTVL)*AGRAV)/DVBE
      CALL MATCAR(VBEL,DVBE,PSIVL,THTVL)
C*** DISPLACEMENT
      CALL MATEQL(SBELD,VBEL,3,1)
C
C*** TARGET RELATIVE POSITION AND VELOCITY
C
      CALL MATABS(DBE,SBEL,3)
      CALL MATSUB(SBT1L,SBEL,ST1EL,3,1)
      CALL MATABS(DBT1,SBT1L,3)
      CALL MATSUB(VBT1L,VBEL,VT1EL,3,1)
C
C*** ALTITUDE OF MISSILE ABOVE SEA LEVEL
C
      HBE=-SBEL(3)
C
C*** KINEMATICS
C
      CALL MAT2TR(TVL,PSIVL,THTVL)
      WVEV(1)=-SIN(THTVL)*PSIVLD
      WVEV(2)=THTVLD
      WVEV(3)=COS(THTVL)*PSIVLD
C
C*** STATUS INDICATOR
C
      MAUTL=INT(MAUT/10.)
      MAUTP=MAUT-MAUTL*10.
      EVENT=100000.*MSEEK+1000.*MGUID+100.*MAUTL+10.*MAUTP
     &+1.*MPROP
C
      THTVLX=THTVL*CRAD
      PSIVLX=PSIVL*CRAD
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE D2I
C**********************************************************************
C*** * The initialization module for the Kinematic Module D2
C*** * Reserved C(3510) locations are 1700-1749
C*** * This module performs the following functions:
C*** *
C*** * (1) Initializes direction cosine matrix TBL, TLB
C*** *
C*** * MODIFICATION HISTORY
C*** * 991103 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION TBV(3,3),TVL(3,3),TLB(3,3),TBL(3,3)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1627),TVL(1,1))
C
C TVL= O T.M. of velocity to local level coordinates - ND
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(1701),TBL(1,1))
      EQUIVALENCE (C(1710),TLB(1,1))
C
      CALL A3TRA(TBV)
      CALL MATMUL(TBL,TBV,TVL,3,3,3)
      CALL MATTRA(TLB,TBL,3,3)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE D2
C**********************************************************************
C*** * Kinematic Module D2
C*** * Reserved C(3510) locations are 1700-1749
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates direction cosine matrix TBL, TLB
C*** * (2) Calculates body rates
C*** * (3) Calculates Euler angles (for diagnostic purposes only)
C*** *
C*** * MODIFICATION HISTORY
C*** * 991103 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION TBV(3,3),WVEV(3),TVL(3,3),TBL(3,3),TLB(3,3),WBEB(3)
     +,WBVB(3),DUM3(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0954),ALPD)
      EQUIVALENCE (C(0955),ALP)
      EQUIVALENCE (C(0956),BETD)
      EQUIVALENCE (C(1413),TBV(1,1))
      EQUIVALENCE (C(1608),WVEV(1))
      EQUIVALENCE (C(1627),TVL(1,1))
C
C ALP= S Angle-of-attack state variable - rad
C TBV= O Trans matrix of body wrt flight path axes
C WVEV= O angular velocity of flight path wrt earth - rad/s
C TVL= O T.M. of velocity to local level coordinates - ND
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1701),TBL(1,1))
      EQUIVALENCE (C(1710),TLB(1,1))
      EQUIVALENCE (C(1719),WBEB(1))
C
C TBL(3,3) = O Trans matrix of body wrt local earth axes
C TLB(3,3) = O Trans matrix of local earth wrt body axes
C WBEB(3) = O Angular velocity of body wrt earth - rad/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1722),PSIBL)
      EQUIVALENCE (C(1723),THTBL)
      EQUIVALENCE (C(1724),PHIBL)
      EQUIVALENCE (C(1725),WBVB(1))
      EQUIVALENCE (C(1730),PSIBLX)
      EQUIVALENCE (C(1731),THTBLX)
      EQUIVALENCE (C(1732),PHIBLX)
C
C PSIBL = G Euler yaw angle of body wrt local earth axes - rad
C THTBL = G Euler pitch angle of body wrt local earth axes - rad
C PHIBL = G Euler roll angle of body wrt local earth axes - rad
C WBVB(3) = G Angular vel of body wrt flight path frame - rad/s
C PSIBLX = G Euler yaw angle of body wrt local earth axes - deg
C THTBLX = G Euler pitch angle of body wrt local earth axes - deg
C PHIBLX = G Euler roll angle of body wrt local earth axes - deg
C
C*** DIRECTION COSINE MATRIX TBL(3,3)
C
      CALL MATMUL(TBL,TBV,TVL,3,3,3)
      CALL MATTRA(TLB,TBL,3,3)
C
C*** YAW TO TURN
C
      WBVB(1)=+BETD*SIN(ALP)
      WBVB(2)=ALPD
      WBVB(3)=-BETD*COS(ALP)
C
C*** BODY RATES
C
      CALL MATMUL(DUM3,TBV,WVEV,3,3,1)
      CALL MATADD(WBEB,WBVB,DUM3,3,1)
C
C*** EULER ANGLES
C
      PSIBL=ATAN2(TBL(1,2),TBL(1,1))
      THTBL=ASIN(-TBL(1,3))
      PHIBL=ATAN2(TBL(2,3),TBL(3,3))
C
      PSIBLX=PSIBL*CRAD
      THTBLX=THTBL*CRAD
      PHIBLX=PHIBL*CRAD
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G1I
C**********************************************************************
C*** * Target Initialization Module
C*** * Reserved C(3510) locations are 100-199
C*** * This module performs the following functions:
C*** *
C*** * (1) Initializes target position
C*** *
C*** * MTARG=0  Cartesian target initialization: ST1EL0(3), VT1EL(3)
C*** * MTARG=1  Polar target initialization HTE,DHTB,AZTLX, VT1EL(3)
C*** *
C*** * MODIFICATION HISTORY
C*** * 000209 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION SBEL(3),STBL(3),ST1EL0(3),ST1EL(3)	
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0100),MTARG)
      EQUIVALENCE (C(0101),DHTB)
      EQUIVALENCE (C(0102),HTE)
      EQUIVALENCE (C(0103),AZTLX)
      EQUIVALENCE (C(1649),SBEL(1))
C
C MTARG = D =0:Cartesian init, =1:Polar init - ND
C DHTB = D Horizontal range of target wrt launch point - m
C HTE = D Height of target above S.L. - m
C AZTLX = D Target azimuth from north at launch point - deg
C
C*** INPUT FROM D1 MODULE
C
      EQUIVALENCE (C(1649),SBEL(1))
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(0116),ST1EL0(1))
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(1671),DBT1)
C
C ST1EL(3) = O Target position from ref. point E - m
C
C*** Initialize target coordinates ST1EL0(3)
C
      IF(MTARG.EQ.1) THEN
         DUMH=SBEL(3)+HTE
         THTTL0=ATAN2(DUMH,DHTB)
         DBT1=SQRT(DHTB**2+DUMH**2)
         CALL MATCAR(STBL,DBT1,AZTLX/CRAD,THTTL0)
	   CALL MATADD(ST1EL0,STBL,SBEL,3,1)
      ENDIF
C
	CALL MATEQL(ST1EL,ST1EL0,3,1)
      RETURN
      END
C**********************************************************************
      SUBROUTINE G1
C**********************************************************************
C*** * Target Module G1
C*** * Reserved C(3510) locations are 100-199
C*** * Simulates straight and level, constant speed target trajectories
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates target position
C*** *
C*** * MODIFICATION HISTORY
C*** * 991103 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION ST1EL0(3),ST1EL(3),VT1EL(3),DUM3(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0116),ST1EL0(1))
      EQUIVALENCE (C(0119),VT1EL(1))
C
C ST1EL0(3) = D Initial target position relative to point E - m
C VT1EL(3) = D Constant target velocity vector - m/s
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2000),T)
C
C T = E Time since launch - s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0125),ST1EL(1))
C
C ST1EL(3) = O Target position from ref. point E - m
C
C*** CALCULATE TARGET POSITION
C
      CALL MATCON(DUM3,T,VT1EL,3,1)
	CALL MATADD(ST1EL,DUM3,ST1EL0,3,1)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G2
C**********************************************************************
C*** * Atmosphere Module G2
C*** * Reserved C(3510) locations are 200-299
C*** * 1962 ISO in SI units  (m, Pa, Kg/m3, degK, m/s)
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates atmopheric properties
C*** * (2) Calculates Mach number and dynamic pressure
C*** *
C*** * MODIFICATION HISTORY
C*** * 991103 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
      COMMON /ARDC/ ALT, PRES, RHO, TEMP, VS
C
      DIMENSION VBEL(3)
C
C*** INPUTS FROM OTHER MODULES
C
      EQUIVALENCE (C(1652),VBEL(1))
      EQUIVALENCE (C(1615),HBE)
C
C*** OUTPUTS TO OTHER MODULES
C
      EQUIVALENCE (C(0219),DVBA)
      EQUIVALENCE (C(0212),PPRES)
      EQUIVALENCE (C(0220),PDYNMC)
      EQUIVALENCE (C(0221),VMACH)
C
C DVBA = O Speed of missile wrt air - m/s
C PPRES = O Atmospheric pressure - Pa
C PDYNMC = O Dynamic pressure - Pa
C VMACH = O Mach number - ND
C
      ALT=HBE
      CALL ISO62M
      PPRES=PRES
C
C*** FLIGHT CONDITIONS
C
      CALL MATABS(DVBA,VBEL,3)
      PDYNMC=.5*RHO*DVBA*DVBA
      VMACH=DVBA/VS
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE ISO62M
C**********************************************************************
C*** * 1962 ISO Atmosphere in SI units  (m, Pa, Kg/m3, degK, m/s)
C*** * WARNING:Error above 20km is greater than 1%
C*** *
C*** * MODIFICATION HISTORY
C*** * 991103 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON/ARDC/ALT,PRES,RHO,TEMP,VS
C
      IF(ALT.LE.11000.) THEN
         TEMP=288.-.0065*ALT
         PRES=101325.*(TEMP/288.)**5.2559
      ELSE
         PRES=22630.*EXP(-.00015769*(ALT-11000.))
         TEMP=216.
      ENDIF
      RHO=PRES/(287.053*TEMP)
      VS=20.05*SQRT(TEMP)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G4
C**********************************************************************
C*** * End-of-Run Subroutine called by STAGE3, not a module
C*** * Reserved C(3510) locations are 1750-1799
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Stops run
C*** * (3) Calcuates miss distance and intercept time
C*** *
C*** * MODIFICATION HISTORY
C*** * 991103 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      CHARACTER*100 TITLE
      COMMON C(3510)
      COMMON /HCOM/TITLE
C
      DIMENSION ST1EL(3),SBEL(3),SBELM(3),SBBML(3),ST1ELM(3)
     +,STTML(3),DUM1(3),DUM2(3),DUM6(3),SHBML(3),SJTML(3),SHEL(3)
     +,SJEL(3),SHJL(3),SBT1L(3),VBT1L(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(2020),LCONV)
      EQUIVALENCE (C(2664),DER)
C
C T = E Time since launch - s
C LCONV = E LCONV=2 stops run
C DER = E Integration interval - s
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(1649),SBEL(1))
      EQUIVALENCE (C(1671),DBT1)
      EQUIVALENCE (C(1672),SBT1L(1))
      EQUIVALENCE (C(1675),VBT1L(1))
C
C ST1EL= O Target position from ref. point E - m
C SBEL= S Displacement of vehicle wrt to point E - m/s	
C DBT1= O Distance between vehicle and target - m
C SBT1L= O Displacement of vehicle wrt target - m
C VBT1L= O Velocity of vehicle wrt target - m/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1773),DBT)
C
C DBT = G Miss distance - m
C
      IF(DBT1.LT.100.) THEN
C
C*** CLOSING SPEED
C
         CALL MATCON(DUM6,-1./DBT1,SBT1L,3,1)
         CALL MATSCA(CVEL,VBT1L,DUM6,3)
C
         IF(CVEL.LT.0.) THEN
C
C***        END-OF-RUN CALCULATIONS
C
            CALL MATSUB(SBBML,SBEL,SBELM,3,1)
            CALL MATSUB(STTML,ST1EL,ST1ELM,3,1)
            CALL MATSUB(DUM1,SBBML,STTML,3,1)
            CALL MATSUB(DUM2,SBELM,ST1ELM,3,1)
C
C***        TIME INCREMENT OF CLOSEST APPROACH
C
            CALL MATSCA(DUM3,DUM1,DUM2,3)
            CALL MATSCA(DUM4,DUM1,DUM1,3)
            DTIM=-DER*DUM3/DUM4
C
C***        POINTS OF CLOSEST APPROACH
C
            DUM5=DTIM/DER
            CALL MATCON(SHBML,DUM5,SBBML,3,1)
            CALL MATCON(SJTML,DUM5,STTML,3,1)
            CALL MATADD(SHEL,SHBML,SBELM,3,1)
            CALL MATADD(SJEL,SJTML,ST1ELM,3,1)
            CALL MATSUB(SHJL,SHEL,SJEL,3,1)
C
C***        DISTANCE AND TIME OF CLOSEST APPROACH
C
            CALL MATABS(DHJ,SHJL,3)
            DBT=DHJ
            EPCHIM=T-DER+DTIM
C
            LCONV=2
C
            WRITE(6,99) TITLE
            WRITE(6,1) EPCHIM
            WRITE(6,3) DHJ,SHJL(1),SHJL(2),SHJL(3)
C
            RETURN
C
         ENDIF
C
C***     SAVE VALUES FOR NEXT INTEGRATION CYCLE
C
         CALL MATEQL(SBELM,SBEL,3,1)
         CALL MATEQL(ST1ELM,ST1EL,3,1)
C
         RETURN
C
      ENDIF
C
      IF(SBEL(3).LE.0.) RETURN
C
C*** IMPACT ON HORIZONTAL PLANE
C
      LCONV=3
      WRITE (6,4)
C
 1    FORMAT(1X,43H PARAMETERS AT TIME OF CLOSEST APPROACH T= ,1PG12.5)
 3    FORMAT(1X ,'    DBT=',1PG12.5,'  SBTL1=',1PG12.5,'  SBTL2='
     +,1PG12.5,'  SBTL3=',1PG12.5/)
 4    FORMAT (1H0,41H  IMPACT OCCURRED IN THE HORIZONTAL PLANE)
 99   FORMAT(2X,A)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE S1
C**********************************************************************
C*** * Sensor Module S1
C*** * Reserved C(3510) locations are 400-499
C*** * Simple LOS seeker
C*** *
C*** * MSEEK=0 No seeker
C*** *      =1 Seeker lock-on
C*** *
C*** * This module performs the following functions:
C*** * (1) Calculates LOS rates and look angles
C*** * (2) Calculates time-to-go
C*** *
C*** * MODIFICATION HISTORY
C*** * 991103 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION ST1EL(3),VT1EL(3),DUM3(3)
     +,ST1BL(3),UT1BL(3),TBL(3,3),UT1BB(3),DUM9(3,3),WOELK(3)
     +,VBEL(3),SBEL(3),VT1BL(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0400),MSEEK)
C
C MSEEK = D =0:No seeker, =1:Seeker lock-on
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C CRAD = E Conversion to radians 57.29577951 deg/rad
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(1649),SBEL(1))
      EQUIVALENCE (C(1652),VBEL(1))
      EQUIVALENCE (C(1701),TBL(1,1))
C
C VT1EL= D Constant target velocity vector - m/s
C ST1EL= O Target position from ref. point E - m
C SBEL= S Displacement of vehicle wrt to point E - m/s	
C VBEL= O Velocity of vehicle wrt earth in L-coord - m
C TBL= O Trans matrix of body wrt local earth axes
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0410),DBT1K)
      EQUIVALENCE (C(0411),DVBT1K)
      EQUIVALENCE (C(0412),WOELK(1))
      EQUIVALENCE (C(0418),TGOK)
      EQUIVALENCE (C(0447),THTSB)
      EQUIVALENCE (C(0454),PSISB)
C
C DBT1K = O Seeker measured distance to target - m
C DVBT1K = O Closing velocity - m/s
C WOELK(3) = O Inertial LOS rate rad/s
C TGOK = O Calculated time-to-go - s
C THTSB = O Seeker elevation angle from body frame - rad
C PSISB =  O Seeker azimuth angle from body frame - rad
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0448),THTSBX)
      EQUIVALENCE (C(0455),PSISBX)
C
C THTSBX = G Seeker elevation angle from body frame - deg
C PSISBX = G Seeker azimuth angle from body frame - deg
C
      IF(MSEEK.EQ.1)THEN
C
C***    LINE OF SIGHT KINEMATICS
C
         CALL MATSUB(ST1BL,ST1EL,SBEL,3,1)
         CALL MATABS(DBT1K,ST1BL,3)
C
C***    UNIT LOS VECTOR
C
         CALL MATCON(UT1BL,1./DBT1K,ST1BL,3,1)
         CALL MATMUL(UT1BB,TBL,UT1BL,3,3,1)
C
C***    LOS ANGLES WRT MISSILE BODY
C
         CALL MATPOL(DUM,PSISB,THTSB,UT1BB)
C
C***    RELATIVE VELOCITY
C
         CALL MATSUB(VT1BL,VT1EL,VBEL,3,1)
C
C***    CLOSING VELOCITY
C
         CALL MATSCA(DUM2,UT1BL,VT1BL,3)
         DVBT1K=ABS(DUM2)
C
C***    TIME TO GO
C
         TGOK=DBT1K/DVBT1K
C
C***    INERTIAL LOS RATES IN LOCAL COORDINATES
C
         CALL MATSKS(DUM9,UT1BL)
         CALL MATMUL(DUM3,DUM9,VT1BL,3,3,1)
         CALL MATCON(WOELK,1./DBT1K,DUM3,3,1)
C
C***    CONVERSION TO DEGREE
C
         THTSBX=THTSB*CRAD
         PSISBX=PSISB*CRAD
	ENDIF
C
      RETURN
      END
C**********************************************************************
C************************* DUMMY SOUBROUTINES *************************
C**********************************************************************
	SUBROUTINE A2I
	RETURN
	END
	SUBROUTINE A3I
	RETURN
	END
	SUBROUTINE A4I
	RETURN
	END
	SUBROUTINE A4
	RETURN
	END
	SUBROUTINE A5I
	RETURN
	END
	SUBROUTINE A5
	RETURN
	END
C
	SUBROUTINE C1I
	RETURN
	END
	SUBROUTINE C3I
	RETURN
	END
	SUBROUTINE C3
	RETURN
	END
	SUBROUTINE C4I
	RETURN
	END
	SUBROUTINE C4
	RETURN
	END
	SUBROUTINE C5I
	RETURN
	END
	SUBROUTINE C5
	RETURN
	END
C
	SUBROUTINE D3I
	RETURN
	END
	SUBROUTINE D3
	RETURN
	END
	SUBROUTINE D4
	RETURN
	END
	SUBROUTINE D4I
	RETURN
	END
	SUBROUTINE D5I
	RETURN
	END
	SUBROUTINE D5
	RETURN
	END
C
	SUBROUTINE G2I
	RETURN
	END
	SUBROUTINE G3I
	RETURN
	END
	SUBROUTINE G3
	RETURN
	END
	SUBROUTINE G4I
	RETURN
        END
	SUBROUTINE G5I
	RETURN
        END
	SUBROUTINE G5
	RETURN
        END
C
	SUBROUTINE S1I
	RETURN
	END
	SUBROUTINE S2I
	RETURN
	END
	SUBROUTINE S2
	RETURN
	END
	SUBROUTINE S3I
	RETURN
	END
	SUBROUTINE S3
	RETURN
	END
	SUBROUTINE S4I
	RETURN
	END
	SUBROUTINE S4
	RETURN
	END
	SUBROUTINE S5I
	RETURN
	END
	SUBROUTINE S5
	RETURN
	END
