C**********************************************************************
C******************** MODULES AND DUMMY FOR FALCON6 *******************
C**********************************************************************
C*** *
C*** * Calling sequence of Modules:
C*** *
C*** *  G2   ENVIRONMENT
C*** *  G3   KINEMATICS
C*** *  A1   AERODYNAMICS
C*** *  A2   PROPULSION
C*** *  A3   FORCES
C*** *  D1   NEWTONS LAW
C*** *  D2   KINEMATICS
C*** *
C*** * MODIFICATION HISTORY
C*** * 000413 Version 1,0 Created by Peter Zipfel
C*** *
C**********************************************************************
C**********************************************************************
      SUBROUTINE A1I
C**********************************************************************
C*** * Initialization module A1, FALCON6 Aircraft
C*** * Reserved C(3510) locations are 1200-1299
C*** * * References: (1) Stevens & Lewis, "Aircraft Control and
C*** *   Simulation" Wiley-Interscience Publication, 1992.
C*** * * Data based on: (2) Nguyen, J.T., et al., "Simulator Study of
C*** *   Stall/Post-Stall Characteristics of a Fighter Airplane with
C*** *   Relaxec Longitudinal Static Stability", NASA Tech. Paper 1538,
C*** *   NASA, Washington, D.C. Dec. 1979.
C*** * * Data tables valid up to Mach = 0.6
C*** *
C*** * Ref AREA= 300 ft^2, SPAN= 30 ft, CHORD= 11.32 ft
C*** * Ref AREA= 27.87 m^2, SPAN= 9.14 m, CHORD= 3.45 m
C*** *
C*** * This module performs the following functions:
C*** * 1) Initializes counters for tables
C*** *
C*** * MODIFICATION HISTORY
C*** * 000405 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C***  INITIALIZATION
C
      EQUIVALENCE (C(1204),REFA)
      EQUIVALENCE (C(1205),CHORD)
      EQUIVALENCE (C(1206),SPAN)
      EQUIVALENCE (C(1234),IXA1)
      EQUIVALENCE (C(1235),IXB2)
      EQUIVALENCE (C(1236),IXD2)
C
C REFA = I Reference area - m^2
C CHORD = I Chord reference length - m
C SPAN = I Span reference length -m
C
	REFA=27.87
	CHORD=3.45
	SPAN=9.14
C
	IXA1=1
	IXB2=1
	IXD2=1
C
	RETURN
	END
 
C**********************************************************************
      SUBROUTINE A1
C**********************************************************************
C*** * Aerodynamic Module A1, FALCON6 Aircraft
C*** * Reserved C(3510) locations are 1200-1299
C*** * * References: (1) Stevens & Lewis, "Aircraft Control and
C*** *   Simulation" Wiley-Interscience Publication, 1992.
C*** * * Data based on: (2) Nguyen, J.T., et al., "Simulator Study of
C*** *   Stall/Post-Stall Characteristics of a Fighter Airplane with
C*** *   Relaxec Longitudinal Static Stability", NASA Tech. Paper 1538,
C*** *   NASA, Washington, D.C. Dec. 1979.
C*** * * Data tables valid up to Mach = 0.6
C*** *
C*** * Ref AREA= 300 ft^2, SPAN= 30 ft, CHORD= 11.32 ft
C*** * Ref AREA= 27.87 m^2, SPAN= 9.14 m, CHORD= 3.45 m
C*** *
C*** * This module performs the following functions:
C*** * 1) Determines the  force coefficients
C*** *    CXT, CYT, CZT.
C*** * 2) Determines the moment coefficients
C*** *    CLT, CMT, CNT.
C*** *
C*** * Note: Rolling moment from positive aileron deflection is negative
C*** *		in high subsonic regime. This phenomena is caused by wing twist.
C*** *		Therefore, the aircraft uses spoilers in this region. In this sim
C*** *		the sign of DLDA is reversed since spoiler are not implements
C*** *
C*** * MODIFICATION HISTORY
C*** * 921207 Created by Leonard Frantz
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION WBEB(3)
      DIMENSION VECALPHA(12), ALPDEL(17)
      DIMENSION ALPBET(19), ALPBET2(19)
      DIMENSION TABCXQ(12), TABCYR(12), TABCYP(12), TABCZQ(12)
      DIMENSION TABCLR(12), TABCLP(12), TABCMQ(12), TABCNR(12)
      DIMENSION TABCNP(12), TABCX(60), TABCZ(12), TABCL(84)
      DIMENSION TABDLDA(84), TABDLDR(84), TABDNDA(84)
      DIMENSION TABDNDR(84), TABCM(60), TABCN(84)
      DIMENSION ITDIM1(2), ITDIM2(2)
	DIMENSION ALPI(12),DELI(5),BETI(7)
C
      DATA ITDIM1 /12,5/, ITDIM2 /12,7/
C
C*** ALPHA DEPENCE OF SINGLE TABLES
C
      DATA VECALPHA /-10.0,-5.0,0.0,5.0,10.0,15.0,20.0,25.0,
     x 30.0,35.0,40.0,45.0/
C
C*** ALPHA AND ELEVATOR DEPENCE OF 2-DIM TABLES (12 COLUMNS OF ALPHA,
C     5 ROWS OF ELEVATOR CONTROL)
C
      DATA ALPDEL /
     x -10.,-5.0, 0.0, 5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,
     X -24.,-12., 0.0,12.,24./
C
      DATA ALPI /
     x -10.,-5.0, 0.0, 5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0/
C
      DATA DELI /-24.,-12., 0.0,12.,24./
C
C*** ALPHA AND BETA DEPENDENCE OF OF 2-DIM TABLES (12 COLUMNS OF ALPHA,
C     7 ROWS OF BETA)
C
      DATA ALPBET /
     x -10.,-5.0, 0.0, 5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,
     x  0.0, 5.0,10.0,15.0,20.0,25.0,30.0/
C
      DATA BETI / 0.0, 5.0,10.0,15.0,20.0,25.0,30.0/
C
C*** ALPHA AND BETA DEPENDENCE OF OF 2-DIM TABLES (12 COLUMNS OF ALPHA,
C     7 ROWS OF BETA)
C
      DATA ALPBET2 /
     x -10.,-5.0, 0.0, 5.0,10.0,15.0,20.0,25.0,30.0,35.0,40.0,45.0,
     x -30.,-20.,-10., 0.0,10.0,20.0,30.0/
C
      DATA TABCXQ /
     x -.267, -.110,  .308,  1.34,  2.08,  2.91,  2.76,
     x  2.05,  1.50,  1.49,  1.83,  1.21/
C
      DATA TABCYR /
     x  .882,  .852,  .876,  .958,  .962,  .974,  .819,
     x  .483,  .590,  1.21, -.493, -1.04/
C
      DATA TABCYP /
     x -.108, -.108, -.188,  .110,  .258,  .226,  .344,
     x  .362,  .611,  .529,  .298, -2.27/
C
      DATA TABCZQ /
     x -8.80, -25.8, -28.9, -31.4, -31.2, -30.7, -27.7,
     x -28.2, -29.0, -29.8, -38.3, -35.3/
C
      DATA TABCLR /
     x -.126, -.026,  .063,  .113,  .208,  .230,  .319,
     x  .437,  .680,  .100,  .447, -.330/
C
      DATA TABCLP /
     x -.360, -.359, -.443, -.420, -.383, -.375, -.329,
     x -.294, -.230, -.210, -.120, -.100/
C
      DATA TABCMQ /
     x -7.21, -.540, -5.23, -5.26, -6.11, -6.64, -5.69,
     x -6.00, -6.20, -6.40, -6.60, -6.00/
C
      DATA TABCNR /
     x -.380, -.363, -.378, -.386, -.370, -.453, -.550,
     x -.582, -.595, -.637, -1.02, -.840/
C
      DATA TABCNP /
     x  .061,  .052,  .052, -.012, -.013, -.024,  .050,
     x  .150,  .130,  .158,  .240,  .150/
C
      DATA TABCX /
     x -.099, -.081, -.081, -.063, -.025,  .044,  .097,
     x  .113,  .145,  .167,  .174,  .166,
     x -.048, -.038, -.040, -.021,  .016,  .083,  .127,
     x  .137,  .162,  .177,  .179,  .167,
     x -.022, -.020, -.021, -.004,  .032,  .094,  .128,
     x  .130,  .154,  .161,  .155,  .138,
     x -.040, -.038, -.039, -.025,  .006,  .062,  .087,
     x  .085,  .100,  .110,  .104,  .091,
     x -.083, -.073, -.076, -.072, -.046,  .012,  .024,
     x  .025,  .043,  .053,  .047,  .040/
C
      DATA TABCZ /
     x  .770,  .241, -.100, -.416, -.731, -1.053, -1.366,
     x -1.646, -1.917, -2.120, -2.248, -2.229/
C
      DATA TABCM /
     x  .205,  .168,  .186,  .196,  .213,  .251,  .245,
     x  .238,  .252,  .231,  .198,  .192,
     x  .081,  .077,  .107,  .110,  .110,  .141,  .127,
     x  .119,  .133,  .108,  .081,  .093,
     x -.046, -.020, -.009, -.005, -.006,  .010,  .006,
     x -.001,  .014,  .000, -.013,  .032,
     x -.174, -.145, -.121, -.127, -.129, -.102, -.097,
     x -.113, -.087, -.084, -.069, -.006,
     x -.259, -.202, -.184, -.193, -.199, -.150, -.160,
     x -.167, -.104, -.076, -.041, -.005/
C
      DATA TABCL /12*0.0,
     x -.001, -.004, -.008, -.012, -.016, -.022, -.022,
     x -.021, -.015, -.008, -.013, -.015,
     x -.003, -.009, -.017, -.024, -.030, -.041, -.045,
     x -.040, -.016, -.002, -.010, -.019,
     x -.001, -.010, -.020, -.030, -.039, -.054, -.057,
     x -.054, -.023, -.006, -.014, -.027,
     x  .000, -.010, -.022, -.034, -.047, -.060, -.069,
     x -.067, -.033, -.036, -.035, -.035,
     x  .007, -.010, -.023, -.034, -.049, -.063, -.081,
     x -.079, -.060, -.058, -.062, -.059,
     x  .009, -.011, -.023, -.037, -.050, -.068, -.089,
     x -.088, -.091, -.076, -.077, -.076/
C
      DATA TABCN /12*0.0,
     x  .018,  .019,  .018,  .019,  .019,  .018,  .013,
     x  .007,  .004, -.014, -.017, -.033,
     x  .038,  .042,  .042,  .042,  .043,  .039,  .030,
     x  .017,  .004, -.035, -.047, -.057,
     x  .056,  .057,  .059,  .058,  .058,  .053,  .032,
     x  .012,  .002, -.046, -.071, -.073,
     x  .064,  .077,  .076,  .074,  .073,  .057,  .029,
     x  .007,  .012, -.034, -.065, -.041,
     x  .074,  .086,  .093,  .089,  .080,  .062,  .049,
     x  .022,  .028, -.012, -.002, -.013,
     x  .079,  .090,  .106,  .106,  .096,  .080,  .068,
     x  .030,  .064,  .015,  .011, -.001/
C
      DATA TABDLDA /
     x -.041, -.052, -.053, -.056, -.050, -.056, -.082,
     x -.059, -.042, -.038, -.027, -.017,
     x -.041, -.053, -.053, -.053, -.050, -.051, -.066,
     x -.043, -.038, -.027, -.023, -.016,
     x -.042, -.053, -.052, -.051, -.049, -.049, -.043,
     x -.035, -.026, -.016, -.018, -.014,
     x -.040, -.052, -.051, -.052, -.048, -.048, -.042,
     x -.037, -.031, -.026, -.017, -.012,
     x -.043, -.049, -.048, -.049, -.043, -.042, -.042,
     x -.036, -.025, -.021, -.016, -.011,
     x -.044, -.048, -.048, -.047, -.042, -.041, -.020,
     x -.028, -.013, -.014, -.011, -.010,
     x -.043, -.049, -.047, -.045, -.042, -.037, -.003,
     x -.013, -.010, -.003, -.007, -.008/
C
      DATA TABDLDR /
     x  .005,  .017,  .014,  .010, -.005,  .009,  .019,
     x  .005, -.000, -.005, -.011,  .008,
     x  .007,  .016,  .014,  .014,  .013,  .009,  .012,
     x  .005,  .000,  .004,  .009,  .007,
     x  .013,  .013,  .011,  .012,  .011,  .009,  .008,
     x  .005, -.002,  .005,  .003,  .005,
     x  .018,  .015,  .015,  .014,  .014,  .014,  .014,
     x  .015,  .013,  .011,  .006,  .001,
     x  .015,  .014,  .013,  .013,  .012,  .011,  .011,
     x  .010,  .008,  .008,  .007,  .003,
     x  .021,  .011,  .010,  .011,  .010,  .009,  .008,
     x  .010,  .006,  .005,  .000,  .001,
     x  .023,  .010,  .011,  .011,  .011,  .010,  .008,
     x  .010,  .006,  .014,  .020,  .000/
C
      DATA TABDNDA /
     x  .001, -.027, -.017, -.013, -.012, -.016,  .001,
     x  .017,  .011,  .017,  .008,  .016,
     x  .002, -.014, -.016, -.016, -.014, -.019, -.021,
     x  .002,  .012,  .016,  .015,  .011,
     x -.006, -.008, -.006, -.006, -.005, -.008, -.005,
     x  .007,  .004,  .007,  .006,  .006,
     x -.011, -.011, -.010, -.009, -.008, -.006,  .000,
     x  .004,  .007,  .010,  .004,  .010,
     x -.015, -.015, -.014, -.012, -.011, -.008, -.002,
     x  .002,  .006,  .012,  .011,  .011,
     x -.024, -.010, -.004, -.002, -.001,  .003,  .014,
     x  .006, -.001,  .004,  .004,  .006,
     x -.022,  .002, -.003, -.005, -.003, -.001, -.009,
     x -.009, -.001,  .003, -.002,  .001/
C
      DATA TABDNDR /
     x -.018, -.052, -.052, -.052, -.054, -.049, -.059,
     x -.051, -.030, -.037, -.026, -.013,
     x -.028, -.051, -.043, -.046, -.045, -.049, -.057,
     x -.052, -.030, -.033, -.030, -.008,
     x -.037, -.041, -.038, -.040, -.040, -.038, -.037,
     x -.030, -.027, -.024, -.019, -.013,
     x -.048, -.045, -.045, -.045, -.044, -.045, -.047,
     x -.048, -.049, -.045, -.033, -.016,
     x -.043, -.044, -.041, -.041, -.040, -.038, -.034,
     x -.035, -.035, -.029, -.022, -.009,
     x -.052, -.034, -.036, -.036, -.035, -.028, -.024,
     x -.023, -.020, -.016, -.010, -.014,
     x -.062, -.034, -.027, -.028, -.027, -.027, -.023,
     x -.023, -.019, -.009, -.025, -.010/
C
C***  INPUT DATA
C
      EQUIVALENCE (C(1205),CHORD)
      EQUIVALENCE (C(1206),SPAN)
      EQUIVALENCE (C(1207),XCGR)
      EQUIVALENCE (C(1208),XCG)
C
C CHORD = D Airfoil average chord - ft
C SPAN = D Wing span - ft
C XCGR = D Reference c.g. position - ft
C XCG = D Actual c.g. position - ft
C
C***  INITIALIZATION
C
      EQUIVALENCE (C(1234),IXA1)
      EQUIVALENCE (C(1235),IXB2)
      EQUIVALENCE (C(1236),IXD2)
 
C
C IXA1 = I Table lookup integer to save table location - ND
C IXB2 = I Table lookup integer to save table location - ND
C IXD2 = I Table lookup integer to save table location - ND
C
C***  INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2000),TIME)
C
C***  INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0219),DVBA)
      EQUIVALENCE (C(0301),ALPHA)
      EQUIVALENCE (C(0302),BETA)
      EQUIVALENCE (C(0344),ALPHAX)
      EQUIVALENCE (C(0345),BETAX)
      EQUIVALENCE (C(1119),DELAX)
      EQUIVALENCE (C(1120),DELEX)
      EQUIVALENCE (C(1121),DELRX)
      EQUIVALENCE (C(1713),WBEB(1))
C
C DVBA= O Speed of vehicle wrt air mass - m/s
C ALPHA= O Angle of attack - rad
C BETA= O Sideslip angle - rad
C ALPHAX= O Angle of attack - deg
C BETAX= O Sideslip angle - deg
C DELAX= O Aileron deflection - deg
C DELEX= O Elevator deflection - deg
C DELRX= O Rudder deflection - deg
C WBEB= S Body rates - rad/sec
C
C***  OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1209),CXT)
      EQUIVALENCE (C(1210),CYT)
      EQUIVALENCE (C(1211),CZT)
      EQUIVALENCE (C(1212),CLT)
      EQUIVALENCE (C(1213),CMT)
      EQUIVALENCE (C(1214),CNT)
C
C CXT = O BODY FRAME TOTAL X FORCE COEFFICIENT
C CYT = O BODY FRAME TOTAL Y FORCE COEFFICIENT
C CZT = O BODY FRAME TOTAL Z FORCE COEFFICIENT
C CLT = O BODY FRAME TOTAL ROLL MOMENT COEFFICIENT
C CMT = O BODY FRAME TOTAL PITCH MOMENT COEFFICIENT
C CNT = O BODY FRAME TOTAL YAW MOMENT COEFFICIENT
C
C*** DIAGNOSTIC VARIABLES
C
      EQUIVALENCE (C(1220),CX)
      EQUIVALENCE (C(1221),CM)
      EQUIVALENCE (C(1222),CL)
      EQUIVALENCE (C(1223),CN)
      EQUIVALENCE (C(1224),CZ)
      EQUIVALENCE (C(1225),CXQ)
      EQUIVALENCE (C(1226),CYR)
      EQUIVALENCE (C(1227),CYP)
      EQUIVALENCE (C(1228),CZQ)
      EQUIVALENCE (C(1229),CLR)
      EQUIVALENCE (C(1230),CLP)
      EQUIVALENCE (C(1231),CMQ)
      EQUIVALENCE (C(1232),CNR)
      EQUIVALENCE (C(1233),CNP)
      EQUIVALENCE (C(1237),CDD)
      EQUIVALENCE (C(1238),CYY)
      EQUIVALENCE (C(1239),CLL)
      EQUIVALENCE (C(1240),ALOD)
C
C CX = G X-AXIS AERO COEFF.
C CM = G PITCHING MOMENT COEFF.
C CL = G ROLLING MOMENT COEFF.
C CN = G YAWING MOMENT COEFF.
C CZ = G Z-AXIS AERO COEFF.
C CXQ = G DAMPING COEFF. CXQ
C CYR = G DAMPING COEFF. CYR
C CYP = G DAMPING COEFF. CYP
C CZQ = G DAMPING COEFF. CZQ
C CLR = G DAMPING COEFF. CLR
C CLP = G DAMPING COEFF. CLP
C CMQ = G DAMPING COEFF. CMQ
C CNR = G DAMPING COEFF. CNR
C CNP = G DAMPING COEFF. CNP
C CDD = G Drag coefficient - ND
C CYY = G Side force coefficient - ND
C CLL = G Lift coefficient - ND
C ALOD = G Lift over drag ratio - ND
C
	PARAMETER(MXA1=12,MXD2=5,MXB2=7)
C
C*** SET LOCAL ALPHA, BETA, DELEX IN DEG FOR TABLE CALLS
C
      TALPHA = ALPHA * CRAD
      ALPHADEG = TALPHA
C
      TBETA = BETA * CRAD
      BETADEG = TBETA
C
C*** EXECUTE TABLE CALLS
C
      TDELEX = DELEX
      CALL TABL2(ALPHADEG,TDELEX,ALPDEL,TABCX,ITDIM1,CX)
      ALPHADEG = TALPHA
      TDELEX = DELEX
 
      CALL TABL2(ALPHADEG,BETADEG,ALPBET2,TABDLDA,ITDIM2,DLDA)
	DLDA=-DLDA ! see note at Header
      ALPHADEG = TALPHA
      BETADEG = TBETA
 
      CALL TABL2(ALPHADEG,BETADEG,ALPBET2,TABDLDR,ITDIM2,DLDR)
      ALPHADEG = TALPHA
      BETADEG = TBETA
C   Pitching moment as a function of alpha and elevator
	CALL TBLP2(ALPHAX,DELEX,IXA1,IXD2,ALPI,DELI,MXA1,MXD2
     &,TABCM,CM,CMA,CMDE)
cc      CALL TABL2(ALPHADEG,TDELEX,ALPDEL,TABCM,ITDIM1,CM)
cc      ALPHADEG = TALPHA
cc      TDELEX = DELEX
C   Yawing moment as a function of alpha and beta
      CALL TBLP2(ALPHAX,BETAX,IXA1,IXB2,ALPI,BETI,MXA1,MXB2
     &,TABCN,CN,CNA,CNB)
cc      BETADEG = ABS(BETADEG)
cc      CALL TABL2(ALPHADEG,BETADEG,ALPBET,TABCN,ITDIM2,CN)
cc      ALPHADEG = TALPHA
cc      BETADEG = TBETA
C
      CALL TABL2(ALPHADEG,BETADEG,ALPBET,TABCL,ITDIM2,CL)
      ALPHADEG = TALPHA
      BETADEG = TBETA
 
      CALL TABL2(ALPHADEG,BETADEG,ALPBET2,TABDNDA,ITDIM2,DNDA)
      ALPHADEG = TALPHA
      BETADEG = TBETA
 
      CALL TABL2(ALPHADEG,BETADEG,ALPBET2,TABDNDR,ITDIM2,DNDR)
      ALPHADEG = TALPHA
      BETADEG = TBETA
 
      CALL TABLE(ALPHADEG,VECALPHA,TABCXQ,12,CXQ)
      ALPHADEG = TALPHA
 
      CALL TABLE(ALPHADEG,VECALPHA,TABCYR,12,CYR)
      ALPHADEG = TALPHA
 
      CALL TABLE(ALPHADEG,VECALPHA,TABCYP,12,CYP)
      ALPHADEG = TALPHA
C   Normal force as a function of alpha
	CALL TBLP1(ALPHAX,IXA1,ALPI,MXA1,TABCZ,CZ,CZA)
CC      CALL TABLE(ALPHADEG,VECALPHA,TABCZ,12,CZ)
CC      ALPHADEG = TALPHA
 
      CALL TABLE(ALPHADEG,VECALPHA,TABCZQ,12,CZQ)
      ALPHADEG = TALPHA
 
      CALL TABLE(ALPHADEG,VECALPHA,TABCLR,12,CLR)
      ALPHADEG = TALPHA
 
      CALL TABLE(ALPHADEG,VECALPHA,TABCLP,12,CLP)
      ALPHADEG = TALPHA
C   Pitch damping derivative
      CALL TABLE(ALPHADEG,VECALPHA,TABCMQ,12,CMQ)
      ALPHADEG = TALPHA
 
      CALL TABLE(ALPHADEG,VECALPHA,TABCNR,12,CNR)
      ALPHADEG = TALPHA
 
      CALL TABLE(ALPHADEG,VECALPHA,TABCNP,12,CNP)
C
C*** CALCULATE TOTAL FORCE AND MOMENT COEFFICIENTS IN BODY COORDINATES
C
      C2V = CHORD/(2.*DVBA)
      B2V = SPAN/(2.*DVBA)
      P = WBEB(1)
      Q = WBEB(2)
      R = WBEB(3)
C
      CXT = CX + C2V*CXQ*Q
      CYT = -0.02*BETA + 0.021*DELAX/20. + 0.086*DELRX/30. +
     &      B2V*( CYR*R + CYP*P )
      CZT = CZ*( 1 - (BETA/CRAD)**2 ) - 0.19*DELEX/25. + C2V*CZQ*Q
      CLT = CL + DLDA*DELAX/20. + DLDR*DELRX/30. +
     &      B2V*( CLR*R + CLP*P )
      CMT = CM + C2V*CMQ*Q + CZT*(XCGR - XCG)
      CNT = CN + DNDA*DELAX/20. + DNDR*DELRX/30. - CYT*(XCGR - XCG)*
     &      (CHORD/SPAN) + B2V*( CNR*R + CNP*P )
C
C*** LIFT AND DRAG COEFFICIENTS (small beta assumption)
C
	COSA=COS(ALPHA)
	SINA=SIN(ALPHA)
	CDD=-CXT*COSA-CZT*SINA
	CYY=CYT
	CLL=CXT*SINA-CZT*COSA
	ALOD=CLL/CDD
C
C*** DERIVATIVES
C
	CLA=-CZA		!per deg
	CLDE=0.19/25.	!per deg
	CMA=CMA			!per deg
	CMQ=CMQ			!per rad/s
	CMDE=CMDE		!per deg
	CYB=-0.02/CRAD	!per DEG
	CYDR=-0.086/25.	!per deg
	CLNB=CNB		!per deg
	CLNR=B2V*CNR	!per rad/s
	CLNDR=DNDR/30.	!per deg
	CLLP=B2V*CLP	!per rad/s
	CLLDA=DLDA/20.	!per deg
C
      CALL A1DER(CLA,CLDE,CMA,CMQ,CMDE,CYB,CYDR,CLNB,CLNR,CLNDR
     &,CLLP,CLLDA)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A1DER(CLA,CLDE,CMA,CMQ,CMDE,CYB,CYDR,CLNB,CLNR,CLNDR
     &,CLLP,CLLDA)
C**********************************************************************
C*** * Aerodynamic derivaties for autopilot
C*** *
C*** * This subroutine performs the following functions:
C*** * (1) Calculates Dimensional Derivaties (radians, meters, seconds)
C*** * (2) Calculates the Airframe Roots
C*** *
C*** * Input:
C*** *        CLA=Lift slope derivative wrt alpha - 1/deg
C*** *        CLDE=Lift force control derivative - 1/deg
C*** *        CMA=Pitching moment derivatve wrt alpha - 1/deg
C*** *        CMQ=Pitch damping derivative - 1/rad
C*** *        CMDE=Pitch moment control derivative - 1/deg
C*** *        CYB=Side force derivative wrt beta - 1/deg
C*** *        CYDR=Side force control derivative - 1/deg
C*** *        CLNB=Yawing moment derivatve wrt beta - 1/deg
C*** *        CLNR=Yaw damping derivative - 1/rad
C*** *        CLNDR=Rudder moment control derivative - 1/deg
C*** *        CLLP=Roll damping derivative - 1/rad
C*** *        CLLDA=Aileron control derivative - 1/deg
C*** *
C*** * MODIFICATION HISTORY
C*** * 990614 Created by Peter Zipfel
C*** * 000329 Adapted from GHAME6 for FALCON6, PZi
C*** *
C*** **************************************************************
C
      COMMON C(3510)
C
	DIMENSION AIB(3,3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2000),TIME)
C
C*** INPUT FROM INITIALIZATION MODULE
C
      EQUIVALENCE (C(1204),REFA)
      EQUIVALENCE (C(1205),CHORD)
      EQUIVALENCE (C(1206),SPAN)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0206),VMACH)
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(0219),DVBA)
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1313),THRUST)
      EQUIVALENCE (C(1338),AIB(1,1))
C
C VMACH= O Vehicle Mach number based on DVBA
C PDYNMC= O Dynamic pressure - Pa
C DVBA= O Speed of vehicle wrt air mass - m/s
C VMASS= I Vehicle mass - kg
C THRUST= O Tubojet thrust (lbf)
C AIB= I Moment of inertia matrix - kgm^2
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1245),DLA)
      EQUIVALENCE (C(1246),DLDE)
      EQUIVALENCE (C(1247),DMA)
      EQUIVALENCE (C(1248),DMQ)
      EQUIVALENCE (C(1249),DMDE)
      EQUIVALENCE (C(1250),DYB)
      EQUIVALENCE (C(1251),DYDR)
      EQUIVALENCE (C(1252),DNB)
      EQUIVALENCE (C(1253),DNR)
      EQUIVALENCE (C(1254),DNDR)
      EQUIVALENCE (C(1255),DLP)
      EQUIVALENCE (C(1256),DLDA)
C
C DLA = O Lift force slope derivative - m/s^2
C DLDE = O Lift force control derivative - m/s^2
C DMA = O Pitch moment derivative - 1/s^2
C DMQ = O Pitch damping derivative - 1/s
C DMDE = O Pitch control derivative - 1/s^2
C DYB = O Side force slope derivative - m/s^2
C DYDR = O Side force control derivative - m/s^2
C DNB = O Yaw moment derivative - 1/s^2
C DNR = O Yaw damping derivative - 1/s
C DNDR = O Yaw control derivative - 1/s^2
C DLP = O Roll damping derivative - 1/s
C DLDA = O Roll control derivative - 1/s^2
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1268),STMARG)
      EQUIVALENCE (C(1269),REALP1)
      EQUIVALENCE (C(1270),REALP2)
      EQUIVALENCE (C(1271),WNP)
      EQUIVALENCE (C(1272),ZETP)
      EQUIVALENCE (C(1273),RPREAL)
      EQUIVALENCE (C(1274),REALY1)
      EQUIVALENCE (C(1275),REALY2)
      EQUIVALENCE (C(1276),WNY)
      EQUIVALENCE (C(1277),ZETY)
      EQUIVALENCE (C(1278),RYREAL)
C
C STMARG = G Static margin (+stable, -unstable) - caliber
C REALP1 = G First real root of airframe pitch dynamics - rad/s
C REALP2 = G Second real root of airframe pitch dynamics - rad/s
C WNP = G Natural frequency of airframe pitch dynamics - rad/s
C ZETP = G Damping of airframe pitch dynamics - ND
C RPREAL = G Real part or mean value (real roots) of airf pitch - rad/s
C REALY1 = G First real root of airframe yaw dynamics - rad/s
C REALY2 = G Second real root of airframe yaw dynamics - rad/s
C WNY = G Natural frequency of airframe yaw dynamics - rad/s
C ZETY = G Damping of airframe yaw dynamics - ND
C RYREAL = G Real part or mean value (real roots) of airf yaw - rad/s
C
C*** Dimensional derivatives in pitch (converted to 1/rad where requried)
C
      DUML=PDYNMC*REFA/VMASS
      DLA=DUML*CLA*CRAD
	DLDE=DUML*CLDE*CRAD
C
      DUMM=PDYNMC*REFA*CHORD/AIB(2,2)
      DMA=DUMM*CMA*CRAD
      DMQ=DUMM*(CHORD/(2.*DVBA))*CMQ
      DMDE=DUMM*CMDE*CRAD
C
C*** Dimensional derivatives in yaw (converted to 1/rad where requried)
C
      DUMY=PDYNMC*REFA/VMASS
      DYB=DUMY*CYB*CRAD
	DYDR=DUMY*CYDR*CRAD
C
      DUMN=PDYNMC*REFA*SPAN/AIB(3,3)
      DNB=DUMN*CLNB*CRAD
      DNR=DUMN*(SPAN/(2.*DVBA))*CLNR
      DNDR=DUMN*CLNDR*CRAD
C
C*** Dimensional derivatives in roll (converted to 1/rad where requried)
C
      DUMLL=PDYNMC*REFA*SPAN/AIB(1,1)
      DLP=DUMLL*(SPAN/(2.*DVBA))*CLLP
      DLDA=DUMLL*CLLDA*CRAD
C
C*** Static margin STMARG (per cord length CHORD)
C
      IF(CLA.NE.0.)STMARG=-CMA/CLA
C
C*** Pitch plane roots
C
      A11=DMQ
      A12=DMA/DLA
      A21=DLA
      A22=-DLA/DVBA
C
      ARG=(A11+A22)**2-4.*(A11*A22-A12*A21)
      IF(ARG.GE.0.) THEN
         WNP=0.
         ZETP=0.
         DUM=A11+A22
         REALP1=(DUM+SQRT(ARG))/2.
         REALP2=(DUM-SQRT(ARG))/2.
         RPREAL=(REALP1+REALP2)/2.
      ELSE
         REALP1=0.
         REALP2=0.
         WNP=SQRT(A11*A22-A12*A21)
         ZETP=-(A11+A22)/(2.*WNP)
         RPREAL=-ZETP*WNP
      ENDIF
C
C*** Yaw plane roots
C
      A11=DNR
      A12=DNB/DYB
      A21=-DYB
      A22=DYB/DVBA
C
      ARG=(A11+A22)**2-4.*(A11*A22-A12*A21)
      IF(ARG.GE.0.) THEN
         WNY=0.
         ZETY=0.
         DUM=A11+A22
         REALY1=(DUM+SQRT(ARG))/2.
         REALY2=(DUM-SQRT(ARG))/2.
         RYREAL=(REALY1+REALY2)/2.
      ELSE
         REALY1=0.
         REALY2=0.
         WNY=SQRT(A11*A22-A12*A21)
         ZETY=-(A11+A22)/(2.*WNY)
         RYREAL=-ZETY*WNY
      ENDIF
C
C*** ROLL RATE ROOT
C
      REALP=DLP
C
 100  RETURN
      END
C**********************************************************************
      SUBROUTINE A2I
C**********************************************************************
C*** * Engine Initialization Module of FALCON6 Aircraft
C*** * Reserved C(3510) locations are 1300-1399
C*** *
C*** * Single turbofan engine with afterburner
C*** * References: (1) Stevens & Lewis, "Aircraft Control and
C*** * Simulation" Wiley-Interscience Publication, 1992.
C*** * Data based on: (2) Nguyen, J.T., et al., "Simulator Study of
C*** * Stall/Post-Stall Characteristics of a Fighter Airplane with
C*** * Relaxec Longitudinal Static Stability", NASA Tech. Paper 1538,
C*** * NASA, Washington, D.C. Dec. 1979.
C*** * This module performs the following functions:
C*** *
C*** * * Defines the locations of state variable used in A2
C*** *
C*** * MODIFICATION HISTORY
C*** * 921207 Created by Leonard Frantz
C*** * 000405 Adapted to SI units, PZi
C*** *
C*** ******************************************************************
C
      DIMENSION IPL(100),IPLV(100),AIB(3,3)
C
      COMMON C(3510)
C
C***  INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
      EQUIVALENCE (C(2561),NIP)
C
C***  INITIALIZATION
C
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1337),AIE)
	EQUIVALENCE (C(1338),AIB(1,1))
C
C VMASS = I Vehicle mass - kg
C AIB(3,3) = I Moment of inertia matrix - kgm^2
C AIE = I Engine angular momentum - kgm^2
C
C***  INITIALIZATION OF INTEGRATION
C
      IPL(NIP) = 1301
      IPLV(NIP) = 1302
      NIP=NIP+1
C
	VMASS=9296.
	AIB(1,1) = 9496.*1.3558
	AIB(2,1) = 0.00000
	AIB(3,1) = -982.00*1.3558
	AIB(1,2) = 0.00000
	AIB(2,2) = 55814.*1.3558
	AIB(3,2) = 0.00000
	AIB(1,3) = -982.00*1.3558
	AIB(2,3) = 0.00000
	AIB(3,3) = 63100.*1.3558
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A2
C**********************************************************************
C*** * Engine Module of FALCON6 aircraft
C*** * Reserved C(3510) locations are 1300-1399
C*** *
C*** * Single turbofan engine with afterburner
C*** * References: (1) Stevens & Lewis, "Aircraft Control and
C*** * Simulation" Wiley-Interscience Publication, 1992.
C*** * Data based on: (2) Nguyen, J.T., et al., "Simulator Study of
C*** * Stall/Post-Stall Characteristics of a Fighter Airplane with
C*** * Relaxec Longitudinal Static Stability", NASA Tech. Paper 1538,
C*** * NASA, Washington, D.C. Dec. 1979.
C*** *
C*** * This module performs the following functions:
C*** *
C*** * * CalculateS the thrust of the Falcon aircraft as a function of
C*** *   throttle setting, altitude, and Mach. Includes
C*** *   a lag filter.
C*** * * Effect of fuel consumption on vehicle mass is neglected
C*** *
C*** * MODIFICATION HISTORY
C*** * 921207 Created by Leonard Frantz
C*** * 000405 Adapted to SI units, PZi
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C***  INITIALIZATION
C
      DIMENSION VECALTMACH(12), SBEL(3), ITDIM(2), TABIDLE(6,6)
      DIMENSION TABMIL(6,6), TABMAX(6,6)
C
C*** TABLES(MACH,ALT);(ROWS OF 6 MACH, COLUMNS OF 6 ALT IN FT)
C
      DATA VECALTMACH /
     x 0.,10000.,20000.,30000.,40000.,50000.,
     X 0.,   0.2,   0.4,   0.6,   0.8,   1.0/
C
      DATA ITDIM /6,6/
C
      DATA TABIDLE /
     x  1060.0,   670.0,   880.0,  1140.0,  1500.0,  1860.0,
     x   635.0,   425.0,   690.0,  1010.0,  1330.0,  1700.0,
     x    60.0,    25.0,   345.0,   755.0,  1130.0,  1525.0,
     x -1020.0,  -710.0,  -300.0,   350.0,   910.0,  1360.0,
     x -2700.0, -1900.0, -1300.0,  -247.0,   600.0,  1100.0,
     x -3600.0, -1400.0,  -595.0,  -342.0,  -200.0,   700.0/
C
      DATA TABMIL /
     x 12680.0,  9150.0,  6200.0,  3950.0,  2450.0,  1400.0,
     x 12680.0,  9150.0,  6313.0,  4040.0,  2470.0,  1400.0,
     x 12610.0,  9312.0,  6610.0,  4290.0,  2600.0,  1560.0,
     x 12640.0,  9839.0,  7090.0,  4660.0,  2840.0,  1660.0,
     x 12390.0, 10176.0,  7750.0,  5320.0,  3250.0,  1930.0,
     x 11680.0,  9848.0,  8050.0,  6100.0,  3800.0,  2310.0/
C
      DATA TABMAX /
     x 20000.0, 15000.0, 10800.0,  7000.0,  4000.0,  2500.0,
     x 21420.0, 15700.0, 11225.0,  7323.0,  4435.0,  2600.0,
     x 22700.0, 16860.0, 12250.0,  8154.0,  5000.0,  2835.0,
     x 24240.0, 18910.0, 13760.0,  9285.0,  5700.0,  3215.0,
     x 26070.0, 21075.0, 15975.0, 11115.0,  6860.0,  3950.0,
     x 28886.0, 23319.0, 18300.0, 13484.0,  8642.0,  5057.0/
C
C***  INPUT DATA
C
      EQUIVALENCE (C(1304), THRTL)
C
C THRTL = D Nominal throttle setting (0 -> 1)
C
C***  INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0053),OPTMET)
      EQUIVALENCE (C(2000), TIME)
C
C***  INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0206), VMACH)
      EQUIVALENCE (C(1619), SBEL(1))
C
C VMACH= O Vehicle Mach number based on DVBA
C SBEL= S Vehicle pos. wrt point E in local level axes - m
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1301), POWD)
      EQUIVALENCE (C(1302), POW)
C
C POWD = S Derivative of achieved power setting (%/sec)
C POW = S Achieved power setting (%)
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1313), THRUST)
C
C THRUST = O Tubojet thrust (lbf)
C
C*** DIAGNOSTIC VARIABLES
C
      EQUIVALENCE (C(1306), PC)
      EQUIVALENCE (C(1307), TPOW)
      EQUIVALENCE (C(1308), PIDL)
      EQUIVALENCE (C(1310), PMAX)
      EQUIVALENCE (C(1311), THR)
      EQUIVALENCE (C(1312), PMIL)
C
C PC = G Commanded power setting - %
C TPOW = G Power time constant - sec
C PIDL = G Idle power from table - lbf
C PMAX = G Maximum power from table - lbf
C THR = G Actual throttle setting 0 -> 1
C PMIL = G Military power from table - lbf
C
      THR = THRTL
C
C*** CALCULATE COMMANDED POWER FROM THROTTLE SETTING
C
      IF (THR.LE.0.77) THEN
        PC = 64.94*THR
      ELSE
        PC = 217.38*THR - 117.38
      ENDIF
C
C*** CALCULATE THE POWER TIME CONSTANT
C
      IF (PC.LE.50.) THEN
        TPOW = 1.0
      ELSE
        TPOW = 0.2
      ENDIF
C
C*** SET THE DERIVATIVE OF ACHIEVED POWER
C
      POWD = (PC - POW)/TPOW
C
C*** CALCULATE THE THRUST FROM LOOK-UP TABLES
C
      TMACH = VMACH
      TALT = -SBEL(3)*(1.+2.28*OPTMET)
C
      CALL TABL2(TALT,TMACH,VECALTMACH,TABIDLE,ITDIM,PIDL)
	PIDL=PIDL*(1.+3.448*OPTMET)
      TMACH = VMACH
      TALT = -SBEL(3)*(1.+2.28*OPTMET)
C
      CALL TABL2(TALT,TMACH,VECALTMACH,TABMIL,ITDIM,PMIL)
	PMIL=PMIL*(1.+3.448*OPTMET)
      TMACH = VMACH
      TALT = -SBEL(3)*(1.+2.28*OPTMET)
C
      CALL TABL2(TALT,TMACH,VECALTMACH,TABMAX,ITDIM,PMAX)
	PMAX=PMAX*(1.+3.448*OPTMET)
C
      IF (POW.LE.50.) THEN
        THRUST = PIDL + POW*0.02*(PMIL - PIDL)
      ELSE
        THRUST = PMIL + (POW-50.)*0.02*(PMAX-PMIL)
      ENDIF
C
      RETURN
      END
C******************************************************************
      SUBROUTINE A3
C******************************************************************
C*** * Force Module A3
C*** * Reserved C(3510) locations are 1400-1499
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates the non-gravitational forces
C*** * (2) Calculates the aerodynamic moments
C*** *
C*** * MODIFICATION HISTORY
C*** * 931117 Created by Peter Zipfel
C*** * 980814 For RT-CADAC (C3 deleted) thrust included directly, PZi
C*** *
C*** **************************************************************
C
      COMMON C(3510)
C
      DIMENSION FAPB(3),FMB(3)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0207),PDYNMC)
	EQUIVALENCE (C(1204),REFA)
      EQUIVALENCE (C(1205),CHORD)
      EQUIVALENCE (C(1206),SPAN)
      EQUIVALENCE (C(1209),CXT)
      EQUIVALENCE (C(1210),CYT)
      EQUIVALENCE (C(1211),CZT)
      EQUIVALENCE (C(1212),CLT)
      EQUIVALENCE (C(1213),CMT)
      EQUIVALENCE (C(1214),CNT)
      EQUIVALENCE (C(1313),THRUST)
C
C PDYNMC= O Dynamic pressure - Pa
C REFA= I Reference area - m^2
C CHORD= I Chord reference length - m
C SPAN= I Span reference length -m
C CXT= O BODY FRAME TOTAL X FORCE COEFFICIENT
C CYT= O BODY FRAME TOTAL Y FORCE COEFFICIENT
C CZT= O BODY FRAME TOTAL Z FORCE COEFFICIENT
C CLT= O BODY FRAME TOTAL ROLL MOMENT COEFFICIENT
C CMT= O BODY FRAME TOTAL PITCH MOMENT COEFFICIENT
C CNT= O BODY FRAME TOTAL YAW MOMENT COEFFICIENT
C THRUST= O Tubojet thrust (lbf)
C
C*** OUTPUTS TO OTHER MODULES
C
      EQUIVALENCE (C(1410),FAPB(1))
      EQUIVALENCE (C(1413),FMB(1))
C
C FAPB(3) = O Aerodynamic and propulsive forces in body axes - N
C FMB(3) = O Aerodynamic and propulsive moments in body axes - N*m
C
C*** TOTAL NON-GRAVITATIONAL FORCES
C
      FAPB(1)=PDYNMC*REFA*CXT+THRUST
      FAPB(2)=PDYNMC*REFA*CYT
      FAPB(3)=PDYNMC*REFA*CZT
C
C*** AERODYNAMIC AND PROPULSIVE MOMENTS
C
      FMB(1)=PDYNMC*REFA*SPAN*CLT
      FMB(2)=PDYNMC*REFA*CHORD*CMT
      FMB(3)=PDYNMC*REFA*SPAN*CNT
C
      RETURN
      END
C******************************************************************
      SUBROUTINE D1I
C******************************************************************
C*** * The initialization module for the Newton Module D1.
C*** * Reserved C(3510) locations are 1600-1699
C*** *
C*** * Initialization of Missile:
C*** *		Euler angles PSIBLX,THTBLX,PHIBLX - (G3I)
C*** *		Incidence Angles ALPHAX,BETAX - (G3I)
C*** *        Body rates PP,QQ,RR - (D2I)
C*** *		Speed DVBE - (D1I)
C*** *		Location SBEL(3) - (D1I)
C*** *
C*** * This module performs the following functions:
C*** * (1) Initializes flight path angles PSIVLX, THTVLX
C*** * (2) Compute initial values of VBEL
C*** * (3) Initialize the state vector VBEB
C*** *
C*** * MODIFICATION HISTORY
C*** * 000309 Created by Peter Zipfel
C*** *
C*** **************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100),VBEB(3),SBEL(3),VBEL(3)
     &,TBL(3,3),TWB(3,3),TWL(3,3),TLW(3,3),V1W(3),V1L(3),X1L(3)
     &,L1L(3),L2L(3),L3L(3)	
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1600),MINIT)
      EQUIVALENCE (C(1619),SBEL(1))
      EQUIVALENCE (C(1636),DVBE)
C
C MINIT = D Initialization modes, see table in Module D1I
C ALPHA0X = D Initial angle-of-attack - deg
C BETA0X = D Initial side slip angle - deg
C
C*** INPUT FROM EXEC
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0309),TBL(1,1))
      EQUIVALENCE (C(0344),ALPHAX)
      EQUIVALENCE (C(0345),BETAX)
C
C TBL= O TM of body wrt local level coord - ND
C ALPHAX= O Angle of attack - deg
C BETAX= O Sideslip angle - deg
C
C*** INITIALIZATIONS
C
      EQUIVALENCE (C(1613),VBEB(1))
      EQUIVALENCE (C(1633),VBEL(1))
      EQUIVALENCE (C(1639),HBE)
      EQUIVALENCE (C(1640),PSIVLX)
      EQUIVALENCE (C(1641),THTVLX)
      EQUIVALENCE (C(1642),GNDTCK)
C
      ILOC=1610
      DO I=0,2
         IPL(NIP)=ILOC+I
         IPLV(NIP)=ILOC+I+3
         NIP=NIP+1
      ENDDO
C
      ILOC=1616
      DO I=0,2
         IPL(NIP)=ILOC+I
         IPLV(NIP)=ILOC+I+3
         NIP=NIP+1
      ENDDO
C
C*** CALCULATE FLIGHT PATH ANGLES
C
	CALP=COS(ALPHAX/CRAD)
	SALP=SIN(ALPHAX/CRAD)
	CBET=COS(BETAX/CRAD)
	SBET=SIN(BETAX/CRAD)
	TWB(1,1)=CALP*CBET
	TWB(1,2)=SBET
	TWB(1,3)=SALP*CBET
	TWB(2,1)=-CALP*SBET
	TWB(2,2)=CBET
	TWB(2,3)=-SALP*SBET
	TWB(3,1)=-SALP
	TWB(3,2)=0.
	TWB(3,3)=CALP	
C
	CALL MATMUL(TWL,TWB,TBL,3,3,3)
	CALL MATTRA(TLW,TWL,3,3)
	CALL VECVEC(V1W,1.,0.,0.)
	CALL MATMUL(V1L,TLW,V1W,3,3,1)
	CALL VECUVC(X1L,V1L(1),V1L(2),0.)
	CALL VECVEC(L1L,1.,0.,0.)
C
	CALL MATSCA(DUM,X1L,L1L,3)
	PSIVL=ACOS(DUM)
	CALL MATSCA(DUM,X1L,V1L,3)
	THTVL=ACOS(DUM)
C
C*** SIGN ADJUSTMENTS
C
	CALL VECVEC(L2L,0.,1.,0.)
	CALL VECVEC(L3L,0.,0.,1.)
	CALL MATSCA(DUM1,X1L,L2L,3)
	CALL MATSCA(DUM2,V1L,L3L,3)
C
	PSIVLX=PSIVL*CRAD*SIGN(1.,DUM1)
	THTVLX=THTVL*CRAD*SIGN(1.,-DUM2)
C
C*** INITIALIZE MISSILE VELOCITY VECTOR VBEL(3)
C
      CALL MATCAR(VBEL,DVBE,PSIVL,THTVL)
C
C*** INITIALIZATION OF VBEB(3)
C
      ALPHA=ALPHAX/CRAD
      BETA=BETAX/CRAD
      SALP=SIN(ALPHA)
      CALP=COS(ALPHA)
      SBET=SIN(BETA)
      CBET=COS(BETA)
C
      VBEB(1)=CALP*CBET*DVBE
      VBEB(2)=SBET*DVBE
      VBEB(3)=SALP*CBET*DVBE
C
C*** INITIALIZE HBE
C
      HBE=-SBEL(3)
	GNDTCK=0.
C
      RETURN
      END
C******************************************************************
      SUBROUTINE D1
C******************************************************************
C*** * Newton Module D1
C*** * Reserved C(3510) locations are 1600-1699
C*** * This module performs the following functions:
C*** *
C*** * (1) Integrates vehicle velocity in body axes
C*** * (2) Integrates vehicle position in local level axes
C*** *
C*** * MODIFICATION HISTORY
C*** * 931117 Created by Peter Zipfel
C*** * 980413 Added groundtrack diagnostic: GNDTCK, PZi
C*** *
C*** **************************************************************
C
      COMMON C(3510)
C
      DIMENSION VBEBD(3),VBEB(3),SBELD(3),SBEL(3),WBEB(3),DUM33(3,3)
     &,ATB(3),FAPB(3),FSPB(3),DUM4(3),GRAVL(3),GRAVB(3),TBL(3,3)
     &,TLB(3,3),VBEL(3),SBELM(3),DUM3(3),SBT1L(3),ST1EL(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2866),ICOOR)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(0205),GRAV)
      EQUIVALENCE (C(0309),TBL(1,1))
      EQUIVALENCE (C(0901),MFREEZE)
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1410),FAPB(1))
      EQUIVALENCE (C(1713),WBEB(1))
C
C GRAV= O Gravitational acceleration - m/s^2
C TBL= O TM of body wrt local level coord - ND
C VMASS= I Vehicle mass - kg
C FAPB= O Aerodynamic and propulsive forces in body axes - N
C WBEB= S Body rates - rad/sec
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1610),VBEBD(1))
      EQUIVALENCE (C(1613),VBEB(1))
      EQUIVALENCE (C(1616),SBELD(1))
      EQUIVALENCE (C(1619),SBEL(1))
C
C VBEB(3) = S Vehicle velocity in body axes - m/s
C SBEL(3) = S Vehicle pos. wrt point E in local level axes - m
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1630),FSPB(1))
      EQUIVALENCE (C(1633),VBEL(1))
      EQUIVALENCE (C(1636),DVBE)
      EQUIVALENCE (C(1639),HBE)
	EQUIVALENCE (C(1671),DBT1)
C
C FSPB(3) = O Specific force in body axes - m/s^2
C VBEL(3) = O Vehicle velocity in local level axes - m/s
C DVBE = I/O Vehicle speed - m/s
C HBE = O Height above sea level - m
C DBT1 = O Distance between vehicle and target - m
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1640),PSIVLX)
      EQUIVALENCE (C(1641),THTVLX)
      EQUIVALENCE (C(1642),GNDTCK)
      EQUIVALENCE (C(1644),AN)
      EQUIVALENCE (C(1645),AL)
      EQUIVALENCE (C(1646),ATB)
      EQUIVALENCE (C(1672),SBT1L(1))
C
C PSIVLX = G Heading angle - deg
C THTVLX = G Vertical flight path angle - deg
C GNDTCK = G Ground track - m
C AN = G Normal specific force component - g's
C AL = G Lateral specific force component - g's
C ATB = G Tangential accel, solving Newton's Law - m/s^2
C SBT1L(3) = G Vehicle wrt Target displacement - m
C
C*** VEHICLE ACCEL IN BODY AXIS AND INTEGRATE FOR VELOCITY VBEB(3)
C
      CALL MATSKS(DUM33,WBEB)
      CALL MATMUL(ATB,DUM33,VBEB,3,3,1)
      CALL MATCON(FSPB,1./VMASS,FAPB,3,1)
      CALL MATSUB(DUM4,FSPB,ATB,3,1)
      CALL VECVEC(GRAVL,0.,0.,GRAV)
      CALL MATMUL(GRAVB,TBL,GRAVL,3,3,1)
      CALL MATADD(VBEBD,DUM4,GRAVB,3,1)
      CALL MATABS(DVBE,VBEB,3)
C
C*** VEHICLE VEL IN LOCAL LEVEL AXES AND INTEGRATED FOR POSTION SBEL(3)
C
      CALL MATTRA(TLB,TBL,3,3)
      CALL MATMUL(VBEL,TLB,VBEB,3,3,1)
      CALL MATEQL(SBELD,VBEL,3,1)
C
C*** RANGE TO TARGET
C
	CALL MATSUB(SBT1L,SBEL,ST1EL,3,1)
	CALL MATABS(DBT1,SBT1L,3)
C
C*** FLIGHT PATH ANGLES
C
      IF(VBEL(1).EQ.0..AND.VBEL(2).EQ.0.) THEN
         PSIVL=0.
      ELSE
         PSIVL=ATAN2(VBEL(2),VBEL(1))
      ENDIF
      THTVL=ATAN2(-VBEL(3),SQRT(VBEL(1)**2+VBEL(2)**2))
      PSIVLX=PSIVL*CRAD
      THTVLX=THTVL*CRAD
C
      HBE=-SBEL(3)
C
C*** DIAGNOSTIC: ACCELERATIONS ACHIEVED
C
      AN=-FSPB(3)/GRAV
      AL=FSPB(2)/GRAV
C
C*** DIAGNOSTIC: DISTANCE TRAVELLED ALONG GROUNDTRACK
C
	IF(ICOOR.EQ.-1)CALL MATEQL(SBELM,SBEL,3,1)
      IF(ICOOR.EQ.1) THEN
         CALL MATSUB(DUM3,SBEL,SBELM,3,1)
         DUM3(3)=0.
         CALL MATABS(DUM,DUM3,3)
         GNDTCK=DUM+GNDTCK
         CALL MATEQL(SBELM,SBEL,3,1)
      END IF
C
C*** FREEZE VARIABLES FOR AUTOPILOT RESPONSE CALCULATIONS
C
      IF(MFREEZE.EQ.0) THEN
         MFREEZEM=0
      ELSE
         IF(MFREEZE.NE.MFREEZEM) THEN
            MFREEZEM=MFREEZE
            DVBEF=DVBE
         ENDIF
         DVBE=DVBEF
      ENDIF
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE D2I
C**********************************************************************
C*** * Rotational Equations Initialization Module
C*** * Reserved C(3510) locations are 1700-1749
C*** *
C*** * This module performs the following functions:
C*** * * Defines the locations of the 3 state variables used in D2
C*** *
C*** * MODIFICATION HISTORY
C*** * 921207 Created by Leonard Frantz
C*** * 000404 Modified, PZi
C*** *
C*** ******************************************************************
C
      DIMENSION IPL(100),IPLV(100)
C
      COMMON C(3510)
C
C***  INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
      EQUIVALENCE (C(2561),NIP)
C
C***  INITIALIZATION CODE
C
 
      LOCATION = 1710
      DO II = NIP,NIP+2
        IPL(II) = LOCATION
        IPLV(II) = LOCATION + 3
        LOCATION = LOCATION + 1
      ENDDO
      NIP=NIP+3
 
      RETURN
      END
C**********************************************************************
      SUBROUTINE D2
C**********************************************************************
C*** * Rotational Equations  Module
C*** * Reserved C(3510) locations are 1700-1749
C*** *
C*** * This module performs the following functions:
C*** * *  Calculates the body angular rate derivatives for
C*** *    use in integrating the body rates.
C*** *
C*** * MODIFICATION HISTORY
C*** * 921207 Created by Leonard Frantz
C*** * 000404 Modified, PZi
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION WBEB(3),FMB(3),AIB(3,3),WBEBD(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1337),AIE)
	EQUIVALENCE (C(1338),AIB(1,1))
      EQUIVALENCE (C(1413),FMB(1))
C
C VMASS= I Vehicle mass - kg
C AIE= I Engine angular momentum - kgm^2
C AIB= I Moment of inertia matrix - kgm^2
C FMB= O Aerodynamic and propulsive moments in body axes - N*m
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1710),WBEBD(1))
      EQUIVALENCE (C(1713),WBEB(1))
C
C WBEB(3) = S Body rates - rad/sec
C WBEBD(3) = S Derivative of body rates - rad/s^2
C
C*** DIAGNOSTICS
C
	EQUIVALENCE (C(1716),PPX)
	EQUIVALENCE (C(1717),QQX)
	EQUIVALENCE (C(1718),RRX)
C
C PPX = D Body roll angular velocity in body axes - deg/s
C QQX = D Body pitch angular velocity in body axes - deg/s
C RRX = D Body yaw angular velocity in body axes - deg/s
 
C
C*** CALCULATE THE WBEBD VARIABLE FOR INTEGRATION CYCLE
C
      P = WBEB(1)
      Q = WBEB(2)
      R = WBEB(3)
C
      RM1 = FMB(1)
      RM2 = FMB(2)
      RM3 = FMB(3)
C
      AIB11 = AIB(1,1)
      AIB12 = AIB(1,2)
      AIB13 = AIB(1,3)
      AIB21 = AIB(2,1)
      AIB22 = AIB(2,2)
      AIB23 = AIB(2,3)
      AIB31 = AIB(3,1)
      AIB32 = AIB(3,2)
      AIB33 = AIB(3,3)
C
      PD = (((AIB22*AIB33 - AIB33*AIB33 - AIB13*AIB13)*R -
     x        AIB13*(AIB33 + AIB11 - AIB22)*P - AIB13*AIE)*Q +
     x      AIB33*RM1 - AIB13*RM3) / (AIB11*AIB33 - AIB13*AIB13)
C
      QD = (((AIB33 - AIB11)*P - AIE)*R + AIB13*(P*P - R*R) +
     x      RM2) / AIB22
C
      RD = (((-AIB11*AIB22 + AIB11*AIB11 + AIB13*AIB13)*P +
     x        AIB13*(AIB33 + AIB11 - AIB22)*R + AIB11*AIE)*Q +
     x      AIB11*RM3 - AIB13*RM1) / (AIB11*AIB33 - AIB13*AIB13)
C
      WBEBD(1) = PD
      WBEBD(2) = QD
      WBEBD(3) = RD
C
	PPX=WBEB(1)*CRAD
	QQX=WBEB(2)*CRAD
	RRX=WBEB(3)*CRAD
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE G2I
C***********************************************************************
C*** * Atmosphere and Wind Initialization
C*** * Reserved C(3510) locations are 200-299
C*** *
C*** * This module performs the following functions:
C*** * (1) Initializes state variables
C*** * (2) Calls modules D1I and G2 so that atmospheric variables are
C*** *     available for other initialization modules
C*** *
C*** * The switch MAIR controls the atmosphere, wind and gust options:
C*** *
C*** *   MAIR=|MTURB|MWIND|MATMO|
C*** *
C*** *         MTURB =0 No turbulence
C*** *			   =1 Dryden turbulence model
C*** *               MWIND =0 No Wind
C*** *                     =1 Constant Wind, Input: DVAEG,PSIWGX
C*** *    	   	         =2 Shear Wind, Input: WALTL,DVAEGL;WALTH,DVAEGH;PSIWGX
C*** * 	           	     =3 Tabular Wind, Input: Weather deck from INPUT.ASC
C*** *					 MATMO =0 Std. Atmosphere, US 76  atmosphere, internal	
C*** *   	                       =3 Tabular Atmosph, Input: Weather deck from IN
C*** *
C*** * MODIFICATION HISTORY:
C*** * 990415 Created by Peter H Zipfel
C*** * 000309 Conversion to flat earth, PZi
C*** *
C**********************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100)
C
C*** INPUTS FROM OTHER MODULES
C
	EQUIVALENCE (C(1636),DVBE)
C
C*** INPUTS FROM EXECUTIVE
C
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(0253),QN)
      EQUIVALENCE (C(0219),DVBA)
C
C*** INITIALIZING STATE VARIABLES
C
	ILOC=239
	DO I=0,2
	   IPL(NIP)=ILOC+I
	   IPLV(NIP)=ILOC+I+3
	   NIP=NIP+1
	ENDDO
	IPL(NIP)=260
	IPLV(NIP)=261
	NIP=NIP+1
	IPL(NIP)=262
	IPLV(NIP)=263
	NIP=NIP+1
C
C*** INITIALIZE DVBA BY GEOGRAPHIC SPEED
C
	DVBA=DVBE
C
C*** INITIALIZE HEATING VARIABLE
C
      QN=0.
C
 
      RETURN
      END
C***********************************************************************
      SUBROUTINE G2
C***********************************************************************
C*** * Atmosphere and Wind
C*** * Reserved C(3510) locations are 200-299
C*** *
C*** * This module performs the following functions:
C*** * (1) This Module supplies the atmospheric data: density, DRHO, speed of
C*** * sound, VSOUND, atmospheric pressure, PPRES, temprature, RTEMP; and
C*** * vehicle related data: dynamic pressure, PDYNMC, Mach number, VMACH,
C*** * vehicle air speed, DVBA, corrected for wind VAEG.
C*** *
C*** * The switch MAIR controls the atmosphere, wind and gust options:
C*** *
C*** *   MAIR=|MTURB|MWIND|MATMO|
C*** *
C*** *         MTURB =0 No turbulence
C*** *			   =1 Dryden turbulence model
C*** *               MWIND =0 No Wind
C*** *                     =1 Constant Wind, Input: DVAEG,PSIWGX
C*** *    	   	         =2 Shear Wind, Input: WALTL,DVAEGL;WALTH,DVAEGH;PSIWGX
C*** * 	           	     =3 Tabular Wind, Input: Weather deck from INPUT.ASC
C*** *					 MATMO =0 Std. Atmosphere, US 76  atmosphere internal	
C*** *   	                       =3 Tabular Atmosph, Input: Weather deck from IN
C*** *
C*** *
C*** * *Constant horizontal wind is input by DVAEG and wind direction PSIWGX
C*** * *Shear wind is provided through low altitude input WALTL, DVAEGL
C*** *  and high altitude input WALTH, DVAEGH with fixed wind angle PSIWGX.
C*** * *Vertical component is provided by DVAE3 (down pos) for all wind options
C*** * *Tabular atmosphere and horizontal wind is read in from INPUT.ASC Weather
C*** *  and stored in COMMOM /WINDS/
C*** *  (OPTMET=1 required, SI units)
C*** * *Weatherdeck format for INPUT.ASC
C*** *  WEATHER
C*** *   WALT WDIR WVEL RHX CTMP WPRES
C*** *    >>> numerical entries up to 50 records <<<
C*** *  END
C*** * *Labeled COMMOM /WINDS/
C*** *  WALT= Altitude - m
C*** *  WDIR= Wind Direction (from North) - deg
C*** *  WVEL= Wind Velocity - m/s
C*** *  RHX= Air density - kg/m^3
C*** *  CTMP= Temprature - deg C
C*** *  WPRES= Atmospheric pressure - Pa
C*** *  KOUNTW= Number of altitude records
C*** *  RHW= Last altitude record
C*** * *Effect of wind is smoothed by a filter with 0.5 sec time constant
C*** *
C*** * (2)Heat equilibrium calculations on nose of vehicle
C*** *
C*** * MODIFICATION HISTORY:
C*** * 990415 Created by Peter H Zipfel
C*** * 000412 GHAME6 version modified for FALCON6, PZi
C*** *
C**********************************************************************
C
      COMMON C(3510)
      COMMON /WINDS/WALT(50),WDIR(50),WVEL(50),RHX(50),
     &              CTMP(50),WPRES(50),KOUNTW,RHW
C
      DIMENSION VAEG(3),VAESGD(3),VAESG(3),DUM3(3),VBAG(3),VBEL(3)
     &,VTAG(3),VTAEG(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0200),MAIR)
      EQUIVALENCE (C(0204),PSIWGX)
      EQUIVALENCE (C(0208),DVAE3)
      EQUIVALENCE (C(0209),DVAEG)
      EQUIVALENCE (C(0214),DVAEGL)
      EQUIVALENCE (C(0215),DVAEGH)
      EQUIVALENCE (C(0216),WALTL)
      EQUIVALENCE (C(0217),WALTH)
      EQUIVALENCE (C(0251),RN)
      EQUIVALENCE (C(0257),FLAGQN)
C
C MAIR = D = |MTURB|MWIND|MATMO|, see Module G2
C PSIWGX = D Wind direction from north - deg
C DVAE3 = D Vertical Wind, pos. down - m/s
C DVAEG = D Constant wind magnitude - m/s
C DVAEGL = D Wind at low altitude - m/s
C DVAEGH = D Wind at high altitude - m/s
C WALTL = D Altitude of DVAELL - m
C WALTH = D Altitude of DVAELH - m
C RN = D Radius of nose (if =0, no heat flux calc) - m
C FLAGQN = D =1:restart heat integration
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(2664),DER)
      EQUIVALENCE (C(2866),ICOOR)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1215),TRQN)
      EQUIVALENCE (C(1216),TRQNDOT)
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1283),TRMACH)
      EQUIVALENCE (C(1633),VBEL(1))
      EQUIVALENCE (C(1639),HBE)
C
C VBEL= O Vehicle velocity in local level axes - m/s
C HBE= O Height above sea level - m
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0201),VAEG(1))
      EQUIVALENCE (C(0205),GRAV)
      EQUIVALENCE (C(0206),VMACH)
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(0212),PPRES)
      EQUIVALENCE (C(0213),TEMPK)
      EQUIVALENCE (C(0219),DVBA)
      EQUIVALENCE (C(0221),VSOUND)
C
C VAEG(3) = O Vel of air wrt earth in geographic axes - m/s
C GRAV = O Gravitational acceleration - m/s^2
C VMACH = O Vehicle Mach number based on DVBA
C PDYNMC = O Dynamic pressure - Pa
C DRHO = O Air density - kg/m3
C PPRES = O Atmospheric pressure - Pa
C TEMPK = O Atmospheric temperature - C
C DVBA = O Speed of vehicle wrt air mass - m/s
C VSOUND = O Sonic speed - m/s
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0239),VAESGD(1))
      EQUIVALENCE (C(0242),VAESG(1))
C
C VAESG(3) = S Airmass velocity wrt earth in geographic coor - m/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0218),DVW)
      EQUIVALENCE (C(0252),QNDOT)
      EQUIVALENCE (C(0253),QN)
      EQUIVALENCE (C(0254),QNDMAX)
      EQUIVALENCE (C(0255),QNDTIM)
      EQUIVALENCE (C(0265),VTAG(1))
C
C DVW = G Wind magnitude interpoated - m/s
C QNDOT = G Equilibrium heat rate at nose - (MJ/m2)/s
C QN = G Equilibrium heat at nose - MJ/m2
C QNDMAX = G Max equilibrium heat rate at nose - (MJ/m2)/s
C QNDTIM = G Time of max equil heat rate - s
C VTAG(3) = G Tubulence in geographic coorcinates - m/s
C
      PARAMETER (R=287.053)
      PARAMETER (G=6.673E-11)
      PARAMETER (EMASS=5.973E24)
C
C R =Gas constant - m^2/(K*sec^2
C G =Gravitaional constant - N*m^2/kg^2
C EMASS =Mass of earth - kg
C
      MTURB=INT(MAIR/100.)
      MWIND=INT((MAIR-MTURB*100)/10.)
      MATMO=MAIR-MTURB*100-MWIND*10
C
C
C*** ALTITUDE ABOVE EARTH
C
      RAD=REARTH+HBE
C
C*** CALCULATE THE GRAVITY ACCELERATION
C
      GRAV=G*EMASS/RAD**2
C
C*** SELECTION OF ATMOSPHERIC OPTIONS
C
      IF(MATMO.EQ.0) THEN
C
C*** CALL US 76 STANDARD ATMOSPHERE FROM UTL2.FOR
C
         BALT = HBE
         CALL US76(RHO,PRESS,TEMPK,BALT)
         TEMP=TEMPK-273.16
         DRHO=RHO
         PPRES=PRESS
         VSOUND=SQRT(1.4*R*TEMPK)
      ENDIF
C
      IF(MATMO.EQ.3) THEN
C
C*** ATMOSPHERE FROM TYPE 8 INPUTS
C
         CALL TABLE(HBE,WALT,RHX,KOUNTW,DRHO)
         CALL TABLE(HBE,WALT,CTMP,KOUNTW,CTEMP)
         CALL TABLE(HBE,WALT,WPRES,KOUNTW,PPRES)
	   TEMPK=CTEMP+273.16
         VSOUND=SQRT(1.4*R*TEMPK)
      ENDIF
C
C*** WIND OPTIONS
C
      IF(MWIND.GT.0) THEN
C
         IF(MWIND.EQ.1) THEN
C
C***     CONSTANT WIND
C
            DVW=DVAEG
         ENDIF
C
         IF(MWIND.EQ.2) THEN
C
C***        WIND WITH CONSTANT SHEAR
C
            DVW=DVAEGL+(DVAEGH-DVAEGL)*(HBE-WALTL)/(WALTH-WALTL)
            IF(HBE.LT.WALTL) DVW=0.
            IF(HBE.GT.WALTH) DVW=0.
         ENDIF
C
         IF(MWIND.EQ.3) THEN
C
C***     WIND FROM TYPE 8 INPUTS
C
            CALL TABLE(HBE,WALT,WDIR,KOUNTW,PSIWGX)
            CALL TABLE(HBE,WALT,WVEL,KOUNTW,DVW)
         ENDIF
C
C***     WIND COMPONENTS
C
         VAEG(1)=-DVW*COS(PSIWGX/CRAD)
         VAEG(2)=-DVW*SIN(PSIWGX/CRAD)
         VAEG(3)=DVAE3
C
C***     SMOOTHING WIND BY FILTER WITH TIME CONSTANT .5 SEC
C
	   CALL MATSUB(DUM3,VAEG,VAESG,3,1)
	   CALL MATCON(VAESGD,2.,DUM3,3,1)
	   CALL MATEQL(VTAEG,VAESG,3,1) !? WHY IS VTAEG =0
       ENDIF
C
C*** WIND TURBULENCE IN NORMAL LOAD PLANE
C
	IF(MTURB.EQ.1)THEN
	   CALL G2TURB(VTAG,DVBA)
	   CALL MATADD(VTAEG,VTAG,VAESG,3,1)
	ENDIF
C
C*** FLIGHT CONDITIONS
C
      CALL MATSUB(VBAG,VBEL,VTAEG,3,1)
      CALL MATABS(DVBA,VBAG,3)
      PDYNMC=0.5*DRHO*DVBA*DVBA
C
      IF(VSOUND.LE.1.E-10) THEN
        VMACH=0.
      ELSE
        VMACH=DVBA/VSOUND
        IF(VMACH.LT.TRMACH)TRCOND=2.
      END IF
C
C*** HEAT EQUILIBRIUM BETWEEN INPUT CONVECTING AND OUTPUT RADIATIVE
C    HEATING. HEAT RATE QDOT [(MJ/M2)/S] CALCULATED FROM NOSE RADIUS
C    RN [M], DENSITY DRHO [KG/M3], AND VELOCITY DVBA [M/S]. TO RE-
C    INITIALIZE THE HEAT INTEGRATION (FOR A NEW STAGE) SET FLAGQN=1.
C
      IF(RN.GT.0) THEN
         QNDOT=1.584E-10*(DRHO/RN)**.5*DVBA**3.15
         IF(QNDOT.GT.QNDMAX) THEN
            QNDMAX=QNDOT
            QNDTIM=T
         ENDIF
         IF(FLAGQN.GT..5)QN=0.
         FLAGQN=0.
         IF(ICOOR.EQ.1)QN=QN+QNDOT*DER
         IF(QN.GT.TRQN)TRCOND=3
         IF(QNDOT.GT.TRQNDOT)TRCOND=4
      ENDIF
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G2TURB(VTAG,DVBA)
C**********************************************************************
C*** * Dryden turbulence model
C*** * Ref: Etkin, Dynamics of Flight,Wiley 1958, p.318
C*** * Argument Output:
C*** *          VTAG(3)=Velocity of turbulence wrt steady air mass - m/s
C*** * Argument Input:
C*** *          DVBA=Vehicle speed wrt air mass - m/s
C*** *
C*** * MODIFICATION HISTORY
C*** * 000120 Created by Peter Zipfel
C*** *
C**********************************************************************
C
      COMMON C(3510)
C
      DIMENSION VTAA(3),TBL(3,3),TAB(3,3),TGA(3,3),TAG(3,3),VTAG(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0258),TURBL)
      EQUIVALENCE (C(0259),TURBSIG)
C
C TURBL = D Turbulence correlation length - m
C TURBSIG = D Turbulence magnitude (1sigma) - m/s
C
C*** INPUT FORM EXECUTIVE
C
      EQUIVALENCE (C(2664),DER)
C
C*** INPUT FORM OTHER MODULES
C
      EQUIVALENCE (C(0342),ALPP)
      EQUIVALENCE (C(0343),PHIP)
      EQUIVALENCE (C(0309),TBL(1,1))
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0260),TAUX1D)
      EQUIVALENCE (C(0261),TAUX1)
      EQUIVALENCE (C(0262),TAUX2D)
      EQUIVALENCE (C(0263),TAUX2)
C
C TAUX1 = S First turbulence state variable - ND
C TAUX2 = S Second turbulence state variable - ND
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0264),TAU)
      EQUIVALENCE (C(0268),GAUSS)
C
C TAU = G Turbulence component in load factor plane - m/s
C GAUSS = G White Gaussian time series - ND
C
      DATA ISEED1/19283/,ISEED2/24579/
      PARAMETER(PI=3.1416)
C
C*** White Gaussian noise with zero mean
C
      V1=RAN(ISEED1)
      V2=RAN(ISEED2)
      GAUSS=(1./SQRT(DER))*SQRT(2.*ALOG(1./V1))*COS(2.*PI*V2)
C
C*** Filter converting white gaussian noise into time sequence of Dryden
C    turbulence TAU (One-dimensional cross-velocity Dryden spectrum)
C
      VL=DVBA/TURBL
      VL2=VL**2
      DUM1=SQRT(1./(PI*VL))
      DUM2=(1./VL)*SQRT(3./(PI*VL))
      TAUX1D=TAUX2
      TAUX2D=-VL2*TAUX1-2*VL*TAUX2+VL2*GAUSS
      TAU=TURBSIG*(DUM1*TAUX1+DUM2*TAUX2)
C
C*** TAU inserted into the load factor plane (aeroballistic 1A-3A plane)
C
      CALL VECVEC(VTAA,0.,0.,TAU)
C
C*** TM of aerballistic wrt body coordinates
C
      COSA=COS(ALPP)
      SINA=SIN(ALPP)
      COSP=COS(PHIP)
      SINP=SIN(PHIP)
      TAB(1,1)=COSA
      TAB(1,2)=SINA*SINP
      TAB(1,3)=SINA*COSP
      TAB(2,1)=0.
      TAB(2,2)=COSP
      TAB(2,3)=-SINP
      TAB(3,1)=-SINA
      TAB(3,2)=COSA*SINP
      TAB(3,3)=COSA*COSP
C
C*** Converting turbulence to geographic coordinates
C
      CALL MATMUL(TAG,TAB,TBL,3,3,3)
	CALL MATTRA(TGA,TAG,3,3)
      CALL MATMUL(VTAG,TGA,VTAA,3,3,1)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G3I
C**********************************************************************
C*** * Kinematics Initialization  Module
C*** * Reserved C(3510) locations are 300-399
C*** *
C*** * * Defines the locations of the 3 state variables used in G3
C*** * * Initializes direction cosine matrix TBL(3,3)
C*** *
C*** * MODIFICATION HISTORY
C*** * 921207 Created by Leonard Frantz
C*** *
C**********************************************************************
C
      DIMENSION IPL(100),IPLV(100),TBL(3,3)
C
      COMMON C(3510)
C
C***  INITIAL INPUT
C
      EQUIVALENCE (C(0337), PSIBLX)
      EQUIVALENCE (C(0338), THTBLX)
      EQUIVALENCE (C(0339), PHIBLX)
C
C PSIBLX = I/G Euler angle yaw - deg
C THTBLX = I/G Euler angle pitch - deg
C PHIBLX = I/G Euler angle roll - deg
C
C***  INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052), CRAD)
      EQUIVALENCE (C(2562), IPL(1))
      EQUIVALENCE (C(2867), IPLV(1))
      EQUIVALENCE (C(2561), NIP)
C
C***  INITIALIZATION
C
      EQUIVALENCE (C(0306), PSI)
      EQUIVALENCE (C(0307), THETA)
      EQUIVALENCE (C(0308), PHI)
      EQUIVALENCE (C(0309), TBL(1,1))
C
C***  INITIALIZATION CODE
C
      LOCATION = 303
      DO II = NIP,NIP+2
        IPL(II) = LOCATION
        IPLV(II) = LOCATION + 3
        LOCATION = LOCATION + 1
      ENDDO
      NIP=NIP+3
C
C*** INITIALIZATION OF EULER ANGLES AND DIRECTION COSINE MATRIX
C
	PSI=PSIBLX/CRAD
	THETA=THTBLX/CRAD
	PHI=PHIBLX/CRAD
	CALL MAT3TR(TBL,PSI,THETA,PHI)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G3
C**********************************************************************
C*** * Kinematics  Module
C*** * Reserved C(3510) locations are 300-399
C*** *
C*** * This module performs the following functions:
C*** * 1) Integrates the body angle rates to get body angles
C*** * 2) Calculates the TBL matrix
C*** * 3) Calculates the angles of attack and sideslip
C*** *
C*** * MODIFICATION HISTORY
C*** * 921207 Created by Leonard Frantz
C*** *
C**********************************************************************
C
      COMMON C(3510)
C
      DIMENSION WBEB(3),VBEB(3),TBL(3,3),TLB(3,3)
     &,VBEL(3),VAEG(3),VBAL(3),VBAB(3)
C
C***  INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C***  INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0201),VAEG(1))
      EQUIVALENCE (C(1613),VBEB(1))
      EQUIVALENCE (C(1633),VBEL(1))
      EQUIVALENCE (C(1636),DVBE)
      EQUIVALENCE (C(1713),WBEB(1))
C
C VAEG= O Vel of air wrt earth in geographic axes - m/s
C VBEB= S Vehicle velocity in body axes - m/s
C VBEL= O Vehicle velocity in local level axes - m/s
C DVBE= I/O Vehicle speed - m/s
C WBEB= S Body rates - rad/sec
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0303),PSID)
      EQUIVALENCE (C(0304),THETAD)
      EQUIVALENCE (C(0305),PHID)
      EQUIVALENCE (C(0306),PSI)
      EQUIVALENCE (C(0307),THETA)
      EQUIVALENCE (C(0308),PHI)
C
C PSI = S Euler yaw angle - rad
C THETA = S Euler pitch angle - rad
C PHI = S Euler roll angle - rad
C
C***  OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0301),ALPHA)
      EQUIVALENCE (C(0302),BETA)
      EQUIVALENCE (C(0309),TBL(1,1))
      EQUIVALENCE (C(0340),ALPPX)
      EQUIVALENCE (C(0341),PHIPX)
      EQUIVALENCE (C(0342),ALPP)
      EQUIVALENCE (C(0343),PHIP)
      EQUIVALENCE (C(0344),ALPHAX)
      EQUIVALENCE (C(0345),BETAX)
      EQUIVALENCE (C(0348),TLB(1,1))
C
C ALPHA = O Angle of attack - rad
C BETA = O Sideslip angle - rad
C TBL(3,3) = O TM of body wrt local level coord - ND
C ALPPX = O Total angle of attack - deg
C PHIPX = O Aerodynamic roll angle - deg
C ALPP = O Total angle of attack - rad
C PHIP = O Aerodynamic roll angle - rad
C ALPHAX = O Angle of attack - deg
C BETAX = O Sideslip angle - deg
C TLB(3,3) = O TM of local level to body coord. - ND
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0337),PSIBLX)
      EQUIVALENCE (C(0338),THTBLX)
      EQUIVALENCE (C(0339),PHIBLX)
C
C PSIBLX = I/G Euler angle yaw - deg
C THTBLX = I/G Euler angle pitch - deg
C PHIBLX = I/G Euler angle roll - deg
C
C*** SOLVE EULER DIFFERENTIAL EQUATIONS
C
      P = WBEB(1)
      Q = WBEB(2)
      R = WBEB(3)
      U = VBEB(1)
      V = VBEB(2)
      W = VBEB(3)
 
      PHID = P + ( SIN(THETA)/COS(THETA ))*( Q*SIN(PHI) + R*COS(PHI) )
      THETAD = Q*COS(PHI) - R*SIN(PHI)
      PSID = ( Q*SIN(PHI) + R*COS(PHI) ) / COS(THETA)
C
C*** CALCULATE THE TBL MATRIX
C
      TBL(1,1) = COS(THETA)*COS(PSI)
      TBL(1,2) = COS(THETA)*SIN(PSI)
      TBL(1,3) = -SIN(THETA)
      TBL(2,1) = SIN(PHI)*SIN(THETA)*COS(PSI) - COS(PHI)*SIN(PSI)
      TBL(2,2) = SIN(PHI)*SIN(THETA)*SIN(PSI) + COS(PHI)*COS(PSI)
      TBL(2,3) = SIN(PHI)*COS(THETA)
      TBL(3,1) = COS(PHI)*SIN(THETA)*COS(PSI) + SIN(PHI)*SIN(PSI)
      TBL(3,2) = COS(PHI)*SIN(THETA)*SIN(PSI) - SIN(PHI)*COS(PSI)
      TBL(3,3) = COS(PHI)*COS(THETA)
	CALL MATTRA(TLB,TBL,3,3)
C
C*** INCIDENCE ANGLES
C
	CALL MATSUB(VBAL,VBEL,VAEG,3,1) !NEW
	CALL MATMUL(VBAB,TBL,VBAL,3,3,1) !NEW
      ALPHA=ATAN2(VBAB(3),VBAB(1)) !Replaced VBEB by VBAB
      CALL MATABS(DVBA,VBAB,3)
      BETA=ASIN(VBAB(2)/DVBA)
      DUM=VBAB(1)/DVBA
      IF(ABS(DUM).GE.1.) DUM=SIGN(1.,DUM)
      ALPP=ACOS(DUM)
      IF(VBAB(2).EQ.0..AND.VBAB(3).EQ.0.)THEN
         PHIP=0.
      ELSE
         PHIP=ATAN2(VBAB(2),VBAB(3))
      ENDIF
C
      ALPHAX=ALPHA*CRAD
      BETAX=BETA*CRAD
      ALPPX=ALPP*CRAD
      PHIPX=PHIP*CRAD
C
	PSIBLX=PSI*CRAD
	THTBLX=THETA*CRAD
	PHIBLX=PHI*CRAD
C
      RETURN
      END
C*********************** DUMMY RETURNS ****************************************
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
	SUBROUTINE C1
	RETURN
	END
	SUBROUTINE C2I
	RETURN
	END
	SUBROUTINE C2
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
	SUBROUTINE G1I
	RETURN
	END
	SUBROUTINE G1
	RETURN
	END
	SUBROUTINE G4I
	RETURN
	END
	SUBROUTINE G4
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
	SUBROUTINE S1
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
