C**********************************************************************
C******************** CRUISE5: MODULES AND DUMMY **********************
C**********************************************************************
C*** *
C*** * Calling sequence of Modules:
C*** *   G1   TARGET
C*** *   G2   AIR DATA
C*** *   S2   GPS
C*** *   S1   SENSOR
C*** *   S4   INS
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
C*** * (1) Provides the trimmed aerodynamic tables of a generic cruise
C*** *     missile CRUISE, Ref AREA = 0.929 m^2
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      COMMON/AEROA/CLTBL,CDTBL
C
      DIMENSION CLTBL(288),CDTBL(288)
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(1202),CY)
      EQUIVALENCE (C(1211),TRCODE)
      EQUIVALENCE (C(1214),TRMACH)
      EQUIVALENCE (C(1215),TRDYNM)
      EQUIVALENCE (C(1216),TRFORY)
      EQUIVALENCE (C(1217),TRFORP)
      EQUIVALENCE (C(1218),TRFOVY)
      EQUIVALENCE (C(1219),TRFOVP)
      EQUIVALENCE (C(1221),TRLOAD)
      EQUIVALENCE (C(1222),TRALP)
      EQUIVALENCE (C(1772),TRCOND)
      EQUIVALENCE (C(1220),TMCODE)
      EQUIVALENCE (C(1403),AREA)
C
C CY = I Side force coefficient - ND
C TRCODE = I/G Interger defining type of termination - ND
C TRMACH = I MINIMUM MACH NUMBER (Module G2) - ND
C TRDYNM = I MINIMUM DYNAMIC PRESSURE (Module G2) - Pa
C TRFOVY = I MAXIMUM YAW FIELD OF VIEW (Module S1) - rad
C TRFOVP = I MAXIMUM PITCH FIELD OF VIEW (Module S1) - rad
C TRFORY = I MAXIMUM YAW FIELD OF REGARD (Module S1) - rad
C TRFORP = I MAXIMUM PITCH FIELD OF REGARD (Module S1) - rad
C TRLOAD = I MINIMUM LOAD FACTOR (Module A1) - g's
C TRALP = I MAXIMUM TOTAL INCIDENCE ANGLE (Module A1) - rad
C TRCOND = I/G Terminatin code accumulator - ND
C TMCODE = I/G Stores previos termination code - ND
C
C*** TRIMMED LIFT COEFFICIENT TABLE
C
      DATA (CLTBL(N),N=1,96)/
C
C*** CG AT STATION 130.0
C
     +-.440,-.322,-.214,-.097, .010, .136, .262, .384,
     + .505, .618, .730, .813, .895, .983,1.070,1.098,
C
     +-.510,-.392,-.274,0.147, .000, .131, .262, .384,
     + .505, .618, .710, .803, .895, .958,1.020,1.043,
C
     +-.560,-.442,-.324,-.187,-.020, .096, .212, .314,
     + .415, .523, .630, .728, .825, .898, .970,1.013,
C
     +-.650,-.522,-.384,-.237,-.090, .036, .162, .264,
     + .365, .473, .580, .703, .825, .888, .950,1.003,
C
     +-.690,-.592,-.484,-.337,-.150,-.004, .142, .254,
     + .365, .488, .610, .708, .805, .893, .980,1.013,
C
     +-.700,-.592,-.484,-.357,-.220,-.144,-.068, .024,
     + .115, .233, .350, .463, .575, .673, .770, .883/
C
      DATA (CLTBL(N),N=97,192)/
C
C*** CG AT STATION 134.0
C
     +-.450,-.330,-.220,-.100, .010, .140, .270, .395,
     + .520, .635, .750, .835, .920,1.010,1.110,1.130,
C
     +-.520,-.400,-.280,-.150, .000, .135, .270, .395,
     + .520, .625, .730, .825, .920, .985,1.050,1.075,
C
     +-.570,-.450,-.330,-.190,-.020, .100, .220, .325,
     + .430, .540, .650, .750, .850, .925,1.000,1.095,
C
     +-.660,-.530,-.390,-.240,-.090, .040, .170, .275,
     + .380, .490, .600, .725, .850, .915, .980,1.035,
C
     +-.700,-.600,-.490,-.340,-.150, .000, .150, .265,
     + .380, .505, .630, .730, .830, .920,1.010,1.045,
C
     +-.710,-.600,-.490,-.360,-.220,-.140,-.060, .035,
     + .130, .250, .370, .485, .600, .700, .800, .915/
C
      DATA (CLTBL(N),N=193,288)/
C
C*** CG AT STATION 138.0
C
     +-.466,-.343,-.230,-.106, .010, .145, .280, .410,
     + .539, .658, .776, .865, .953,1.046,1.139,1.172,
C
     +-.536,-.413,-.290,-.156, .000, .140, .280, .410,
     + .539, .648, .756, .855, .953,1.021,1.089,1.117,
C
     +-.586,-.463,-.340,-.196,-.020, .105, .230, .340,
     + .449, .563, .676, .780, .883, .961,1.039,1.087,
C
     +-.676,-.543,-.400,-.246,-.090, .045, .180, .290,
     + .399, .513, .626, .755, .883, .951,1.019,1.077,
C
     +-.716,-.613,-.500,-.346,-.150, .005, .160, .280,
     + .399, .528, .656, .760, .863, .956,1.049,1.087,
C
     +-.716,-.613,-.500,-.366,-.220,-.135,-.050, .050,
     + .149, .273, .396, .515, .633, .736, .839, .957/
C
C*** TRIMMED DRAG COEFFICIENT TABLE
C
      DATA (CDTBL(N),N=1,96)/
C
C*** CG AT STATION 130.0
C
     +.0728,.0656,.0611,.0574,.0545,.0520,.0513,.0541,
     +.0588,.0639,.0693,.0739,.0804,.0869,.0957,.1002,
C
     +.0885,.0684,.0614,.0567,.0530,.0513,.0512,.0543,
     +.0587,.0632,.0689,.0749,.0822,.0917,.1029,.1179,
C
     +.1278,.0921,.0708,.0597,.0531,.0513,.0507,.0522,
     +.0552,.0593,.0648,.0717,.0819,.0954,.1136,.1276,
C
     +.1670,.1324,.0754,.0622,.0548,.0519,.0503,.0511,
     +.0534,.0580,.0637,.0722,.0846,.0973,.1194,.1395,
C
     +.1766,.1416,.1140,.0862,.0663,.0597,.0548,.0560,
     +.0604,.0675,.0785,.0921,.1105,.1299,.1522,.1746,
C
     +.2215,.1826,.1552,.1380,.1112,.1094,.1094,.1103,
     +.1121,.1183,.1309,.1452,.1629,.1862,.2121,.2382/
C
      DATA (CDTBL(N),N=97,192)/
C
C*** CG AT STATION 134.0
C
     +.0785,.0703,.0647,.0596,.0558,.0515,.0493,.0512,
     +.0550,.0589,.0637,.0677,.0740,.0792,.0900,.0970,
C
     +.0950,.0738,.0656,.0595,.0540,.0508,.0493,.0513,
     +.0548,.0585,.0634,.0690,.0765,.0870,.1090,.1200,
C
     +.1350,.0980,.0755,.0630,.0538,.0510,.0492,.0498,
     +.0520,.0551,.0600,.0662,.0768,.0925,.1120,.1400,
C
     +.1750,.1390,.0807,.0660,.0573,.0522,.0492,.0490,
     +.0507,.0544,.0592,.0672,.0790,.0977,.1220,.1450,
C
     +.1850,.1490,.1203,.0910,.0690,.0596,.0538,.0540,
     +.0578,.0645,.0746,.0887,.1080,.1275,.1400,.1600,
C
     +.2300,.1900,.1615,.1430,.1158,.1125,.1112,.1112,
     +.1120,.1181,.1300,.1434,.1618,.1850,.2100,.2500/
C
      DATA (CDTBL(N),N=193,288)/
C
C*** CG AT STATION 138.0
C
     +.0809,.0713,.0647,.0590,.0548,.0503,.0471,.0482,
     +.0509,.0542,.0579,.0617,.0670,.0724,.0890,.0970,
C
     +.0980,.0756,.0662,.0591,.0533,.0496,.0474,.0482,
     +.0508,.0538,.0580,.0633,.0732,.0896,.1180,.1296,
C
     +.1386,.1004,.0766,.0629,.0538,.0501,.0476,.0474,
     +.0488,.0513,.0556,.0622,.0760,.0924,.1155,.1280,
C
     +.1794,.1421,.0824,.0662,.0563,.0514,.0479,.0470,
     +.0480,.0511,.0555,.0633,.0795,.1023,.1257,.1471,
C
     +.1898,.1528,.1231,.0922,.0688,.0583,.0526,.0523,
     +.0555,.0618,.0728,.0874,.1072,.1293,.1483,.1649,
C
     +.2350,.1938,.1643,.1444,.1148,.1113,.1105,.1105,
     +.1115,.1184,.1300,.1436,.1655,.2035,.2274,.2563/
C
C
C*** INITIALIZE CY
C-
      CY=0.0
	AREA=0.92900
C
C*** RUN TERMINATION CRITERIA
C
C    IF ANY OF THE TERMINATION LIMITS IS VIOLATED A NUMBER CODE IS
C    STORED IN TRCOND(1772). IF MSTOP(1750)=1 THE SIMULATION WILL STOP.
C    IF MSTOP=0 (DEFAULT) THE SIMULATION WILL CONTINUE, AND ADDITIONAL
C    CODE NUMBERS, IF ANY, WILL ENTER ON THE LEFT OF TRCOND UNTIL THE
C    RUN IS STOPPED BY OTHER MEANS.
C
C       CODE  TERM.COND. MODULE DESCRIPTION
C         2     TRMACH     G2    MINIMUM MACH NUMBER
C         3     TRDYNM     G2    MINIMUM DYNAMIC PRESSURE
C         4     TRFOVY     S1    MAXIMUM YAW FIELD OF VIEW
C         5     TRFOVP     S1    MAXIMUM PITCH FIELD OF VIEW
C         6     TRFORY     S1    MAXIMUM YAW FIELD OF REGARD
C         7     TRFORP     S1    MAXIMUM PITCH FIELD OF REGARD
C
      TRCODE=0.
      TMCODE=0.
      TRCOND=0.
C
      TRMACH=.4
      TRDYNM=1000.
      TRFOVY=.07
      TRFOVP=.052
      TRFORY=.35
      TRFORP=.26
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A1
C**********************************************************************
C*** * Aerodynamic Module A1 of generic cruise missile CRUISE
C*** * Reserved C(3510) locations are 1200-1299
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates lift and drag coefficients and normal and axial
C*** *     force coefficients
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      COMMON/AEROA/CLTBL,CDTBL
      DIMENSION CLTBL(16,6,3),CDTBL(16,6,3),COAMCG(25),NXYZ(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1207),FRACCD)
      EQUIVALENCE (C(1208),FRACCL)
C
C FRACCD = D Fractional increase of drag coeff (1+FRACCD)*CD - ND
C FRACCL = D Fractional increase of lift coeff (1+FRACCD)*CL - ND
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0053),OPTMET)
      EQUIVALENCE (C(0221),VMACH)
      EQUIVALENCE (C(0908),ALPHA)
      EQUIVALENCE (C(1316),CG)
      EQUIVALENCE (C(1615),HBE)
C
C VMACH= O Mach number
C ALPHA= O Angle-of-attack - rad
C CG= C.G. location from nose - m
C HBE= O altitude of vehicle above S.L.
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1200),CA)
      EQUIVALENCE (C(1201),CN)
      EQUIVALENCE (C(1204),CD)
C
C CA = O Axial force coefficient - ND
C CN = O Normal force coefficient - ND
C CD = 0 Drag coefficient - ND
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1205),CL)
C
C CL = G Lift coefficient - ND
C
C*** INITIALIZE ROWS AND COLUMNS OF TABLES
C
      DATA COAMCG/-6.0,-5.0,-4.0,-3.0,-2.0,-1.0,0.0,1.0,
     +2.0,3.0,4.0,5.0,6.0,7.0,8.0,9.0,
     +.40,.55,.65,.77,.85,.95,130.0,134.0,138.0/
C
C*** CONVERT TO DEGREES, CHANGE TO BODY AXIS
C
      ALPHAD=ALPHA*CRAD
C
C*** PREPARE FOR TABLE LOOKUP
C
      NXYZ(1)=16
      NXYZ(2)=6
      NXYZ(3)=3
C
C*** CALL TABL3 TO FIND CL AT ALPHAD, VMACH, AND CG
C
      VMACHI=VMACH
      ALPHAI=ALPHAD
      CGI=CG
      IF(OPTMET.GE.1) CGI=CG*39.37
C
      CALL TABL3(ALPHAI,VMACHI,CGI,COAMCG,CLTBL,NXYZ,CLL)
C
C*** CALL TABL3 TO FIND CD AT ALPHAD,VMACH, AND CG
C
      CALL TABL3(ALPHAI,VMACHI,CGI,COAMCG,CDTBL,NXYZ,CDD)
C
      CL=CLL*(1.+FRACCL)
      CD=CDD*(1.+FRACCD)
C
      CALPH=COS(ALPHA)
      SALPH=SIN(ALPHA)
      CN=CL*CALPH+CD*SALPH
      CA=CD*CALPH-CL*SALPH
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A2I
C**********************************************************************
C*** * The initialization module for the Turbojet Propulsion Module A2
C*** * Reserved C(3510) locations are 1300-1399
C*** * High efficiency turbojet
C*** * Gross mass of vehicle (full fuel) 1019 kg
C*** * Max fuel mass 195 kg
C*** * This module performs the following functions:
C*** *
C*** * (1) Provides the thrust available and fuel flow tables
C*** * (2) Provides the idle thrust and fuel flow tables
C*** * (3) Provides the c.g. location as a function of missile weight
C*** * NOTE: engine data are given in English units but output is in SI
C*** *	   (you must set OPTMET =1)
C*** *
C*** * MPROP= 0 No thrusting
C*** *		1 Commanded thrust (FCOM input - N)
C*** *		2 Idle thrusting (input)
C*** *		3 Max thrusting (input)
C*** *		4 Required thrust determined by Mach hold loop (input)
C*** *		5 Required thrust < idle thrust (diagnostic)
C*** *		6 Required thrust > max thrust (diagnostic)
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100)
      DIMENSION TTAV(12),TTFF(60),THIDLE(20),FFIDL(5),TCG(5)
C
      COMMON/THRST/TTAV,TTFF,THIDLE,FFIDL,TCG
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1305),AMASSI)
C
C*** EXECUTIVE
C
      EQUIVALENCE (C(2561),N)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INITIALIZATIONS
C
      EQUIVALENCE (C(1303),AMASSE)
      EQUIVALENCE (C(1306),AMASS)
C
C AMASSE = I/D Mass of fuel used - kg
C AMASSI = I Initial missile gross mass - kg
C
C*** THRUST AVAILABLE AT SL, 5K, AND 10K FT VS MACH (LBF)
C
      DATA(TTAV(N),N=1,12)/
     + 487.4, 469.6, 459.5, 446.9,
     + 436.8, 425.2, 418.9, 415.2,
     + 384.8, 377.1, 376.8, 378.5/
C
C*** FUEL FLOW (LBM/HR)
C
      DATA(TTFF(N),N=1,60)/
C
C*** SEA LEVEL FUEL FLOW (ROWS-THRUST, COLUMNS-MACH)
C
     +  40.0,  57.0,  74.0,  90.0,
     + 118.0, 135.0, 153.0, 172.0,
     + 192.0, 214.0, 236.0, 256.0,
     + 292.5, 318.5, 344.5, 373.8,
     + 436.1, 470.4, 499.8, 539.0,
C
C*** FUEL FLOW AT 5000 FT
C
     +  36.0,  48.0,  60.0,  72.0,
     + 109.0, 123.0, 138.0, 154.0,
     + 182.0, 200.0, 218.0, 238.0,
     + 279.5, 305.5, 331.5, 357.5,
     + 441.0, 470.4, 490.0, 524.3,
C
C*** FUEL FLOW AT 10000 FT
C
     +  32.0,  39.0,  46.0,  53.0,
     + 102.0, 113.0, 125.0, 138.0,
     + 172.0, 190.0, 206.0, 223.0,
     + 276.3, 299.0, 318.5, 341.3,
     + 450.8, 480.2, 504.7, 524.3/
C
C*** CENTER OF GRAVITY TABLE
C
      DATA(TCG(N),N=1,5)/ 134.87,137.62,130.18,136.15,132.76/
C
C*** IDLE POWER THRUST (ROWS-ALTITUDE,COLUMNS-MACH)
C
      DATA (THIDLE(N),N=1,20)/
     + 86.8, 68.6, 51.5, 40.0,
     + 68.5, 57.7, 48.2, 40.1,
     + 67.1, 56.1, 45.8, 35.4,
     + 82.4, 72.9, 64.4, 57.2,
     + 86.9, 80.5, 74.8, 68.8/
C
C*** IDLE FUEL FLOW (FUNCTION OF ALTITUDE)
C
      DATA (FFIDL(N),N=1,5)/
     + 114., 85., 67., 67., 67./
C
C*** CONVERT TTFF AND FFIDL FROM LBM/HR TO LBM/SEC
C
      DO  I=1,60
         TTFF(I)=TTFF(I)/3600.
      ENDDO
      DO I=1,5
         FFIDL(I)=FFIDL(I)/3600.
	ENDDO
C
C*** INITIALIZATION
C
      AMASS=AMASSI
      AMASSE=0.
C
C*** INITIALIZE STATE VARIABLES FOR MACH HOLD
C
      IPL(N)=1352
	IPLV(N)=1353
      N=N+1
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A2
C**********************************************************************
C*** * Turbojet Propulsion Module A2
C*** * Reserved C(3510) locations are 1300-1399
C*** * This module performs the following functions:
C*** *
C*** * (1) Looks up idle and max thrust
C*** * (2) Calculates required thrust (equal drag)
C*** * (3) Calculates fuel flow rate
C*** * (4) Calculates vehicle mass and cg location
C*** *
C*** * MPROP= 0 No thrusting
C*** *		1 Commanded thrust (FCOM input - N)
C*** *		2 Idle thrusting (input)
C*** *		3 Max thrusting (input)
C*** *	>>>	4 Required thrust determined by Mach hold loop (input)
C*** *		5 Required thrust < idle thrust (diagnostic)
C*** *		6 Required thrust > max thrust (diagnostic)
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
      COMMON/THRST/TTAV,TTFF,THIDLE,FFIDL,TCG
C
      DIMENSION TTAV(4,3),TTFF(4,5,3),THIDLE(4,5),FFIDL(5),
     +TCG(5),IMH(2),IMTH(3),IMTI(2),COEFMT(7),COEFTH(12),
     +COEFCG(5),COEFTI(9),ICG(1),COEFTJ(5),IMTJ(1)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1302),FCOM)
      EQUIVALENCE (C(1304),AMASSF)
      EQUIVALENCE (C(1312),VMACHC)
      EQUIVALENCE (C(1313),GFTHM)
      EQUIVALENCE (C(1314),TFTH)
C
C MPROP = D/G Propulsion flag =4:thrust for Mach hold
C FCOM = D Commanded thrust - N
C AMASSF = D Initial fuel mass - kg
C VMACHC = D Commanded Mach number - ND
C GFTHM = D Mach hold loop gain - N
C TFTH = D Engine thrust time constant - s
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(1303),AMASSE)
      EQUIVALENCE (C(1305),AMASSI)
C
C*** INPUT FORM OTHER MODULES
C
      EQUIVALENCE (C(0053),OPTMET)
      EQUIVALENCE (C(0221),VMACH)
      EQUIVALENCE (C(0220),PDYNMC)
      EQUIVALENCE (C(0908),ALPHA)
      EQUIVALENCE (C(1204),CD)
      EQUIVALENCE (C(1403),AREA)
      EQUIVALENCE (C(1615),HBE)
      EQUIVALENCE (C(2664),DER)
      EQUIVALENCE (C(2866),ICOOR)
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1306),AMASS)
      EQUIVALENCE (C(1316),CG)
C
C FTHALT = O Thrust - N
C AMASS = O Mass of vehicle - kg
C CG = C.G. location from nose - m
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1352),TREQD)
      EQUIVALENCE (C(1353),TREQ)
C
C TREQ = S Required thrust = N
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1307),AMASSR)
      EQUIVALENCE (C(1309),TAV)
C
C AMASSR = G Mass of fuel remaining - kg
C TAV = G Available thrust - N
C
C*** INITIALIZE ROWS AND COLUMNS,OF TABLES
C
C*** FOR MAX THRUST TABLE (MACH; ALTITUDE(FT))
C
      DATA COEFMT/0.40,0.55,0.70,0.85,
     +0.0,5000.,10000./
      IMH(1)=4
      IMH(2)=3
C
C*** FOR FUEL FLOW TABLE (MACH; THRUST(LB); ALT.(FT))
C
      DATA COEFTH/0.40,0.55,0.70,0.85,
     +0.0,100.0,200.0,325.0,490.0,
     +0.0,5000.,10000./
      IMTH(1)=4
      IMTH(2)=5
      IMTH(3)=3
C
C*** FOR C.G. TABLE (WEIGHT(LB))
C
      DATA COEFCG/1824.15,1902.10,2460.18,2980.40,3127.29/
      ICG(1)=5
C
C*** FOR IDLE THRUST TABLE (MACH; ALT.(FT))
C
      DATA COEFTI/0.40,0.55,0.70,0.85,
     +0.0,10000.,20000.,30000.,40000./
      IMTI(1)=4
      IMTI(2)=5
C
C*** FOR IDLE FUEL FLOW TABLE (ALT.(FT))
C
      DATA COEFTJ/0.0,10000.,20000.,30000.,40000./
      IMTJ(1)=5
C
      IF(MPROP.EQ.0) THEN
         FTHALT=0.
         FF=0.
C
C*** DETERMINE CG LOCATION FROM PREVIOUS CYCLE FOR TABLE ENTRIES
C
         AMASSJ=AMASS
         IF(OPTMET.GE.1)AMASSJ=AMASS*2.204623
         CALL TABLE(AMASSJ,COEFCG,TCG,ICG,CG)
         IF(OPTMET.GE.1)CG=CG*.0254
         RETURN
      ENDIF
C
C*** LOOK-UP IDLE THRUST
C
      VMACHJ=VMACH
      HBEJ=HBE
      IF(OPTMET.GE.1) HBEJ=HBE*3.2808
      CALL TABL2(VMACHJ,HBEJ,COEFTI,THIDLE,IMTI,FIDLE)
      IF(OPTMET.GE.1) FIDLE=FIDLE*4.44822
C
C*** LOOK-UP MAX THRUST AVAILABLE
C
      CALL TABL2(VMACHJ,HBEJ,COEFMT,TTAV,IMH,TAV)
      IF(OPTMET.GE.1) TAV=TAV*4.44822
C
C*** BRANCH TO THE DESIRED THRUSTING MODE
C
      IF(MPROP.EQ.1) THEN
         FTHALT=FCOM
C
C*** FOR FUEL FLOW FROM SEA LEVEL TO 10000 FT ALTITUDE
C
         FTHALJ=FTHALT
         IF(OPTMET.GE.1)FTHALJ=FTHALT*.2248089
         CALL TABL3(VMACHJ,FTHALJ,HBEJ,COEFTH,TTFF,IMTH,FF)
      ELSEIF(MPROP.EQ.2) THEN
         FTHALT=FIDLE
C
C*** FOR IDLE FUEL FLOW FROM SEA LEVEL TO 40000 FT ALTITUDE
C
         HBEK=HBE
         IF(OPTMET.GE.1)HBEK=HBE*3.2808
         CALL TABLE(HBEK,COEFTJ,FFIDL,IMTJ,FF)
      ELSEIF(MPROP.EQ.3) THEN
         FTHALT=TAV
C
C*** FOR FUEL FLOW FROM SEA LEVEL TO 10000 FT ALTITUDE
C
         FTHALJ=FTHALT
         IF(OPTMET.GE.1)FTHALJ=FTHALT*.2248089
         CALL TABL3(VMACHJ,FTHALJ,HBEJ,COEFTH,TTFF,IMTH,FF)
      ELSEIF(MPROP.GT.3) THEN
C
C***     CALCULATE THRUST REQUIRED IN STABILITY AXIS
C
         MPROP=4
         TREQS=CD*PDYNMC*AREA
C
C***     MACH HOLD LOOP
C
         EPSMCH=VMACHC-VMACH
         TCOM=EPSMCH*GFTHM+TREQS
         TREQD=(TCOM-2.*TREQ)/TFTH
         TREQSS=TREQ
C
C***     THRUST IN BODY AXIS
C
         TREQB=TREQSS/COS(ALPHA)
C
C***     THRUST LIMITING
C
         IF(TREQB.LT.FIDLE) THEN
            MPROP=5
            TREQB=FIDLE
         ENDIF
         IF(TREQB.GT.TAV) THEN
            MPROP=6
            TREQB=TAV
         ENDIF
         FTHALT=TREQB
C
C*** FOR FUEL FLOW FROM SEA LEVEL TO 10000 FT ALTITUDE
C
         FTHALJ=FTHALT
         IF(OPTMET.GE.1)FTHALJ=FTHALT*.2248089
         CALL TABL3(VMACHJ,FTHALJ,HBEJ,COEFTH,TTFF,IMTH,FF)
      ENDIF
C
C*** CONVERT FUEL FLOW FROM LBM/SEC TO KG/SEC
C
      IF(OPTMET.GE.1) FF=FF*.45359
C
C*** CALCULATE MASS
C
      IF(ICOOR.EQ.1) THEN
         AMASSD=FF*DER
         AMASSE=AMASSE+AMASSD
         AMASSR=AMASSF-AMASSE
         AMASS=AMASSI-AMASSE
      ENDIF
C
C*** CALCULATE C.G.
C
      AMASSJ=AMASS
      IF(OPTMET.GE.1) AMASSJ=AMASS*2.204623
      CALL TABLE(AMASSJ,COEFCG,TCG,ICG,CG)
      IF(OPTMET.GE.1) CG=CG*.0254
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A3
C**********************************************************************
C*** * Force Module A3
C*** * Reserved C(3510) locations are 1400-1499
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates specific fore in velosity axes
C*** * (2) Calculates T.M. TBV for skid-to-turn and bank-to-turn vehicles
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION FB(3),FSPB(3),TVB(3,3),TBV(3,3)
     +,FWL(3),FWB(3),FSPV(3),FAB(3),TBL(3,3),VAELD(3)
     +,ABEL(3),TLB(3,3),FSPL(3)
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
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0220),PDYNMC)
      EQUIVALENCE (C(0236),VAELD(1))
      EQUIVALENCE (C(1200),CA)
      EQUIVALENCE (C(1201),CN)
      EQUIVALENCE (C(1202),CY)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1306),AMASS)
      EQUIVALENCE (C(1701),TBL(1,1))
      EQUIVALENCE (C(1710),TLB(1,1))
C
C PDYNMC= O Dynamic pressure - Pa
C VAELD= O Air mass acceleration - m/s2
C CA= O Axial force coefficient - ND
C CN= O Normal force coefficient - ND
C CY= I Side force coefficient - ND
C FTHALT= O Thrust - N
C AMASS= O Vehicle mass - kg
C TBL= O T.M. of body wrt local level coordinates - m
C TLB= O T.M. of local level wrt body coordinates - m
C
C*** OUTPUTS TO OTHER MODULES
C
      EQUIVALENCE (C(1400),FSPB(1))
      EQUIVALENCE (C(1413),TBV(1,1))
      EQUIVALENCE (C(1423),FSPV(1))
      EQUIVALENCE (C(1432),ABEL(1))
C
C FSPB(3) = O Specific force in body coor - N/kg
C TBV(3,3) = O Trans matrix of body wrt flight path axes - ND
C FSPV(3) = O Specific force in flight path axes - N/kg
C ABEL(3) = O Vehicle non-gravity acceleration in local axes - m/s2
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1426),FAB(1))
      EQUIVALENCE (C(1429),FWB(1))
      EQUIVALENCE (C(1437),AL)
      EQUIVALENCE (C(1438),AN)
C
C FAB(3) = G Aero and thrust forces in body axes - N
C FWB(3) = G Wind forces in body axes - N
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
      FAB(1) = FTHALT-CA*PDYNMC*AREA
      FAB(2) =  CY*PDYNMC*AREA
      FAB(3) = -CN*PDYNMC*AREA
C
C*** WIND FORCE IN BODY AXES
C
      CALL MATCON(FWL,AMASS,VAELD,3,1)
      CALL MATMUL(FWB,TBL,FWL,3,3,1)
C
C*** SPECIFIC FORCE IN BODY AXES
C
      CALL MATADD(FB,FAB,FWB,3,1)
      DUM1=1./AMASS
      CALL MATCON(FSPB,DUM1,FB,3,1)
      AL=FSPB(2)/AGRAV
      AN=-FSPB(3)/AGRAV
      CALL MATMUL(FSPL,TLB,FSPB,3,3,1)
      CALL MATEQL(ABEL,FSPL,3,1)
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
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
C**********************************************************************
 
C
C*** TRANSFORMATION MATRIX OF BODY WRT. FLIGHT PATH AXES
C
      COMMON C(3510)
C
      DIMENSION TBV(3,3)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0908),ALPHA)
      EQUIVALENCE (C(0909),BETA)
      EQUIVALENCE (C(0926),PHIBV)
      EQUIVALENCE (C(1700),MTURN)
C
C ALPHA= O Angle-of-attack - rad
C BETA= O Sideslip angle - rad
C PHIBV= O Roll angle about velocity vector - rad
C MTURN= D =0:Skid-to-turn, =1:Bank-to-turn
C
      IF(MTURN.EQ.0) THEN
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
      ELSE
C
C*** BANK-TO-TURN
C
         CALP=COS(ALPHA)
         SALP=SIN(ALPHA)
         CPHI=COS(PHIBV)
         SPHI=SIN(PHIBV)
 
 
         TBV(1,1)=CALP
         TBV(1,2)=SALP*SPHI
         TBV(1,3)=-SALP*CPHI
         TBV(2,1)=0.
         TBV(2,2)=CPHI
         TBV(2,3)=SPHI
         TBV(3,1)=SALP
         TBV(3,2)=-CALP*SPHI
         TBV(3,3)=CALP*CPHI
C
      ENDIF
C
      RETURN
      END
 
C**********************************************************************
      SUBROUTINE C1I
C**********************************************************************
C*** * The initialization module for the Guidance Module C1
C*** * Reserved C(3510) locations are 800-899
C*** * This module performs the following functions:
C*** *
C*** * (1) Initializes terrain cells
C*** *
C*** * MGUID=|MROLL|MGUIDL|MGUIDP|
C*** *
C*** *	   |MROLL|= 0  Normal attitude of vehicle (default)
C*** *			  = 1  Inverted attitude of missile (input)
C*** *
C*** *			  |MGUIDL|= 0 No lateral guidance (default)
C*** *	                  = 3 Lateral line guidance (input)
C*** *
C*** *					|MGUIDP|= 0 No pitch guidance (default)
C*** *							= 1 Look-down terrain following (D1 Module),(input)
C*** *                            = 2 Look-forward terrain following (input)
C*** *							= 3 Pitch line guidance (input)
C*** *
C*** * MODIFICATION HISTORY
C*** * 991001 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
      COMMON/HOI/HO(100)
      COMMON/HI/H(100)
C
C
C*** INITIALIZATIONS
C
      EQUIVALENCE (C(0883),J)
      EQUIVALENCE (C(0884),GNDPT)
      EQUIVALENCE (C(0886),HGES)
      EQUIVALENCE (C(1669),HGE)
      EQUIVALENCE (C(2866),ICOOR)
C
C J = I Integer of terrain stack - ND
C GNDPT = I Incremental ground range - m
C HGES = I/G Height of terrain+obstacle above SL (look-fwd) - m
C HGE = I/G Overall terrain height above S.L. - m
C
C*** INITIALIZE SWITCHES
C
      J=0
      GNDPT=0.
      HO(1)=HGE
      H(1)=HGE
      HGES=HGE
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C1
C**********************************************************************
C*** * Guidance Module C1
C*** * Reserved C(3510) locations are 800-899
C*** *
C*** * MGUID=|MROLL|MGUIDL|MGUIDP|
C*** *
C*** *	   |MROLL|= 0  Normal attitude of vehicle (default)
C*** *			  = 1  Inverted attitude of missile (input)
C*** *
C*** *			  |MGUIDL|= 0 No lateral guidance (default)
C*** *	                  = 3 Lateral line guidance (input)
C*** *
C*** *					|MGUIDP|= 0 No pitch guidance (default)
C*** *							= 1 Look-down terrain following (D1 Module),(input)
C*** *                            = 2 Look-forward terrain following (input)
C*** *							= 3 Pitch line guidance (input)
C
C LOOK-FWD TERRAIN FOLLOWING (MGUIDP=2)
C -------------------------------------
C
C This mode takes the stochastic terrain model, calulated in Module D1, and
C stores the terrain heights in a stack with cell size DCELL. The length of the
C stack is the look-ahead range in the DTED data base RAHEAD. After an
C initialization length of RAHEAD the TF/OA mode commences. The first element in
C the stack is now the current terrain height HGES. As the missile advances by
C DCELL a new terrain height HGE (from Module D1) is pushed into the other side
C of the stack.
C The obstacles are generated by two stochastic functions. A exponential
C distribution determines the waiting distance for the next obstacle to occur.
C OCCDEN defines the density of the events (pro unit distance). The height of th
C obstacles is determined by a Rayleigh distribution. Its mode is SIGOBS (most
C probable height). From this the mean can be obtained: MEAN = 1.25 * SIGOBS.
C ISEED2 is the seed for the two random number generators that produce the
C sequence of obstacles. It must be a large odd integer. The diagnostic variable
C HGES outputs terrain height plus obstacle height obove S.L. at the current veh
C position.
C
C Two look-ahead modes are active: Terrain Following and Obstacle Avoidance. The
C switching is governed by comparing ELMAX with input value SLOPE. ELMAX is the
C maximum value - within the DTED look ahead - of the LOS that clears the talles
C obstacle by DHTRC and the vehicle velocity vector. If ELMAX < SLOPE the Terrai
C Following mode is engaged. The altitude feedback command HBGS is obtained from
C the current altitude of the vehicle HBE and the terrain altitude anticipated
C TLEAD seconds into the future. This advance in effect puts a lead into the
C altitude hold autopilot (MAUT=13). If ELMAX > SLOPE the Obstacle Avoidance
C mode is engaged. An acceleration command ANCOM is calculated based on ELMAX an
C a gain GELEV and sent to the acceleration hold autopilot MAUT=14. The obstacle
C is detected by the sensor RACQ m away. The diagnostic variable DHOBST displays
C the obstacle height at the time the sensor detects it.
C
C To aid in the analysis of the data the mean, CLMEAN, and the std dev, CLSIGM,
C of the terrain clearance HCLEAR is computed recursively. The number of
C occurenes of clobber is also recorded CLOBN (1686)
C
C
C LOOK-DOWN TERRAIN FOLLOWING (MGUIDP=1)
C -------------------------------------
C
C The TF/OA Look-Ahead mode defaults to a standard TF/OA Look-Down mode.
C Just set SLOPE to a large number, and TLEAD=0. To assure that DHOBST displays
C the obstacles at the current time set RACQ=0. However do not set RAHEAD=0.
C Although it has no effect on the terrain following it must be greater than
C DCELL in order for the terrain to be calculated. The height above terrain HBG
C calculated in Module D1, corrupted in S4 to become HBGM and provided as measur
C to C2, The altitude hold autopilot nulls the HCOM-HBGM signal.
C
C LINE GUIDANCE
C -------------
C
C This module implements the line guidance steering law.
C It provides pitch acceleration and roll position commands that will
C steer the missile to a point along an approach direction.
C
C The guidance point, stored in the processor, is the combination of ST1EL(3),
C the true target position, and the mapping error ESWT1L(3) of the waypoint
C relative to the true target position. This point is approached along the
C Line-of-Attack (LOA) defined by the angles PSIFLCX and THTFLCX, measured from
C L-1 direction (NORTH) positive towards the East and up.
C
C When  new target coordinates are inserted (any new ST1EL(3) component value),
C the guidance law will steer the missile against the new point; also, a differe
C approach line can be defined by providing new PSIFLCX AND THTFLCX values.
C
C Line Guidance steering can be engaged in the pitch and lateral plane by settin
C the mode switch MGUID (800). When line guidance is engaged in the respective p
C ALCOM(802) and/or ANCOM (803) are given commands from the line guidance algori
C Otherwise line guidance is disengaged and the autopilot will provide the
C steering commands.
C
C MTRUN (1700) will determine if the vehicle executes yaw-to-turn (MTRUN=0) or
C bank-to-turn (MTURN=1) maneuvers. The autopilot setting, MAUT (900), for pitch
C plane line guidance is MAUT=x4 (acceleration); for yaw-to-turn in the lateral
C plane it is MAUT=4x (acceleration) and for bank-to-turn it is MAUT=1x (roll
C position control).
C
C*** *
C*** * MODIFICATION HISTORY
C*** * 991001 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
      COMMON/HOI/HO(100)
      COMMON/HI/H(100)
C
      DIMENSION FSPCB(3),SBWLC(3),TFLC(3,3),TOLC(3,3)
     +,VBELC(3),ST1BLC(3),SBWFC(3),VBEFC(3),VBEOC(3),ST1EL(3)
     +,SWEL(3),SWT1L(3),SBT1LC(3),DUM3(3),ESWT1L(3),EL(100)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(0807),ESWT1L(1))
      EQUIVALENCE (C(0814),BGP)
      EQUIVALENCE (C(0815),GNAVN)
      EQUIVALENCE (C(0816),GNAVL)
      EQUIVALENCE (C(0817),ALDEAD)
      EQUIVALENCE (C(0834),GGP)
      EQUIVALENCE (C(0845),PSIFLCX)
      EQUIVALENCE (C(0846),THTFLCX)
      EQUIVALENCE (C(0847),XDBI)
      EQUIVALENCE (C(0848),XDBA)
      EQUIVALENCE (C(0870),GLOAP)
      EQUIVALENCE (C(0871),GLOAY)
      EQUIVALENCE (C(0876),DCELL)
      EQUIVALENCE (C(0877),RAHEAD)
      EQUIVALENCE (C(0878),DHTRC)
      EQUIVALENCE (C(0879),SLOPE)
      EQUIVALENCE (C(0880),OCCDEN)
      EQUIVALENCE (C(0881),SIGOBS)
      EQUIVALENCE (C(0882),ISEED2)
      EQUIVALENCE (C(0890),TLEAD)
      EQUIVALENCE (C(0891),GELEV)
C
C MGUID = D =|MROLL|MGUIDL|MGUIDP see Module C1
C ESWT1L(3) = D Offset target map error - m
C BGP = D Bias term in nonlinear roll gain - g's
C GNAVN = D Normal line guidance gain - 1/s
C GNAVL = D Lateral line guidance gain - 1/s
C ALDEAD = D Dead band of roll command - g's
C GGP = D Roll command gain - rad
C PSIFLCX = D Yaw line-of-attack angle - deg
C THTFLCX = D Pitch line-of-attack angle - deg
C XDBI = D Gain decay factor for range-to-go - 1/m
C XDBA = D Gain decay factor for distance from LOA - 1/m
C GLOAP = D Nonlinear gain factor, pitch - ND
C GLOAY = D Nonlinear gain factor, yaw - ND
C DCELL = D Cell size of terrain data base - m
C RAHEAD = D Look-ahead range into terrain data base - m
C DHTRC = D Commanded obstacle clearance height - m
C SLOPE = D Slope to switch from TF to OA - rad
C OCCDEN = D Mean occurance of obstacles per distance - 1/m
C SIGOBS = D Mode of obstacle height (Rayleigh) - m
C ISEED2 = D Seed of RAN function for obstacles - ND
C TLEAD = D Lead time for data base for attitude command - s
C GELEV = D Gain of obstacle avoidance command - g*s/m
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(0883),J)
      EQUIVALENCE (C(0884),GNDPT)		
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(2664),DER)
      EQUIVALENCE (C(2866),ICOOR)
	EQUIVALENCE (C(2000),T)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0411),RACQ)
      EQUIVALENCE (C(0115),ST1EL(1))
      EQUIVALENCE (C(0412),SWEL(1))
      EQUIVALENCE (C(0703),VBELC(1))
      EQUIVALENCE (C(0731),SBWLC(1))
      EQUIVALENCE (C(0734),FSPCB(1))
      EQUIVALENCE (C(0923),PHILIM)
      EQUIVALENCE (C(1615),HBE)
      EQUIVALENCE (C(1617),GNDTCK)
      EQUIVALENCE (C(1643),DVBE)
      EQUIVALENCE (C(1647),PSIVL)
      EQUIVALENCE (C(1651),THTVL)
      EQUIVALENCE (C(1669),HGE)
      EQUIVALENCE (C(1700),MTURN)
C
C RACQ= D Sensor acquisition range - m
C ST1EL= O Target position from ref. point E - m
C SWEL= D Waypoint coordinates - m
C VBELC= O Computed body vel in earth coor - m/s
C SBWLC= O Displmt of vehicle wrt stored waypoint (target) - m
C FSPCB= O Computed specific force on body - N/kg
C PHILIM= D roll limiter - rad
C HBE= O altitude of vehicle above S.L.
C GNDTCK= G Groundtrack since launch - m
C DVBE= S Vehicle speed - m/s
C PSIVL= S Heading angle - rad
C THTVL= S Flight path angle - rad
C HGE= I/G Overall terrain height above S.L. - m
C MTURN= D =0:Skid-to-turn, =1:Bank-to-turn
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0801),MROLL)
      EQUIVALENCE (C(0802),ALCOM)
      EQUIVALENCE (C(0803),ANCOM)
      EQUIVALENCE (C(0810),SBT1LC(1))
      EQUIVALENCE (C(0885),HBGS)
      EQUIVALENCE (C(0889),MGUIDP)
      EQUIVALENCE (C(0900),MAUT)
      EQUIVALENCE (C(0924),PHIBVC)
      EQUIVALENCE (C(1686),CLOBN)
C
C MROLL = O Part of MGUID=|MROLL|MGUIDLMGUIDP
C ALCOM = O Lateral acceleration command - g's
C ANCOM = O Normal accelration command - g's
C SBT1LC(3) = O Position of vehicle wrt stored target - m
C HBGS = O Height AGL projected TLEAD sec ahead - m
C MGUIDP = O Part of MGUID=|MROLL|MGUIDP|MGUIDL|
C PHIBVC = O Roll command = rad
C CLOBN = O Number of clobber (crashing) events - ND
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0819),PC)
      EQUIVALENCE (C(0851),PSIVFCX)
      EQUIVALENCE (C(0852),THTVFCX)
      EQUIVALENCE (C(0853),PSIOFCX)
      EQUIVALENCE (C(0854),THTOFCX)
      EQUIVALENCE (C(0855),SBWFC(1))
      EQUIVALENCE (C(0858),VBEFC(1))
      EQUIVALENCE (C(0864),DBWC)
      EQUIVALENCE (C(0865),DBA)
      EQUIVALENCE (C(0866),PSIVOCX)
      EQUIVALENCE (C(0867),THTVOCX)
      EQUIVALENCE (C(0868),PSIOLC)
      EQUIVALENCE (C(0869),THTOLC)
      EQUIVALENCE (C(0872),GLP)
      EQUIVALENCE (C(0873),GLY)
      EQUIVALENCE (C(0874),ACV2)
      EQUIVALENCE (C(0875),ACV3)
      EQUIVALENCE (C(0886),HGES)
      EQUIVALENCE (C(0887),ELMAX)
      EQUIVALENCE (C(0888),HCLEAR)
      EQUIVALENCE (C(0892),DHOBST)
      EQUIVALENCE (C(0895),CLMEAN)
      EQUIVALENCE (C(0896),CLSIGM)
      EQUIVALENCE (C(0897),TFOA)
C
C PC = G Nonlinear roll gain - rad/g's
C PSIVFCX= G Heading of vel vec wrt LOA - deg
C THTVFCX= G Flight path angle  of vel vec wrt LOA - deg
C PSIOFCX= G Yaw angle of LOS wrt LOA - deg
C THTOFCX= G Pitch angle  of LOS wrt LOA - deg
C SBWFC(3) = G Vehicicle pos wrt way point in LOA coord - m
C VBEFC(3) = G Vehicle velocity in LOA coordiantes - m/s
C DBWC = G Computed vehicle distance from waypoint - m
C DBA = G Vehicle distance from LOA - m
C PSIVOCX = G Heading of vel vec wrt LOS - deg
C THTVOCX = G Flight path angle  of vel vec wrt LOS - deg
C PSIOLC = G Heading angle of LOS wrt north - rad
C THTOLC = G Pitch angle of LOS wrt horizontal - rad
C GLP = G Pitch gain of line guidance - rad
C GLY = G Yaw gain of line guidance - rad
C ACV2 = G Lateral LATEX - m/s^2
C ACV3 = G Normal LATEX - m/s^2
C HGES = G Current height above terrain - m
C ELMAX = G Max angle of LOS and vel vec within RAHEAD - rad
C HCLEAR = G Actual clearance above terrain - m
C DHOBST = G Obstacle height - m
C CLMEAN = G Actual mean vehicle clearance height - m
C CLSIGM = G Actual stad deviation from CLMEAN - m
C TFOA = G =10:Terrain following, =9:Obstavle avoidance
C
      IF(MGUID.EQ.0) RETURN
C
      MROLL=INT(MGUID/100.)
      MGUIDL=INT((MGUID-MROLL*100)/10.)
      MGUIDP=MGUID-MGUIDL*10-MROLL*100
C
C*** OFFSET AIMPOINT
C
      CALL MATSUB(SWT1L,SWEL,ST1EL,3,1)
      CALL MATADD(DUM3,SBWLC,SWT1L,3,1)
      CALL MATADD(SBT1LC,DUM3,ESWT1L,3,1)
C
 
      CALL MAT2TR(TFLC,PSIFLCX/CRAD,THTFLCX/CRAD)
 
C
C*** TF/OA WITH FWD LOOKING SENSOR AND DATA BASE
C
      IF(MGUIDP.EQ.2) THEN
         IF(ICOOR.EQ.0) THEN
            N=NINT(RAHEAD/DCELL)
            K=NINT(RACQ/DCELL)+1
            DGND=GNDTCK-GNDPT
C
            IF(DGND.GE.DCELL) THEN
C
               IF(J.LT.N) THEN
C
C***           INITIALIZE TERRAIN STACK
C
                  J=J+1
                  GNDPT=GNDTCK
                  H(J)=HGE
                  HO(J)=HGE
                  NL=1
                  NCELL=0
                  ISETC1=1
                  SIG2=0.
                  CLMEAN=0.
                  GNDOCC=0.
               ELSE
C
C***           INSERT NEW ALTITUDE INTO STACK
C
                  GNDPT=GNDTCK
                  DO I=1,N-1
                     H(I)=H(I+1)
                     HO(I)=HO(I+1)
				ENDDO
                  H(N)=HGE
                  HO(N)=HGE
C
C***             ADD OBSTACLE AT SENSOR ACQUISITION RANGE
C
                  DHOBST=0.
                  IF(GNDTCK.GE.GNDOCC.AND.OCCDEN.GT.0.) THEN
C                   GROUNDTRACK DISTANCE AT NEXT OBSTACLE OCCURENCE
                     U1=RAN(ISEED2)
                     DOCC=-ALOG(1.-U1)/OCCDEN
                     GNDOCC=GNDTCK+DOCC
C                   HEIGHT OF OBSTACLE
                     U2=RAN(ISEED2)
                     RAYL=SQRT(-2.*ALOG(1.-U2))
                     DHOBST=RAYL*SIGOBS
                     HO(K)=HO(K)+DHOBST
                   ENDIF
C
C***             MAX ELEVATION ANGLE
C
                  ELM=-99999.
                  DO 20 I=2,N
                  EL(I)=(HO(I)+DHTRC-HBE)/((I-1)*DCELL)
                  IF(EL(I).GT.ELM) THEN
                     ELM=EL(I)
                     IMAX=I
                  ENDIF
C
                  ELMAX=ELM-THTVL
                  RLEAD=DVBE*TLEAD
                  NL=NINT(RLEAD/DCELL)+1
C
C***             NUMBER OF CLOBBER EVENTS
C
                  CLEAR=HBE-HO(1)
                  IF(CLEAR.LE.0) THEN
                     IF(ISETC1.EQ.1) THEN
                        ISETC1=0
                        CLOBN=CLOBN+1
                     ENDIF
                  ELSE
                     ISETC1=1
                  ENDIF
C
C***             MEAN CLEARANCE AND STANDARD DEVIATION
C
                  NCELL=NCELL+1
                  CLMM2=CLMEAN*CLMEAN
                  CLMEAN=CLMEAN+(CLEAR-CLMEAN)/(NCELL+1)
                  CLM2=CLMEAN*CLMEAN
                  SIG2=SIG2+(CLEAR*CLEAR-SIG2-CLMM2)/(NCELL+1)
     &                 +CLMM2-CLM2
                  CLSIGM=SQRT(SIG2)
 20               CONTINUE
               ENDIF
            ENDIF
C
            HGES=HO(1)
            HCLEAR=HBE-HO(1)
C
            IF(ELMAX.LT.SLOPE) THEN
C
C***       TERRAIN FOLLOWING. OUTPUT TO S4: HBGS
C
               TFOA=10.
               MAUT=13
               HBGS=HBE-H(NL)
            ELSE
C
C***       OBSTACLE AVOIDANCE AND CLIMB. OUTPUT: ACCEL COMMAND ANCOM
C
               TFOA=9.
               MAUT=14
               ANCOM=1.+GELEV*ELMAX*DVBE
            ENDIF
         ENDIF
      ENDIF
C
      IF(MGUID.GE.3) THEN
C
C***    VELOCITIES
C
         DUM=-1.
         CALL MATCON(ST1BLC,DUM,SBT1LC,3,1)
         CALL MATPOL(DBWC,PSIOLC,THTOLC,ST1BLC)
         CALL MAT2TR(TOLC,PSIOLC,THTOLC)
         CALL MATMUL(VBEFC,TFLC,VBELC,3,3,1)
         CALL MATMUL(VBEOC,TOLC,VBELC,3,3,1)
C
C***     VEHICLE DISTANCE FROM ATTACK LINE
C
         CALL MATMUL(SBWFC,TFLC,SBT1LC,3,3,1)
         DBA=SQRT(SBWFC(2)**2+SBWFC(3)**2)
C
C***     NONLINEAR GAIN
C
         G1=1.-EXP(-DBWC*XDBI)
         G2=1.-EXP(-DBA*XDBA)
         GLP=GLOAP*G1*G2
         GLY=GLOAY*G1*G2
C
C***     LINE GUIDANCE STEERING LAW
C
         ACV2=GNAVL*(-VBEOC(2)+GLY*VBEFC(2))
         ACV3=GNAVN*(-VBEOC(3)+GLP*VBEFC(3))-AGRAV*COS(THTVL)
C
         IF(MTURN.EQ.0) THEN
C
C***       YAW-TO-TURN
C
           IF(MGUIDL.EQ.3) THEN
              ALCOM=ACV2/AGRAV
           ENDIF
           IF(MGUIDP.EQ.3) THEN
              ANCOM=-ACV3/AGRAV
           ENDIF
C
         ELSE
C
C***       BANK-TO-TURN
C
           IF(MGUIDL.EQ.3) THEN
              DUM=ACV2/AGRAV
              IF(ABS(DUM).LT.ALDEAD)DUM=0.
              PC=GGP/(.01+BGP+ABS(FSPCB(3)/AGRAV))
              PHIBVC=-SIGN(PC,FSPCB(3))*DUM
              IF(PHIBVC.GT.PHILIM)PHIBVC=PHILIM
              IF(PHIBVC.LT.-PHILIM)PHIBVC=-PHILIM
              IF(MROLL.EQ.1) PHIBVC=3.1412-PHIBVC
           ENDIF
           IF(MGUIDP.EQ.3) THEN
              ANCOM=-ACV3/(COS(PHIBVC)*AGRAV)
           ENDIF
C
         ENDIF
C
C***     DIAGNOSTIC ANGLES
C
         PSIOFCX=CRAD*(PSIOLC-PSIFLCX/CRAD)
         THTOFCX=CRAD*(THTOLC-THTFLCX/CRAD)
         PSIVFCX=CRAD*(PSIVL-PSIFLCX/CRAD)
         THTVFCX=CRAD*(THTVL-THTFLCX/CRAD)
         PSIVOCX=CRAD*(PSIVL-PSIOLC)
         THTVOCX=CRAD*(THTVL-THTOLC)
C
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
C*** *                       MTURN=0                  MTURN=1
C*** *
C*** *       |MAUTL|= 1   Sideslip Angle Hold    Roll Position Hold
C*** *                2   Heading  Angle Hold    Heading Angle Hold
C*** *                4   Acceleration Hold      Acceleration Hold
C*** *                6   Yaw rate               Roll rate
C*** *
C*** *             |MAUTP|= 1 Angle-of-Attack Hold
C*** *                      2 Flight Path Angle Hold
C*** *                      3 Altitude Hold
C*** *                      4 Acceleration Hold
C*** *					  6 Pitch rate
C*** *
C*** *
C*** *
C*** * MODIFICATION HISTORY
C*** * 990930 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INITIALIZE STATE VARIABLES
C
      EQUIVALENCE (C(0942),XTHTD)
      EQUIVALENCE (C(0943),XTHT)
      EQUIVALENCE (C(0944),XPHID)
      EQUIVALENCE (C(0945),XPHI)
      EQUIVALENCE (C(0946),XPSID)
      EQUIVALENCE (C(0947),XPSI)
      EQUIVALENCE (C(0948),XID)
      EQUIVALENCE (C(0949),XI)
      EQUIVALENCE (C(0950),PHD)
      EQUIVALENCE (C(0951),PH)
      EQUIVALENCE (C(0952),ALPD)
      EQUIVALENCE (C(0953),ALP)
      EQUIVALENCE (C(0954),RATEPD)
      EQUIVALENCE (C(0955),RATEP)
      EQUIVALENCE (C(0956),BETD)
      EQUIVALENCE (C(0957),BET)
      EQUIVALENCE (C(0958),RATEYD)
      EQUIVALENCE (C(0959),RATEY)
 
	ILOC=942 !4+1+2+2
	DO J=1,9
	   IPL(NIP)=ILOC
	   IPLV(NIP)=ILOC +1
	   ILOC=ILOC+2
	   NIP=NIP+1
	ENDDO
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C2
C**********************************************************************
C*** * Control Module C2
C*** * Reserved C(3510) locations are 900-999
C*** *
C*** *
C    This Autopilot Module accepts acceleration commnds from the
C    Guidance Module and converts them into angle-of-attack and sideslip
C    angle outputs which are used in the aerodynamic table look-ups of
C    the Aerodynamic Module. Several other autopilot modes may be
C    selected by following the Autopilot Option Table. The options are
C    invoked by the mode switch MAUT that consists of two digits. The
C    first digit governs the lateral autopilot modes for skid-to-turn
C    and bank-to-turn airframes and the second digit determines the
C    pitch modes:
C
C      MAUT =|MAUTL|MAUTP|
C
C                            MTURN=0                  MTURN=1
C
C            |MAUTL|= 1   Sideslip Angle Hold    Roll Position Hold
C                     2   Flight Path Angle      Flight Path Angle
C                     4   Acceleration Hold      Acceleration Hold
C                     6   Yaw rate               Roll rate
C
C                  |MAUTP|= 1 Angle-of-Attack Hold
C                           2 Flight Path Angle Hold
C                           3 Altitude Hold
C                           4 Acceleration Hold
C						  6 Pitch rate
C
C    The baseline acceleration hold autopilot is a proportional loop
C    with gain GR as input. To invoke the proportional-integral loop
C    (P-I) TA and GACP must be provided. GR will be calculated
C    internally.
C
C    The rate loop has an aerodynamic and thrust vector branch. APTVC
C    apportions the contribution of TVC.  For APTVC=0 (no TVC, all aero
C    control) the following data must be provided: CNALP, ALPLIM, CMDEL,
C    RLEN, GA, PARM, AIZ. With TVC (0<APTVC<1) the inputs TVCLIM and GP
C        must be added.
C
C    The rate loop can be simplified by only specifying the rate loop
C    time constant TR > 0. Then all detailed rate loop calculations are
C    replaced by a simple first order lag transfer function. If the P-I
C    autopilot mode is selected the rate loop must be in this simplified
C    form.
C
C*** *
C*** *
C*** *
C*** * MODIFICATION HISTORY
C*** * 990930 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION VBELC(3),FSPCB(3),SBELC(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0900),MAUT)
      EQUIVALENCE (C(0901),ALPHAC)
      EQUIVALENCE (C(0902),THTVLCX)
      EQUIVALENCE (C(0903),TTHT)
      EQUIVALENCE (C(0905),GTHT)
      EQUIVALENCE (C(0906),BETAC)
      EQUIVALENCE (C(0910),ANPLIM)
      EQUIVALENCE (C(0911),ANNLIM)
      EQUIVALENCE (C(0912),CNALP)
      EQUIVALENCE (C(0915),ALLIM)
      EQUIVALENCE (C(0916),CYBET)
      EQUIVALENCE (C(0919),GH)
      EQUIVALENCE (C(0920),GV)
      EQUIVALENCE (C(0921),HCOM)
      EQUIVALENCE (C(0924),PHIBVC)
      EQUIVALENCE (C(0929),HDTLIM)
      EQUIVALENCE (C(0963),PSIVLCX)
      EQUIVALENCE (C(0964),TPSI)
      EQUIVALENCE (C(0966),GPSI)
      EQUIVALENCE (C(0967),TA)
      EQUIVALENCE (C(0972),GACP)
      EQUIVALENCE (C(0995),GR)
      EQUIVALENCE (C(0997),TR)
C
C MAUT = D Mode Switch MAUT=|MAUTL||MAUTP| (see Module C2)
C ALPHAC = D Angle-of-attack command - rad
C THTVLCX = D Vertical flight path angle command - deg
C TTHT = D Lead time constant for vert.flight path - s
C GTHT = D Gain of vert flight path hold loop - (rad/s)/rad
C BETAC = D Sideslip angle command - rad
C ANPLIM = D Pos pitch accel limiter - g's
C ANNLIM = D Neg pitch accel limiter - g's
C CNALP = D Normal force derivative - 1/rad
C ALLIM = D Lateral accel limiter - g's
C CYBET = D Side force derivative - 1/rad
C GH = D Altitude loop position gain - g's/m
C GV = D Altitude loop velocity gain - g's/(m/s)
C HCOM = D Commanded altitude above ground- m
C PHIBVC = D Commanded roll angle - rad
C HDTLIM = D Altitude rate limiter - m/s
C PSIVLCX = D Commanded horizontal flight path angle - deg
C TPSI = D Lead time constant for horiz.flight path - s
C GPSI = D Gain of horiz flight path hold loop - (rad/s)/rad
C TA = D Ratio of prop/integral gains. If > 0, P-I engaged.
C GACP = D Root locus gain of accel loop - rad/s2
C GR = D/G Rate gain (D: P-loop, G: P-I loop) (rad/s)/(m/s)
C TR = D/G Rate loop time const (if > 0: simple rate loop)
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
      EQUIVALENCE (C(0702),HBGM)
      EQUIVALENCE (C(0703),VBELC(1))
      EQUIVALENCE (C(0734),FSPCB(1))
      EQUIVALENCE (C(0776),SBELC(1)) !<
      EQUIVALENCE (C(0802),ALCOM)
      EQUIVALENCE (C(0803),ANCOM)
      EQUIVALENCE (C(0835),WPC) !<
      EQUIVALENCE (C(0861),WQC) !<
      EQUIVALENCE (C(0862),WRC) !<
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1403),AREA)
      EQUIVALENCE (C(1306),AMASS)
      EQUIVALENCE (C(1647),PSIVL)
      EQUIVALENCE (C(1651),THTVL)
      EQUIVALENCE (C(1700),MTURN)
C
C DVBA= O Speed of vehicle wrt air - m/s
C PDYNMC= O Dynamic pressure - Pa
C HBGM= O Computed body alt above terrain - m
C VBELC= O Computed body vel in earth coor - m/s
C FSPCB= O Computed specific force on body - N/kg
C ALCOM= O Lateral acceleration command - g's
C ANCOM= O Normal accelration command - g's
C FTHALT= O Thrust - N
C AREA= D Reference area for aerodynamic data - m^2
C AMASS= O Vehicle mass - kg
C PSIVL= S Heading angle - rad
C THTVL= S Flight path angle - rad
C MTURN= D =0:Skid-to-turn, =1:Bank-to-turn
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0908),ALPHA)
      EQUIVALENCE (C(0909),BETA)
      EQUIVALENCE (C(0926),PHIBV)
C
C ALPHA = O Angle-of-attack - rad
C BETA = O Sideslip angle - rad
C PHIBV = O Roll angle about velocity vector - rad
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0942),XTHTD)
      EQUIVALENCE (C(0943),XTHT)
      EQUIVALENCE (C(0944),XPHID)
      EQUIVALENCE (C(0945),XPHI)
      EQUIVALENCE (C(0946),XPSID)
      EQUIVALENCE (C(0947),XPSI)
      EQUIVALENCE (C(0948),XID)
      EQUIVALENCE (C(0949),XI)
C
C XTHT = S state variable of vert Lead shaping - rad
C XPHI = S Roll loop state variable - rad/s
C XPSI = S state variable of horiz Lead shaping - rad
C XI = S Integral feedback, pitch loop - rad/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0907),PHIBVX)
      EQUIVALENCE (C(0927),ALPHAX)
      EQUIVALENCE (C(0928),BETAX)
      EQUIVALENCE (C(0962),EH)
      EQUIVALENCE (C(0996),TIP)
C
C PHIBVX = G Roll anlge about vel vector - deg
C ALPHAX = G Angle-of-attack - deg
C BETAX = G Sideslip angle - deg
C EH = G Altitude error - m
C TIP = G Time incidence lag, pitch loop - s
C
      MAUTL=INT(MAUT/10.)
      MAUTP=MAUT-MAUTL*10
C
C*** ANGLE OF ATTACK HOLD
C
      IF(MAUTP.EQ.1) THEN
         ALPHA=ALPHAC
      ENDIF
C
C*** VERTICAL FLIGHT PATH ANGLE HOLD
C
      IF(MAUTP.EQ.2) THEN
         ETHTVL=THTVLCX/CRAD-THTVL
         XTHTD=(ETHTVL-XTHT)/(TTHT)
         PITCH=GTHT*(TTHT*XTHTD+XTHT)
         CALL C2PITCH(ALPHA,PITCH)
      ENDIF
C
C*** ALTITUDE HOLD
C
      IF(MAUTP.EQ.3) THEN
         EH=(HCOM-HBGM)*GH
         IF(EH.GT.HDTLIM) EH=HDTLIM
         IF(EH.LT.-HDTLIM) EH=-HDTLIM
         COSPHI=COS(PHIBV)
         DUM=SIGN(1.,COSPHI)
         ANCOM=DUM*(GV/AGRAV*(EH+VBELC(3)))+1./COSPHI
         IF(ANCOM.GT.ANPLIM) ANCOM=ANPLIM
         IF(ANCOM.LT.ANNLIM) ANCOM=ANNLIM
         ABECZ=-ANCOM*AGRAV
         EP=(ABECZ-FSPCB(3))
         TIP=DVBA*AMASS/(PDYNMC*AREA*CNALP+FTHALT)
         IF(TA.GT.0.)THEN
            GR=GACP*TIP*TR/DVBA
            GI=GR/TA
            XID=GI*EP
         ENDIF
         PITCH=-(EP*GR+XI)
         CALL C2PITCH(ALPHA,PITCH)
      ENDIF
C
C*** PITCH ACCELERATION HOLD
C
      IF(MAUTP.EQ.4) THEN
         IF(ANCOM.GT.ANPLIM) ANCOM=ANPLIM
         IF(ANCOM.LT.ANNLIM) ANCOM=ANNLIM
         ABECZ=-ANCOM*AGRAV
         EP=(ABECZ-FSPCB(3))
         TIP=DVBA*AMASS/(PDYNMC*AREA*CNALP+FTHALT)
         IF(TA.GT.0.)THEN
            GR=GACP*TIP*TR/DVBA
            GI=GR/TA
            XID=GI*EP
         ENDIF
         PITCH=-(EP*GR+XI)
         CALL C2PITCH(ALPHA,PITCH)
      ENDIF
C
C*** PITCH RATE HOLD
C
      IF(MAUTP.EQ.6) THEN
         PITCH=WQC
         CALL C2PITCH(ALPHA,PITCH)
      ENDIF
C
C*** YAW-TO-TURN
C
      IF(MTURN.EQ.0) THEN
C
         PHIBV=0.
C
C***     SIDE SLIP ANGLE HOLD
C
         IF(MAUTL.EQ.1) THEN
            BETA=BETAC
         ENDIF
C
C***     HORIZONTAL FLIGHT PATH ANGLE HOLD
C
         IF(MAUTL.EQ.2) THEN
            EPSIVL=PSIVLCX/CRAD-PSIVL
            XPSID=(EPSIVL-XPSI)/(TPSI)
            YAW=GPSI*(TPSI*XPSID+XPSI)
            CALL C2YAW(BETA,YAW)
         ENDIF
C
C***     YAW ACCELERATION HOLD
C
CC         IF(MAUTL.EQ.4) THEN
CC            IF(ALCOM.GT.ALLIM) ALCOM=ALLIM
CC            IF(ALCOM.LT.-ALLIM) ALCOM=-ALLIM
CC            ABECY=ALCOM*AGRAV
CC            EY=(ABECY-FSPCB(2))
CC            TIY=DVBA*AMASS/(PDYNMC*AREA*CYBET+FTHALT)
CC            IF(TA.GT.0.)THEN
CC               GR=GACP*TIY*TR/DVBA
CC               GI=GR/TA
CC               YID=GI*EY
CC            ENDIF
CC            YAW=-(EY*GR+YI)
CC            CALL C2YAW(BETA,YAW)
CC         ENDIF
C
C***     YAW RATE HOLD
C
CC         IF(MAUTL.EQ.6) THEN
CC            YAW=WRC
CC            CALL C2YAW(BETA,YAW)
CC         ENDIF
C
C*** BANK TO TURN
C
      ELSE
C
         BETA=0.
C
C***     ROLL POSITION HOLD
C
         IF(MAUTL.EQ.1) THEN
            PHIC=PHIBVC
            CALL C2PHI(PHI,PHID,PHIC)
            PHIBV=PHI
         ENDIF
C
C***     HORIZONTAL FLIGHT PATH ANGLE HOLD
C
         IF(MAUTL.EQ.2) THEN
            EPSIVL=PSIVLCX/CRAD-PSIVL
            XPSID=(EPSIVL-XPSI)/(TPSI)
            ALCOM=GPSI*(TPSI*XPSID+XPSI)
            CALL C2ACCL(PHIC,ALCOM)
            CALL C2PHI(PHI,PHID,PHIC)
            PHIBV=PHI
         ENDIF
C
C***     LATERAL ACCELERATION HOLD
C
         IF(MAUTL.EQ.4) THEN
            CALL C2ACCL(PHIC,ALCOM)
            CALL C2PHI(PHI,PHID,PHIC)
            PHIBV=PHI
         ENDIF
C
C***     ROLL RATE HOLD
C
         IF(MAUTL.EQ.6) THEN
            PHIC=WPC
            CALL C2PHI(PHI,PHID,PHIC)
            XPHID=PHI
            PHIBV=XPHI
         ENDIF
      ENDIF
      ALPHAX=CRAD*ALPHA
      BETAX=CRAD*BETA
      PHIBVX=CRAD*PHIBV
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C2ACCL(PHIC,ALCOM)
C**********************************************************************
C*** * Lateral acceleration through rolling
C*** *
C*** * Argument Output:
C*** *                PHIC= Roll angle command - rad
C*** * Argument Input:
C*** *				ALCOM= commanded acceleration - g's
C*** *
C*** * MODIFICATION HISTORY
C*** * 990930Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION FSPCB(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0915),ALLIM)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0734),FSPCB(1))
C
C FSPCB= O Computed specific force on body - N/kg
C
      IF(ALCOM.GT.ALLIM) ALCOM=ALLIM
      IF(ALCOM.LT.-ALLIM) ALCOM=-ALLIM
      PC=(ALCOM*AGRAV)/(ABS(FSPCB(3))+.001)
      PHIC=-SIGN(1.,FSPCB(3))*PC
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C2PHI(PHI,PHID,PHIC)
C**********************************************************************
C*** * Roll autopilot with lag
C*** *
C*** * Argument Output:
C*** *                PHI= Roll angle - rad
C*** *                PHID= Roll rate = rad/s
C*** * Argument Input:
C*** *                PHIC= Roll angle command - rad
C*** *
C*** * MODIFICATION HISTORY
C*** * 990930Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0922),TPHI)
      EQUIVALENCE (C(0923),PHILIM)
C
C TPHI = D Roll time constant - s
C PHILIM = D roll limiter - rad
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(801),MROLL)
C
C MROLL= O Part of MGUID=|MROLL|MGUIDLMGUIDP
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0950),PHD)
      EQUIVALENCE (C(0951),PH)
C
C PH = S Roll state variable = rad
C
      IF(MROLL.EQ.0) THEN
         IF(PHIC.GT.PHILIM) PHIC=PHILIM
         IF(PHIC.LT.-PHILIM) PHIC=-PHILIM
      ENDIF
      PHD=(PHIC-PH)/TPHI
      PHID=PHD
      PHI=PH
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C2PITCH(ALPHA,PITCH)
C**********************************************************************
C*** * Pitch rate loop for TVC and aero contral
C*** *
C*** * Argument Output:
C*** *                ALPHA= Angle of attack - rad
C*** * Argument Input:
C*** *                PITCH= Pitch rate input - rad/s
C*** *
C*** * TR=0: detailed rate loop
C*** * TR>0: rate loop represented by time consant TR
C*** *
C*** * MODIFICATION HISTORY
C*** * 990930Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0912),CNALP)
      EQUIVALENCE (C(0977),ALPLIM)
      EQUIVALENCE (C(0978),TVCLIM)
      EQUIVALENCE (C(0988),CMDEL)
      EQUIVALENCE (C(0989),RLENG)
      EQUIVALENCE (C(0990),APTVC)
      EQUIVALENCE (C(0991),GA)
      EQUIVALENCE (C(0992),GP)
      EQUIVALENCE (C(0993),PARM)
      EQUIVALENCE (C(0994),AIZ)
      EQUIVALENCE (C(0997),TR)
      EQUIVALENCE (C(0999),FLPLIM)
C
C CNALP = D Normal force derivative - 1/rad
C ALPLIM = D Angle-of-attack limiter - rad
C TVCLIM = D Nozzle gimbal limiter - rad
C CMDEL = D Control moment derivative at trim cond.- 1/rad
C RLENG = D Ref length of CMDEL - m
C APTVC = D =0: Aero Control, =1: TVC only, =x: mixture
C GA = D Aero control gain - rad/(rad/s)
C GP = D TVC control gain - rad/(rad/s)
C PARM = D Nozzle moment arm - m
C AIZ = D Transverse moment of inertia - kg*m2
C TR = D/G Rate loop time const (if > 0: simple rate loop) - s
C FLPLIM = D Control surface excursion limiter - rad
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0219),DVBA)
      EQUIVALENCE (C(0220),PDYNMC)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1403),AREA)
      EQUIVALENCE (C(1306),AMASS)
C
C DVBA= O Speed of vehicle wrt air - m/s
C PDYNMC= O Dynamic pressure - Pa
C FTHALT= O Thrust - N
C AREA= D Reference area for aerodynamic data - m^2
C AMASS= O Vehicle mass - kg
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0952),ALPD)
      EQUIVALENCE (C(0953),ALP)
      EQUIVALENCE (C(0954),RATEPD)
      EQUIVALENCE (C(0955),RATEP)
C
C ALP = S Angle-of-attack state variable - rad
C RATEP = S pitchrate - rad/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0936),AERMP)
      EQUIVALENCE (C(0937),TVCMP)
      EQUIVALENCE (C(0938),ETA)
      EQUIVALENCE (C(0939),DELQ)
      EQUIVALENCE (C(0996),TIP)
      EQUIVALENCE (C(0998),TRCALC)
C
C AERMP = G Pitch aero control moment - N*m
C TVCMP = G Pitch TVC control moment - N*m
C ETA = G Pitch nozzle angle - rad
C DELQ = G Pitch control surface angle - rad
C TIP = G Time incidence lag, pitch loop - s
C TRCALC = G Internally calculated TR = s
C
      RATEPC=PITCH
C
C*** SIMPLIFIED RATE LOOP
C
      IF(TR.GT.0.) THEN
         RATEPD=(RATEPC-RATEP)/TR
      ELSE
C
C*** DETAILED RATE LOOP
C
         CMOM=PDYNMC*AREA*RLENG*CMDEL
         POLEA=(1.-APTVC)*GA*CMOM/AIZ
         POLEP=APTVC*GP*FTHALT*PARM/AIZ
         POLE=POLEA+POLEP
         IF(POLE.NE.0.)TRCALC=1./POLE
C
         ERATEP=RATEPC-RATEP
         DELQ=-ERATEP*(1.-APTVC)*GA
         IF(DELQ.GT.FLPLIM) DELQ=FLPLIM
         IF(DELQ.LT.-FLPLIM) DELQ=-FLPLIM
         AERMP=-DELQ*CMOM
C
         IF(FTHALT.GT.0) THEN
            ETA=-ERATEP*APTVC*GP
         ELSE
            ETA=0.
         ENDIF
         IF(ETA.GT.TVCLIM) ETA=TVCLIM
         IF(ETA.LT.-TVCLIM) ETA=-TVCLIM
         TVCMP=-ETA*FTHALT*PARM
C
         AMP=AERMP+TVCMP
         RATEPD=AMP/AIZ
      ENDIF
C
      TIP=DVBA*AMASS/(PDYNMC*AREA*CNALP+FTHALT)
      ALPD=(TIP*RATEP-ALP)/TIP
      ALPHA=ALP
      IF(ALPHA.GT.ALPLIM) ALPHA=ALPLIM
      IF(ALPHA.LT.-ALPLIM) ALPHA=-ALPLIM
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C2YAW(BETA,YAW)
C**********************************************************************
C*** * Yaw rate loop for TVC and aero contral
C*** *
C*** * Argument Output:
C*** *                BETA= Sideslip angle - rad
C*** * Argument Input:
C*** *                YAW= Yaw rate input - rad/s
C*** *
C*** * TR=0: detailed rate loop
C*** * TR>0: rate loop represented by time consant TR
C*** *
C*** * MODIFICATION HISTORY
C*** * 990930Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0916),CYBET)
      EQUIVALENCE (C(0978),TVCLIM)
      EQUIVALENCE (C(0979),BETLIM)
      EQUIVALENCE (C(0988),CMDEL)
      EQUIVALENCE (C(0989),RLENG)
      EQUIVALENCE (C(0990),APTVC)
      EQUIVALENCE (C(0991),GA)
      EQUIVALENCE (C(0992),GP)
      EQUIVALENCE (C(0993),PARM)
      EQUIVALENCE (C(0994),AIZ)
      EQUIVALENCE (C(0997),TR)
      EQUIVALENCE (C(0999),FLPLIM)
C
C CYBET = D Lateral force derivative - 1/rad
C TVCLIM = D Nozzle gimbal limiter - rad
C BETLIM = D Sideslip angle limiter - rad
C CMDEL = D Control moment derivative at trim cond.- 1/rad
C RLENG = D Ref length of CMDEL - m
C APTVC = D =0: Aero Control, =1: TVC only, =x: mixture
C GA = D Aero control gain - rad/(rad/s)
C GP = D TVC control gain - rad/(rad/s)
C PARM = D Nozzle moment arm - m
C AIZ = D Transverse moment of inertia - kg*m2
C TR = D/G Rate loop time const (if > 0: simple rate loop) - s
C FLPLIM = D Control surface excursion limiter - rad
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0219),DVBA)
      EQUIVALENCE (C(0220),PDYNMC)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1403),AREA)
      EQUIVALENCE (C(1306),AMASS)
C
C DVBA= O Speed of vehicle wrt air - m/s
C PDYNMC= O Dynamic pressure - Pa
C FTHALT= O Thrust - N
C AREA= D Reference area for aerodynamic data - m^2
C AMASS= O Vehicle mass - kg
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0956),BETD)
      EQUIVALENCE (C(0957),BET)
      EQUIVALENCE (C(0958),RATEYD)
      EQUIVALENCE (C(0959),RATEY)
C
C BET = S Sideslip angle state variable - rad
C RATEY = S yawrate- rad/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0934),AERMY)
      EQUIVALENCE (C(0935),TVCMY)
      EQUIVALENCE (C(0940),ZETA)
      EQUIVALENCE (C(0941),DELR)
C
C AERMY = G Yaw aero control moment - N*m
C TVCMY = G Yaw TVC control moment - N*m
C ZETA = G Yaw nozzle angle - rad
C DELR = G Yaw control surface angle - rad
C
      RATEYC=YAW
C
C*** SIMPLIFIED RATE LOOP
C
      IF(TR.GT.0.) THEN
         RATEYD=(RATEYC-RATEY)/TR
      ELSE
C
C*** DETAILED RATE LOOP
C
         CMOM=PDYNMC*AREA*RLENG*CMDEL
C
         ERATEY=RATEYC-RATEY
         DELR=-ERATEY*(1.-APTVC)*GA
         IF(DELR.GT.FLPLIM) DELR=FLPLIM
         IF(DELR.LT.-FLPLIM) DELR=-FLPLIM
         AERMY=-DELR*CMOM
C
         IF(FTHALT.GT.0) THEN
            ZETA=-ERATEY*APTVC*GP
         ELSE
            ZETA=0.
         ENDIF
         IF(ZETA.GT.TVCLIM) ZETA=TVCLIM
         IF(ZETA.LT.-TVCLIM) ZETA=-TVCLIM
         TVCMY=-ZETA*FTHALT*PARM
C
         AMY=AERMY+TVCMY
         RATEYD=AMY/AIZ
      ENDIF
C
      TIY=DVBA*AMASS/(-PDYNMC*AREA*CYBET+FTHALT)
      BETD=-(TIY*RATEY+BET)/TIY
      BETA=BET
      IF(BETA.GT.BETLIM) BETA=BETLIM
      IF(BETA.LT.-BETLIM) BETA=-BETLIM
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
      DIMENSION VBEL(3),IPL(100),IPLV(100),SBEL(3),TVL(3,3)
     1,ST2EL(3),SBELP(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1665),PSIVLX)
      EQUIVALENCE (C(1666),THTVLX)
      EQUIVALENCE (C(1667),HGMEAN)
      EQUIVALENCE (C(1668),HGBIAS)
C
C HGMEAN = D Mean terrain height (constant) - m
C HGBIAS = D Deterministic variation about HGMEAN - m
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2561),N)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INITIALIZATIONS
C
      EQUIVALENCE (C(0121),ST2EL(1))
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1611),GNDE)
      EQUIVALENCE (C(1612),GNDN)
      EQUIVALENCE (C(1615),HBE)
      EQUIVALENCE (C(1617),GNDTCK)
      EQUIVALENCE (C(1627),TVL(1,1))
      EQUIVALENCE (C(1655),SBELP(1))
      EQUIVALENCE (C(1643),DVBE)
      EQUIVALENCE (C(1647),PSIVL)
      EQUIVALENCE (C(1651),THTVL)
      EQUIVALENCE (C(1670),HBG)
      EQUIVALENCE (C(1686),CLOBN)
      EQUIVALENCE (C(1687),ISETD1)
      EQUIVALENCE (C(1688),TSTART)
C
C ST2EL(3) = I Initialization of launch aircraft (not uesed) - m
C ISETD1 = I Initial integer value for clobber calcualtions - ND
C TSTART = I Initialize clock to bypass clobber counter - ND
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1669),HGE)
C
C*** INITIALIZE STATE VARIABLES
C
	IPL(N)=1642
	IPLV(N)=1643
	N=N+1
	IPL(N)=1646
	IPLV(N)=1647
	N=N+1
	IPL(N)=1650
	IPLV(N)=1651
	N=N+1
C
	DO J=1652,1654
         IPL(N)=J
         IPLV(N)=J+3
         N=N+1
	ENDDO
C
	IPL(N)=1678
	IPLV(N)=1679
	N=N+1
	IPL(N)=1682
	IPLV(N)=1683
	N=N+1
			
C
C*** INITIALIZE MISSILE VELOCITY VECTOR
C
      PSIVL=PSIVLX/CRAD
      THTVL=THTVLX/CRAD
      CALL MATCAR(VBEL,DVBE,PSIVL,THTVL)
      CALL MAT2TR(TVL,PSIVL,THTVL)
C
C*** INITIALIZE STATE VARIABLES
C
	CALL MATEQL(SBELP,SBEL,3,1)
C
C*** LINK LAUNCH AIRCRAFT TO LAUNCH POINT
C
	CALL MATEQL(ST2EL,SBEL,3,1)
C
C*** INITIALIZE GNDTCK
C
      GNDTCK=0.
      GNDE=0.
      GNDN=0.
C
C*** INITIALIZE HBE AND HBG
C
      HBE=-SBEL(3)
      HGE=HGBIAS+HGMEAN
      HBG=HBE-HGE
      ISETD1=1
      CLOBN=0.
      TSTART=0.
      RETURN
      END
C**********************************************************************
      SUBROUTINE D1
C**********************************************************************
C*** * Dynamic Module D1.
C*** * Reserved C(3510) locations are 1600-1699
C*** * This module performs the following functions:
C*** *
C*** * (1) Solves Newton's equations in velocity coordinates
C*** * (2) Caculates missile height above terrain
C*** * (3) Calculates clobber events
C*** * (4) Calculates terrain height for look-ahead terrain following
C*** * (5) Calculates various ground track distances
C*** * (6) Concatinates event flags into EVENT
C*** *
C*** *						
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION SBEL(3),VBEL(3),FSPV(3),TVL(3,3),WVEV(3)
     +,DUM3(3),SBELM(3),SBELPD(3),SBELP(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1673),RCOR)
C
C RCOR = D Correlation length of terrain - m
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(2866),ICOOR)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0400),MSEEK)
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(0900),MAUT)
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1423),FSPV(1))
      EQUIVALENCE (C(1687),ISETD1)
      EQUIVALENCE (C(1688),TSTART)
C
C MSEEK= D/G Seeker mode =1:seeker enable
C MGUID= D =|MROLL|MGUIDL|MGUIDP see Module C1
C MAUT= D Mode Switch MAUT=|MAUTL||MAUTP| (see Module C2)
C MPROP= D/G Propulsion flag =4:thrust for Mach hold
C FSPV= O Specific force in flight path axes - N/kg
C ISETD1= I Initial integer value for clobber calcualtions - ND
C TSTART= I Initialize clock to bypass clobber counter - ND
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1642),DVBED)
      EQUIVALENCE (C(1643),DVBE)
      EQUIVALENCE (C(1646),PSIVLD)
      EQUIVALENCE (C(1647),PSIVL)
      EQUIVALENCE (C(1650),THTVLD)
      EQUIVALENCE (C(1651),THTVL)
      EQUIVALENCE (C(1652),SBELPD(1))
      EQUIVALENCE (C(1655),SBELP(1))
C
C DVBE = S Vehicle speed - m/s
C PSIVL = S Heading angle - rad
C THTVL = S Flight path angle - rad
C SBELP(3) = S Displacement of vehicle wrt to point E - m/s	
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1608),WVEV(1))
      EQUIVALENCE (C(1615),HBE)
      EQUIVALENCE (C(1627),TVL(1,1))
      EQUIVALENCE (C(1670),HBG)
C
C SBEL(3) = O Displmnt of vehicle wrt point E in L coord - m
C VBEL(3) = O Velocity of vehicle wrt earth in L-coord - m
C WVEV(3) = O angular velocity of flight path wrt earth - rad/s
C HBE = O altitude of vehicle above S.L.
C TVL(3,3) = O T.M. of velocity to local level coordinates - ND
C HBG = O Height of vehicle above ground - m
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1601),EVENT)
      EQUIVALENCE (C(1611),GNDE)
      EQUIVALENCE (C(1612),GNDN)
      EQUIVALENCE (C(1617),GNDTCK)
      EQUIVALENCE (C(1621),GNDEX)
      EQUIVALENCE (C(1622),GNDNX)
      EQUIVALENCE (C(1623),GDTCKX)
      EQUIVALENCE (C(1665),PSIVLX)
      EQUIVALENCE (C(1666),THTVLX)
      EQUIVALENCE (C(1669),HGE)
      EQUIVALENCE (C(1671),DBE)
      EQUIVALENCE (C(1686),CLOBN)
C
C EVENT = G =|MSEEK|MGUIDL|MGIDP|MAUTL|MAUTP|MPROP|
C GNDE = G Ground distance east since launch - m	
C GNDN = G Ground distance north since launch - m
C GNDTCK = G Groundtrack since launch - m
C GNDEX = G Ground distance east since launch - nm	
C GNDNX = G Ground distance north since launch - nm
C GDTCKX = G Groundtrack since launch - nm
C PSIVLX = G Heading angle from north - deg
C THTVLX = G Flight path angle - deg
C HGE = G I/G Terrain height above S.L.(look-down) - m
C DBE = G Vehicle slant range wrt to E poiint - m
C CLOBN = G No of crashing events (clobber) - ND
C
      MROLL=INT(MGUID/100.)
      MGUIDL=INT((MGUID-MROLL*100)/10.)
      MGUIDP=MGUID-MGUIDL*10-MROLL*100
C
C*** SAVE VALUES BEFORE INTEGRATION
C
      CALL MATEQL(SBELM,SBEL,3,1)
C
C*** EQUATIONS OF MOTION
C
      DVBED=FSPV(1)-SIN(THTVL)*AGRAV
      PSIVLD=FSPV(2)/(DVBE*COS(THTVL))
      THTVLD=-(FSPV(3)+COS(THTVL)*AGRAV)/DVBE
      CALL MATCAR(VBEL,DVBE,PSIVL,THTVL)
      THTVLX=THTVL*CRAD
      PSIVLX=PSIVL*CRAD
C
      CALL MATEQL(SBELPD,VBEL,3,1)
      CALL MATEQL(SBEL,SBELP,3,1)
      CALL MATABS(DBE,SBEL,3)
C
C*** KINEMATICS
C
      CALL MAT2TR(TVL,PSIVL,THTVL)
      WVEV(1)=-SIN(THTVL)*PSIVLD
      WVEV(2)=THTVLD
      WVEV(3)=COS(THTVL)*PSIVLD
C
C*** ALTITUDE OF MISSILE ABOVE SEA LEVEL
C
      HBE=-SBEL(3)
C
      IF(ICOOR.EQ.0) THEN
C
C***    MISSILE ABOVE STOCHASTIC TERRAIN (HGE), WITH LOOK-DOWN SENSOR
C
         IF(MGUIDP.EQ.1) THEN
            VHOR=SQRT(VBEL(1)*VBEL(1)+VBEL(2)*VBEL(2))
            CALL D1TER(HGE,VHOR)
            HBG=HBE-HGE
C
C***        NUMBER OF TERRAIN IMPACTS: CLOBN
C
            IF(HBG.LE.0.) THEN
               IF(ISETD1.EQ.1) THEN
                  ISETD1=0
                  TSTART=T
                  CLOBN=CLOBN+1.
               ENDIF
            ELSE
               DTIM=T-TSTART
               IF(DTIM.GT.RCOR/VHOR) THEN
                  ISETD1=1
               ENDIF
            ENDIF
         ELSE
            HBG=HBE
         ENDIF
C
C***     CALCULATE TERRAIN FOR LOOK-FOREWARD TERRAIN FOLLOWING
C
         IF(MGUIDP.EQ.2) THEN
            VHOR=SQRT(VBEL(1)*VBEL(1)+VBEL(2)*VBEL(2))
            CALL D1TER(HGE,VHOR)
         ENDIF
C
C***    DISTANCE TRAVELLED ALONG GROUNDTRACK
C
         CALL MATSUB(DUM3,SBEL,SBELM,3,1)
         DUM3(3)=0.
         CALL MATABS(DUM4,DUM3,3)
         GNDTCK=DUM4+GNDTCK
         GNDN=SBEL(1)-SBELM(1)+GNDN
         GNDE=-(SBEL(2)-SBELM(2))+GNDE
C
      ENDIF
C
C*** STATUS INDICATOR
C
      MCTRL=INT(MAUT/100.)
      MAUTL=INT((MAUT-MCTRL*100)/10.)
      MAUTP=MAUT-MAUTL*10-MCTRL*100
      EVENT=100000.*MSEEK+10000.*MGUIDL+1000.*MGUIDP+100.*MAUTL
     &+10.*MAUTP+1.*MPROP
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE D1TER(HGE,VHOR)
C**********************************************************************
C*** * Stochastic Terrain Generator
C*** *
C*** * The terrain above sea level is modeled by a constant value
C*** * HGMEAN(1667), a deterministic function HGBIAS(1668), described
C*** * by a ramp, triangular, or sinusoidal function (Type 11 input),
C*** * and a stochstic part HG(1683). The total terrain height above
C*** * S.L. is HGE(1669). The true altitude of the vehicle above
C*** * terrain is HBG=HBE-HGE. This value is sent to the altimeter
C*** * in Module S4 and is corrupted there by measurement noise and bias.
C*** *
C*** * The stochastic model is a second order auto-correlation function
C*** * driven by white gaussian noise. The RAN(I) function, embedded
C*** * in the Gaussian generator must be seeded by a large odd integer
C*** * ISEED(1675). The follwing parameters describe the roughness
C*** * of the terrain at three typical locations:
C
C Location/roughness|FACTH(1672)|RCOR(1673)|SIGMAH(1674)|Sigma-Slope
C
C Ellsworth,Ka        .726         536.4 m    9.14 m       .02
C     smooth
C Green River,Ut      .444         609.6 m    30.48 m      .075
C     medium
C Black Top Mt,NM     .444         914.4 m    152.4 m      .25
C     rough
C
C*** * The parameters FACTH, RCOR, SIGMAH ar input data for the stochastic
C*** * terrain model.
C*** *
C*** * The number of events of crashing into the terrain during the simulation
C*** * run is recorded by CLOBN(1686). To prevent recording of multiple events
C*** * in rapid succession when the missile flies tangential near ground level
C*** * a threshold equal to the correlation lenght RCOR is implemented.
C*** *
C*** * Argument output:						
C*** *				HGE= I/G Terrain height above S.L.(look-down) - m				
C*** *
C*** * Argument input:
C*** *				VHOR= Horizontal vehicle velocity - m/s
C*** *
C*** *
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1667),HGMEAN)
      EQUIVALENCE (C(1668),HGBIAS)
      EQUIVALENCE (C(1672),FACTH)
	EQUIVALENCE (C(1673),RCOR)
      EQUIVALENCE (C(1674),SIGMAH)
      EQUIVALENCE (C(1675),ISEED)
C
C HGMEAN = D Mean terrain height (constant) - m
C HGBIAS = D Deterministic variation about HGMEAN - m
C FACTH = D Factor of second correlation length - ND
C RCOR = D Terrain correlation length - m
C SIGMAH = D Terrain roughness std deviation - m
C ISEED = D Seed for RAN, large odd integer - ND
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2664),DER)
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1678),HRD)
      EQUIVALENCE (C(1679),HR)
      EQUIVALENCE (C(1682),HGD)
      EQUIVALENCE (C(1683),HG)
C
C HR = S First stochastic variable - m/s
C HG = S Terrain deviation - m
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1684),HGSLOP)
      EQUIVALENCE (C(1685),GAUSS)
C
C HGSLOP = G Terrain slope - rad
C GAUSS = G White gaussian noise - ND
C
C*** WHITE GAUSSIAN NOISE WITH ZERO MEAN
C
      SIGMA=1./SQRT(DER)
      V1=RAN(ISEED)
      V2=RAN(ISEED)
      GAUSS=SIGMA*SQRT(2.*ALOG(1./V1))*COS(6.2831853072*V2)
C
      DUM=SIGMAH*SQRT(2.*(1.+FACTH))/(FACTH*(RCOR/VHOR)**1.5)
      HRD=-HR/(RCOR/VHOR)+DUM*GAUSS
      HGD=-HG/(FACTH*RCOR/VHOR)+HR
      HGSLOP=-HG/(FACTH*RCOR)+HR/VHOR
      HGE=HG+HGBIAS+HGMEAN
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
C*** * (1) Calculates initial direction cosine matrix TBL, TLB
C*** * (2) Initializes DEC=1 for termination codition calculations
C*** *	 (initialized here, since G4 cannot have an initialization module)
C*** *
C*** * MODIFICATION HISTORY
C*** * 990930 Created by Peter Zipfel
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
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1701),TBL(1,1))
      EQUIVALENCE (C(1710),TLB(1,1))
      EQUIVALENCE (C(1771),DEC)
C
C DEC = I aux variable for termination conditions - ND
C
      CALL A3TRA(TBV) ! call to A3TRA in Module A3 (exception to the rule)
      CALL MATMUL(TBL,TBV,TVL,3,3,3)
      CALL MATTRA(TLB,TBL,3,3)
C
      DEC=1.
C
      RETURN
      END
C
C******************************************************************
      SUBROUTINE D2
C******************************************************************
C***  * Kinematic Rotation Module
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Calculates direction cosine matrix TBL
C***  * (2) Calculation of angular velocity of body wrt velocity frame WBVB
C***  *       Skid-to-Turn: ALP,ALPD,BETD from autopilot
C***  *       Bank-to-Turn: ALP,ALPD,PHD from autopilot
C***  * (3) Calculates body rates without solving Euler's equations
C***  * (4) Off-line calculation of Euler angles
C***  *
C***  * MODIFICATION HISTORY
C***  * 910311 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 1700-1749
C
      COMMON C(3510)
C
      DIMENSION TBV(3,3),WVEV(3),TVL(3,3),TBL(3,3),TLB(3,3),WBEB(3)
     +,WBVB(3),DUM3(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1700),MTURN)
C
C MTURN = D =0:skid-to-turn, =1:bank-to-turn
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0926),PHIBV)
      EQUIVALENCE (C(0950),PHD)
      EQUIVALENCE (C(0952),ALPD)
      EQUIVALENCE (C(0953),ALP)
      EQUIVALENCE (C(0956),BETD)
      EQUIVALENCE (C(1413),TBV(1,1))
      EQUIVALENCE (C(1608),WVEV(1))
      EQUIVALENCE (C(1627),TVL(1,1))
C
C PHIBV= O Roll angle about velocity vector - rad
C ALP= S Angle-of-attack state variable - rad
C TBV= O Trans matrix of body wrt flight path axes - ND
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
      IF(MTURN.EQ.0) THEN
C
C*** YAW TO TURN
C
         WBVB(1)=+BETD*SIN(ALP)
         WBVB(2)=ALPD
         WBVB(3)=-BETD*COS(ALP)
C
      ELSE
C
C*** BANK TO TURN
C
         WBVB(1)=PHD*COS(ALP)
         WBVB(2)=ALPD
         WBVB(3)=PHD*SIN(ALP)
C
      ENDIF
C
C*** BODY RATES
C
      CALL MATMUL(DUM3,TBV,WVEV,3,3,1)
      CALL MATADD(WBEB,WBVB,DUM3,3,1)
C
C*** EULER ANGLES
C
      THTBL=ASIN(-TBL(1,3))
      CTHTBL=COS(THTBL)
      DUM=TBL(1,1)/CTHTBL
      IF(ABS(DUM).LE.1.) PSIBL=ACOS(DUM)*SIGN(1.,TBL(1,2))
      DUM=TBL(3,3)/CTHTBL
      IF(ABS(DUM).LE.1.) PHIBL=ACOS(DUM)*SIGN(1.,TBL(2,3))
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
      DIMENSION SBEL(3),STBL(3),ST1EL0(3)	
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0100),MTARG)
      EQUIVALENCE (C(0101),DHTB)
      EQUIVALENCE (C(0102),HTE)
      EQUIVALENCE (C(0103),AZTLX)
C
C MTARG = D =0:Cartesian init, =1:Polar init - ND
C DHTB = D Horizontal range of target wrt launch point - m
C HTE = D Height of target above S.L. - m
C AZTLX = D Target azimuth from north at launch point - deg
C
C*** INPUT FROM D1 MODULE
C
      EQUIVALENCE (C(1602),SBEL(1))
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(0112),ST1EL0(1))
      EQUIVALENCE (C(0130),DBT1)
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
      DIMENSION ST1EL0(3),ST1EL(3),VT1EL(3),DUM3(3),SBEL(3),SBT1L(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0109),VT1EL(1))
      EQUIVALENCE (C(0112),ST1EL0(1))
C
C VT1EL(3) = D Constant target velocity vector - m/s
C ST1EL0(3) = D Initial target position relative to point E - m
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2000),T)
C
C T = E Time since launch - s
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1602),SBEL(1))
C
C SBEL= O Displmnt of vehicle wrt point E in L coord - m
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0115),ST1EL(1))
      EQUIVALENCE (C(0127),SBT1L(1))
      EQUIVALENCE (C(0130),DBT1)
C
C ST1EL(3) = O Target position from ref. point E - m
C SBT1L(3) = O Vehicle wrt target position - m
C DBT1 = O Vehicle wrt target distance - m
C
C*** CALCULATE TARGET POSITION
C
      CALL MATCON(DUM3,T,VT1EL,3,1)
	CALL MATADD(ST1EL,DUM3,ST1EL0,3,1)
C
C*** DIAGNOSTICS
C
      CALL MATSUB(SBT1L,SBEL,ST1EL,3,1)
      CALL MATABS(DBT1,SBT1L,3)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G2I
C**********************************************************************
C*** * The initialization module for the Environmental Module G2
C*** * Reserved C(3510) locations are 200-299
C*** * This module performs the following functions:
C*** *
C*** * (1) Initializes state variables for wind dynamics
C*** *
C*** *    The switch MAIR controls the atmosphere and wind options:
C*** *
C*** *        MAIR=|MATM|MWIND|
C*** *
C*** *             |MATM|= 0 Std. Atmosphere	
C*** *        	       = 1 Tabular Atmosphere
C*** *
C*** *                   |MWIND|= 0 No Wind
C*** *                          = 1 Constant Wind
C*** *        	   	          = 2 Shear Wind
C*** *        	       	      = 3 Tabular Wind
C*** *
C*** * MODIFICATION HISTORY
C*** * 860926 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100)
C
C*** INPUT
C
      EQUIVALENCE (C(0200),MAIR)
C
C*** INPUTS FROM EXECUTIVE
C
      EQUIVALENCE (C(2561),N)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INITIALIZE STATE VARIABLES
C
      MATM=INT(MAIR/10.)
      MWIND=MAIR-MATM*10.
C
      IF(MWIND.NE.0) THEN
C
		DO J=236,238
		   IPL(N)=J
		   IPLV(N)=J+3
		   N=N+1
		ENDDO
      ENDIF
C
      CALL G2
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G2
C**********************************************************************
C*** * Environmental Module G2
C*** * Reserved C(3510) locations are 200-299
C*** * This module performs the following functions:
C*** *
C*** * (1) Envoques standard ARDC or tabular atmosphere
C*** * (2) Introduces winds
C*** *
C*** *
C*** * INSTRUCTIONS:
C
C    This Module supplies the atmospheric data: density, DRHO, speed of
C    sound, VSOUND, atmospheric pressure, PPRES, temprature, RTEMP; and
C    vehicle related data: dynamic pressure, PDYNMC, Mach number, VMACH,
C    vehicle air speed, DVBA. The wind effects are modeled in terms of
C    air mass acceleration which are combined in Module A3 with the
C    aerodynamic and propulsion accelerations to form the total specific
C    force that is acting on the vehicle.
C
C    The switch MAIR controls the atmosphere and wind options:
C
C        MAIR=|MATM|MWIND|
C
C             |MATM|= 0 Std. Atmosphere	
C        	          = 1 Tabular Atmosph.	
C
C                   |MWIND|= 0 No Wind
C                          = 1 Constant Wind
C        	   	             = 2 Shear Wind
C        	       	         = 3 Tabular Wind
C
C
C    Constant wind is input by DVAEL(0209) and PSIWLX(0204); shear wind
C    is provided through low altitude input WALTL(0216), DVAELL(0214)
C    and high altitude input WALTH(0217), DVAELH(0215) with the same
C    heading angle.
C
C    The tabular data starts in the input stream with an '8' in the
C    second column, followed by up to 50 rows of weather records
C    containing each six free field data entries in the following
C    sequence:
C
C        	Altitude		m (S.L.)	ft(S.L.)
C        	Wind Direction	deg			deg
C        	Wind Velocity	m/s			ft/s
C        	Density			kg/m**3		slugs/ft**3
C        	Temperature		deg Cel		deg Far
C        	Pressure		Pascals		lb/ft**2
C
C    The end of the weather data is indicated by a negative altitude
C    value followed by five zeros for the remaining five variables.
C
C*** *
C*** * MODIFICATION HISTORY
C*** * 860926 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
      COMMON /ARDC/ ALT, PRES, RHQ, TEMP, VS
      COMMON /WINDS/ HAER(50), PSIWGR(50), DVAELR(50), RHOR(50)
     1  ,RTEMPR(50), PPRESR(50), NWINDR, RHW
C
      DIMENSION VAEL(3),VAELD(3),VBAG(3),VBEL(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0200),MAIR)
      EQUIVALENCE (C(0204),PSIWLX)
      EQUIVALENCE (C(0208),DVAE3)
      EQUIVALENCE (C(0209),DVAEL)
      EQUIVALENCE (C(0214),DVAELL)
      EQUIVALENCE (C(0215),DVAELH)
      EQUIVALENCE (C(0216),WALTL)
      EQUIVALENCE (C(0217),WALTH)
C
C MAIR = D =|MATM|MWIND| (see Table in Module G2)
C PSIWLX = D Wind direction from north. Use with MWIND=1,2 - deg
C DVAE3 = D Vertical Wind, pos. down. Use with MWIND=1,2,3 - m/s
C DVAEL = D Constant wind magnitude. Use  with I1=1 - m/s
C DVAELL = D Wind at low altitude. Use with MWIND=2 - m/s
C DVAELH = D Wind at high altitude. Use with MWIND=2 - m/s
C WALTL = D Altitude of DVAELL - m
C WALTH = D Altitude of DVAELH - m
C
C*** INPUT  FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0053),OPTMET)
C
C*** INPUT  FROM OTHER MODULES
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0053),OPTMET)
      EQUIVALENCE (C(1214),TRMACH)
      EQUIVALENCE (C(1215),TRDYNM)
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1615),HBE)
C
C TRMACH= I MINIMUM MACH NUMBER (Module G2) - ND
C TRDYNM= I MINIMUM DYNAMIC PRESSURE (Module G2) - Pa
C VBEL= O Velocity of vehicle wrt earth in L-coord - m
C HBE= O altitude of vehicle above S.L.
C
C*** OUTPUTS TO OTHER MODULES
C
      EQUIVALENCE (C(0205),DRHO)
      EQUIVALENCE (C(0206),VSOUND)
      EQUIVALENCE (C(0212),PPRES)
      EQUIVALENCE (C(0213),RTEMP)
      EQUIVALENCE (C(0219),DVBA)
      EQUIVALENCE (C(0220),PDYNMC)
      EQUIVALENCE (C(0221),VMACH)
      EQUIVALENCE (C(1211),TRCODE)
C
C VAEL(3) = O Velocity of air wrt earth in L-axes - m/s
C DRHO = O Air density - kg/m3
C VSOUND = O Sonic speed - m/s
C PPRES = O Atmospheric pressure - Pa
C RTEMP = O Atmospheric temprature - Celsius
C DVBA = O Speed of vehicle wrt air - m/s
C PDYNMC = O Dynamic pressure - Pa
C VMACH = O Mach number
C VAELD(3) = O Air mass acceleration - m/s2
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0236),VAELD(1))
      EQUIVALENCE (C(0239),VAEL(1))
C
C VAELD(3) = S Airmass acceleration - m/s^2
C VAEL(3) = S Airmass velocity - m/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0218),DVW)
C
C DVW = G Wind magnitude interpolated
C
      MATM=INT(MAIR/10.)
      MWIND=MAIR-MATM*10.
C
C*** SELECTION OF ATMOSPHERIC OPTIONS
C
      IF(MATM.EQ.0) THEN
C
C*** STANDARD ATMOSPHERE
C
         ALT = HBE
         IF(OPTMET.LT.1.)THEN
C           CALL ARDCMA
           TEMP=TEMP-460.
           PRES=PRES*144.
         ELSE
           CALL ARDCMM
           TEMP=TEMP-273.
         ENDIF
         VSOUND=VS
         DRHO=RHQ
         RTEMP=TEMP
         PPRES=PRES
      ENDIF
C
      IF(MATM.EQ.1) THEN
C
C*** ATMOSPHERE FROM TYPE 8 INPUTS
C
         HBEH=HBE
         CALL TABLE(HBEH,HAER,RHOR,NWINDR,DRHO)
         CALL TABLE(HBEH,HAER,RTEMPR,NWINDR,RTEMP)
         CALL TABLE(HBEH,HAER,PPRESR,NWINDR,PPRES)
C
         IF(OPTMET.LT.1)THEN
           VSOUND=49.*SQRT(RTEMP+460.)
         ELSE
           VSOUND=20.*SQRT(RTEMP+273.)
         ENDIF
      ENDIF
C
C*** WIND OPTIONS
C
      IF(MWIND.NE.0) THEN
C
         IF(MWIND.EQ.1) THEN
C
C***     CONSTANT WIND
C
            DVW=DVAEL
         ENDIF
C
         IF(MWIND.EQ.2) THEN
C
C***        WIND WITH CONSTANT SHEAR
C
            DVW=DVAELL+(DVAELH-DVAELL)*(HBE-WALTL)/(WALTH-WALTL)
            IF(HBE.LT.WALTL) DVW=0.
            IF(HBE.GT.WALTH) DVW=0.
         ENDIF
C
         IF(MWIND.EQ.3) THEN
C
C***     WIND FROM TYPE 8 INPUTS
C
            HBEH=HBE
            CALL TABLE(HBEH,HAER,PSIWGR,NWINDR,PSIWLX)
            CALL TABLE(HBEH,HAER,DVAELR,NWINDR,DVW)
         ENDIF
C
C***     WIND COMPONENTS
C
         V1 = -DVW * COS(PSIWLX/CRAD)
         V2 = -DVW * SIN(PSIWLX/CRAD)
         V3 = DVAE3
C
C***     TIME RATE OF CHANGE OF WIND AS A FCTN.OF VEHICLE TRAJ.
C***     TIME CONSTANT OF 1. SEC SMOOTHES WIND
C
         VAELD(1)=(V1-VAEL(1))/1.
         VAELD(2)=(V2-VAEL(2))/1.
         VAELD(3)=(V3-VAEL(3))/1.
C
      ENDIF
C
C*** FLIGHT CONDITIONS
C
      CALL MATSUB(VBAG,VBEL,VAEL,3,1)
      CALL MATABS(DVBA,VBAG,3)
      PDYNMC=0.5*DRHO*DVBA*DVBA
C
      IF(VSOUND.LE.1.E-10) THEN
        VMACH=0.
      ELSE
        VMACH=DVBA/VSOUND
      END IF
C
C*** SET TERMINATION FLAGS
C
      IF(VMACH.LT.TRMACH)  TRCODE=2.
      IF(PDYNMC.LT.TRDYNM)  TRCODE=3.
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE ARDCMM
C**********************************************************************
C*** * Standard Atmosphere Subroutine
C*** *
C*** * (1) ARDC Std Atmosphere tables
C*** * (2) Conversion from english to metric units
C*** *
C*** * MODIFICATION HISTORY
C*** * 750101 Eglin AFB standard atmosphere
C*** *
C*** ******************************************************************
C
      COMMON/ARDC/YY,PRES,RHO,TEM,VA
      DIMENSION BY(23),DTH(22),QTMB(22),BTEM(23),BPRES(23)
      DIMENSION TMW(14)
      DATA R,BY,DTH,QTMB,BTEM,BPRES/20855531.,0.,36089.,65617.,104987.,1
     154199.,170604.,200131.,259186.,291152.,295276.,328084.,360892.,393
     2701.,492126.,524934.,557743.,623360.,754593.,984252.,1312336.,1640
     1420.,1968504.,2296588.,-.19812E-02,.0,.3048E-03,.85344E-03,.0,-.60
     196E-03,-.12192E-02,.0,.0,.9144E-03,.1524E-02,.3048E-02,.6096E-02,.
     14572E-02,.3048E-02,.21336E-02,.1524E-02,.12192E-02,.100584E-02,.79
     1248E-03,.51816E-03,.33528E-03,                                   -
     55.2558761,-.00015768841,34.163195,12.201141,-.00012622647,-17.0815
     697,-8.5407987,-.00018911262,-.00018911262,11.387732,6.8326390,3.41
     763195,1.7081597,2.2775463,3.4163195,4.8804564,6.8326390,8.5407987,
     810.352483,13.13969,20.095997,31.057450,288.15,216.65,216.65,228.65
     9,270.65,270.65,252.65,180.65,180.65,180.65,210.65,260.65,360.65,96
     10.65,1110.65,1210.65,1350.65,1550.65,1830.65,2160.65,2420.65,2590.
     165,2700.65,14.695972,3.2825087,.79406678,.12589565,.016085624,.855
     173762E-02,.26411526E-02,.15050764E-03,.23842975E-04,.18798462E-04,
     1.32680828E-05,.76260471E-06,.25147919E-06,.47175393E-07,.33900235E
     1-07,.25251720E-07,.14803047E-07,.57622378E-08,.13959564E-08,.25102
     1062E-09,.56402386E-10,.14418928E-10,.39632286E-11/
      DATA TMW/28.9644,28.88,28.56,28.07,26.92,26.66,26.40,25.85,24.70,2
     12.66,19.94,17.94,16.84,16.17/
C
C   CONVERT H FROM METERS TO FEET
C
      Y = YY/.3048
      IF(Y-295276.)5,10,10
    5 ALT=Y/(1.+Y/R)
      GO TO 15
   10 ALT=Y
   15 DO 20 I=1,23
      IF(ALT-BY(I))25,20,20
   20 CONTINUE
      RHO=0.
      VA=0.
      PRES=0.
      TEM=0.
      RETURN
   25 IF(ALT-295276.)27,27,26
   26 IF(I-11)32,32,31
   31 J=I-1
      GO TO 33
   32 J=I
   33 X0STR=BY(J-1)
      X1STR=BY(J)
      X2STR=BY(J+1)
      Y0STR=TMW(J-10)
      Y1STR=TMW(J-9)
      Y2STR=TMW(J-8)
      COF1=(X2STR-ALT)/(X1STR-X0STR)*((X1STR-ALT)/(X2STR-X0STR))
      COF2=(X0STR-ALT)/(X0STR-X1STR)*((X2STR-ALT)/(X2STR-X1STR))
      COF3=(X0STR-ALT)/(X0STR-X2STR)*((X1STR-ALT)/(X1STR-X2STR))
      WT=COF1*Y0STR+COF2*Y1STR+COF3*Y2STR
      GO TO 29
   27 WT=28.9644
   29 IF(I-11)30,50,50
   30 GO TO (40,50,70,50,50,70,50,50,70,70),I
   50 TEM=BTEM(I-1)+DTH(I-1)*(ALT-BY(I-1))
      PRES=BPRES(I-1)*(BTEM(I-1)/TEM)**QTMB(I-1)
   60 TEM=TEM*1.8
      VA=(49.0221958+.571E-07)*SQRT(TEM)
      RHO=PRES/TEM*(.0838899+.78E-07)
      TEM=(WT*TEM)/28.9644
C
C  CONVERT PRESSURE (PRES) FROM LBS/SQ IN TO PASCALS
C  CONVERT VELOCITY OF SOUND (VA) FROM FT/SEC TO M/SEC
C  CONVERT TEMPERATURE (TEM) FROM DEG RANKINE TO DEG KELVIN
C  CONVERT DENSITY (RHO) FROM SLUGS/CUBIC FT TO KG/CUBIC METER
C
      PRES = PRES*6894.757
      VA = VA*.3048
      TEM = TEM/1.8
      RHO = RHO*515.3788
      RETURN
   70 PRES=BPRES(I-1)*(2.7182818+.285E-07)**((ALT-BY(I-1))*QTMB(I-1)*.30
     148)
      TEM=BTEM(I-1)
      GO TO 60
   40 TEM=BTEM(1)+DTH(1)*ALT
      PRES=BPRES(1)*(BTEM(1)/TEM)**QTMB(1)
      GO TO 60
      END
C**********************************************************************
      SUBROUTINE G4
C**********************************************************************
C*** * End-of-Run Subroutine called by STAGE3, not a module
C*** * Reserved C(3510) locations are 1750-1799
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Stops run
C*** * (2) Concatinates termination conditions and stores them in TRCOND
C*** * (3) Calcuates at waypoint miss distance, navigation and guidance errors
C*** * (4) Calcuates at target miss distance, navigation and guidance errors
C*** *
C*** * NOTE: Target plane established by PSIPLX(1752) and THTPLX(1753)
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      CHARACTER*100 TITLE
      COMMON C(3510)
      COMMON /HCOM/TITLE
C
      DIMENSION ST1EL(3),SBWLC(3),SBEL(3),SBELM(3),SBBML(3),
     +STTML(3),DUM1(3),DUM2(3),SHBML(3),SHEL(3),SHJL(3)
     +,SBWLCM(3),SBBMLC(3),SHBMLC(3),SHILC(3),SBT1L(3),VBEL(3)
     +,SIHLC(3),SIJL(3),SWEL(3),SWBL(3),STTMLC(3)
     +,SBT1LC(3),SWCWL(3),SBT1PM(3),STTMPC(3)
     +,STBMP(3),SBT1P(3),SHTP(3),STTMP(3),SITP(3),SHIP(3),SBBMP(3)
     +,SBT1LM(3),TPL(3,3),SBTP(3),SBTCP(3),STCTP(3),U3P(3),U3L(3)
     +,TLP(3,3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1750),MSTOP)
      EQUIVALENCE (C(1752),PSIPLX)
      EQUIVALENCE (C(1753),THTPLX)
C
C MSTOP = D =0:Nostop, =1:Stop on termination condition
C PSIPLX = D Yaw angle of target plane from "North" - deg
C THTPLX = D Pitch angle of target plane - deg
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0115),ST1EL(1))
      EQUIVALENCE (C(0130),DBT1)
      EQUIVALENCE (C(0401),NMAP)
      EQUIVALENCE (C(0405),NFIX)
      EQUIVALENCE (C(0412),SWEL(1))
      EQUIVALENCE (C(0449),DWB)
      EQUIVALENCE (C(0450),WAY)
      EQUIVALENCE (C(0455),SWBL(1))
      EQUIVALENCE (C(0461),FVYAWM)
      EQUIVALENCE (C(0462),TFVYAW)
      EQUIVALENCE (C(0463),FVPITM)
      EQUIVALENCE (C(0464),TFVPIT)
      EQUIVALENCE (C(0465),FRPSIM)
      EQUIVALENCE (C(0466),TFRPSI)
      EQUIVALENCE (C(0467),FRTHTM)
      EQUIVALENCE (C(0468),TFRTHT)
      EQUIVALENCE (C(0471),ISETG4)
      EQUIVALENCE (C(0731),SBWLC(1))
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(0810),SBT1LC(1))
      EQUIVALENCE (C(1211),TRCODE)
      EQUIVALENCE (C(1220),TMCODE)
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1643),DVBE)
      EQUIVALENCE (C(1665),PSIVLX)
      EQUIVALENCE (C(1666),THTVLX)
      EQUIVALENCE (C(1771),DEC)
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(2020),LCONV)
      EQUIVALENCE (C(2664),DER)
      EQUIVALENCE (C(2866),ICOOR)
C
C ST1EL= O Target position from ref. point E - m
C DBT1= O Vehicle wrt target distance - m
C NMAP= D Scene map sequential number
C NFIX= I/G Current fix number
C SWEL= D Waypoint coordinates - m
C DWB= G True distance between waypoint and missile - m
C WAY= O =0:Waypoint not crossed, =1:Crossed (set in G4)
C SWBL= O True waypoint wrt missile displacement - m
C FVYAWM= G Max yaw half-FOV encountered - rad
C TFVYAW= G Time at which FVYAWM was encountered - s
C FVPITM= G Max pitch half-FOV encountered - rad
C TFVPIT= G Time at which FVPITM was encountered - s
C FRPSIM= G Max yaw half-FOR encountered - rad
C TFRPSI= G Time at which FRPSIM was encountered - s
C FRTHTM= G Max pitch half-FOR encountered - rad
C TFRTHT= G Time at which FRTHTM was encountered - s
C ISETG4= O Flag for  print-out in G4 subroutine - ND
C SBWLC= O Displmt of vehicle wrt stored waypoint (target) - m
C MGUID= D =|MROLL|MGUIDL|MGUIDP see Module C1
C SBT1LC= O Position of vehicle wrt stored target - m
C TRCODE= I/G Interger defining type of termination - ND
C TMCODE= I/G Stores previos termination code - ND
C SBEL= O Displmnt of vehicle wrt point E in L coord - m
C VBEL= O Velocity of vehicle wrt earth in L-coord - m
C DVBE= S Vehicle speed - m/s
C PSIVLX= G Heading angle from north - deg
C THTVLX= G Flight path angle - deg
C DEC= I aux variable for termination conditions - ND
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1754),SBTP(1))
      EQUIVALENCE (C(1757),SBTCP(1))
      EQUIVALENCE (C(1760),STCTP(1))
      EQUIVALENCE (C(1763),EPSVPX)
      EQUIVALENCE (C(1764),DBT)
      EQUIVALENCE (C(1765),DBTC)
      EQUIVALENCE (C(1766),DTCT)
      EQUIVALENCE (C(1772),TRCOND)
      EQUIVALENCE (C(1792),SWCWL(1))
C
C SBTP(3) = G Miss distance in target plane and coor. - m
C SBTCP(3) = G Guidance & control error in target plane - m
C STCTP(3) = G Navigation error in target plane - m
C EPSVPX = G Angle between normal to plane and vel vector- rad
C DBT = G Miss magnitude in target plane - m
C DBTC = G Guidance & control miss mag. in target plane - m
C DTCT = G Navigation miss magnitude in target plane - m
C TRCOND = G Termination condition code (filing in from left)
C SWCWL(3) = G Navigation error at way point - m
C
C*** STOP RUN ON TERMINATION CONDITIONS
C
      IF(TRCODE.NE.TMCODE) THEN
         TRCOND=TRCOND+TRCODE*DEC
         DEC=DEC*10.
         TMCODE=TRCODE
         IF(MSTOP.EQ.1.AND.DBT1.GT.500.) THEN
            WRITE(6,5)T,TRCOND
            LCONV=2
         ENDIF
      ENDIF
C
      IF(DBT1.LE.100.AND.MGUID.GT.32) THEN
C
C***     REFINE DBT1 WITH NEW SBEL CALCULATION
C
         CALL MATSUB(SBT1L,SBEL,ST1EL,3,1)
         CALL MATABS(DBT1,SBT1L,3)
         CALL MAT2TR(TPL,PSIPLX/CRAD,THTPLX/CRAD)
         CALL MATMUL(SBT1P,TPL,SBT1L,3,3,1)
C
         IF(SBT1P(3).GT.0.) THEN
C
C***        END-OF-RUN CALCULATIONS
C
C***        MISS DISTANCE
C
            CALL MATSUB(SBBML,SBEL,SBELM,3,1)
            CALL MATMUL(SBBMP,TPL,SBBML,3,3,1)
            CALL MATSUB(STBMP,SBBMP,SBT1P,3,1)
            WW=STBMP(3)/SBBMP(3)
            CALL MATCON(DUM1,WW,SBBMP,3,1)
            CALL MATSUB(SHTP,DUM1,STBMP,3,1)
            DHT=SQRT(SHTP(1)*SHTP(1)+SHTP(2)*SHTP(2))
            CALL MATEQL(SBTP,SHTP,3,1)
C
C***        NAVIGATION ERROR
C
            CALL MATSUB(STTMLC,SBT1LC,SBT1LM,3,1)
            CALL MATMUL(STTMPC,TPL,STTMLC,3,3,1)
            CALL MATSUB(STTML,SBT1LM,SBT1L,3,1)
            CALL MATMUL(STTMP,TPL,STTML,3,3,1)
            WWW=STTMP(3)/STTMPC(3)
            CALL MATCON(DUM1,WWW,STTMPC,3,1)
            CALL MATSUB(SITP,DUM1,STTMP,3,1)
            DIT=SQRT(SITP(1)*SITP(1)+SITP(2)*SITP(2))
            CALL MATEQL(STCTP,SITP,3,1)
C
C***        GUIDANCE ERROR
C
            CALL MATSUB(SHIP,SHTP,SITP,3,1)
            DHI=SQRT(SHIP(1)*SHIP(1)+SHIP(2)*SHIP(2))
            CALL MATEQL(SBTCP,SHIP,3,1)
C
C***        TIME AT INTERCEPT
C
            EPCHIM=TM+WW*DER
C
C***        HALF CONE ANGLE BETWEEN NORMAL OF TARGET PLANE AND VEL.VEC.
C
            U3P(1)=0.
            U3P(2)=0.
            U3P(3)=1.
            CALL MATTRA(TLP,TPL,3,3)
            CALL MATMUL(U3L,TLP,U3P,3,3,1)
            CALL MATSCA(DUM,VBEL,U3L,3)
            DUMM=DUM/DVBE
            EPSVPX=ACOSD(DUMM)
C
            DBT=DHT
            DBTC=DHI
            DTCT=DIT
C
            LCONV=2
C
            WRITE(6,99) TITLE
            WRITE(6,1) EPCHIM,TRCOND
            WRITE(6,3) DHT,SHTP(1),SHTP(2),EPSVPX
            WRITE(6,6) DHI,SHIP(1),SHIP(2),PSIVLX
            WRITE(6,7) DIT,SITP(1),SITP(2),THTVLX
            WRITE(6,12) FVYAWM,TFVYAW,FVPITM,TFVPIT
            WRITE(6,13) FRPSIM,TFRPSI,FRTHTM,TFRTHT
C
            RETURN
C
         ENDIF
C
C***     SAVE VALUES FOR NEXT INTEGRATION CYCLE
C
         TM=T
         CALL MATEQL(SBT1PM,SBT1P,3,1)
         CALL MATEQL(SBELM,SBEL,3,1)
         CALL MATEQL(SBT1LM,SBT1LC,3,1)
C
         RETURN
C
      ENDIF
C
C*** WAYPOINT MISSDISTANCE CALCULATIONS
C
      IF(DWB.LT.100.AND.ISETG4.EQ.1) THEN
C
C***     CLOSING SPEED ON WAYPOINT
C
         CALL MATSCA(DUM6,VBEL,SWBL,3)
         IF(DWB.NE.0.) CVL=DUM6/DWB
C
         IF(CVL.LT.0.) THEN
C
C***        WAYPOINT CALCULATIONS
C
            CALL MATSUB(SBBML,SBEL,SBELM,3,1)
            CALL MATEQL(DUM1,SBBML,3,1)
            CALL MATSUB(DUM2,SBELM,SWEL,3,1)
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
            CALL MATADD(SHEL,SHBML,SBELM,3,1)
            CALL MATSUB(SHJL,SHEL,SWEL,3,1)
C
C***        DISTANCE AND TIME OF CLOSEST APPROACH
C
            CALL MATABS(DHJ,SHJL,3)
            DHJH=SQRT(SHJL(1)*SHJL(1)+SHJL(2)*SHJL(2))
            EPCHIM=TM+DTIM
C
C***        GUIDANCE ERROR
C
            CALL MATSUB(SBBMLC,SBWLC,SBWLCM,3,1)
            CALL MATCON(SHBMLC,DUM5,SBBMLC,3,1)
            CALL MATADD(SHILC,SHBMLC,SBWLCM,3,1)
            CALL MATABS(DHIC,SHILC,3)
            DHICH=SQRT(SHILC(1)*SHILC(1)+SHILC(2)*SHILC(2))
C
C***        NAVIGATION ERROR
C
            DUM=-1.
            CALL MATCON(SIHLC,DUM,SHILC,3,1)
            CALL MATADD(SIJL,SIHLC,SHJL,3,1)
            CALL MATABS(DIJ,SIJL,3)
            DIJH=SQRT(SIJL(1)*SIJL(1)+SIJL(2)*SIJL(2))
C
            IF(ICOOR.EQ.1) THEN
               ISETG4=0
               WAY=1.
               NFIX=0
               WRITE(6,8) EPCHIM,NMAP
               WRITE(6,9) DHJ,SHJL(1),SHJL(2),SHJL(3)
               WRITE(6,10) DHIC,SHILC(1),SHILC(2),SHILC(3)
               WRITE(6,11) DIJ,SIJL(1),SIJL(2),SIJL(3)
               WRITE(6,14) DHJH,DHICH,DIJH
               WRITE(6,12) FVYAWM,TFVYAW,FVPITM,TFVPIT
               WRITE(6,13) FRPSIM,TFRPSI,FRTHTM,TFRTHT
            ENDIF
C
            CALL MATEQL(SWCWL,SIJL,3,1)
C
            RETURN
C
         ENDIF
C
C***     SAVE VALUES FOR NEXT INTEGRATION CYCLE
C
         TM=T
         CALL MATEQL(SBELM,SBEL,3,1)
         CALL MATEQL(SBWLCM,SBWLC,3,1)
C
      ENDIF
C
      IF(SBEL(3).LE.0.) RETURN
C
C*** IMPACT ON HORIZONTAL PLANE
C
      LCONV=2
      WRITE (6,4)
C
 99   FORMAT(2X,A)
 1    FORMAT(1X,'     PARAMETERS AT IMPACT TIME  T= ',1PG12.5
     +,' TRCOND= ',1PG12.5)
 2    FORMAT(1H ,'    DBW=',1PG12.5,' SBWT11=',1PG12.5,' SBWT12='
     +,1PG12.5,' SBWT13=',1PG12.5)
 3    FORMAT(1H ,'    DBT=',1PG12.5,'  SBTP1=',1PG12.5,'  SBTP2='
     +,1PG12.5,' EPSVPX=',1PG12.5)
 4    FORMAT (1H0,41H  IMPACT OCCURRED IN THE HORIZONTAL PLANE)
 5    FORMAT(1H ,'  RUN STOPPED AT TIME=',1PG12.5,
     +'  WITH CONDITION TRCOND=',1PG12.5)
 6    FORMAT(1H ,'   DBTC=',1PG12.5,' SBTCP1=',1PG12.5,' SBTCP2='
     +,1PG12.5,' PSIVLX=',1PG12.5)
 7    FORMAT(1H ,'   DTCT=',1PG12.5,' STCTP1=',1PG12.5,' STCTP2='
     +,1PG12.5,' THTVLX=',1PG12.5/)
 8    FORMAT(1X,'   TIME OF CLOSEST APPROACH T= ' ,1PG12.5
     +,'    AT WAYPOINT # ',I2)
 9    FORMAT(1H ,'    DBW=',1PG12.5,'  SBWL1=',1PG12.5,'  SBWL2='
     +,1PG12.5,'  SBWL3=',1PG12.5)
 10   FORMAT(1H ,'   DBWC=',1PG12.5,' SBWCL1=',1PG12.5,' SBWCL2='
     +,1PG12.5,' SBWCL3=',1PG12.5)
 11   FORMAT(1H ,'   DWCW=',1PG12.5,' SWCWL1=',1PG12.5,' SWCWL2='
     +,1PG12.5,' SWCWL3=',1PG12.5)
 12   FORMAT(1H ,' FVYAWM=',1PG12.5,' TFVYAW=',1PG12.5,' FVPITM='
     +,1PG12.5,' TFVPIT=',1PG12.5)
 13   FORMAT(1H ,' FRPSIM=',1PG12.5,' TFRPSI=',1PG12.5,' FRTHTM='
     +,1PG12.5,' TFRTHT=',1PG12.5/)
 14   FORMAT(1H ,'   DBWH='1PG12.5,'  DBWCH=',1PG12.5,'  DWCWH='
     +,1PG12.5/)
C
      RETURN
      END
 
C**********************************************************************
      SUBROUTINE S1I
C**********************************************************************
C*** * The initialization module for the Sensor Module S1
C*** * Reserved C(3510) locations are 400-499
C*** * This module performs the following functions:
C*** *
C*** * (1) Initializes flags
C*** *
C*** * MSEEK	=0 sensor absent (default)
C*** *		=1 sensor enabled (input)
C*** *		=2 sensor within range and start of imaging (diagnostic)
C*** *        =3 scene matching process completed (diagnostic)
C*** *		=4 update sent to INS (diagnostic)
C*** *
C*** * NMAP	=0 scene map absent (default)	
C*** *		=1 scene map available (input)
C*** *		=N+1 new map becomes available, (input)
C*** *
C*** * NFIX	=0 no fixes have taken place (default)
C*** *		=N number of fixes on same map (diagnostic)
C*** *		=777 field of view exceeded, no more fixes (diagnostic)
C*** *		=888 field of regard exceeded, no more fixes (diagnostic)
C*** *
C*** * WAY    =0 Waypoint not crossed (initialized in S1I and set to zero in S1)
C*** *		=1 Waypoint crossed (set in G4), used as staging criteria
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0403),NS11)
      EQUIVALENCE (C(0404),NS12)
      EQUIVALENCE (C(0405),NFIX)
      EQUIVALENCE (C(0406),ISETN3)
      EQUIVALENCE (C(0407),ISETN4)
      EQUIVALENCE (C(0450),WAY)
C
C NS11 = I Flag for NMAP
C NS12 = I Flag for NFIX
C NFIX = I/G Current fix number
C ISETN3 = I Flag for EPCHN3
C ISETN4 = I Flag for EPCHN4
C
C*** INITIALIZE SWITCHES
C
      NS11=0
      NS12=0
      NFIX=0
      ISETN3=1
      ISETN4=1
      WAY=0.
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE S1
C**********************************************************************
C*** * Sensor Module S1
C*** * simple imaging LOS sensor with noise corruption
C*** * Reserved C(3510) locations are 400-499
C*** * This module performs the following functions:
C*** *
C*** * MSEEK	=0 sensor absent (default)
C*** *		=1 sensor enabled (input)
C*** *		=2 sensor within range and start of imaging (diagnostic)
C*** *        =3 scene matching process completed (diagnostic)
C*** *		=4 update sent to INS (diagnostic)
C*** *
C*** * NMAP	=0 scene map absent (default)	
C*** *		=1 scene map available (input)
C*** *		=N+1 new map becomes available, (input)
C*** *
C*** * NFIX	=0 no fixes have taken place (default)
C*** *		=N number of fixes on same map (diagnostic)
C*** *		=777 field of view exceeded, no more fixes (diagnostic)
C*** *		=888 field of regard exceeded, no more fixes (diagnostic)
C*** *
C*** * WAY    =0 Waypoint not crossed (initialized in S1I and set to zero in S1)
C*** *		=1 Waypoint crossed (set in G4), used as staging criteria
C*** *
C This subroutine models a los sensor that has the capability to image a scene,
C compare it with a prestored template, and to calculate a position update vecto
C for the INS processor. It models uncertainties that result from the following
C sources: mission planning, INS, mechanical pointing of centerline of the
C sensor, and the scene correlation process.
C
C The mode sequence of the seeker. The sensor is enabeled with
C MSEEK=1. When the sensor comes within the detection range (RACQ) of the target
C MSEEK=2, and the imaging takes place for the duration of DTIMMP seconds.
C The scene matching correlation process (MSEEK=3) is completed
C after DTIMCR seconds and the position update correction is send to the INS
C processor (MSEEK=4). The time for the first fix is therefore DTIMMP+DTIMCR.
C For additional fixes on the same scene the next imaging occurs during
C the current correlation process. Since DTIMCR is usually greater than DTIMMP,
C the second and the following fix duration is given by the correation time
C DTIMCR.
C
C At the scene taking epoch (MSEEK=3) the INS direction cosine matrix and the
C missile position wrt the waypoint are stored. They are used at the update epoc
C (MSEEK=4) to calculate the position update vector from the correlation process
C
C The uncertainties are introduced into the simulation as a mission planning
C error SWRWL(3) that displaces the true waypoint (or target) by the mapping
C erros. The INS uncertainties are input in Module S4. The pointing of the senso
C centerline is affected by INS tilt errors and mechanical and electrical errors
C that influence the pointing of the beam. The INS contribution is
C determined in the S4 Module. The mechanical and electrical errors are
C introduced in subroutine S1LOS as RANDPB in the body yaw plane and RANDTB in
C the body pitch plane. These angular errors are added to the sensor axsis as it
C is pointed by the INS towards the INS stored waypoint at the scene making
C epoch. The correlation uncertainties include such errors as scene coordinate
C transformations, vehicle dynamic effects and processing errors. They are added
C to the sensor axis in the yaw plane, RANDPC, in the pitch plane, RANDTC, and a
C distance to the target, RANDDC, at the update epoch. All errors are modeled as
C Gaussian processes.
C
C The scene mapping process is initiated by setting NMAP=1. The number of
C fixes on the same scene is determined by NFIXM. A new scene is introduced
C by incrementing NMAP.
C
C For diagnostic purposes several error variables are available that are
C projected into the sensor plane normal to the sensor centerline. They are:
C mission planning error EWRWS, INS error EWCWR, Update correction EWAS, and
C pointing error EWCAS.
C
C The maximum absolute value of the half-field-of-view, FVYAWM,FVPITM, and
C half-field-of-regard, FRPSIM, FRTHTM, that are encountered between waypoints
C are displayed on the screen together with their times of occurrences, TFVYAW,
C TFVPIT; TFRPSI, TFRTHT, respectively. If the maximum sensor half-field-of view
C FOVYAW, FOVPIT, is exceeded, NFIX is set to 777 and no additional fixes will
C occur on that particular scene. If the maximum sensor half-field-of regard,
C FORYAW,FORPIT, is exceeded, NFIX is set to 888 and again no futher fixes will
C occur. If the absolute FOV values in a particular engagement never drop below
C FOVYAW or FOVPIT then zeroes will be displayed on the screen.
C
C*** *
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION  SBEL(3),SWEL(3),SWRWL(3),SWREL(3),SWBL(3),TBLC(3,3)
     &,SBWLC(3),SWALC(3),SWBLCN(3),SWBLC(3),SABB(3),TLCBN(3,3),SABLC(3)
     &,SBELM(3),TBLM(3,3),TBL(3,3),TLCLN(3,3),TLCB(3,3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0400),MSEEK)
      EQUIVALENCE (C(0401),NMAP)
      EQUIVALENCE (C(0402),NFIXM)
      EQUIVALENCE (C(0411),RACQ)
      EQUIVALENCE (C(0412),SWEL(1))
      EQUIVALENCE (C(0415),SWRWL(1))
      EQUIVALENCE (C(0440),RANDPC)
      EQUIVALENCE (C(0441),RANDTC)
      EQUIVALENCE (C(0442),RANDDC)
      EQUIVALENCE (C(0469),RMIN)
C
C MSEEK = D/G Seeker mode =1:seeker enable
C NMAP = D Scene map sequential number
C NFIXM = D Maximum number of fixes on same map
C RACQ = D Sensor acquisition range - m
C SWEL(3) = D Waypoint coordinates - m
C SWRWL(3) = D Mission planning error - m
C RANDPC = D Correlation noise on PSI - rad
C RANDTC = D Correlation noise on THT - rad
C RANDDC = D Correlation noise on range to scene - m
C RMIN = D Min range for fixes
C
C*** INPUT FROM INITIALIZATION MODULE
C
      EQUIVALENCE (C(0403),NS11)
      EQUIVALENCE (C(0404),NS12)
      EQUIVALENCE (C(0405),NFIX)
      EQUIVALENCE (C(0406),ISETN3)
      EQUIVALENCE (C(0407),ISETN4)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2000),T)
 
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0715),TBLC(1,1))
      EQUIVALENCE (C(0731),SBWLC(1))
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1701),TBL(1,1))
C
C TBLC= O Comp trans matrix of vehicle wrt earth coor - ND
C SBWLC= O Displmt of vehicle wrt stored waypoint (target) - m
C SBEL= O Displmnt of vehicle wrt point E in L coord - m
C TBL= O T.M. of body wrt local level coordinates - m
C
C*** OUTPUT TO OTHER MODLUES
C
      EQUIVALENCE (C(0409),EPCHN4)
      EQUIVALENCE (C(0418),SWREL(1))
      EQUIVALENCE (C(0443),SWALC(1))
      EQUIVALENCE (C(0450),WAY)
      EQUIVALENCE (C(0455),SWBL(1))
      EQUIVALENCE (C(0471),ISETG4)
C
C EPCHN4 = O Update Epoch - s
C SWREL(3) = O Scene refrence point coordinates - m
C SWALC(3) = O Update correction output to INS - m
C WAY = O =0:Waypoint not crossed, =1:Crossed (set in G4)
C SWBL(3) = O True waypoint wrt missile displacement - m
C ISETG4 = O Flag for  print-out in G4 subroutine - ND
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0410),EPCHN3)
      EQUIVALENCE (C(0423),PSISBX)
      EQUIVALENCE (C(0424),THTSBX)
      EQUIVALENCE (C(0446),DTIMFX)
      EQUIVALENCE (C(0449),DWB)
      EQUIVALENCE (C(0453),PSISB)
      EQUIVALENCE (C(0454),THTSB)
      EQUIVALENCE (C(0461),FVYAWM)
      EQUIVALENCE (C(0463),FVPITM)
      EQUIVALENCE (C(0465),FRPSIM)
      EQUIVALENCE (C(0467),FRTHTM)
C
C EPCHN3 = G Scene taking epoch - s
C PSISBX = G Sensor wrt body yaw angle - deg
C THTSBX = G Sensor wrt body pitch angle - deg
C DTIMFX = G Time between fixes - s
C DWB = G True distance between waypoint and missile - m
C PSISB = G Sensor wrt body yaw angle - rad
C THTSB = G Sensor wrt body pitch angle - rad
C FVYAWM = G Max yaw half-FOV encountered - rad
C FVPITM = G Max pitch half-FOV encountered - rad
C FRPSIM = G Max yaw half-FOR encountered - rad
C FRTHTM = G Max pitch half-FOR encountered - rad
C
      IF(MSEEK.EQ.0) THEN
C
C***     INITIALIZE SBELM AND TBLM
C
         CALL MATEQL(SBELM,SBEL,3,1)
         CALL MATEQL(TBLM,TBL,3,3)
         RETURN
      ENDIF
C
C*** TRUE AND STORED WAYPOINT CALCULATIONS
C
      CALL MATSUB(SWBL,SWEL,SBELM,3,1)
      CALL MATABS(DWB,SWBL,3)
      CALL MATADD(SWREL,SWRWL,SWEL,3,1)
      DUM=-1.
      CALL MATCON(SWBLC,DUM,SBWLC,3,1)
C
      WAY=0.
C
C*** ENTERING ACTIVE SEEKER RANGE
C
      IF(DWB.LT.RACQ) THEN
C
         IF(NMAP.NE.NS11) THEN
            NS11=NMAP
            NFIX=1
            ISETG4=1
            FVYAWM=0.
            FVPITM=0.
            FRPSIM=0.
            FRTHTM=0.
         ENDIF
C
         CALL S1LOS(FVYAWM,FVPITM,FRPSIM,FRTHTM,DWBC,PSISB,THTSB
     &   ,SWBL,DWB,SWBLC,SWREL,SBELM,TBLM,NFIX,NFIXM,MSEEK,RMIN)
C
         IF(NFIX.NE.NS12.AND.NFIX.LE.NFIXM.AND.DWB.GT.RMIN) THEN
C
C***     START OF LASING
C
            NS12=NFIX
            MSEEK=2
            ISETN3=0
            ISETN4=0
            CALL S1EPCH(EPCHN3,EPCHN4,DTIMFX,NFIX)
         ENDIF
C
         IF(T.GE.EPCHN3.AND.ISETN3.EQ.0) THEN
C
C***     SCENE TAKING EPOCH
C
            ISETN3=1
            MSEEK=3
C
C***     STORE: SENSOR CENTERLINE VECTOR POINTED BY INS AND CORRUPTED
C        BY POINTING ERRORS
C
            PSIAB=PSISB
            THTAB=THTSB
            DAB=DWBC
C
C***     STORE: CURRENT INS DIR.COS.MATRIX
C
            CALL MATTRA(TLCB,TBLC,3,3)
            CALL MATEQL(TLCBN,TLCB,3,3)
            CALL MATMUL(TLCLN,TLCBN,TBLM,3,3,3)
C
C***     STORE: TRUE VEHICLE POSITION WRT WAYPOINT IN INS COORDINATES
C
            CALL MATMUL(SWBLCN,TLCLN,SWBL,3,3,1)
         ENDIF
C
         IF(T.GE.EPCHN4.AND.ISETN4.EQ.0) THEN
C
C***     CORRELATION AND UPDATE EPOCH
C
            ISETN4=1
            MSEEK=4
            NS12=0
            NFIX=NFIX+1
C***       CORRELATION ERRORS ADDED
            PSIABK=PSIAB+RANDPC
            THTABK=THTAB+RANDTC
            DABK=DAB+RANDDC
            CALL MATCAR(SABB,DABK,PSIABK,THTABK)
            CALL MATMUL(SABLC,TLCBN,SABB,3,3,1)
C***       POSITION UPDATE OUTPUT FOR INS
            CALL MATSUB(SWALC,SWBLCN,SABLC,3,1)
         ENDIF
      ENDIF
C
C*** STORE: SBEL AND TBL FROM PRIOR INTEGRATION CYCLE
C
      CALL MATEQL(SBELM,SBEL,3,1)
      CALL MATEQL(TBLM,TBL,3,3)
C
C*** DIAGNOSTICS
C
	PSISBX=CRAD*PSISB
	THTSBX=CRAD*THTSB
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE S1LOS(FVYAWM,FVPITM,FRPSIM,FRTHTM,DWBC,PSISB,THTSB
     &,SWBL,DWB,SWBLC,SWREL,SBELM,TBLM,NFIX,NFIXM,MSEEK,RMIN)
C**********************************************************************
C*** * LOS calculations
C*** * This module performs the following functions:
C*** *
C*** * (1) Corrupts boresight with errors
C*** * (2) Calculates update correction
C*** * (3) Calculates errors in plane normal to boresight
C*** * (4) Generates diagnostics when scene is out of FOV and FOR
C*** *
C*** *	Argument Ouput:
C*** *					FVYAWM= Max yaw half-FOV encountered - rad
C*** *					FVPITM= Max pitch half-FOV encountered - rad
C*** *					FRPSIM= Max yaw half-FOR encountered - rad
C*** *					FRTHTM= Max pitch half-FOR encountered - rad
C*** *					DWBC= Computed scene wrt vehicle distance - m
C*** *					PSISB= Sensor wrt body yaw angle - rad
C*** *					THTSB= Sensor wrt body pitch angle - rad
C*** *	Argument Input;	
C*** *					SWBL(3)= True waypoint wrt vehicle location - m					
C*** *					DWB= True distance between waypoint and vehicle - m
C*** *					SWREL(3)= Scene reference point coordiantes - m
C*** *					SBELM(3)= Stored true vehicle coordiantes - m
C*** *					TBLM(3,3)= Stored direction cosine matrix - m
C*** *					NFIX= Current fix number - ND
C*** *					NFIXM= Maximum number of fixes permitted on scene - ND
C*** *					MSEEK= seeker mode switch (used here with MSEEK=1)
C*** *					RMIN= Min range for fixes
C*** *
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION TBLC(3,3),EWRWS(3),EWCWRS(3),EWCWS(3)
     &,EWAS(3),EWCAS(3),SWBLC(3),SWCBB(3),UABS(3),TSB(3,3),TSL(3,3)
     &,SWBL(3),SWBS(3),UWBS(3),SWCBS(3),UWCBS(3),SWREL(3),SWRBL(3)
     &,SWRBS(3),UWRBS(3),SBELM(3),TBLM(3,3),DUMWAS(3),EWRAS(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0421),RANDPB)
      EQUIVALENCE (C(0422),RANDTB)
      EQUIVALENCE (C(0451),FOVYAW)
      EQUIVALENCE (C(0452),FOVPIT)
      EQUIVALENCE (C(0472),FORYAW)
      EQUIVALENCE (C(0473),FORPIT)
C
C RANDPB = D Boresight error in yaw -rad
C RANDTB = D Boresight error in pitch -rad
C FOVYAW = D Half-FOV in yaw - rad
C FOVPIT = D Half-FOV in pitch - rad
C FORYAW = D Half-FOR in yaw - rad
C FORPIT = D Half-FOR in pitch - rad
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0715),TBLC(1,1))
      EQUIVALENCE (C(1216),TRFORY)
      EQUIVALENCE (C(1217),TRFORP)
      EQUIVALENCE (C(1218),TRFOVY)
      EQUIVALENCE (C(1219),TRFOVP)
      EQUIVALENCE (C(2000),T)
C
C TBLC= O Comp trans matrix of vehicle wrt earth coor - ND
C TRFORY= I MAXIMUM YAW FIELD OF REGARD (Module S1) - rad
C TRFORP= I MAXIMUM PITCH FIELD OF REGARD (Module S1) - rad
C TRFOVY= I MAXIMUM YAW FIELD OF VIEW (Module S1) - rad
C TRFOVP= I MAXIMUM PITCH FIELD OF VIEW (Module S1) - rad
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1211),TRCODE)
C
C*** DIAGNOSICS
C
      EQUIVALENCE (C(0425),EWRWS(1))
      EQUIVALENCE (C(0428),EWCWRS(1))
      EQUIVALENCE (C(0431),EWCWS(1))
      EQUIVALENCE (C(0434),EWAS(1))
      EQUIVALENCE (C(0437),EWCAS(1))
      EQUIVALENCE (C(0458),EWRAS(1))
      EQUIVALENCE (C(0462),TFVYAW)
      EQUIVALENCE (C(0464),TFVPIT)
      EQUIVALENCE (C(0466),TFRPSI)
      EQUIVALENCE (C(0468),TFRTHT)
      EQUIVALENCE (C(0470),EWASU)
C
C EWRWS(3) = G Mission planning error in sensor plane - rad
C EWCWRS(3) = G INS wrt map error in sensor plane - rad
C EWCWS(3) = G Nav erro in sensor plane - rad
C EWAS(3) = G Update correction in sensor plane - rad
C EWCAS(3) = G Pointing error in sensor plane - rad
C EWRAS(3) = G Mapping erro in sensor plane - rad
C TFVYAW = G Time at which FVYAWM was encountered - s
C TFVPIT = G Time at which FVPITM was encountered - s
C TFRPSI = G Time at which FRPSIM was encountered - s
C TFRTHT = G Time at which FRTHTM was encountered - s
C EWASU = G =-EWAS(3) - m
C
C*** WAYPOINT SCENE ACQUISITION
C
      CALL MATMUL(SWCBB,TBLC,SWBLC,3,3,1)
      CALL MATPOL(DWBC,PSIWB,THTWB,SWCBB)
C
C*** BORESIGHT ERROR (GIMBAL POINTING ERROR, ANGULAR DIFFRACTION)
C
      PSISB=PSIWB+RANDPB
      THTSB=THTWB+RANDTB
C
C*** DIAGNOSTCS (POJECTING SEVERAL ERRORS ON THE SENSOR PLANE)
C
C*** SEEKER CENTERLINE UNIT VECTOR
      UABS(1)=1.
      UABS(2)=0.
      UABS(3)=0.
C*** TRUE WAYPOINT UNIT VECTOR
      CALL MAT2TR(TSB,PSISB,THTSB)
      CALL MATMUL(TSL,TSB,TBLM,3,3,3)
      CALL MATMUL(SWBS,TSL,SWBL,3,3,1)
      DUM=1./DWB
      CALL MATCON(UWBS,DUM,SWBS,3,1)
C*** INS COMPUTED WAYPOINT UNIT VECTOR
      CALL MATMUL(SWCBS,TSB,SWCBB,3,3,1)
      DUM=1./DWBC
      CALL MATCON(UWCBS,DUM,SWCBS,3,1)
C*** WAYPOINT MISSION PLANNING UNIT VECTOR (MAPPING POINT)
      CALL MATSUB(SWRBL,SWREL,SBELM,3,1)
      CALL MATMUL(SWRBS,TSL,SWRBL,3,3,1)
      CALL MATABS(DWRB,SWRBS,3)
      DUM=1./DWRB
      CALL MATCON(UWRBS,DUM,SWRBS,3,1)
C
C*** UPDATE CORRECTION
      CALL MATSUB(DUMWAS,UWBS,UABS,3,1)
C
      IF(ABS(DUMWAS(2)).LT.FOVYAW.AND.ABS(DUMWAS(3)).LT.FOVPIT) THEN
C
C***    UPDATE CORRECTION
         CALL MATEQL(EWAS,DUMWAS,3,1)
         EWASU=-EWAS(3)
C***    MAPPING ERROR
         CALL MATSUB(EWRWS,UWRBS,UWBS,3,1)
C***    INS ERROR
         CALL MATSUB(EWCWRS,UWCBS,UWRBS,3,1)
C***    NAV ERROR = MAPPING ERROR + INS ERROR
         CALL MATSUB(EWCWS,UWCBS,UWBS,3,1)
C***    BORESIGHT ERROR
         CALL MATSUB(EWCAS,UWCBS,UABS,3,1)
C***    MAP WRT SENSOR ERROR
         CALL MATSUB(EWRAS,UWRBS,UABS,3,1)
C
      ENDIF
      IF(NFIX.GT.NFIXM.AND.MSEEK.EQ.1.OR.DWB.LT.RMIN) THEN
         CALL MATZER(EWAS,3,1)
         EWASU=0.
         CALL MATZER(EWRWS,3,1)
         CALL MATZER(EWCWRS,3,1)
         CALL MATZER(EWCWS,3,1)
         CALL MATZER(EWCAS,3,1)
         CALL MATZER(EWRAS,3,1)
         PSISB=0.
         THTSB=0.
      ENDIF
C
C*** MAXIMUM PITCH AND YAW FOV AND FIELD-OF-REGARD ENCOUNTERED
C
      DEAWS2=ABS(EWAS(2))
      IF(DEAWS2.GT.FVYAWM) THEN
         FVYAWM=DEAWS2
         TFVYAW=T
      ENDIF
C
      DEAWS3=ABS(EWAS(3))
      IF(DEAWS3.GT.FVPITM) THEN
         FVPITM=DEAWS3
         TFVPIT=T
      ENDIF
C
      DPSISB=ABS(PSISB)
      IF(DPSISB.GT.FRPSIM) THEN
         FRPSIM=DPSISB
         TFRPSI=T
      ENDIF
C
      DTHTSB=ABS(THTSB)
      IF(DTHTSB.GT.FRTHTM) THEN
         FRTHTM=DTHTSB
         TFRTHT=T
      ENDIF
C
C*** IF SCENE IS NOT WITHIN FIELD-OF-VIEW OR REGARD NO MORE FIXES
C
      IF(ABS(DUMWAS(2)).GT.FOVYAW.OR.ABS(DUMWAS(3)).GT.FOVPIT) THEN
         NFIX=777
      ENDIF
      IF(ABS(PSISB).GT.FORYAW.OR.ABS(THTSB).GT.FORPIT) THEN
         NFIX=888
      ENDIF
C
C*** TERMINATION CODE, TRCODE, IF FOV OR FOR IS EXCEEDED
C
      IF(ABS(DUMWAS(2)).GT.TRFOVY)TRCODE=4.
      IF(ABS(DUMWAS(3)).GT.TRFOVP)TRCODE=5.
      IF(ABS(PSISB).GT.TRFORY)TRCODE=6.
      IF(ABS(THTSB).GT.TRFORP)TRCODE=7.
      RETURN
      END
C**********************************************************************
	SUBROUTINE S1EPCH(EPCHN3,EPCHN4,DTIMFX,NFIX)
C**********************************************************************
C*** * Epoch Calculations
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates measurement and update epochs
C*** *
C*** * Argument Ouput:
C*** *				EPCHN3= Scene taking epoch - s
C*** *				EPCHN4= Correlation and update Epoch
C*** *				DTIMFX= Time between fixes - s
C*** * Argument Input:
C*** *				NFIX= Current fix number - ND
C*** *
C*** * MODIFICATION HISTORY
C*** * 990929 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0447),DTIMMP)
      EQUIVALENCE (C(0448),DTIMCR)
C
C DTIMMP = D Time duration to image a scene - s
C DTIMCR = D Time duration to correlate a scene - s
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(2000),T)
C
      IF(NFIX.EQ.1) THEN
         EPCHN3=DTIMMP+T
         DTIMFX=DTIMMP+DTIMCR
         EPCHN4=DTIMFX+T
      ELSE
         EPCHN3=T
         DTIMFX=DTIMCR
         EPCHN4=DTIMFX+T
      ENDIF
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE S2I
C**********************************************************************
C*** * Initialization of GPS and Filter Module S2
C*** * Reserved C(3510) locations are 500-599
C*** * This module performs the following functions:
C*** *
C*** * (1) Initializes satellite positions
C*** * (2) Calculates GDOP
C*** * (3) Initializes filter covariance matrix
C*** *
C*** * MODIFICATION HISTORY
C*** * 930317 Created by Peter Zipfel
C*** * 991014 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
      COMMON/FILTINIT/EE,PP,QQ,RR,XH,CFREQM,TGPS
      COMMON/GPS/SGE
C
      DIMENSION PP(17,17),EE(17,17),XH(17)
     &,SGE(3,4),SGEL(3),SBEL(3),UGEL(3)
     &,HGPS(4,4),HGPST(4,4),DUM44(4,4),DUM44I(4,4),QQ(17,17),RR(8,8)
     &,AZGEX(4),ELGEX(4)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0502),MS2PRT)
      EQUIVALENCE (C(0503),AZGEX(1))
      EQUIVALENCE (C(0507),ELGEX(1))
      EQUIVALENCE (C(0511),PSPOS)
      EQUIVALENCE (C(0512),PSVEL)
      EQUIVALENCE (C(0513),PSTIL)
      EQUIVALENCE (C(0514),PSACC)
      EQUIVALENCE (C(0515),PSGYR)
      EQUIVALENCE (C(0516),PSCBI)
      EQUIVALENCE (C(0517),PSCFR)
      EQUIVALENCE (C(0518),FRAPI)
      EQUIVALENCE (C(0519),FRAPA)
      EQUIVALENCE (C(0520),FRAPG)
      EQUIVALENCE (C(0521),FRAPC)
      EQUIVALENCE (C(0551),QSPOS)
      EQUIVALENCE (C(0552),QSVEL)
      EQUIVALENCE (C(0553),QSTIL)
      EQUIVALENCE (C(0554),QSACC)
      EQUIVALENCE (C(0555),QSGYR)
      EQUIVALENCE (C(0556),QSCBI)
      EQUIVALENCE (C(0557),QSCFR)
      EQUIVALENCE (C(0558),FRAQ)
      EQUIVALENCE (C(0559),RSPOS)
      EQUIVALENCE (C(0560),RSVEL)
      EQUIVALENCE (C(0561),FRAR)
C
C AZGEX(4) = D Azimuth angle of four satellites - deg
C ELGEX(4) = D Elevation angle of four satellites - deg
C PSPOS = D Init 1 sig pos values of cov matrix - m
C PSVEL = D Init 1 sig vel values of cov matrix - m/s
C PSTIL = D Init 1 sig tilt values of cov matrix - rad
C PSACC = D Init 1 sig accel values of cov matrix - m/s2
C PSGYR = D Init 1 sig gyro values of cov matrix - rad/s
C PSCBI = D Init 1 sig clock bias value of cov matrix - m
C PSCFR = D Init 1 sig clock freq value of cov matrix - m/s
C FRAPI = D Fractn to mod initial INS cov matrix (1+FRAPI)*PP
C FRAPA = D Fractn to mod initial accel cov matrix (1+FRAPA)*PP
C FRAPG = D Fractn to mod initial gyro cov matrix (1+FRAPG)*PP
C FRAPC = D Fractn to mod initial clock cov matrix (1+FRAPC)*PP
C QSPOS = D 1 sig pos values of process cov matrix - m
C QSVEL = D 1 sig vel values of process cov matrix - m/s
C QSTIL = D 1 sig tilt values of process cov matrix - rad
C QSACC = D 1 sig accel values of process cov matrix - m/s2
C QSGYR = D 1 sig gyro values of process cov matrix - rad/s
C QSCBI = D 1 sig clock bias value of process cov matrix - m
C QSCFR = D 1 sig clock freq value of process cov matrix - m/s
C FRAQ = D Fractn to mod process cov matrix (1+FRAQ)*QQ
C RSPOS = D 1 sig pos value of meas spectral density matrix - m
C RSVEL = D 1 sig vel value of meas spectral density matrix - m/s
C FRAR = D Fractn to mod meas spectral dens matrix (1+FRAR)*RR
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(1602),SBEL(1))
C
C SBEL= O Displmnt of vehicle wrt point E in L coord - m
C
      DATA RGPS/20183E+3/
C
C*** FILTER INITIALIZATION
C
      CALL MATUNI(EE,17)
      CALL MATZER(XH,17,1)
      CFREQM=0.
      TGPS=0.
C
C*** SATELLITE TO EARTH POSITIONS IN L-COORDINATES (NORTH, EAST, DOWN)
C
      DO I=1,4
         ANGL=1.570796+ELGEX(I)/CRAD
         DUM=REARTH*COS(ANGL)
         DGE=DUM+SQRT(DUM**2+RGPS**2-REARTH**2)
         CALL MATCAR(SGEL,DGE,AZGEX(I)/CRAD,ELGEX(I)/CRAD)
         DO J=1,3
            SGE(J,I)=SGEL(J)
         ENDDO
C
C***    GDOP PRE-CALCULATIONS
C
         CALL VECUVC(UGEL,SGEL(1),SGEL(2),SGEL(3))
         DO J=1,3
            HGPS(I,J)=UGEL(J)
         ENDDO
            HGPS(I,4)=1.
      ENDDO
C
C*** DILLUTION OF GEOMETRY
C
      CALL MATTRA(HGPST,HGPS,4,4)
      CALL MATMUL(DUM44,HGPST,HGPS,4,4,4)
      CALL MATINV(DUM44I,DUM44,D,4)
      TDOP2=DUM44I(4,4)
      VDOP2=DUM44I(3,3)
      HDOP2=DUM44I(1,1)+DUM44I(2,2)
      PDOP2=VDOP2+HDOP2
      GDOP2=PDOP2+TDOP2
C
      TDOP=SQRT(TDOP2)
      VDOP=SQRT(VDOP2)
      HDOP=SQRT(HDOP2)
      PDOP=SQRT(PDOP2)
      GDOP=SQRT(GDOP2)
C
C*** COVARIANCE MATRIX INITIALIZATION
C
      CALL MATZER(PP,17,17)
      DO I=1,3
         PP(I,I)=((1.+FRAPI)*PSPOS)**2
         PP(I+3,I+3)=((1.+FRAPI)*PSVEL)**2
         PP(I+6,I+6)=((1.+FRAPI)*PSTIL)**2
         PP(I+9,I+9)=((1.+FRAPA)*PSACC)**2
         PP(I+12,I+12)=((1.+FRAPG)*PSGYR)**2
      ENDDO
      PP(16,16)=((1.+FRAPC)*PSCBI)**2
      PP(17,17)=((1.+FRAPC)*PSCFR)**2
C
C*** PROCESS COVARIANCE MATRIX INITIALIZATION
C
      CALL MATZER(QQ,17,17)
      DO I=1,3
         QQ(I,I)=((1.+FRAQ)*QSPOS)**2
         QQ(I+3,I+3)=((1.+FRAQ)*QSVEL)**2
         QQ(I+6,I+6)=((1.+FRAQ)*QSTIL)**2
         QQ(I+9,I+9)=((1.+FRAQ)*QSACC)**2
         QQ(I+12,I+12)=((1.+FRAQ)*QSGYR)**2
      ENDDO
      QQ(16,16)=((1.+FRAQ)*QSCBI)**2
      QQ(17,17)=((1.+FRAQ)*QSCFR)**2
C
C*** MEASUREMENT NOISE SPECTRAL DENSITY MATRIX INITIALIZATION
C
      CALL MATZER(RR,8,8)
      DO I=1,4
         RR(I,I)=((1.+FRAR)*RSPOS)**2
         RR(I+4,I+4)=((1.+FRAR)*RSVEL)**2
      ENDDO
      IF(MS2PRT.GE.1) THEN
         PRINT*,' $$$$$$$$$$ SATELLITE DILUTION OF PRECISION $$$$$$$$ '
         PRINT*,' $$ TDOP      HDOP      VDOP      PDOP     GDOP  $$$ '
         WRITE(6,89)TDOP,HDOP,VDOP,PDOP,GDOP
         PRINT*,' $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ '
      ENDIF
C
 89   FORMAT(2X,5(1X,1PG9.2))
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE S2
C**********************************************************************
C*** * GPS and Filter Module S2
C*** * Reserved C(3510) locations are 500-599
C*** * This module performs the following functions:
C*** *
C*** * (1) GPS measurements
C*** * Four satellites are modeled at fixed positions over a flat earth
C*** * Pseudo-range measurements are corrupted by ionospheric and tropospheric
C*** * path delays, receiver noise and resolution, receiver dynamic noise,
C*** * clock bias and frequency errors.
C*** * (2) 17 State filter
C*** * States: 3 INS positions, 3 INS velocities, 3 INS tilts, 3 INS accel
C*** * biases, 3 INS gyro biases, 1 clock bias, 1 clock frequency error.
C*** *
C*** * (1) Sets up fundamental matrix for 17 state filter
C*** * (2) Calculates state transition matrix
C*** * (3) User clock dynamic model
C*** * (4) Sets up measurement matrix
C*** * (5) Calculates updates for INS mav solution
C*** *
C*** * MODIFICATION HISTORY
C*** * 930317 Created by Peter Zipfel
C*** * 991014 Updated by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
      COMMON/FILTINIT/EE,PP,QQ,RR,XH,CFREQM,TGPS
      COMMON/GPS/SGE
C
      DIMENSION FF(17,17),TBLC(3,3),TLCB(3,3),FSPCB(3),TVL(3,3)
     &,FSPLC(3),FDT(17,17),FDT2(17,17),EE(17,17),PHI(17,17),QDT(17,17)
     &,DUM17(17),DUMY17(17),XH(17),QQ(17,17),PP(17,17),RR(8,8)
     &,DUMY1717(17,17),COVPL(3,3),COVPV(3,3),COV23(2,2),SGE(3,4)
     &,SGEL(3),SBEL(3),SGBL(3),SGBLC(3),BRPAT(4),RRREC(4),DUM1717(17,17)
     &,UGBL(3),VBEL(3),RDDYN(4)
     &,SBELC(3),UGBLC(3),VBELC(3),HH(8,17),GK(17,8),ZZ(8)
     &,GUSTTCL(3),GUVBEL(3),GURECEL(3),GUWBEL(3),GUFSPL(3),GUWBEB(3)
     &,GUFSPB(3),DVBGR(4),DVBGC(4)
     &,ESTTC(3)
C
      DOUBLE PRECISION DSBEL(3),DSGEL(3),DSGBL(3),DSBELC(3),DDUM
     &,DDGBR(4),DDGBC(4),DSGBLC(3),DDGB,DBRPAT,DRRREC,DCBIAS
     &,DHH(8,17),DPP(17,17),DRR(8,8),DHT(17,8),DGK(17,8)
     &,DDET,DB178(17,8),DB88(8,8),DBY88(8,8)
 
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0500),MGPS)
      EQUIVALENCE (C(0502),MS2PRT)
      EQUIVALENCE (C(0522),TC)
      EQUIVALENCE (C(0523),BRPAT(1))
      EQUIVALENCE (C(0527),RRREC(1))
      EQUIVALENCE (C(0531),RDDYN(1))
      EQUIVALENCE (C(0535),CBIAS)
      EQUIVALENCE (C(0536),CFREQ)
      EQUIVALENCE (C(0543),DTIMGPS)
C
C MGPS = D/G =0:no GPS, =1:GPS enbld, =2:Update (reset=1 in S4)
C MS2PRT = D =0: no print-out; =1:diagonals; =2: detailed print
C TC = D Clock freq error correlation time constant - s
C BRPAT(4) = D Bias of range caused by path delay - m
C RRREC(4) = D Rand range err of receiv caused by noise & resol - m
C RDDYN(4) = D Rand d-range err of receiver dynamic noise - m/s
C CBIAS = D/G Bias range err of clock (TYPE 3 Gauss) - m
C CFREQ = D Rand d-range clock frequency error (TYPE 11)- m/s
C DTIMGPS = D GPS update interval - s
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0703),VBELC(1))
      EQUIVALENCE (C(0715),TBLC(1,1))
      EQUIVALENCE (C(0727),TANLAT)
      EQUIVALENCE (C(0734),FSPCB(1))
      EQUIVALENCE (C(0752),ESTTC(1))
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1627),TVL(1,1))
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(2664),DER)
      EQUIVALENCE (C(2866),ICOOR)
C
C VBELC= O Computed body vel in earth coor - m/s
C TBLC= O Comp trans matrix of vehicle wrt earth coor - ND
C TANLAT= I Tangent of latitude angle of body at release - None
C FSPCB= O Computed specific force on body - N/kg
C ESTTC= O INS pos error - m
C SBEL= O Displmnt of vehicle wrt point E in L coord - m
C VBEL= O Velocity of vehicle wrt earth in L-coord - m
C TVL= O T.M. of velocity to local level coordinates - ND
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0580),GUSTTCL(1))
      EQUIVALENCE (C(0583),GUVBEL(1))
      EQUIVALENCE (C(0586),GURECEL(1))
      EQUIVALENCE (C(0589),GUFSPB(1))
      EQUIVALENCE (C(0592),GUWBEB(1))
C
C GUSTTCL(3) = O GPS Position update - m
C GUVBEL(3) = O GPS Velocity update - m/s
C GURECEL(3) = O GPS Tilt update - rad
C GUFSPB(3) = O GPS accel bias update in body coor - m/s2
C GUWBEB(3) = O GPS gyro bias update in body coor - m/s2
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0511),PSPOS)
      EQUIVALENCE (C(0512),PSVEL)
      EQUIVALENCE (C(0513),PSTIL)
      EQUIVALENCE (C(0514),PSACC)
      EQUIVALENCE (C(0515),PSGYR)
      EQUIVALENCE (C(0516),PSCBI)
      EQUIVALENCE (C(0517),PSCFR)
      EQUIVALENCE (C(0565),ELMA)
      EQUIVALENCE (C(0566),ELMI)
      EQUIVALENCE (C(0567),ELPHIX)
      EQUIVALENCE (C(0568),GUCBIAS)
      EQUIVALENCE (C(0569),GUCFREQ)
      EQUIVALENCE (C(0570),UCFREQ)
      EQUIVALENCE (C(0571),ZZ(1))
C
C PSPOS = D/G 1 sig pos values of cov matrix - m
C PSVEL = D/G 1 sig vel values of cov matrix - m/s
C PSTIL = D/G 1 sig tilt values of cov matrix - rad
C PSACC = D/G 1 sig accel values of cov matrix - m/s2
C PSGYR = D/G 1 sig gyro values of cov matrix - rad/s
C PSCBI = D/G 1 sig clock bias value of cov matrix - m
C PSCFR = D/G 1 sig clock freq value of cov matrix - m/s
C ELMA = G 1Sig major axis of err ell. in plane norm to vel - m
C ELMI = G 1Sig minor axis of err ell. in plane norm to vel - m
C ELPHIX = G Angle of maj axis wrt horizontal 2-V axis - deg
C GUCBIAS = G Update of clock bias - m
C GUCFREQ = G Update of clock frequency - m/s
C UCFREQ = G Updated clock frequency
C ZZ(8) = G Measurement residuals - m, m/s
C
C*** COMPUTED VEHICLE LOCATION WRT E REFERENCE POINT
C
      CALL MATADD(SBELC,ESTTC,SBEL,3,1)
C
C*** FILTER ENTRY NOT AT PREDICTOR LOOP
C
      IF(ICOOR.EQ.0) RETURN
C
C*** STATE MATRIX FF (17x17)
C
      CALL MATTRA(TLCB,TBLC,3,3)
      CALL MATMUL(FSPLC,TLCB,FSPCB,3,3,1)
      CALL MATZER(FF,17,17)
      FF(6,3)=2.*AGRAV/REARTH
      FF(7,5)=1./REARTH
      FF(8,4)=-1./REARTH
      FF(9,5)=-TANLAT/REARTH
      FF(1,4)=1.
      FF(2,5)=1.
      FF(3,6)=1.
      FF(4,8)=-FSPLC(3)
      FF(4,9)=FSPLC(2)
      FF(5,7)=FSPLC(3)
      FF(5,9)=-FSPLC(1)
      FF(6,7)=-FSPLC(2)
      FF(6,8)=FSPLC(1)
C
      DO 10 I=1,3
      DO 10 J=1,3
      FF(3+I,9+J)=TLCB(I,J)
 10   FF(6+I,12+J)=TLCB(I,J)
C
      FF(16,17)=1.
      FF(17,17)=-1./TC
C
C*** STATE TRANSITION MATRIX
C
      CALL MATCON(FDT,DER,FF,17,17)
      CALL MATMUL(DUM1717,FDT,FDT,17,17,17)
      CALL MATCON(FDT2,.5,DUM1717,17,17)
      CALL MATADD(DUM1717,EE,FDT,17,17)
      CALL MATADD(PHI,DUM1717,FDT2,17,17)
C*** STATE ESTIMATE EXTRAPOLATION
      CALL MATMUL(DUM17,PHI,XH,17,17,1)
      CALL MATEQL(XH,DUM17,17,1)
C*** COVARIANCE ESTIMATE EXTRAPOLATION
      CALL MATCON(QDT,DER/2.,QQ,17,17)
      CALL MATADD(DUM1717,PP,QDT,17,17)
      CALL MATTRF(DUMY1717,PHI,DUM1717,17)
      CALL MATADD(PP,DUMY1717,QDT,17,17)
C
C*** DIAGNOSTICS: ONE SIGMA VALUES
C
      PSPOS=SQRT(PP(1,1))
      PSVEL=SQRT(PP(4,4))
      PSTIL=SQRT(PP(7,7))
      PSACC=SQRT(PP(10,10))
      PSGYR=SQRT(PP(13,13))
      PSCBI=SQRT(PP(16,16))
      PSCFR=SQRT(PP(17,17))
C
C*** DIAGNOSTICS: 1SIGMA POSITON ERROR ELLIPSE IN PLANE
C    NORMAL TO VELOCITY VECTOR
C
      DO I=1,3
         DO J=1,3
            COVPL(I,J)=PP(I,J)
         ENDDO
      ENDDO
      CALL MATTRF(COVPV,TVL,COVPL,3)
      COV23(1,1)=COVPV(2,2)
      COV23(2,2)=COVPV(3,3)
      COV23(1,2)=COVPV(2,3)
      COV23(2,1)=COV23(1,2)
      CALL CADELP(AA,AI,ELPHI,COV23)
      ELMA=SQRT(AA)
      ELMI=SQRT(AI)
      ELPHIX=ELPHI*CRAD
C
C*** USER CLOCK DYNAMIC MODEL: CFREQ must be TYPE 11 CARD, CBIAS initialized
C    as a TYPE 3 Gaussian CARD; trapezoidal integration.
C
      UCFREQ=CFREQ-GUCFREQ
      CBIAS=CBIAS+((UCFREQ)+CFREQM)*DER/2.
      CFREQM=UCFREQ
C
C*** GPS ENABLED (IF GPS NOT ENABLED, RETURN)
C
      IF(MGPS.EQ.0) RETURN
C
C*** GPS PSEUDO RANGE AND DELTA RANGE, MEASUREMENT RESID AND OBS MATRIX
C
      CALL MATZER(HH,8,17)
      DO I=1,4
         DO J=1,3
            SGEL(J)=SGE(J,I)
         ENDDO
         CALL DMATDBL(DSBEL,SBEL,3,1)
         CALL DMATDBL(DSGEL,SGEL,3,1)
         CALL DMATSUB(DSGBL,DSGEL,DSBEL,3,1)
C***    TRUE RANGE
         CALL DMATABS(DDGB,DSGBL,3)
C***    PSEUDO-RANGE (MEASURED RANGE)
         DBRPAT=DBLE(BRPAT(I))
         DRRREC=DBLE(RRREC(I))
         DCBIAS=DBLE(CBIAS)
         DDGBR(I)=DDGB+DBRPAT+DRRREC+DCBIAS
C***    TRUE RANGE-RATE
         CALL DMATSGL(SGBL,DSGBL,3,1)
         CALL VECUVC(UGBL,SGBL(1),SGBL(2),SGBL(3))
         CALL MATSCA(DVBG,VBEL,UGBL,3)
C***    DELTA RANGE (MEASURED RANGE-RATE)
         DVBGR(I)=DVBG+RDDYN(I)+UCFREQ
C***    INS DERIVED RANGE MEASUREMENT
         CALL DMATDBL(DSGEL,SGEL,3,1)
         CALL DMATDBL(DSBELC,SBELC,3,1)
         CALL DMATSUB(DSGBLC,DSGEL,DSBELC,3,1)
         CALL DMATABS(DDUM,DSGBLC,3)
         DDGBC(I)=DDUM
C***    INS DERIVED RANGE-RATE MEASUREMENT
         CALL DMATSGL(SGBLC,DSGBLC,3,1)
         CALL VECUVC(UGBLC,SGBLC(1),SGBLC(2),SGBLC(3))
         CALL MATSCA(DUM,VBELC,UGBLC,3)
         DVBGC(I)=DUM
C***    MEASUREMENT RESIDUALS
         ZZ(I)=SNGL(DDGBR(I)-DDGBC(I))
         ZZ(I+4)=DVBGR(I)-DVBGC(I)
C***    OBSERVATION MATRIX
         DO J=1,3
            HH(I,J)=UGBLC(J)
            HH(I+4,J+3)=UGBLC(J)*DTIMGPS
         ENDDO
         HH(I,16)=1.
         HH(I+4,17)=DTIMGPS
      ENDDO
C
C*** GPS UPDATE (IF 'NOT UPDATE' RETURN)
C
      IF((T-TGPS).GE.DTIMGPS) MGPS=2
      IF(MGPS.LT.2) RETURN
      TGPS=T
C
C*** FILTER GAIN CALCULATION IN DOUBLE PRECISION
C*** INPUT CONVERSION TO DOUBLE PRECISION
      CALL DMATDBL(DHH,HH,8,17)
      CALL DMATDBL(DPP,PP,17,17)
      CALL DMATDBL(DRR,RR,8,8)
C*** GAIN CALCULATION
      CALL DMATTRA(DHT,DHH,8,17)
      CALL DMATMUL(DB178,DPP,DHT,17,17,8)
      CALL DMATMUL(DB88,DHH,DB178,8,17,8)
      CALL DMATADD(DBY88,DB88,DRR,8,8)
      CALL DMATINV(DB88,DBY88,DDET,8)
      CALL DMATMUL(DGK,DB178,DB88,17,8,8)
C*** STATE CORRECTION
      CALL DMATSGL(GK,DGK,17,8)
      CALL MATMUL(DUM17,GK,ZZ,17,8,1)
      CALL MATADD(DUMY17,XH,DUM17,17,1)
      CALL MATEQL(XH,DUMY17,17,1)
C*** COVARIANCE CORRECTION
      CALL MATMUL(DUM1717,GK,HH,17,8,17)
      CALL MATSUB(DUMY1717,EE,DUM1717,17,17)
      CALL MATMUL(DUM1717,DUMY1717,PP,17,17,17)
      CALL MATEQL(PP,DUM1717,17,17)
C*** UPDATE TO INS
      DO I=1,3
         GUSTTCL(I)=XH(I)
         GUVBEL(I)=XH(I+3)
         GURECEL(I)=XH(I+6)
         GUFSPL(I)=XH(I+9)
         GUWBEL(I)=XH(I+12)
      ENDDO
      CALL MATMUL(GUWBEB,TBLC,GUWBEL,3,3,1)
      CALL MATMUL(GUFSPB,TBLC,GUFSPL,3,3,1)
C*** CLOCK UPDATE
      GUCBIAS=XH(16)
      GUCFREQ=XH(17)
C*** CLOCK ERROR BIAS UPDATE
      CBIAS=CBIAS-GUCBIAS
C
      IF(MS2PRT.GE.1) THEN
         PRINT*,' $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ '
         PRINT*,' $$$$$$$$ GPS UPDATE EPOCH    TIME = ',T
         PRINT*,' $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$ '
         IF(MS2PRT.EQ.2) THEN
            PRINT*,' ****** F MATRIX OF INS ************** FF ='
            WRITE(6,90)((FF(I,J),J=1,9),I=1,9)
            PRINT*,' ** COV MAT OF INS AFTER UPDATE ****** PP ='
            WRITE(6,90)((PP(I,J),J=1,9),I=1,9)
            PRINT*,' ***** DETERMINANT OF INVERSE = ',DDET
         ENDIF
         PRINT*,' ** COV MAT DIAG 1-9 LOCATIONS (INS)     ='
         WRITE(6,90)(PP(I,I),I=1,9)
         PRINT*,' ** COV MAT DIAG 10-17 LOC (ACC,GYRO,CBIAS,CFREQ)  ='
         WRITE(6,91)(PP(I,I),I=10,17)
         PRINT*,' ***** MEASUREMENT RESIDUALS ********* ZZ ='
         WRITE(6,91)(ZZ(I),I=1,8)
         PRINT*,' ********** INS UPDATES ********** XH(1-9)='
         WRITE(6,90)(XH(I),I=1,9)
         PRINT*,' ********* OTHER UPDATES ******* XH(10-17)='
         WRITE(6,91)(XH(I),I=10,17)
      ENDIF
C
 90   FORMAT(9(1X,1PG9.2))
 91   FORMAT(8(1X,1PG9.2))
C
C*** RESET STATE
      CALL MATZER(XH,17,1)
C
      RETURN
      END
 
C**********************************************************************
      SUBROUTINE S4I
C**********************************************************************
C*** * Initialization of INS Module S4
C*** * Reserved C(3510) locations are 700-799
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Identifies variables for integration
C*** * (2) Initializes INS error states using the Cholesky method
C*** *
C*** *  MINS= 0: Perfect INS, no update from sensors is accepted
C*** *      = 1: Instrumentation error and transfer alignment
C*** *      = 2: Simple Doppler aiding, maintains constant vel error
C*** *
C*** * MODIFICATION HISTORY
C*** * 991004 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),VBELC(3)
     +,VBEL(3),IPLV(100),EVBE(3),ESTTC(3)
     +,RECE(3),PP0(9,9),APP0(9,9),GAUSS(9)
     +,XX0(9),DUM9(9)
     +,SBELC(3),SBEL(3),SBWLC(3),SWREL(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0700),MINS)
      EQUIVALENCE (C(0701),FRAX)
C
C MINS = D INS mode=0, No INS; =1, INS only; =2, Doppler Aid INS
C FRAX = D Fractn to modify init INS err state: XXO=XXO(1+FRAX)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0418),SWREL(1))
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(2561),N)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C SWREL= O Scene refrence point coordinates - m
C SBEL= O Displmnt of vehicle wrt point E in L coord - m
C VBEL= O Velocity of vehicle wrt earth in L-coord - m
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(0703),VBELC(1))
      EQUIVALENCE (C(0740),RECE(1))
      EQUIVALENCE (C(0746),EVBE(1))
      EQUIVALENCE (C(0752),ESTTC(1))
      EQUIVALENCE (C(0731),SBWLC(1))
C
C VBELC(3) = O Computed body vel in earth coor - m/s
C RECE(3) = S INS tilt errors - rad
C EVBE(3) = S INS vel error - m/s
C ESTTC(3) = S INS pos error - m
C
C*** INITIAL COVARIANCE MATRIX AFTER TRANSFER ALIGNMENT FROM A GPS
C    EQUIPPED AIRCRAFT. UNITS: METER, METER/SEC, MILLI-RAD.
C
      DATA ((PP0(I,N),N=1,9),I=1,9)/
 
     & 20.701,     0.12317,    0.10541,
     & 6.3213E-02, 2.2055E-03, 1.7234E-03,
     & 1.0633E-03, 3.4941E-02,-3.5179E-02,
 
     & 0.12317,    20.696,    -0.27174,
     & 4.8366E-03, 5.9463E-02,-1.3367E-03,
     &-3.4903E-02, 2.6112E-03,-4.2663E-02,
 
     & 0.10541,   -0.27174,    114.12,
     & 5.6373E-04,-8.3147E-03, 5.4059E-02,
     & 1.5496E-02, 7.6463E-02,-3.5302E-03,
 
     & 6.3213E-02, 4.8366E-03, 5.6373E-04,
     & 1.9106E-03, 8.0945E-05, 1.9810E-06,
     & 2.5755E-04, 2.8346E-03,-5.6482E-04,
 
     & 2.2055E-03, 5.9463E-02,-8.3147E-03,
     & 8.0945E-05, 1.7201E-03,-1.5760E-05,
     &-2.8341E-03, 2.6478E-04,-1.0781E-03,
 
     & 1.7234E-03,-1.3367E-03, 5.4059E-02,
     & 1.9810E-06,-1.5760E-05, 3.0070E-03,
     & 4.1963E-04,-1.3297E-04, 4.1190E-05,
 
     & 1.0638E-03,-3.4903E-02, 1.5496E-02,
     & 2.5755E-04,-2.8341E-03, 4.1963E-04,
     & 5.4490E-02,-1.8695E-03, 8.9868E-04,
 
     & 3.4941E-02, 2.6112E-03, 7.6463E-02,
     & 2.8346E-03, 2.6478E-04,-1.3297E-04,
     &-1.8695E-03, 5.2819E-02, 1.0990E-02,
 
     &-3.5179E-02,-4.2663E-02,-3.5302E-03,
     &-5.6482E-04,-1.0781E-03, 4.1190E-05,
     & 8.9868E-04, 1.0990E-02, 0.1291/
C
      CALL S4ALT
C
C*** INITIALIZE STATE VARIABLE VECTORS
C
      ILOC=737
      DO I=0,2
         IPL(N)=ILOC+I
         IPLV(N)=ILOC+I+3
         N=N+1
      ENDDO
C
      ILOC=743
      DO I=0,2
         IPL(N)=ILOC+I
         IPLV(N)=ILOC+I+3
         N=N+1
      ENDDO
C
      ILOC=749
      DO I=0,2
         IPL(N)=ILOC+I
         IPLV(N)=ILOC+I+3
         N=N+1
      ENDDO
C
      IF(MINS.EQ.0) THEN
C
C***     OUTPUT WITHOUT INS ERRORS
C
         CALL MATEQL(SBELC,SBEL,3,1)
         CALL MATSUB(SBWLC,SBEL,SWREL,3,1)
         CALL MATEQL(VBELC,VBEL,3,1)
C
      ELSE
C
C***     ERROR VARIABLE INITIALIZATION
C
         CALL MATCHO(APP0,PP0,9)
C
         DO I=1,100
            DISCARD=FNGAUS(0.,1.)
         ENDDO
         DO I=1,9
            GAUSS(I)=FNGAUS(0.,1.)
         ENDDO
C
         CALL MATMUL(DUM9,APP0,GAUSS,9,9,1)
         CALL MATCON(XX0,(1.+FRAX),DUM9,9,1)
         ESTTC(1)=XX0(1)
         ESTTC(2)=XX0(2)
         ESTTC(3)=XX0(3)
         EVBE(1)=XX0(4)
         EVBE(2)=XX0(5)
         EVBE(3)=XX0(6)
         RECE(1)=XX0(7)*.001
         RECE(2)=XX0(8)*.001
         RECE(3)=XX0(9)*.001
C
C***     VELOCITY INITIALIZATION
C
         CALL MATADD(VBELC,EVBE,VBEL,3,1)
C
C***     POSITION INITIALIZATION
C
         CALL MATADD(SBELC,ESTTC,SBEL,3,1)
         CALL MATSUB(SBWLC,SBELC,SWREL,3,1)
      ENDIF
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE S4
C**********************************************************************
C*** * INS Module
C*** * Reserved C(3510) locations are 700-799
C*** * This subroutine performs the following functions:
C*** *
C*** *  MINS= 0: Perfect INS, no update from sensors is accepted
C*** *      = 1: Instrumentation error and transfer alignment
C*** *      = 2: Simple Doppler aiding, maintains constant vel error
C*** *
C*** * MODIFICATION HISTORY
C*** * 991004 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION FSPLC(3),FSPCB(3),FSPB(3),TLB(3,3),EWBEL(3),EWBEB(3)
     +,RECED(3),RECE(3),RERE(3,3),DUM2(3,3),TLLC(3,3)
     +,TBL(3,3),TBLC(3,3),TLCB(3,3),VBEL(3),VBELC(3),EF(3)
     +,DUM3(3),WBEB(3),WBECB(3),EFSPB(3),EFSPLC(3),EVBED(3)
     +,SWALC(3),ESTTC(3),ESTTCD(3),USTTCL(3),EVBE(3),UVBEL(3)
     +,URECEL(3),TVL(3,3),STCTV(3)
     +,GUSTTCL(3),GUVBEL(3),GURECEL(3),GUFSPB(3),GUWBEB(3)
     +,SWREL(3),SBEL(3),SBELC(3),SBWLC(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0700),MINS)
      EQUIVALENCE (C(0727),TANLAT)
C
C MINS = D INS mode=0, No INS; =1, INS only; =2, Doppler Aided INS
C TANLAT = I Tangent of latitude angle of body at release - None
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0400),MSEEK)
      EQUIVALENCE (C(0418),SWREL(1))
      EQUIVALENCE (C(0443),SWALC(1))
      EQUIVALENCE (C(0500),MGPS)
      EQUIVALENCE (C(0580),GUSTTCL(1))
      EQUIVALENCE (C(0583),GUVBEL(1))
      EQUIVALENCE (C(0586),GURECEL(1))
      EQUIVALENCE (C(0589),GUFSPB(1))
      EQUIVALENCE (C(0592),GUWBEB(1))
      EQUIVALENCE (C(0637),USTTCL(1))
      EQUIVALENCE (C(0640),UVBEL(1))
      EQUIVALENCE (C(0643),URECEL(1))
      EQUIVALENCE (C(1400),FSPB(1))
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1627),TVL(1,1))
      EQUIVALENCE (C(1643),DVBE)
      EQUIVALENCE (C(1701),TBL(1,1))
      EQUIVALENCE (C(1710),TLB(1,1))
      EQUIVALENCE (C(1719),WBEB(1))
      EQUIVALENCE (C(2000),T)
C	
C MSEEK= D/G Seeker mode =1:seeker enable
C SWREL= O Scene refrence point coordinates - m
C SWALC= O Update correction output to INS - m
C MGPS= D/G =0:no GPS, =1:GPS enbld, =2:Update (reset=1 in S4)
C GUSTTCL= O GPS Position update - m
C GUVBEL= O GPS Velocity update - m/s
C GURECEL= O GPS Tilt update - rad
C GUFSPB= O GPS accel bias update in body coor - m/s2
C GUWBEB= O GPS gyro bias update in body coor - m/s2
C FSPB= O Specific force in body coor - N/kg
C SBEL= O Displmnt of vehicle wrt point E in L coord - m
C VBEL= O Velocity of vehicle wrt earth in L-coord - m
C TVL= O T.M. of velocity to local level coordinates - ND
C DVBE= S Vehicle speed - m/s
C TBL= O T.M. of body wrt local level coordinates - m
C TLB= O T.M. of local level wrt body coordinates - m
C WBEB= O Body rates in body coordinates - rad/s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0703),VBELC(1))
      EQUIVALENCE (C(0706),WBECB(1))
      EQUIVALENCE (C(0715),TBLC(1,1))
      EQUIVALENCE (C(0730),DVBEC)
      EQUIVALENCE (C(0731),SBWLC(1))
      EQUIVALENCE (C(0734),FSPCB(1))
C
C VBELC(3) = O Computed vel of vehicle in earth coor - m/s
C WBECB(3) = O Computed ang vel of vehicle wrt earth - rad/s
C TBLC(3,3) = O Comp trans matrix of vehicle wrt earth coor - ND
C SBWLC(3) = O Displmt of vehicle wrt stored waypoint (target) - m
C DVBEC = G Computed body speed wrt earth - m/s
C FSPCB(3) = O Computed specific force on body - N/kg
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0737),RECED(1))
      EQUIVALENCE (C(0740),RECE(1))
      EQUIVALENCE (C(0743),EVBED(1))
      EQUIVALENCE (C(0746),EVBE(1))
      EQUIVALENCE (C(0749),ESTTCD(1))
      EQUIVALENCE (C(0752),ESTTC(1))
C
C RECE(3) = S INS tilt errors - rad
C EVBE(3) = S INS vel error - m/s
C ESTTC(3) = S INS pos error - m
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0724),EWBEB(1))
      EQUIVALENCE (C(0770),STCTV(1))
      EQUIVALENCE (C(0773),EFSPB(1))
C
C EWBEB(3) = G Error in angular vel of body wrt earth - rad/s
C STCTV(3) = G Total nav error in vel coor - m
C EFSPB(3) = G Error in specific force on body in body coor - N/kg
C
      CALL S4ALT
C
      IF(MINS.EQ.0) THEN
C
C***     OUTPUT WITHOUT INS ERRORS
C
         CALL MATEQL(TBLC,TBL,3,3)
         CALL MATEQL(FSPCB,FSPB,3,1)
         CALL MATEQL(WBECB,WBEB,3,1)
         CALL MATEQL(SBELC,SBEL,3,1)
         CALL MATSUB(SBWLC,SBEL,SWREL,3,1)
         CALL MATEQL(VBELC,VBEL,3,1)
         DVBEC=DVBE
C
      ELSE
C
C***     OUTPUT WITH INS ERRORS
C
C***    CALL INSTRUMENT MEASUREMENTS
C
         CALL S4ACCL(FSPCB,EFSPB)
         CALL S4GYRO(EWBEB)
C
C***    GPS UPDATES OF INSTRUMENT BIASES
C
         IF(MGPS.GE.1) THEN
            CALL MATSUB(EFSPB,EFSPB,GUFSPB,3,1)
            CALL MATSUB(EWBEB,EWBEB,GUWBEB,3,1)
         ENDIF
C
C***     ATTITUDE ERROR EQUATIONS
C
         CALL MATMUL(EWBEL,TLB,EWBEB,3,3,1)
         RECED(1)=EWBEL(1)+EVBE(2)/REARTH
         RECED(2)=EWBEL(2)-EVBE(1)/REARTH
         RECED(3)=EWBEL(3)-EVBE(2)*TANLAT/REARTH
C
C***     TRANSFORMATION MATRICES
C
         CALL MATSKS(RERE,RECE)
         CALL MATUNI(DUM2,3)
         CALL MATADD(TLLC,RERE,DUM2,3,3)
         CALL MATMUL(TBLC,TBL,TLLC,3,3,3) !ok
C
C***     VELOCITY AND POSITION ERROR EQUATIONS
C
         CALL MATTRA(TLCB,TBLC,3,3)
         CALL MATMUL(FSPLC,TLCB,FSPCB,3,3,1)
         CALL MATMUL(DUM3,RERE,FSPLC,3,3,1)
         CALL MATMUL(EFSPLC,TLCB,EFSPB,3,3,1)
         CALL MATSUB(EF,EFSPLC,DUM3,3,1) !ok
C
C*** FOR MINS=2(DOPPLER AIDED) INITIAL VEL.ERROR HELD CONSTANT
C
         IF(MINS.EQ.1) THEN
            EVBED(1)=EF(1)
            EVBED(2)=EF(2)
            EVBED(3)=EF(3)+2.*AGRAV*ESTTC(3)/REARTH
         ENDIF
C
         CALL MATEQL(ESTTCD,EVBE,3,1)
C
C***     INS UPDATE
C
C
C***       UPDATE FROM SENSOR DIRECTLY
C
         IF(MSEEK.EQ.4.) THEN
            MSEEK=1
            CALL MATSUB(ESTTC,ESTTC,SWALC,3,1)
C***       SET DERIVATIVES OF INS ERROR STATES TO ZERO
		  DO I=737,739
		    C(I)=0.
		  ENDDO
		  DO I=743,745
		    C(I)=0.
		  ENDDO
		  DO I=749,751
		    C(I)=0.
		  ENDDO
	   ENDIF
C
C***    GPS UPDATE OF NAVIGATION SOLUTION
C
         IF(MGPS.EQ.2) THEN
            MGPS=1
            CALL MATSUB(ESTTC,ESTTC,GUSTTCL,3,1)
            CALL MATSUB(EVBE,EVBE,GUVBEL,3,1)
            CALL MATSUB(RECE,RECE,GURECEL,3,1)
            DO I=737,739
              C(I)=0.
            ENDDO
            DO I=743,745
              C(I)=0.
            ENDDO
            DO I=749,751
              C(I)=0.
            ENDDO
         ENDIF
C
C***    POS OF VEHICLE WRT PROCESSOR STORED TARGET.
C       INCLUDES INS AND TARGETING ERRORS.
C       SENT TO GUIDANCE MODULE C1.
C
	   CALL MATADD(SBELC,ESTTC,SBEL,3,1) !NEW
         CALL MATSUB(SBWLC,SBELC,SWREL,3,1) !NEW
C
C*** INS VELOCITY VECTORS
C
         CALL MATADD(VBELC,EVBE,VBEL,3,1)
         CALL MATABS(DVBEC,VBELC,3)
C
      ENDIF
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE S4GYRO(EWBEB)
C**********************************************************************
C*** * Gyro Cluster
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Introduces gyro errors: Random walk
C*** *                             Gyro cluster misalignment
C*** *                             Scale factor error
C*** *                             Bias error
C*** *                             Mass unbalence
C*** * (2) Outputs gyro measurements of body angular velocities
C*** *
C*** * Argument Output:
C*** *
C*** *          EWBEB(3)=Three gyro errors - rad/s
C*** *
C*** * MODIFICATION HISTORY
C*** * 991004Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION EWBEB(3),EUNBG(3),EMISG(3),ESCALG(3),EBIASG(3)
     +,WBEB(3),FSPB(3),WBECB(3),EUG(3),EMISCG(3),EGB(3,3)
     +,EMSBG(3),EWALKG(3),DUM1(3),EWG(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0788),EUNBG(1))
      EQUIVALENCE (C(0791),EMISG(1))
      EQUIVALENCE (C(0794),ESCALG(1))
      EQUIVALENCE (C(0797),EBIASG(1))
      EQUIVALENCE (C(0755),EWALKG(1))
C
C EUNBG(3) = G Gyro mass unbalance - (rad/s)/(m/s2)
C EMISG(3) = G Gyro cluster misalignment - rad
C ESCALG(3) = G Gyro scale factor error - parts
C EBIASG(3) = G Gyro bias - rad/s
C EWALKG(3) = G Random walk - rad/sqrt(s)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1719),WBEB(1))
      EQUIVALENCE (C(1400),FSPB(1))
      EQUIVALENCE (C(2664),DER)
      EQUIVALENCE (C(2000),T)
C
C WBEB= O Body rates in body coordinates - rad/s
C FSPB= O Specific force in body coor - N/kg
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0706),WBECB(1))
C
C WBECB(3) = O Computed angular vel of body wrt earth - rad/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0761),EUG(1))
      EQUIVALENCE (C(0764),EMISCG(1))
      EQUIVALENCE (C(0767),EWG(1))
C
C EUG(3) = G Gyro spin axis accel sensitivity - rad/s
C EMISCG(3) = G Gyro miss alignment and scale factor errors - rad/s
C EWG(3) = G Gyro random walk errors - rad/s
C
C*** GYRO CLUSTER MISALIGNMENT AND SCALE FACTOR ERRORS
C
      CALL MATSKS(EGB,EMISG)
      EGB(1,1)=ESCALG(1)
      EGB(2,2)=ESCALG(2)
      EGB(3,3)=ESCALG(3)
      CALL MATMUL(EMISCG,EGB,WBEB,3,3,1)
      CALL MATADD(EMSBG,EBIASG,EMISCG,3,1)
C
C*** GYRO SPIN AXIS ACCELERATION SENSITIVITY (MASS UNBALANCE)
C
      EUG(1)=EUNBG(1)*FSPB(1)
      EUG(2)=EUNBG(2)*FSPB(2)
      EUG(3)=EUNBG(3)*FSPB(3)
C
C*** RANDOM WALK ERRORS
C
      CALL MATCON(EWG,1./SQRT(DER),EWALKG,3,1)
C
C*** TOTAL GYRO OUTPUT UNCERTAINTIES
C
      CALL MATADD(DUM1,EMSBG,EUG,3,1)
C
      CALL MATADD(EWBEB,DUM1,EWG,3,1)
C
C*** GYRO MEASURED BODY RATES
C
      CALL MATADD(WBECB,WBEB,EWBEB,3,1)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE S4ACCL(FSPCB,EFSPB)
C**********************************************************************
C*** * Accelerometer Cluster
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Introduces accelerometer errors: Bias
C*** *                                      Random bias
C*** *                                      Scale factor error
C*** *                                      Accelerometer cluster misalignment
C*** * (2) Outputs acceleration measurements in body axes
C*** *
C*** * Argument Output
C*** *
C*** *          FSPBCB(3)=Three accelerometer outputs, in body axes - m/s^2
C*** *          EFSPB(3)=Three accel output errors - m/s^2
C*** *
C*** * MODIFICATION HISTORY
C*** * 991004 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION EMISA(3),ESCALA(3),EBIASA(3),FSPB(3),EFSPB(3)
     +,EAB(3,3),DUM3(3),FSPCB(3),DUM4(3),EWALKA(3)
C
C*** INNPUT DATA
C
      EQUIVALENCE (C(0779),EMISA(1))
      EQUIVALENCE (C(0782),ESCALA(1))
      EQUIVALENCE (C(0785),EBIASA(1))
      EQUIVALENCE (C(0758),EWALKA(1))
C
C EMISA(3) = D Accel cluster misalign - rad
C ESCALA(3) = D Accel scale factor error - parts
C EBIASA(3) = D Accel bias - m/s2
C EWALKA(3) = D Accel random walk - m/s2
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1400),FSPB(1))
      EQUIVALENCE (C(2000),T)
C
C FSPB= O Specific force in body coor - N/kg
C
      CALL MATSKS(EAB,EMISA)
      EAB(1,1)=ESCALA(1)
      EAB(2,2)=ESCALA(2)
      EAB(3,3)=ESCALA(3)
      CALL MATMUL(DUM3,EAB,FSPB,3,3,1)
      CALL MATADD(EFSPB,EBIASA,DUM3,3,1)
C
C*** RANDOM WALK ERRORS
C
      CALL MATADD(DUM4,EFSPB,FSPB,3,1)
      CALL MATADD(FSPCB,EWALKA,DUM4,3,1)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE S4ALT
C**********************************************************************
C*** * Altimeter
C*** * This subroutine performs the following functions:
c*** *
C*** * (1) Intruduces altimeter errors: Bias
C*** *                                  Noise
C*** * (2) Outputs altimeter measurement above sea level
C*** *
C*** * MODIFICATION HISTORY
C*** * 991004 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0712),BIASAL)
      EQUIVALENCE (C(0713),RANDAL)
C
C BIASAL = D Bias of altimeter measurement - m
C RANDAL = D Noise of altimeter measurement - m
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0885),HBGS)
      EQUIVALENCE (C(0889),MGUIDP)
      EQUIVALENCE (C(1670),HBG)
C
C HBGS= O Height AGL projected TLEAD sec ahead - m
C MGUIDP= O Part of MGUID=|MROLL|MGUIDP|MGUIDL|
C HBG= O Height of vehicle above ground - m
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0702),HBGM)
      EQUIVALENCE (C(0714),EHBE)
C
C HBGM = O Computed body alt above terrain - m
C EHBE = O Altimeter error - m
C
      EHBE=+BIASAL+RANDAL
      IF(MGUIDP.EQ.2) THEN
         HBGM=HBGS+EHBE
      ELSE
         HBGM=HBG+EHBE
      ENDIF
C
      RETURN
      END
C***************************** DUMMY RETURNS **************************
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
	SUBROUTINE S3I
	RETURN
	END
	SUBROUTINE S3
	RETURN
	END
	SUBROUTINE S5I
	RETURN
	END
	SUBROUTINE S5
	RETURN
	END

