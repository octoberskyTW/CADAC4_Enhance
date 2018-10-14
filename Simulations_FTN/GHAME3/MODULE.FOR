C**********************************************************************
C************************ MODULES FOR GHAME3 **************************
C**********************************************************************
C*** *
C*** * Calling sequence of Modules:
C*** *   G2   ENVIRONMENT
C*** *   A2   PROPULSION 
C*** *   A1   AERODYNAMICS
C*** *   A3   FORCES
C*** *   D1   NEWTONS LAW
C*** * with dummy RETURNs for unused modules
C*** *
C*** * MODIFICATION HISTORY
C*** * 000419 Version 1.0 Created by Peter Zipfel
C*** *
C**********************************************************************
C**********************************************************************
      SUBROUTINE A1I
C**********************************************************************
C*** * Initialization of Aerodynamic Module A1.
C*** * Reserved C(3510) locations are 1200-1299
C*** * (1) Initializes placeholder index for table look-up
C*** *
C*** * MODIFICATION HISTORY
C*** * 991122 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INITIALIZATION
C
	EQUIVALENCE (C(1210),IX1)
C
C IX1 = I placeholder for table look-up - ND
C
	IX1=1
C
	RETURN
	END
C**********************************************************************
      SUBROUTINE A1
C**********************************************************************
C*** * The Aerodynamic Module A1.
C*** * Reserved C(3510) locations are 1200-1299
C*** * Ref: "Handbook of Intelligent Control", Chapter 11 "Flight
C*** *       Propulsion, and Thermal Control of Advanced Aircraft and
C*** *       Hypersonic Vehicles", Edited by D.A.White and D.A. Sofge,
C*** *       Van Nostrand Reinhold,New York, NY, 1992
C*** *
C*** * This module performs the following functions:
C*** * (1) Provides the aerodynamic tables of hypersonic vehicle GHAME
C*** *     Ref Area REFA= 557.42 m^2
C*** *     Ref Lengths REFB= 24.38m, REFC= 22.86 m
C*** *     Moment coefficients referenced to fixed c.g.
C*** * (2) Calculates the drag polar CD(M)=CD0(M)+K(M)*(CL-CL0)^2
C*** *
C*** * Table Look-up variables:
C*** * VMACHTBL =Vector of mach values
C*** * CD0TBL=Vector of Cd0 values associated with table
C*** * CL0TBL=Vector of Cl0 values associated with table
C*** * KKKTBL=Vector of induced k-values associated with table
C*** * CLATBL=Vector of lift slope values associated with table - 1/deg
C*** * MX1=Dimension of data
C*** *
C*** * MODIFICATION HISTORY
C*** * 991122 Created by Peter Zipfel
C*** * 000107 Corrected error in CD calculation, PZi
C*** * 000112 Changed CL calculation, PZi
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION VMACHTBL(13),CD0TBL(13),CL0TBL(13),CKKTBL(13),CLATBL(13)
     &,CLA0TBL(13)	
C
      PARAMETER(MX1=13)
C
C***  INPUT DATA
C
      EQUIVALENCE (C(1203),ALPHAX)
C
C ALPHAX = D Angle of attack - deg
C
C*** INITIALIZATION
C
	EQUIVALENCE (C(1210),IX1)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2000),T)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0206),VMACH)
C
C VMACH= O Mach number of rocket - ND
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1201),CD)
      EQUIVALENCE (C(1202),CL)
C
C CD = O Drag coefficient - ND
C CL = O Lift coefficient - ND
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1204),CD0)
      EQUIVALENCE (C(1205),CL0)
      EQUIVALENCE (C(1206),CKK)
      EQUIVALENCE (C(1207),CLA)
      EQUIVALENCE (C(1208),CLOVERCD)
C
C CD0 = G Zero lift drag coefficient - ND
C CL0 = G Lift at minimum drag coefficient - ND
C CKK = G Induced drag factor - ND
C CLA = G Lift slope - 1/deg
C CLOVERCD = G Lift over drag - ND
C
 
      DATA VMACHTBL/.4,.6,.8,.9,.95,1.05,1.2,1.5,2.,3.,6.,12.,24./
      DATA CD0TBL/.0340,.0337,.0382,.0477,.0600,.0827,.0816,.0780,
     &.0689,.0503,.0347,.0252,.0209/
      DATA CL0TBL/.0195,.0363,.0177,.0060,.0190,.0191,.0038,.0143,
     &.0149,.0106,.0059,.0020,.0018/
      DATA CKKTBL/.3492,.4136,.4356,.4410,.4229,.4190,.3894,.4574,
     &.5586,.8381,1.2739,1.7523,2.0709/
	DATA CLA0TBL/-.0418,-.0424,-.0454,-.0412,-.0371,-.0313,-.0409,
     &-.0401,-.0327,-.0283,-.0257,-.0240,-.0200/
	DATA CLATBL/.0304,.0311,.0320,.0324,.0353,.0362,.0350,.0300,
     &.0241,.0166,.0109,.0077,.0064/
C
C*** FIND THE DRAG POLAR COEFFICIENTS FOR CURRENT MACH
C
	CALL TBLP1(VMACH,IX1,VMACHTBL,MX1,CD0TBL,CD0,DD0DUM)
	CALL TBLP1(VMACH,IX1,VMACHTBL,MX1,CL0TBL,CL0,DL0DUM)
	CALL TBLP1(VMACH,IX1,VMACHTBL,MX1,CKKTBL,CKK,DKKDUM)
	CALL TBLP1(VMACH,IX1,VMACHTBL,MX1,CLATBL,CLA,DLADUM)
	CALL TBLP1(VMACH,IX1,VMACHTBL,MX1,CLA0TBL,CLA0,DLA0DUM)
C
C*** CALCULATE THE DRAG FORCE
C
	CL=CLA0+CLA*ALPHAX
      CD=CD0+CKK*(CL-CL0)**2
	CLOVERCD=CL/CD
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A2I
C**********************************************************************
C*** * Initialization of Propulsion Module A2.
C*** * Reserved C(3510) locations are 1300-1399
C*** * Ref: "Handbook of Intelligent Control", Chapter 11 "Flight
C*** *       Propulsion, and Thermal Control of Advanced Aircraft and
C*** *       Hypersonic Vehicles", Edited by D.A.White and D.A. Sofge,
C*** *       Van Nostrand Reinhold,New York, NY, 1992
C*** * Type: turbojet 0-2 Mach
C*** *       ramjet   2-6 Mach
C*** *       scramjet 6-
C*** * Mass properties: take-off mass 136,077 kg
C*** *                  total fuel mass 81,646 kg
C*** *
C*** * MPROP=0 No thrusting
C*** *      =1 Throttle command input
C*** *      =2 Autothrottle (maintain dynamic pressure)
C*** *
C*** * This module performs the following functions:
C*** * (1) Provides propulsion deck (capture area ratio CA and ISP)
C*** * (2) Initializes vehicle mass properties
C*** * (3) Sets up fuel mass integration variable
C*** *
C*** * MODIFICATION HISTORY
C*** * 991122 Created by Peter H Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
	COMMON /PROPULSION/TBLCA(117),TBLSPI(117)
     &,ALPHAXI(9),VMACHI(13),THRI(9)
C
      DIMENSION IPL(100),IPLV(100)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C NIP = E Number of variables to integrate
C IPL(100) = E Start of derivitave c-array locations
C IPLV(100) = E Start of state c-array locations
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(1301),IP1)
      EQUIVALENCE (C(1302),IP2)
      EQUIVALENCE (C(1303),IT2)
      EQUIVALENCE (C(1306),FMASS0)
      EQUIVALENCE (C(1307),VMASS0)
      EQUIVALENCE (C(1309),VMASS)
C
C IP1 = I Initialize propulsion table pointer to 1 - ND
C IP2 = I Initialize propulsion table pointer to 1 - ND
C IT2 = I Initialize propulsion table pointer to 1 - ND
C FMASS0 = I Initial fuel mass - kg
C VMASS0 = I Take-off gross mass - kg
C
C  MACH (13) across: .4 .6 .8 .9 .95 1.05 1.2 1.5 2. 3. 6. 12. 24. (first variab
C  AOA (9) down (deg): -3. 0. 3. 6. 9. 12. 15. 18. 21. (second variable for CA)
C  THRT (9) down: 0. .25 .5 .75 1. 1.25 1.5 1.75 2. (second variable for ISP)
C
      DATA VMACHI/.4,.6,.8,.9,.95,1.05,1.2,1.5,2.,3.,6.,12.,24./
      DATA ALPHAXI/-3.,0.,3.,6.,9.,12.,15.,18.,21./
	DATA THRI/0.,.25,.5,.75,1.,1.25,1.5,1.75,2./
C
C*** Propulsion tables based on NASA's GHAME model
C
C
C*** Capture area ratio
c
      DATA TBLCA /
     1 1.09449, 0.53018, 0.31459, 0.26226, 0.24452, 0.22157, 0.20981,
     2 0.23464, 0.34159, 0.62377, 1.45141, 2.76052, 4.68122,
     1 1.18766, 0.58304, 0.35204, 0.29597, 0.27698, 0.25238, 0.23978,
     2 0.26638, 0.38098, 0.68332, 1.57007, 2.97269, 5.03057,
     1 1.28082, 0.63590, 0.38950, 0.32969, 0.30943, 0.28319, 0.26975,
     2 0.29813, 0.42037, 0.74286, 1.68873, 3.18486, 5.37993,
     1 1.37399, 0.68875, 0.42696, 0.36341, 0.34188, 0.31401, 0.29973,
     2 0.32987, 0.45975, 0.80240, 1.80739, 3.39702, 5.72929,
     1 1.46715, 0.74161, 0.46441, 0.39713, 0.37433, 0.34482, 0.32970,
     2 0.36162, 0.49914, 0.86194, 1.92605, 3.60919, 6.07865,
     1 1.56032, 0.79447, 0.50187, 0.43085, 0.40679, 0.37563, 0.35967,
     2 0.39337, 0.53852, 0.92148, 2.04471, 3.82136, 6.42801,
     1 1.65348, 0.84732, 0.53933, 0.46457, 0.43924, 0.40644, 0.38964,
     2 0.42511, 0.57791, 0.98102, 2.16336, 4.03352, 6.77737,
     1 1.74664, 0.90018, 0.57678, 0.49829, 0.47169, 0.43726, 0.41962,
     2 0.45686, 0.61729, 1.04057, 2.28202, 4.24569, 7.12673,
     1 1.83981, 0.95304, 0.61424, 0.53201, 0.50414, 0.46807, 0.44959,
     2 0.48860, 0.65668, 1.10011, 2.40068, 4.45786, 7.47609/
C
C*** Specific impulse - sec
C
      DATA TBLSPI /
     1    0.0000,    0.0000,    0.0000,    0.0000,    0.0000,    0.0000,
     2    0.0000,    0.0000,    0.0000,    0.0000,    0.0000,    0.0000,
     3    0.0000,
     1 1693.1500, 1693.1500, 1686.9000, 1682.2125, 1679.0875, 1661.9000,
     2 1636.9000, 1568.1500, 1443.1500, 1568.1500, 1318.1500,  755.6500,
     3  505.6500,
     1 2262.3999, 2262.3999, 2253.5625, 2246.9343, 2242.5156, 2218.2126,
     2 2182.8625, 2085.6499, 1908.9000, 2085.6499, 1732.1499,  936.7750,
     3  583.2750,
     1 2699.6499, 2699.6499, 2688.8250, 2680.7063, 2675.2937, 2645.5249,
     2 2602.2251, 2483.1499, 2266.6501, 2483.1499, 2050.1499, 1075.9000,
     3  642.9000,
     1 3068.1499, 3068.1499, 3055.6499, 3046.2749, 3040.0249, 3005.6499,
     2 2955.6499, 2818.1499, 2568.1499, 2818.1499, 2318.1499, 1193.1500,
     3  693.1500,
     1 3392.6501, 3392.6501, 3378.6748, 3368.1936, 3361.2063, 3322.7749,
     2 3266.8750, 3113.1499, 2833.6499, 3113.1499, 2554.1501, 1296.4000,
     3  737.4000,
     1 3686.8999, 3686.8999, 3671.5874, 3660.1030, 3652.4468, 3610.3374,
     2 3549.0874, 3380.6499, 3074.3999, 3380.6499, 2768.1499, 1390.0250,
     3  777.5250,
     1 3956.4001, 3956.4001, 3939.8625, 3927.4595, 3919.1907, 3873.7124,
     2 3807.5625, 3625.6499, 3294.8999, 3625.6499, 2964.1501, 1475.7750,
     3  814.2750,
     1 4206.6499, 4206.6499, 4188.9751, 4175.7188, 4166.8813, 4118.2749,
     2 4047.5750, 3853.1499, 3499.6499, 3853.1499, 3146.1499, 1555.4000,
     3  848.4000/
C
C*** Initialization of integration variable
C
      IPL(NIP)=1304
      IPLV(NIP)=1305
      NIP=NIP+1
C
C*** Initializations of table pointers
C
	IP1=1
	IP2=1
	IT2=1
C
C*** Initialization of mass properties
C
      VMASS0=136077.
	VMASS=VMASS0
	FMASS0=81646.
C
      RETURN
      END
C******************************************************************
      SUBROUTINE A2
C******************************************************************
C*** * Propulsion Module A2.
C*** * Reserved C(3510) locations are 1300-1399
C*** * Ref: "Handbook of Intelligent Control", Chapter 11 "Flight
C*** *       Propulsion, and Thermal Control of Advanced Aircraft and
C*** *       Hypersonic Vehicles", Edited by D.A.White and D.A. Sofge,
C*** *       Van Nostrand Reinhold,New York, NY, 1992
C*** * Type: turbojet 0-2 Mach
C*** *       ramjet   2-6 Mach
C*** *       scramjet 6-
C*** * Mass properties: take-off mass 136,077 kg
C*** *                  total fuel mass 81,646 kg
C*** *
C*** * MPROP=0 No thrusting
C*** *      =1 Throttle command input
C*** *      =2 Autothrottle (maintain dynamic pressure)
C*** *
C*** * This module performs the following functions:
C*** * (1) Calculates engine thrust
C*** * (2) Calculates mass properties
C*** *
C*** * Note: if MPROP=2, throttle must be opened by input:THRTL=>0
C*** *
C*** * MODIFICATION HISTORY
C*** * 991122 Created by Peter H Zipfel
C*** *
C*** *************************************************************
C
      COMMON C(3510)
C
	COMMON /PROPULSION/TBLCA(117),TBLSPI(117)
     &,ALPHAXI(9),VMACHI(13),THRI(9)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1310),THRTL)
	EQUIVALENCE (C(1318),QHOLD)
	EQUIVALENCE (C(1319),THRTLMAX)
C
C MPROP = D =0:Motor off,=1:throttle command,=2:autothrottle
C THRTL = D Throttle, controlling the fuel-air ratio - ND
C QHOLD = D Dynamic pressure hold command - Pa
C THRTLMAX = D Maximum throttle setting permissable - ND
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(2000),T)
	EQUIVALENCE (C(2866),ICOOR)
C
C*** INITIALIZED IN A2I
C
      EQUIVALENCE (C(1301),IP1)
      EQUIVALENCE (C(1302),IP2)
      EQUIVALENCE (C(1303),IT2)
      EQUIVALENCE (C(1306),FMASS0)
      EQUIVALENCE (C(1307),VMASS0)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0203),RHO)
      EQUIVALENCE (C(0206),VMACH)
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(1201),CD)
      EQUIVALENCE (C(1203),ALPHAX)
      EQUIVALENCE (C(1401),SREF)
      EQUIVALENCE (C(1613),DVBE)
C
C RHO= O Atmospheric density - kg/m^3
C VMACH= O Mach number of rocket - ND
C PDYNMC= G Dynamic pressure - Pa
C CD= O Drag coefficient - ND
C ALPHAX= D Angle of attack - deg
C SREF= D Aerodynamic reference area - m^2
C DVBE= I/G Geographic speed - m/s
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1304),FMASSED)
      EQUIVALENCE (C(1305),FMASSE)
C
C FMASSED = S Derivitave of fuel mass expended - kg/sec
C FMASSE = S Fluel mass expended - kg
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1313),THRUSTX)
C
C VMASS = O Vehicle mass - kg
C THRUSTX = O Rocket thrust parallel to vehicle centerline - kN
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1315),CA)
      EQUIVALENCE (C(1316),SPI)
      EQUIVALENCE (C(1317),FMASSR)
      EQUIVALENCE (C(1320),THRSTREX)
      EQUIVALENCE (C(1321),THRTLREQ)
C
C CA = G Capture area factor - ND
C SPI = G Specific impulse - s
C FMASSR = G Remaining fuel mass - kg
C THRSTREX = G Thrust required - kN
C THRTLREQ = G Throttle setting required - ND
C
	PARAMETER(MX1=13,MX2=9)
	PARAMETER(ACOWL=27.87)
C
C ACOWL= Cowl area of engine - m^2
C
C*** Thrusting
C
	IF(MPROP.GE.1)THEN
         CALL TBLP2(VMACH,ALPHAX,IP1,IP2,VMACHI,ALPHAXI,MX1,MX2,TBLCA
     &   ,CA,DUM1,DUM2)	
         CALL TBLP2(VMACH,THRTL,IP1,IT2,VMACHI,THRI,MX1,MX2,TBLSPI
     &   ,SPI,DUM1,DUM2)
	   IF(MPROP.EQ.1)THEN
C***  Commanded thrust
	      THRUST=SPI*.029*THRTL*AGRAV*RHO*DVBE*CA*ACOWL
         ELSEIF(MPROP.EQ.2)THEN
C***  Dynamic pressure hold (Autothrottl)
            THRSTREQ=SREF*CD*QHOLD/COS(ALPHAX/CRAD)
	      DUMK=.029*SPI*AGRAV*RHO*DVBE*CA*ACOWL
	      IF(DUMK.NE.0.)THEN
		     THRTLREQ=THRSTREQ/DUMK
	         GAINQ=2.*VMASS/(RHO*DVBE*DUMK) ! 1 sec time constant
	      ENDIF
	      ETHRTL=GAINQ*(QHOLD-PDYNMC)
	      THRTL=ETHRTL+THRTLREQ
	      IF(THRTL.LT.0.)THRTL=0.
	      IF(THRTL.GT.THRTLMAX)THRTL=THRTLMAX
            CALL TBLP2(VMACH,THRTL,IP1,IT2,VMACHI,THRI,MX1,MX2,TBLSPI
     &      ,SPI,DUM1,DUM2)
	      THRUST=SPI*.029*THRTL*AGRAV*RHO*DVBE*CA*ACOWL
         ENDIF
C
C *** Fuel consumption and vehicle mass
C
	   IF(SPI.NE.0.) FMASSED=THRUST/(SPI*AGRAV)
	   VMASS=VMASS0-FMASSE
      ENDIF
C
	FMASSR=FMASS0-FMASSE
	IF(FMASSR.LE.0.)THEN
	   MPROP=0
	   FMASSED=0.
	   THRUST=0.
	ENDIF
C
	THRUSTX=THRUST/1000.
	THRSTREX=THRSTREQ/1000.
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
C*** * Calculates the specific force acting on the vehicle
C*** *
C*** * MODIFICATION HISTORY
C*** * 960701  Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION FAPM(3),FSPV(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1401),SREF)
      EQUIVALENCE (C(1402),PHIMVX)
C
C SREF = D Aerodynamic reference area - m^2
C PHIMVX = D Bank angle of maneuver plane wrt vertical - deg
C
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(1201),CD)
      EQUIVALENCE (C(1202),CL)
      EQUIVALENCE (C(1203),ALPHAX)
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1313),THRUSTX)
      EQUIVALENCE (C(1613),DVBE)
C
C PDYNMC= G Dynamic pressure - Pa
C CD= O Drag coefficient - ND
C CL= O Lift coefficient - ND
C ALPHAX= D Angle of attack - deg
C VMASS= O Vehicle mass - kg
C THRUSTX= O Rocket thrust parallel to vehicle centerline - kN
C DVBE= I/G Geographic speed - m/s
C
C*** OUTPUTS TO OTHER MODULES
C
      EQUIVALENCE (C(1405),FSPV(1))
C
C FSPV(3) = O Specific force in velocity coordinates - m/s^2
C
c*** DIAGNOSTICS
C
      EQUIVALENCE (C(1403),FD)
      EQUIVALENCE (C(1404),FL)
C
C FD = G Drag force on vehicle - N
C FL = G Lift force on vehicle - N
C PDYNMC = G Dynamic Pressure - Pa
C
C*** CALCULATE AERODYNAMIC FORCES
C
      FD=PDYNMC*SREF*CD
      FL=PDYNMC*SREF*CL
C
C*** CALCULATE NON-GRAVITATIONAL FORCES IN MANEUVER PLANE
C
      FAPM(1)=-FD+THRUSTX*1000.*COS(ALPHAX/CRAD)
      FAPM(2)=0.
      FAPM(3)=-(FL+THRUSTX*1000.*SIN(ALPHAX/CRAD))
C
C*** SPECIFIC FORCE IN VELOCITY AXES
C
      FSPV(1)=FAPM(1)/VMASS
      FSPV(2)=-SIN(PHIMVX/CRAD)*FAPM(3)/VMASS
      FSPV(3)=COS(PHIMVX/CRAD)*FAPM(3)/VMASS
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G2
C**********************************************************************
C*** *
C*** * Atmosphere and gravity module in SI units
C*** * Reserved C(3510) locations are 200-299
C*** * This module performs the following functions:
C*** * 1) Calculates the atmospheric properties
C*** * 2) Calculates the gravitational acceleration
C*** * 3) Calculates the vehicle Mach number and dynamic pressure
C*** * 4) Inputs special weather deck from INPUT.ASC
C*** *
C*** * MAIR=0 International Standard Atmosphere ISO 1962
C*** *     =1 Weather Deck (Atmophere only, wind not used in 3 DoF sims)
C*** * COMMOM /WINDS/ read in from INPUT.ASC WEATHER deck
C*** *  (OPTMET=1 required, SI units)
C*** *  WALT= Altitude - m
C*** *  WDIR= Wind Direction (from North) - deg
C*** *  WVEL= Wind Velocity - m/s
C*** *  RHX= Air density - kg/m^3
C*** *  CTMP= Temprature - deg Celsius
C*** *  WPRES= Atmospheric pressure - Pa
C*** *  KOUNTW= Number of altitude records
C*** *  RHW= Last altitude record
C*** *
C*** * MODIFICATION HISTORY
C*** * 931007 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
      COMMON /WINDS/WALT(50),WDIR(50),WVEL(50),RHX(50),
     &              CTMP(50),WPRES(50),KOUNTW,RHW
C
C*** INPUT DATA
C
	EQUIVALENCE (C(0200),MAIR)
C
C MAIR = D =0:Std Atmosphere, =1: Weather Deck
C
C*** INPUT FROM EXECUTIVE ROUTINE
C
      EQUIVALENCE (C(0051),REARTH)
C
C REARTH = E Radius of Earth - m
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1606),BALT)
      EQUIVALENCE (C(1613),DVBE)
C
C BALT= I/O Vehicle altitude - m
C DVBE= I/G Geographic speed - m/s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0202),PRESS)
      EQUIVALENCE (C(0203),RHO)
      EQUIVALENCE (C(0205),GRAV)
      EQUIVALENCE (C(0206),VMACH)
      EQUIVALENCE (C(0207),PDYNMC)
C
C PRESS = O Atmospheric pressure - Pa
C RHO = O Atmospheric density - kg/m^3
C GRAV = O Gravity acceleration - m/s^2
C VMACH = O Mach number of rocket - ND
C PDYNMC = O Dynamic pressure - Pa
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0201),TEMPK)
      EQUIVALENCE (C(0204),VSOUND)
C
C TEMPK = G Atmospheric temperature - degK
C VSOUND = G Sonic speed - m/sec
 
C
C*** PARAMETERS
C
      PARAMETER (G=6.673E-11)
      PARAMETER (R=287.053)
      PARAMETER (EMASS=5.973E24)
C
C G =Gravitaional constant - N*m^2/kg^2
C R =Gas constant - m^2/(K*sec^2
C EMASS =Mass of earth - kg
C
C*** ALTITUDE ABOVE CENTER OF EARTH
C
      RAD = REARTH + BALT
C
C*** CALCULATE THE GRAVITY ACCELERATION
C
      GRAV=G*EMASS/RAD**2
C
C*** CALCUL THE ATMOSPH DENSITY, SONIC SPEED AND ROCKET MACH NUMBER
C
	IF(MAIR.EQ.0) THEN
		IF(BALT.LT.11000.)THEN
		  TEMPK=288.15-0.0065*BALT
		  PRESS=101325.*(TEMPK/288.15)**5.2559
		ELSE
		  TEMPK=216.
		  PRESS=22630.*EXP(-0.00015769*(BALT-11000.))
		ENDIF
C
		RHO=PRESS/(R*TEMPK)
	ELSE
         CALL TABLE(BALT,WALT,RHX,KOUNTW,RHO)
         CALL TABLE(BALT,WALT,CTMP,KOUNTW,CTEMP)
         CALL TABLE(BALT,WALT,WPRES,KOUNTW,PRESS)
	   TEMPK=CTEMP+273.16
	ENDIF
C
      VSOUND=SQRT(1.4*R*TEMPK)
C
      VMACH=ABS(DVBE/VSOUND)
C
      PDYNMC=RHO*DVBE**2/2.
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE D1I
C*** ******************************************************************
C*** * Initializes the equations of motions of Module D1
C*** * Reserved C(3510) locations are 1600-1699
C*** * This module performs the following functions
C*** *
C*** * 1) Define the locations of the state and state derivative
C*** *    variables
C*** * 2) Converts geographic inputs into inertial coordinates
C*** *
C*** * MODIFICATION HISTORY
c*** * 960711 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100),VBEG(3),TGE(3,3),TEG(3,3)
     &,SBIE(3),TVG(3,3),TGV(3,3),TIE(3,3),SBII(3),TIG(3,3),WEII(3,3)
     &,DUM3(3),VBEI(3),VBII(3)
C
C*** INPUT DATA INITIALIZATION
C
      EQUIVALENCE (C(1602),PSIVGX)
      EQUIVALENCE (C(1603),THTVGX)
      EQUIVALENCE (C(1604),BLON)
      EQUIVALENCE (C(1605),BLAT)
      EQUIVALENCE (C(1606),BALT)
      EQUIVALENCE (C(1613),DVBE)
C
C PSIVGX = I Heading angle from north - deg
C THTVGX = I Flight path angle from horizontal - deg
C BLON = I/G Vehicle longitude - rad
C BLAT = I/G Vehicle latitude - rad
C BALT = I/O Vehicle altitude - m
C DVBE = I/G Geographic speed - m/s
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0058),WEII3)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
      EQUIVALENCE (C(2561),NIP)
C
C IPL(100) = E State derivitave c-array locations
C IPLV(100) = E State c-array locations
C N = E Number of variables to integrate
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(1622),TGV(1,1))
      EQUIVALENCE (C(1631),TIG(1,1))
      EQUIVALENCE (C(1649),SBII(1))
      EQUIVALENCE (C(1643),VBII(1))
      EQUIVALENCE (C(1658),BALT0)
C
C***  INITIALIZATION OF STATE VARIABLES
C
      ILOC=1640
      DO I=0,2
         IPL(NIP)=ILOC+I
         IPLV(NIP)=ILOC+I+3
         NIP=NIP+1
      ENDDO
C
      ILOC=1646
      DO I=0,2
         IPL(NIP)=ILOC+I
         IPLV(NIP)=ILOC+I+3
         NIP=NIP+1
      ENDDO
C
C***INPUT CONVERSION TO SBII AND VBII AND INITIAL TGV AND TIG
C
      SBIE(1)=(BALT+REARTH)*COS(BLAT)*COS(BLON)
      SBIE(2)=(BALT+REARTH)*COS(BLAT)*SIN(BLON)
      SBIE(3)=(BALT+REARTH)*SIN(BLAT)
      CALL MATUNI(TIE,3)
      CALL MATMUL(SBII,TIE,SBIE,3,3,1)
C
      PSIVG=PSIVGX/CRAD
      THTVG=THTVGX/CRAD
      CALL MATCAR(VBEG,DVBE,PSIVG,THTVG)
      CALL CADTGE3(TGE,BLON,BLAT)
      CALL MATTRA(TEG,TGE,3,3)
      CALL MATZER(WEII,3,3)
      WEII(1,2)=-WEII3
      WEII(2,1)=WEII3
      CALL MATMUL(DUM3,WEII,SBII,3,3,1)
      CALL MATMUL(TIG,TIE,TEG,3,3,3)
      CALL MATMUL(VBEI,TIG,VBEG,3,3,1)
      CALL MATADD(VBII,VBEI,DUM3,3,1)
      CALL MAT2TR(TVG,PSIVG,THTVG)
      CALL MATTRA(TGV,TVG,3,3)
C
C*** SAVE LAUNCH ALTITUDE
C
	BALT0=BALT
C
      RETURN
      END
C*******************************************************************
      SUBROUTINE D1
C*******************************************************************
C*** * Equations of motion Module D1
C*** * Cartesian inertial form, round rotating earth
C*** * Reserved C(3510) locations are 1600-1699
C*** * This module performs the following functions
C*** *
C*** * 1) Solves Newton's Law for spherical rotating earth in
C*** *    inertial coordinates
C*** * 2) Converts output to geographic variables
C*** *
C*** * MODIFICATION HISTORY
C*** * 960711 Created by Peter Zipfel
C*** *
C*** **************************************************************
C
      COMMON C(3510)
C
      DIMENSION FSPG(3),FSPV(3),AGRAVG(3),AI(3),TIG(3,3),TEI(3,3)
     &,SBIE(3),SBII(3),VBEI(3),TGE(3,3),TGI(3,3),TVG(3,3)
     &,TGV(3,3),WEII(3,3),VBIG(3),VBII(3),VBIID(3)
     &,SBIID(3),DUM3(3),VBEG(3),ACCG(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0058),WEII3)
	EQUIVALENCE (C(2000),T)
C
C CRAD = E Conversion from radians to degrees = 57.298
C WEII3 = E Earth rotation - rad/sec
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(1622),TGV(1,1))
      EQUIVALENCE (C(1631),TIG(1,1))
      EQUIVALENCE (C(1658),BALT0)
C
C TGV(3,3) = I T.M. of  geographic wrt velocity coord - ND
C TIG(3,3) = I T.M. of inertial wrt geographic coord - ND
C BALT0 = I Saved value of initial altitude - m
C
C***  INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0205),GRAV)
      EQUIVALENCE (C(1405),FSPV(1))
C
C GRAV= O Gravity acceleration - m/s^2
C FSPV= O Specific force in velocity coordinates - m/s^2
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1640),VBIID(1))
      EQUIVALENCE (C(1643),VBII(1))
      EQUIVALENCE (C(1646),SBIID(1))
      EQUIVALENCE (C(1649),SBII(1))
C
C VBIID(3) = S Time derivative of VBII(3) - m/s^2
C VBII(3) = S Vel of missile wrt inertial frame in inertial axes - m
C SBIID(3) = S Time derivative of SBIE(3) - m/s
C SBII(3) = S Missile displacement from earth center in inertial axes - m
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1606),BALT)
      EQUIVALENCE (C(1613),DVBE)
C
C BALT = O Vehicle altitude = m
C DVBE = I/O Geographic speed - m/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1602),PSIVGX)
      EQUIVALENCE (C(1603),THTVGX)
      EQUIVALENCE (C(1604),BLON)
      EQUIVALENCE (C(1605),BLAT)
      EQUIVALENCE (C(1607),DVBI)
      EQUIVALENCE (C(1608),PSIVIGX)
      EQUIVALENCE (C(1609),THTVIGX)
      EQUIVALENCE (C(1652),VBEG(1))
      EQUIVALENCE (C(1655),VBIG(1))
C
C PSIVGX = G Heading angle from north - deg
C THTVGX = G Flight path angle from horizontal - deg
C BLON = G Vehicle longitude - rad
C BLAT = G Vehicle latitude - rad
C DVBI = G Speed of vehicle wrt inertial frame
C PSIVIGX = G Heading angle of inertial vel vect - deg
C THTVIGX = G Flight path angle of inert vel vec  - deg
C VBEG(3) = G Geographic velocity in geographic coord - m/s
C VBIG(3) = G Inertial velocity in geographic coord - m/s
C
C*** RIGHT HAND SIDE OF DYNAMIC EQUATIONS
C
      CALL MATMUL(FSPG,TGV,FSPV,3,3,1)
      CALL VECVEC(AGRAVG,0.,0.,GRAV)
      CALL MATADD(ACCG,FSPG,AGRAVG,3,1)
C*** IF VEHICLE ON RUNWAY
CC9	IF(ACCG(3).GT.0.AND.ABS(BALT-BALT0).LT.10)THEN
CC9	   CALL VECVEC(DUM3,ACCG(1),ACCG(2),0.)
CC9	ELSE
CC9	   CALL MATEQL(DUM3,ACCG,3,1)
CC9	ENDIF
      CALL MATEQL(DUM3,ACCG,3,1)
      CALL MATMUL(AI,TIG,DUM3,3,3,1)
C
C*** STATE VARIABLE INTEGRATION
C
      CALL MATEQL(VBIID,AI,3,1)
      CALL MATEQL(SBIID,VBII,3,1)
C
C*** UPDATE LONGITUDE, LATITUDE AND ALTITUDE, TVG AND FLIGHT PATH ANGLES
C
      CALL CADTEI3(TEI)
      CALL MATMUL(SBIE,TEI,SBII,3,3,1)
      CALL CADSPH3(BLON,BLAT,BALT,DBI,SBIE)
      CALL CADTGE3(TGE,BLON,BLAT)
C
      CALL MATZER(WEII,3,3)
      WEII(1,2)=-WEII3
      WEII(2,1)=WEII3
      CALL MATMUL(DUM3,WEII,SBII,3,3,1)
      CALL MATSUB(VBEI,VBII,DUM3,3,1)
      CALL MATMUL(TGI,TGE,TEI,3,3,3)
      CALL MATMUL(VBEG,TGI,VBEI,3,3,1)
      CALL MATPOL(DVBE,PSIVG,THTVG,VBEG)
      PSIVGX=PSIVG*CRAD
      THTVGX=THTVG*CRAD
C
C*** FOR NEXT INTEGRATION CYCLE: TIG, TGV
C
      CALL MATTRA(TIG,TGI,3,3)
      CALL MAT2TR(TVG,PSIVG,THTVG)
      CALL MATTRA(TGV,TVG,3,3)
C
C*** DIAGNOSTIC: INERTIAL VELOCITY IN GEOGRAPHIC AXES
C
      CALL MATMUL(VBIG,TGI,VBII,3,3,1)
      CALL MATPOL(DVBI,PSIVIG,THTVIG,VBIG)
      PSIVIGX=PSIVIG*CRAD
      THTVIGX=THTVIG*CRAD
C
      RETURN
      END
C
C*******************************************************************
      SUBROUTINE CADSPH3(BLON,BLAT,BALT,DBI,SBIE)
C*** ***************************************************************
C*** * Calculates longitude, latitude and altitude from earth position
C*** *
C*** * Argument Output:
C*** *          BLON =Missile longitude - rad
C*** *          BLAT =Missile latitude - rad
C*** *          BALT =Missile altitude above sea level - rad
C*** *          DBI =Missile distance from earth center - m
C*** * Argument Input:
C*** *          SBIE(3) =Missile position wrt earth center in earth coor - m
C*** *
C*** * MODIFICATION HISTORY
c*** * 960628 Created by Peter Zipfel
C*** * 000128 Resolved multivalued ARCSIN function, PZi
C*** *
C*** ***************************************************************
      COMMON  C(3510)
C
      DIMENSION SBIE(3)
C
C*** INPUT FROM EXEC
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0052),CRAD)
C
C*** LATITUDE
C
      DBI=SQRT(SBIE(1)**2+SBIE(2)**2+SBIE(3)**2)
      DUM1=SBIE(3)/DBI
      BLAT=ASIN(DUM1)
C
C*** ALTITUDE
C
      BALT=DBI-REARTH
C
C*** LONGITUDE
C
      DUM3=SBIE(2)/SQRT(SBIE(1)**2+SBIE(2)**2)
      DUM4=ASIN(DUM3)
C
C*** RESOLVING THE MUTLIVALUED ARCSIN FUNCTION
C
      IF(SBIE(1).GE.0..AND.SBIE(2).GE.0.) ALAMDA=DUM4 !1. quadrant
      IF(SBIE(1).LT.0..AND.SBIE(2).GE.0.) ALAMDA=180./CRAD-DUM4 !2. quadrant
      IF(SBIE(1).LT.0..AND.SBIE(2).LT.0.) ALAMDA=180./CRAD-DUM4 !3. quadrant
      IF(SBIE(1).GE.0..AND.SBIE(2).LT.0.) ALAMDA=360./CRAD+DUM4 !4. quadrant
      BLON=ALAMDA
      IF(BLON.GT.(180./CRAD)) BLON=-((360./CRAD)-BLON) !east pos., west neg.
C
      RETURN
      END
C*******************************************************************
      SUBROUTINE CADTGE3(TGE,BLON,BLAT)
C*** ***************************************************************
C*** * Calculates transformation matrix TGE from longitude and latitude
C*** *
C*** * Argument Output:
C*** *          TGE(3,3) =Transf. of geographic wrt earth coor
C*** * Argument Input:
C*** *          BLON =Missile longitude - rad
C*** *          BLAT =Missile latitude - rad
C*** *
C*** * MODIFICATION HISTORY
c*** * 960628 Created by Peter Zipfel
C*** *
C*** ***************************************************************
C
      COMMON  C(3510)
C
      DIMENSION TGE(3,3)
C
      SLON=SIN(BLON)
      CLON=COS(BLON)
      SLAT=SIN(BLAT)
      CLAT=COS(BLAT)
      TGE(1,1)=-SLAT*CLON
      TGE(1,2)=-SLAT*SLON
      TGE(1,3)=CLAT
      TGE(2,1)=-SLON
      TGE(2,2)=CLON
      TGE(2,3)=0.
      TGE(3,1)=-CLAT*CLON
      TGE(3,2)=-CLAT*SLON
      TGE(3,3)=-SLAT
C
      RETURN
      END
C*******************************************************************
      SUBROUTINE CADTEI3(TEI)
C*** ***************************************************************
C*** * Calculates transformation matrix TIE from time and WEII3
C*** *
C*** * Argument Output:
C*** *          TEI(3,3) =Transf. of inertial wrt earth coor
C*** *
C*** * MODIFICATION HISTORY
c*** * 960711 Created by Peter Zipfel
C*** *
C*** ***************************************************************
C
      COMMON  C(3510)
C
      DIMENSION TEI(3,3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0058),WEII3)
      EQUIVALENCE (C(2000),T)
C
      XI=WEII3*T
      SXI=SIN(XI)
      CXI=COS(XI)
      CALL MATUNI(TEI,3)
      TEI(1,1)=CXI
      TEI(1,2)=SXI
      TEI(2,1)=-SXI
      TEI(2,2)=CXI
      RETURN
      END
C**************************** DUMMY RETURN ****************************
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
	SUBROUTINE D2I
	RETURN
	END
	SUBROUTINE D2
	RETURN
	END
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
