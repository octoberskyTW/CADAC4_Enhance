C**********************************************************************
C************************ MODULES FOR ROCKET3 **************************
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
C*** * 000112 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INITIALIZATION
C
	RETURN
	END
C**********************************************************************
      SUBROUTINE A1
C**********************************************************************
C*** * The Aerodynamic Module A1.
C*** * Reserved C(3510) locations are 1200-1299
C*** *
C*** *							STAGE1	STAGE2	STAGE3
C*** * Mass with fuel
C*** *  of remaining stages - kg	10800	3200	1000		
C*** * Fuel in stage - kg			6200	1700	580	
C*** * Specific impulse - s		270		280		290	
C*** * Fuel burn rate - kg/s		94		30		15
C*** * Nozzle exit area - m^2		.5		.5		.1					
C*** * Aerodynamic area - m^2		1.5		.5		.5	
C*** *
C*** *  MAERO =|MAERT| |MAERV|   (Type,Vehicle)
C*** *
C*** *          MAERT=1 Test Ascent Rocket
C*** *                  MAERV=1 1.Stage
C*** *                       =2 2.Stage
C*** *                       =3 3.Stage
C*** *
C*** * This module performs the following functions:
C*** * (1) Provides  CL and CD as functions of Mach
C*** *
C*** * MODIFICATION HISTORY
C*** * 000112 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C***  INPUT DATA
C
	EQUIVALENCE (C(1200),MAERO)
      EQUIVALENCE (C(1203),ALPHAX)
C
C MAERO = D =|MAERT|MAERV|, MAERT=1:Type, MAERV:Stage #
C ALPHAX = D Angle of attack - deg
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0206),VMACH)
      EQUIVALENCE (C(1300),MPROP)
C
C VMACH= O Mach number of rocket - ND
C MPROP= D/G =0:No thrust; =1:Coasting =2:Burning
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
      EQUIVALENCE (C(1206),CA)
      EQUIVALENCE (C(1207),CN)
      EQUIVALENCE (C(1208),CLOVERCD)
C
C CA = G Axial force coefficient - ND
C CN = G Normal force coefficient - ND
C CLOVERCD = G Lift over drag - ND
C
      MAERT=INT(MAERO/10.)
      MAERV=MAERO-MAERT*10.
      ALPHA=ALPHAX/CRAD
      CALPH=COS(ALPHA)
      SALPH=SIN(ALPHA)
C
C*** ASCENT TEST ROCKET (STORM)
C
      IF(MAERT.EQ.1)THEN
         IF(MAERV.EQ.1)THEN
C
C***       FIRST STAGE
C
	      IF(MPROP.EQ.2)THEN
              CAA=.281+.186*VMACH-.056*VMACH**2+.00366*VMACH**3
	      ELSE
              CAA=.346+.183*VMACH-.058*VMACH**2+.00382*VMACH**3
	      ENDIF
	      CNN=(5.006-.519*VMACH+.031*VMACH**2)*ALPHA
         ENDIF
C
         IF(MAERV.EQ.2)THEN
C
C***       SECOND STAGE
C
	      IF(MPROP.EQ.2)THEN
              CAA=.236-.043*VMACH+.0029*VMACH**2-.00006*VMACH**3
	      ELSE
              CAA=.327-.067*VMACH+.005*VMACH**2-.0001*VMACH**3
	      ENDIF
	      CNN=(1.714-.038*VMACH+.0014*VMACH**2)*ALPHA
         ENDIF
C
         IF(MAERV.EQ.3)THEN
C
C***       THIRD STAGE
C
            CAA=.02
            CNN=1*ALPHA
         ENDIF
C
C***    CONVERT COEFFICIENTS
C
         CDD=CAA*CALPH+CNN*SALPH
         CLL=CNN*CALPH-CAA*SALPH
         CL=CLL
         CD=CDD
         CLOVERCD=CL/CD
C
         CN=CL*CALPH+CD*SALPH
         CA=CD*CALPH-CL*SALPH
C
	ENDIF
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A2I
C**********************************************************************
C*** * Propulsion Initialization.
C*** * Reserved C(3510) locations are 1300-1399
C*** *
C*** * * Fixed nozzle, variable fuel flow motor, ideal nozzle expansion
C*** * is assumed. Throttle ratio THRTL
C*** * governs the flow rate.
C*** * * Several stages; the total mass of the remaining stages is initialized
C*** * by VMASSI; new engine parameters may also be introduced at every
C*** * stage; the current mass is VMASS;
C*** *
C*** * MPROP =0 No ignition; or engine shut-down (D)
C*** *       =1 Missile is coasting after emgine shut-down.
C*** *          (shut-down occurs either at fuel burn-out or
C*** *           when MPROP=0 set in input (G)
C*** *       =2 Motor ignition; re-ignition; or motor burning (D)
C*** *
C*** * This module performs the following functions:
C*** *
C*** * (1) Initializes vehicle and fuel masses
C*** *
C*** * MODIFICATION HISTORY
C*** * 000113 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1305),VMASSI)
C
C VMASSI = D Initial mass of remaining stages - kg
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(1308),FMASSF)
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1312),FMASSFM)
      EQUIVALENCE (C(1314),VMASSIM)
C
C FMASSF = I/G Expended fuel in stage - kg
C VMASS = I/O Vehicle mass - kg
C FMASSFM = I Expended fuel in stage, stored - kg
C VMASSIM = I Initial mass of prior stage - kg
C
      FMASSF=0.
      VMASS=VMASSI
      FMASSFM=0.
      VMASSIM=0.
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A2
C**********************************************************************
C*** * Propulsion.
C*** * Reserved C(3510) locations are 1300-1399
C*** *
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates missile thrust
C*** * (2) Initiates rocket booster staging
C*** * (3) Calculates missile mass
C*** *
C*** * MODIFICATION HISTORY
C*** * 000113 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1302),FUELI)
      EQUIVALENCE (C(1303),FUELR)
      EQUIVALENCE (C(1304),SPI)
      EQUIVALENCE (C(1305),VMASSI)
      EQUIVALENCE (C(1310),THRTL)
      EQUIVALENCE (C(1316),AEXIT)
C
C MPROP = D/G =0:No thrust; =1:Coasting =2:Burning
C FUELI = D Initial total fuel in stage - kg
C FUELR = D Maximum fuel rate through motor - kg/s
C SPI = D Effective specific impulse - s
C VMASSI = D Initial mass of remaining stages - kg
C THRTL = D Throttle (0-1) - ND
C AEXIT = D Nozzle exit aerea - m^2
C
C*** INIIALIZATION
C
      EQUIVALENCE (C(1308),FMASSF)
      EQUIVALENCE (C(1312),FMASSFM)
      EQUIVALENCE (C(1314),VMASSIM)
C
C FMASSF = I/G Expended fuel in stage - kg
C FMASSFM = I Expended fuel in stage, stored - kg
C VMASSIM = I Initial mass of prior stage - kg
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(2664),DER)
      EQUIVALENCE (C(2866),ICOOR)
C
C AGRAV = E Gravitational acceleration (refrence) - m/s^2
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0202),PRESS)
C
C PRESS= O Atmospheric pressure - Pa
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1313),THRUSTX)
      EQUIVALENCE (C(1309),VMASS)
C
C THRUSTX = O Thrust at altitude - kN
C VMASS = O Vehicle mass - kg
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1306),FMASSFD)
      EQUIVALENCE (C(1311),FUEL)
C
C FMASSFD = G Fuel rate - kg/s
C FUEL = G Fuel remaining in stage - kg
C
C*** FOR NEW STAGE: INITIALIZE EXPENDED FUEL TO ZERO
C
      IF((VMASSIM-VMASSI).GT.0.) THEN
         FMASSFM=0.
         FMASSF=0.
         FMASSFD=0.
      ENDIF
      VMASSIM=VMASSI
C
C*** CALCULATE FUEL EXPENDED IN STAGE
C
      IF(ICOOR.EQ.0) FMASSF=FMASSF+FMASSFD*DER
C
      IF(MPROP.EQ.0) THEN
C
C***     NO BURNING
C
         THRUST=0.
         FMASSFD=0.
         VMASS=VMASSI-FMASSFM
         FUEL=FUELI-FMASSFM
      ENDIF
C
      IF(MPROP.EQ.1) THEN
C
C***     COAST AFTER BURN-OUT
C
         THRUST=0.
         FMASSFD=0.
         FMASSF=0.
         VMASS=VMASSI-FMASSFM
         FUEL=FUELI-FMASSFM
      ENDIF
C
      IF(MPROP.EQ.2) THEN
C
C***     ROCKET MOTOR BURNING
C
         FLR=FUELR*THRTL
         THRUST=FLR*SPI*AGRAV+(101325.-PRESS)*AEXIT
         FMASSFD=FLR
         FUEL=FUELI-FMASSF
         VMASS=VMASSI-FMASSF
         FMASSFM=FMASSF
         IF(FUEL.LE.0.) THEN
            MPROP=1
            THRUST=0.
            FMASSFD=0.
            FMASSF=0.
         ENDIF
      ENDIF
C
	THRUSTX=THRUST/1000.
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
C PDYNMC= G Dynamic Pressure - Pa
C CD= O Drag coefficient - ND
C CL= O Lift coefficient - ND
C ALPHAX= D Angle of attack - deg
C VMASS= I/O Vehicle mass - kg
C THRUSTX= O Thrust at altitude - kN
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
C BALT= I/O Vehicle altitude = m
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
	IF(ABS(DUM1).GT.1.) DUM1=SIGN(1.,DUM1)
      BLAT=ASIN(DUM1)
C
C*** ALTITUDE
C
      BALT=DBI-REARTH
C
C*** LONGITUDE
C
      DUM3=SBIE(2)/SQRT(SBIE(1)**2+SBIE(2)**2)
	IF(ABS(DUM3).GT.1.) DUM3=SIGN(1.,DUM3)
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
C
      RETURN
      END
C********************* DUMMY RETURNS **********************************
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
