C*******************************************************************
      SUBROUTINE A1
C*******************************************************************
C***  *
C***  * Aerodynamic module of a Single Stage to Orbit Vehicle
C***  * launched from a Super Boeing 747. Length = 35 m, span = 21 m
C***  * reference area = 102 m^2.
C***  *
C***  * This module performs the following functions:
C***  *
C***  * 1) Calculates the drag coefficient of the vehicle
C***  *
C***  * 2) Calculates the lift coefficient of the vehicle
C***  *
C***  * Table Look-up variables:
C***  * VMACHTBL =Vector of mach values
C***  * CD0TBL  =Vector of Cd0 values associated with table
C***  * CLATBL  =Vector of Cl-alpha values associated with table
C***  * CLA2TBL  =Vector of induced drag values associated with table
C***  * IDIM  =Dimension of table and cd0tbl vectors
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 930923 Created by Peter Zipfel
C***  *
C***  **************************************************************
 
C***  COMMON LOCATIONS ARE 1200 - 1299
C
      COMMON C(3510)
C
      DIMENSION  VMACHTBL(5),CD0TBL(5),CLATBL(5),CLA2TBL(5)
C
      PARAMETER (IDIM = 5)
C
C***  INPUT DATA
C
      EQUIVALENCE (C(1203),ALPHA)
C
C ALPHA = D Angle of attack - rad
C
C***  INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0206),VMACH)
C
C VMACH= O Mach number of rocket                               
C
C***  OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1201),CD)
      EQUIVALENCE (C(1202),CL)
C
C CD = O Drag coefficient
C CL = O Lift coefficient
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1204),CD0)
      EQUIVALENCE (C(1205),CLA)
      EQUIVALENCE (C(1206),CLA2)
C
C CD0 = G Zero lift drag coefficient
C CLA = G Lift slope
C CLA2 = G Induced drag value
C
      DATA VMACHTBL /0.2,1.2,5.0,10.0,20.0/
      DATA CD0TBL /0.0417,0.085,0.040,0.029,0.032/
      DATA CLATBL /1.569,1.482,1.115,1.063,1.033/
      DATA CLA2TBL /0.815,1.185,1.135,1.040,1.022/
C
C***  FIND THE LIFT AND DRAG COEFFICIENTS FOR CURRENT MACH
C
      VMCH=VMACH
      CALL TABLE(VMCH,VMACHTBL,CD0TBL,IDIM,CD0)
      CALL TABLE(VMCH,VMACHTBL,CLATBL,IDIM,CLA)
      CALL TABLE(VMCH,VMACHTBL,CLA2TBL,IDIM,CLA2)
C
C***  CALCULATE THE DRAG FORCE
C
      CD = CD0 + CLA2*ALPHA**2
C
C***  CALCULATE THE LIFT FORCE
C
      CL = CLA * ALPHA
C
      RETURN
      END
C******************************************************************
      SUBROUTINE A2I
C******************************************************************
C***  *
C***  * The initialization module for the propulsion module A2.
C***  *
C***  * This module performs the following functions:
C***  *
C***  * Defines the loc of the state variable FMASS -> C(1305)
C***  * and its derivitave FMASSD -> C(1304).
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 931007 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 1300 - 1399
C
      COMMON C(3510)
C
      DIMENSION  IPL(100), IPLV(100)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2561),N)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C N = E Number of variables to integrate
C IPL(100) = E Start of derivitave c-array locations
C IPLV(100) = E Start of state c-array locations
C
C*** INITIALIZATION CODE
C
      IPL(N) = 1304
      IPLV(N) = 1305
      N = N+1
C
      RETURN
      END
C*******************************************************************
      SUBROUTINE A2
C*******************************************************************
C***  *
C***  * Propulsion module. Liquid throttable engines, O2/H2 propellant,
C***  * max thrust at sea level 1.51e6 N.
C***  *
C***  * This module performs the following functions:
C***  *
C***  * 1) Calculates fuel mass
C***  *
C***  * 2) Calculates the rocket thrust at altitude
C***  *
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 931013 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 1300 - 1399
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1303),AE)
      EQUIVALENCE (C(1306),SPI)
      EQUIVALENCE (C(1307),VMASS0)
      EQUIVALENCE (C(1308),THRSL)
      EQUIVALENCE (C(1310),THROTL)
C
C MPROP = D =0: Motor off, =1:Motor on
C AE = D Exit area of rocket nozzle - m^2
C SPI = D Specific impulse - 1/s
C VMASS0 = D Vehicle mass at launch - kg
C THRSL = D Rocket thrust at sea level - N
C THROTL = D Throttle setting ( 0 - 1 )
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0202),PRESS)
      EQUIVALENCE (C(1203),ALPHA)
C
C PRESS= O Atmospheric pressure - Pa                           
C ALPHA= D Angle of attack - rad                               
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1304),FMASSD)
      EQUIVALENCE (C(1305),FMASS)
C
C FMASSD = S Derivitave of fuel mass consumed - kg/sec
C FMASS = S Fluel mass consumed - kg
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1311),THRUSTN)
      EQUIVALENCE (C(1312),THRUSTT)
C
C VMASS = O Vehicle mass - kg
C THRUSTN = O Rocket thrust normal to velocity vector - N
C THRUSTT = O Rocket thrust tangential to velocity vector - N
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1313),THRUST)
C
C THRUST = O Rocket thrust paralell to vehicle centerline - N
C
C*** PARAMETERS
C
      PARAMETER (PSL=101325.)
C
C PSL =Atmospheric pressure at sea level - Pa
C
C*** FUEL MASS, ROCKET MASS, AND THRUST
C
      IF (MPROP.EQ.1) THEN
         FMASSD = THRSL * THROTL/ (SPI * 9.81)
         VMASS = VMASS0-FMASS
         THRUST = THRSL * THROTL + (PSL-PRESS) * AE
      ELSE
         FMASSD=0.
         THRUST=0.
      ENDIF
C
C***  CALCULATE THE NORMAL AND TANGENT THRUST VALUES
C
      THRUSTN = THRUST * SIN(ALPHA)
      THRUSTT = THRUST * COS(ALPHA)
C
      RETURN
      END
C*******************************************************************
      SUBROUTINE A3
C*******************************************************************
C***  *
C***  * Force module
C***  *
C***  * This module performs the following functions:
C***  *
C***  * Calculates the sum of forces acting on the vehicle
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 931007  Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 1400 - 1499
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1401),SREF)
C
C SREF = D Aerodynamic reference area - m^2
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0203),RHO)
      EQUIVALENCE (C(1201),CD)
      EQUIVALENCE (C(1202),CL)
      EQUIVALENCE (C(1311),THRUSTN)
      EQUIVALENCE (C(1312),THRUSTT)
      EQUIVALENCE (C(1619),VEL)
C
C RHO= O Atmospheric density - kg/m^3
C CD= O Drag coefficient                              
C CL= O Lift coefficient                                    
C THRUSTN= O Rocket thrust normal to velocity vector - N         
C THRUSTT= O Rocket thrust tangential to velocity vector - N     
C VEL= S Vehicle geographic speed - m/sec                     
C
C*** OUTPUTS TO OTHER MODULES
C
      EQUIVALENCE (C(1405),FN)
      EQUIVALENCE (C(1406),FT)
C
C FN = O Non-gravity forces normal to velocity vector - N
C FT = O Non-gravity forces parallel to velocity vector - N
C
c*** DIAGNOSTICS
C
      EQUIVALENCE (C(1402),FD)
      EQUIVALENCE (C(1403),FL)
      EQUIVALENCE (C(1407),PDYNMC)
C
C FD = G Drag force on vehicle - N
C FL = G Lift force on vehicle - N
C PDYNMC = G Dynamic Pressure - Pa
C
C*** CALCULATE AERODYNAMIC FORCES
C
      PDYNMC=0.5*RHO*VEL**2
      FD=PDYNMC*SREF*CD
      FL=PDYNMC*SREF*CL
C
C*** CALCULATE NON-GRAVITATIONAL FORCES
C
      FN=FL+THRUSTN
      FT=-FD+THRUSTT
C
      RETURN
      END
 
C*******************************************************************
      SUBROUTINE G2
C*******************************************************************
C***  *
C***  * Atmosphere and gravity module for SI units
C***  *
C***  * This module performs the following functions:
C***  *
C***  * 1) Calculates the atmospheric properties
C***  *
C***  * 2) Calculates the gravitational acceleration
C***  *
C***  * 3) Calculates the vehicle Mach number
C***  *
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 931007 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 0200 - 0299
C
      COMMON C(3510)
C
C*** INPUT FROM EXECUTIVE ROUTINE
C
      EQUIVALENCE (C(0051),REARTH)
C
C REARTH = E Radius of Earth - m
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1616),RAD)
      EQUIVALENCE (C(1619),VEL)
C
C RAD= S Vehicle altitude above center of earth - m           
C VEL= S Vehicle geographic speed - m/sec                     
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0202),PRESS)
      EQUIVALENCE (C(0203),RHO)
      EQUIVALENCE (C(0205),GRAV)
      EQUIVALENCE (C(0206),VMACH)
C
C PRESS = O Atmospheric pressure - Pa
C RHO = O Atmospheric density - kg/m^3
C GRAV = O Gravity acceleration - m/s^2
C VMACH = O Mach number of rocket
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
C*** ALTITUDE ABOVE EARTH
C
      HR = RAD - REARTH
C
C*** CALCULATE THE GRAVITY ACCELERATION
C
      GRAV = G * EMASS / RAD**2
C
C*** CALCUL THE ATMOSPH DENSITY, SONIC SPEED AND ROCKET MACH NUMBER
C
      IF (HR.LT.11000.) THEN
        TEMPK = 288.15 - 0.0065*HR
        PRESS = 101325. * (TEMPK/288.15)**5.2559
      ELSE
        TEMPK = 216.
        PRESS = 22630.*EXP(-0.00015769*(HR-11000.))
      ENDIF
C
      RHO = PRESS / (R * TEMPK)
C
      VSOUND = SQRT(1.4*R*TEMPK)
C
      VMACH = ABS(VEL / VSOUND)
C
      RETURN
      END
C*******************************************************************
      SUBROUTINE D1I
C***  **************************************************************
C***  *
C***  * Initializes the equations of motions in module D1
C***  *
C***  * This module performs the following functions
C***  *
C***  * 1) Define the locations of the state and state derivitave
C***  *    variables
C***  *
C***  * MODIFICATION HISTORY
C***  *
c***  * 930922 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C***  COMMON LOCATIONS ARE 1600 - 1699
C
      COMMON C(3510)
C
      DIMENSION IPL(100), IPLV(100)
C
C***  INITIALIZATION
C
      EQUIVALENCE (C(2562), IPL(1))
      EQUIVALENCE (C(2867), IPLV(1))
      EQUIVALENCE (C(2561), N)
C
C IPL(100) = E State derivitave c-array locations
C IPLV(100) = E State c-array locations
C N = E Number of variables to integrate
C
C***  INITIALIZATION OF STATE VARIABLES
C
      LOCATION = 1610
      DO I = N , N + 5
        IPL(I) = LOCATION
        IPLV(I) = LOCATION + 6
        LOCATION = LOCATION + 1
      ENDDO
      N = N + 6
C
      RETURN
      END
C*******************************************************************
      SUBROUTINE D1
C*******************************************************************
C***  *
C***  * This module performs the following functions
C***  *
C***  * 1) Solves Newton's Law for rotating earth in geographic frame
C***  *    and geographic coordinate axes.
C***  *
C***  * 2) Calculates the Vehicle speed wrt inertial frame
C***  *
C***  * 3) Reference: Vinh, Nguyen "Optimal Trajectories in Atmospheric Flight"
C***  *               Elsevier, 1981, pp.50-58
C***  *
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 930922 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C***  COMMON LOCATIONS ARE 1600 - 1699
C
      COMMON C(3510)
C
C***  INPUT DATA
C
      EQUIVALENCE (C(1601),SIGMA)
C
C SIGMA = D Vehicle bank angle - rad
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0058),WEII3)
C
C WEII3 = E Earth rotation - rad/sec
C
C***  INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0205),GRAV)
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1405),FN)
      EQUIVALENCE (C(1406),FT)
C
C GRAV= O Gravity acceleration - m/s^2                         
C VMASS= O Vehicle mass - kg                                    
C FN= O Non-gravity forces normal to velocity vector - N    
C FT= O Non-gravity forces parallel to velocity vector - N   
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1610),RADD)
      EQUIVALENCE (C(1611),THETAD)
      EQUIVALENCE (C(1612),PHID)
      EQUIVALENCE (C(1613),VELD)
      EQUIVALENCE (C(1614),GAMMAD)
      EQUIVALENCE (C(1615),PSID)
      EQUIVALENCE (C(1616),RAD)
      EQUIVALENCE (C(1617),THETA)
      EQUIVALENCE (C(1618),PHI)
      EQUIVALENCE (C(1619),VEL)
      EQUIVALENCE (C(1620),GAMMA)
      EQUIVALENCE (C(1621),PSI)
C
C RADD = S Derivative of RAD - m/sec
C THETAD = S Derivative of THETA - rad/sec
C PHID = S Derivative of PHI - rad/sec
C VELD = S Derivative of VEL - m/sec^2
C GAMMAD = S Derivative of GAMMA - rad/sec
C PSID = S Derivative of PSI - rad/sec
C RAD = S Vehicle altitude above center of earth - m
C THETA = S Vehicle longitude (rad)
C PHI = S Vehicle latitude (rad)
C VEL = S Vehicle geographic speed - m/sec
C GAMMA  = S Vehicle flight path angle - rad
C PSI    = S Vehicle heading - east is zero - rad
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1602),HBE)
      EQUIVALENCE (C(1625),VBIMAG)
C
C HBE = G Height above sea level - m
C VBIMAG = G Vehicle speed wrt inertial frame - m/sec
C
C***  IMPLEMENTATION OF VINH'S EQUATIONS 3.30, 3.33
C
      RADD = VEL * SIN(GAMMA)
C
      THETAD = ( VEL * COS(GAMMA) * COS(PSI) ) / ( RAD * COS(PHI) )
C
      PHID = ( VEL * COS(GAMMA) * SIN(PSI) ) / RAD
C
      VELD = FT/VMASS - GRAV*SIN(GAMMA) + WEII3**2*RAD*COS(PHI)*
     x( SIN(GAMMA)*COS(PHI) - COS(GAMMA)*SIN(PSI)*SIN(PHI) )
C
      GAMMAD = FN*COS(SIGMA)/(VEL*VMASS) - GRAV*COS(GAMMA)/VEL +
     x VEL*COS(GAMMA)/RAD + 2*WEII3*COS(PSI)*COS(PHI) +
     x (WEII3**2)*RAD*COS(PHI)*( COS(GAMMA)*COS(PHI) +
     x SIN(GAMMA)*SIN(PSI)*SIN(PHI) )/VEL
C
      PSID = FN*SIN(SIGMA)/(VEL*VMASS*COS(GAMMA)) -
     x VEL*COS(GAMMA)*COS(PSI)*TAN(PHI)/RAD +
     x 2*WEII3*( TAN(GAMMA)*SIN(PSI)*COS(PHI) - SIN(PHI) ) -
     x WEII3**2*RAD*COS(PSI)*SIN(PHI)*COS(PHI)/(VEL*COS(GAMMA))
C
C***  CALCULATE VEHICLE SPEED WRT INERTIAL FRAME
C
      VBIMAG = SQRT( VEL**2 * ( SIN(GAMMA)**2 + COS(GAMMA)**2
     x * SIN(PSI)**2 ) + ( VEL*COS(GAMMA)*COS(PSI) + WEII3*RAD )**2 )
C
C*** ALTITUDE ABOVE SEA LEVEL
C
      HBE = RAD - REARTH
C
      RETURN
      END
 
