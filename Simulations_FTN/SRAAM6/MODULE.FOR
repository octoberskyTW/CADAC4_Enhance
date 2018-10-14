C**********************************************************************
C******************** MODULES AND DUMMY FOR SRAAM6 ********************
C**********************************************************************
C*** *
C*** * Calling sequence of Modules:
C*** *
C*** *  G1   TARGET
C*** *  G2   ENVIRON
C*** *  G3   KINEMATICS
C*** *  A2   PROPULSION
C*** *  A1   AERO
C*** *  S1   SENSOR
C*** *  S2   AI RADAR
C*** *  S4   INS
C*** *  C1   GUIDANCE
C*** *  C2   AUTOPILOT
C*** *  C4   ACTUATOR
C*** *  A3   FORCES
C*** *  D2   EULER
C*** *  D1   NEWTON
C*** *
C*** * MODIFICATION HISTORY
C*** * 000413 Version 1,0 Created by Peter Zipfel
C*** *
C**********************************************************************
C**********************************************************************
      SUBROUTINE A1I
C**********************************************************************
C*** *
C*** * The initialization module for the Aerodynamic Module A1.
C*** *
C*** * This module performs the following functions:
C*** *
C*** * (1) Provides the aerodynamic tables of missile SR1A
C*** *     Ref Length REFL=0.5 ft; Ref Area REFA=0.1963 sq ft
C*** *
C*** * (2) Defines the run stopping conditions
C*** *
C*** * MODIFICATION HISTORY
C*** *
C*** * 950125 Created by Peter Zipfel
C*** * 960118 3-DIM table look up (interpolates cg location), PZi
C*** * 960228 Aero tables for SR1S, TAO
C*** * 970314 Removed 3-DIM table-moment arm correction for cg, TAO
C*** *
C*** ******************************************************************
C
C***  COMMON LOCATIONS ARE 1200-1299
C
      COMMON C(3510)
C
      COMMON/AERO/TCN(210),TCNP(210),TCLM(210),TCLMP(210),TCYP(210)
     &,TCLNP(210),TCA(14),TCAA(14),TCAD(14),TCAOFF(14),TCNDQ(210)
     &,TCLMDQ(210),TCLMQ(14),TCLLAP(210),TCLLDP(210),TCLLP(210)
     &,ARGX(14),ARGXY(29),NX,NXY(2)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0053),OPTMET)
      EQUIVALENCE (C(1282),TRCVEL)
      EQUIVALENCE (C(1283),TRMACH)
      EQUIVALENCE (C(1284),TRDYNM)
      EQUIVALENCE (C(1285),TRLOAD)
      EQUIVALENCE (C(1286),TRALP)
      EQUIVALENCE (C(1287),TRTHT)
      EQUIVALENCE (C(1289),TRTHTD)
      EQUIVALENCE (C(1290),TRPHID)
      EQUIVALENCE (C(1291),TRATE)
C
C TRCVEL = D Minimum closing speed - m/s
C TRMACH = D Minimum Mach number
C TRDYNM = D Minimum dynamic pressure - Pa
C TRLOAD = D Minimum load capacity - g's
C TRALP = D Maximum total angle of attack - rad
C TRTHT = D Maximum pitch gimbal angle - rad
C TRTHTD = D Maximum pitch gimbal rate - rad/s
C TRPHID = D Maximum roll gimbal rate - rad/s
C TRATE = D Maximum tracking rate - rad/s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1203),REFL)
      EQUIVALENCE (C(1204),REFA)
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1281),TMCODE)
C
C REFL = I Reference length for moment derivatives - m
C REFA = I Reference area for aero coefficients - m^2
C TRCODE = I Termination code number
C TMCODE = I Dummy variable initialized to zero
C
C    ANGLE OF ATTACK ACROSS (15 COLUMNS)(deg): 0 3 6 10 15 20 25 30 35 40 45 50
C    MACH NO DOWN (14 ROWS): .6 .85 .95 1.05 1.2 1.6 2 2.4 2.8 3.2 3.6 4 4.5 5
C
      DATA NXY/15,14/      ! <</Alpha,Mach/
      DATA ARGXY/ 0.,3.,6.,10.,15.,20.,25.,30.,35.,40.,45.,50.,60.
     &,75.,90.
     &,0.6,0.85,0.95,1.05,1.2,1.6,2.,2.4,2.8,3.2,3.6,4.,4.5,5./
      DATA NX/14/          ! <</Mach/
      DATA ARGX/0.6,0.85,0.95,1.05,1.2,1.6,2.,2.4,2.8,3.2,3.6,4.,4.5,5./
C
C*** AERO TABLES BASED ON MISSILE DATCOM V6 (1993). ALTITUDE= 10000 FT,
C    ADJUSTMENTS FOR POWER OFF
C
C*** NORMAL FORCE COEFFICIENT
C
       DATA TCN/
     1  0.000,  0.720,  1.497,  2.631,  3.741,  4.369,  5.296,  6.173,
     2  6.937,  7.700, 15.721, 19.076, 25.750, 32.322, 34.851,
     1  0.000,  0.778,  1.582,  2.690,  3.508,  4.703,  5.853, 10.590,
     2 14.058, 17.418, 20.861, 23.721, 29.175, 34.391, 35.801,
     1  0.000,  0.825,  1.667,  2.760,  3.706,  4.995,  8.687, 12.101,
     2 15.632, 19.153, 22.251, 25.324, 30.829, 35.089, 36.500,
     1  0.000,  0.974,  1.958,  3.133,  4.341,  5.878, 10.897, 15.126,
     2 19.308, 23.045, 26.870, 30.600, 36.120, 41.380, 43.130,
     1  0.000,  0.913,  1.844,  2.847,  4.058,  7.614, 11.604, 15.716,
     2 19.486, 23.412, 26.899, 29.954, 35.796, 41.201, 42.642,
     1  0.000,  0.653,  1.283,  1.955,  4.054,  7.073, 10.184, 13.394,
     2 16.034, 18.818, 21.678, 24.320, 29.201, 34.236, 36.067,
     1  0.000,  0.537,  1.022,  1.579,  4.064,  6.623,  9.231, 11.629,
     2 14.336, 17.000, 19.771, 22.505, 28.040, 33.998, 36.269,
     1  0.000,  0.456,  0.873,  1.928,  3.978,  6.236,  8.362, 10.762,
     2 13.307, 15.981, 18.890, 21.878, 27.567, 33.868, 36.543,
     1  0.000,  0.401,  0.781,  1.977,  3.807,  5.721,  7.862, 10.207,
     2 12.772, 15.652, 18.617, 21.601, 27.328, 33.836, 36.880,
     1  0.000,  0.348,  0.704,  1.956,  3.618,  5.376,  7.426,  9.777,
     2 12.514, 15.391, 18.347, 21.331, 27.060, 33.394, 36.112,
     1  0.000,  0.307,  0.678,  1.924,  3.384,  5.103,  7.130,  9.594,
     2 12.329, 15.200, 18.146, 21.120, 26.833, 33.234, 36.048,
     1  0.000,  0.284,  0.791,  1.861,  3.219,  4.901,  6.962,  9.439,
     2 12.154, 15.023, 17.963, 20.928, 26.648, 33.237, 36.312,
     1  0.000,  0.262,  0.797,  1.804,  3.074,  4.724,  6.851,  9.312,
     2 12.017, 14.901, 17.836, 20.786, 26.509, 33.038, 36.048,
     1  0.000,  0.246,  0.801,  1.717,  2.953,  4.640,  6.771,  9.220,
     2 11.924, 14.819, 17.752, 20.688, 26.396, 32.938, 36.018/
C
C*** CORRECTION TO NORMAL FORCE COEFFICIENT WHEN PHIP IS NOT ZERO
C
       DATA TCNP/
     1  0.000,  0.021,  0.138,  0.151,  0.116, -0.214, -0.529, -0.737,
     2 -0.743, -0.670, -0.643, -0.688, -1.202, -1.793, -2.169,
     1  0.000,  0.025,  0.142,  0.161,  0.180, -0.043, -0.274, -0.452,
     2 -0.507, -0.558, -0.673, -0.881, -1.691, -2.375, -2.687,
     1  0.000,  0.026,  0.147,  0.170,  0.213,  0.038, -0.152, -0.322,
     2 -0.404, -0.507, -0.696, -0.987, -1.954, -2.695, -2.984,
     1  0.000,  0.033,  0.177,  0.213,  0.309,  0.183,  0.032, -0.141,
     2 -0.263, -0.444, -0.761, -1.201, -2.538, -3.479, -3.776,
     1  0.000,  0.033,  0.156,  0.203,  0.339,  0.325,  0.263,  0.136,
     2 -0.025, -0.274, -0.665, -1.172, -2.537, -3.429, -3.633,
     1  0.000,  0.016,  0.062,  0.115,  0.207,  0.227,  0.178,  0.054,
     2 -0.144, -0.377, -0.636, -0.913, -1.489, -1.901, -2.158,
     1  0.000,  0.007,  0.034,  0.128,  0.239,  0.291,  0.315,  0.265,
     2  0.165,  0.016, -0.178, -0.387, -1.096, -1.798, -2.216,
     1  0.000,  0.005,  0.025,  0.107,  0.205,  0.278,  0.299,  0.294,
     2  0.237,  0.159,  0.025, -0.228, -0.843, -1.676, -2.297,
     1  0.000,  0.003,  0.021,  0.091,  0.185,  0.261,  0.322,  0.337,
     2  0.302,  0.228,  0.113, -0.060, -0.728, -1.582, -2.395,
     1  0.000,  0.003,  0.017,  0.082,  0.165,  0.253,  0.321,  0.369,
     2  0.348,  0.288,  0.178, -0.002, -0.705, -1.540, -2.170,
     1  0.000,  0.003,  0.018,  0.074,  0.152,  0.240,  0.317,  0.389,
     2  0.375,  0.321,  0.220,  0.034, -0.628, -1.476, -2.152,
     1  0.000,  0.002,  0.025,  0.069,  0.141,  0.224,  0.316,  0.376,
     2  0.401,  0.348,  0.255,  0.079, -0.526, -1.426, -2.229,
     1  0.000,  0.002,  0.025,  0.066,  0.135,  0.216,  0.319,  0.380,
     2  0.446,  0.374,  0.277,  0.112, -0.519, -1.386, -2.151,
     1  0.000,  0.001,  0.025,  0.065,  0.131,  0.212,  0.321,  0.397,
     2  0.474,  0.394,  0.286,  0.122, -0.500, -1.344, -2.143/
C
C*** PITCHING MOMENT COEFFICIENT
C
      DATA TCLM/
C***  IGNITION CG
     1  0.000, -1.157, -1.628, -3.148, -3.171, -3.349, -3.286, -4.179,
     2  -4.93,  -6.30,  -7.60,  -8.89, -13.16, -17.70, -20.84,
     1  0.000, -1.512, -2.468, -4.434, -4.990, -5.932, -6.900, -8.428,
     2 -10.15, -12.48, -15.61, -18.64, -26.02, -32.09, -35.63,
     1  0.000, -1.782, -3.040, -5.358, -6.254, -7.680, -9.054,-11.242,
     2 -13.61, -16.63, -20.51, -24.37, -33.22, -39.93, -43.40,
     1  0.000, -2.775, -5.021, -8.563,-10.505,-13.309,-16.071,-19.717,
     2 -23.62, -28.26, -33.88, -39.55, -51.90, -60.16, -63.66,
     1  0.000, -2.144, -3.940, -6.502, -7.639, -9.673,-12.230,-15.609,
     2 -19.55, -24.27, -30.01, -36.09, -48.88, -56.92, -59.81,
     1  0.000, -0.188, -0.263,  0.576,  2.340,  3.267,  3.704,  3.697,
     2   3.09,   1.86,  -0.05,  -2.45,  -7.60, -13.98, -19.65,
     1  0.000,  0.130,  0.267,  1.698,  3.628,  4.653,  5.285,  5.470,
     2   5.08,   3.91,   2.33,   0.34,  -4.95, -11.72, -16.61,
     1  0.000,  0.291,  0.601,  2.083,  4.016,  5.206,  5.770,  6.049,
     2   5.80,   4.93,   3.31,   1.47,  -3.08, -10.54, -16.54,
     1  0.000,  0.402,  0.832,  2.323,  4.202,  5.511,  6.446,  6.724,
     2   6.62,   6.04,   4.74,   2.61,  -2.08, -11.07, -19.99,
     1  0.000,  0.429,  0.889,  2.312,  4.075,  5.338,  6.344,  6.877,
     2   6.77,   6.31,   5.28,   3.44,  -1.67, -10.03, -17.10,
     1  0.000,  0.470,  0.997,  2.364,  4.062,  5.327,  6.357,  7.077,
     2   7.06,   6.64,   5.79,   4.20,  -1.10,  -9.57, -16.85,
     1  0.000,  0.493,  1.168,  2.368,  4.004,  5.255,  6.280,  7.051,
     2   7.24,   6.78,   5.97,   4.50,  -0.73,  -9.22, -16.94,
     1  0.000,  0.522,  1.240,  2.395,  3.988,  5.227,  6.274,  7.077,
     2   7.42,   6.86,   6.02,   4.60,  -0.66,  -8.75, -16.05,
     1  0.000,  0.549,  1.299,  2.425,  3.995,  5.237,  6.303,  7.110,
     2   7.56,   7.01,   6.11,   4.70,  -0.61,  -8.62, -16.25/
C
C*** CORRECTION TO PITCH MOMENT COEFFICIENT WHEN PHIP IS NOT ZERO
C
       DATA TCLMP/
C***  IGNITION CG
     1  0.000, -0.100, -0.821, -0.714, -0.922, -0.083, -0.299,  0.246,
     2 -0.395, -0.274,  0.379,  0.670,  3.625,  6.206,  6.836,
     1  0.000, -0.133, -0.906, -0.930, -1.467, -1.125, -1.488, -1.172,
     2 -1.612, -1.321, -0.163,  1.140,  6.559, 10.373, 11.146,
     1  0.000, -0.150, -0.971, -1.040, -1.743, -1.642, -2.062, -1.885,
     2 -2.245, -1.812, -0.403,  1.412,  8.110, 12.575, 13.415,
     1  0.000, -0.216, -1.235, -1.431, -2.516, -2.682, -3.212, -3.045,
     2 -3.249, -2.509, -0.432,  2.403, 12.050, 18.309, 19.451,
     1  0.000, -0.222, -1.124, -1.481, -2.866, -3.727, -4.697, -4.999,
     2 -5.203, -4.437, -2.329,  0.832, 11.081, 17.475, 18.326,
     1  0.000, -0.108, -0.365, -0.872, -1.903, -2.601, -3.092, -3.372,
     2 -3.306, -2.837, -1.664, -0.237,  2.945,  5.392,  6.521,
     1  0.000, -0.062, -0.246, -1.045, -2.048, -2.717, -3.304, -3.904,
     2 -4.019, -3.449, -2.321, -1.482,  1.552,  4.845,  5.632,
     1  0.000, -0.039, -0.168, -0.739, -1.561, -2.479, -3.002, -3.553,
     2 -3.927, -3.594, -2.520, -1.183,  0.694,  4.166,  5.610,
     1  0.000, -0.027, -0.129, -0.610, -1.197, -1.874, -2.821, -3.321,
     2 -3.696, -3.943, -3.482, -2.081,  0.096,  3.631,  6.621,
     1  0.000, -0.020, -0.104, -0.519, -1.026, -1.480, -2.338, -3.210,
     2 -3.523, -3.780, -3.692, -2.813, -0.229,  3.676,  5.775,
     1  0.000, -0.017, -0.102, -0.453, -0.900, -1.282, -1.933, -3.030,
     2 -3.406, -3.617, -3.682, -3.217, -0.493,  3.454,  5.701,
     1  0.000, -0.015, -0.155, -0.404, -0.804, -1.172, -1.680, -2.651,
     2 -3.416, -3.558, -3.652, -3.329, -0.594,  3.179,  5.727,
     1  0.000, -0.013, -0.151, -0.359, -0.728, -1.061, -1.502, -2.379,
     2 -3.620, -3.691, -3.754, -3.401, -0.486,  3.122,  5.465,
     1  0.000, -0.010, -0.148, -0.326, -0.672, -0.982, -1.399, -2.179,
     2 -3.696, -3.880, -3.865, -3.462, -0.374,  2.991,  5.526/
C
C*** SIDE FORCE COEFFICIENT CORRECTION WHEN PHIP IS NOT ZERO
C
       DATA TCYP/
     1  0.000, -0.017, -0.122, -0.171, -0.383, -0.489, -0.507, -0.506,
     2 -0.479, -0.475, -0.477, -0.411,  0.195,  1.784,  2.833,
     1  0.000, -0.019, -0.120, -0.187, -0.435, -0.610, -0.719, -0.836,
     2 -0.893, -0.910, -0.823, -0.654,  0.204,  2.420,  3.511,
     1  0.000, -0.020, -0.123, -0.197, -0.467, -0.680, -0.849, -1.000,
     2 -1.084, -1.102, -0.988, -0.774,  0.211,  2.766,  3.898,
     1  0.000, -0.025, -0.144, -0.251, -0.606, -0.917, -1.196, -1.447,
     2 -1.587, -1.610, -1.465, -1.172,  0.131,  3.553,  4.933,
     1  0.000, -0.025, -0.120, -0.244, -0.590, -0.929, -1.259, -1.544,
     2 -1.690, -1.694, -1.526, -1.203,  0.089,  3.467,  4.747,
     1  0.000, -0.012, -0.043, -0.178, -0.430, -0.661, -0.883, -1.035,
     2 -1.076, -1.002, -0.850, -0.628,  0.054,  1.960,  2.819,
     1  0.000, -0.006, -0.018, -0.173, -0.412, -0.594, -0.733, -0.848,
     2 -0.944, -1.013, -1.027, -0.874, -0.360,  1.487,  2.896,
     1  0.000, -0.004, -0.015, -0.137, -0.330, -0.488, -0.626, -0.749,
     2 -0.847, -0.925, -0.977, -0.850, -0.403,  1.259,  3.001,
     1  0.000, -0.003, -0.012, -0.114, -0.276, -0.420, -0.553, -0.692,
     2 -0.817, -0.909, -0.953, -0.842, -0.373,  1.084,  3.131,
     1  0.000, -0.002, -0.011, -0.099, -0.239, -0.369, -0.499, -0.637,
     2 -0.779, -0.898, -0.949, -0.830, -0.335,  1.054,  2.836,
     1  0.000, -0.002, -0.012, -0.088, -0.212, -0.332, -0.458, -0.596,
     2 -0.737, -0.878, -0.939, -0.817, -0.320,  0.965,  2.812,
     1  0.000, -0.002, -0.022, -0.079, -0.191, -0.303, -0.425, -0.557,
     2 -0.699, -0.839, -0.922, -0.800, -0.330,  0.863,  2.913,
     1  0.000, -0.001, -0.022, -0.072, -0.174, -0.280, -0.400, -0.531,
     2 -0.680, -0.823, -0.925, -0.793, -0.305,  0.826,  2.812,
     1  0.000, -0.001, -0.022, -0.067, -0.161, -0.265, -0.380, -0.517,
     2 -0.667, -0.816, -0.928, -0.788, -0.280,  0.773,  2.800/
C
C*** YAWING MOMENT COEFFICIENT CORRECTION WHEN PHIP IS NOT ZERO
C
       DATA TCLNP/
C *** IGNITION CG
     1  0.000,  0.084,  0.774,  0.979,  2.526,  3.814,  5.286,  6.842,
     2  8.435,  9.656,  9.319,  9.069,  4.866, -5.554, -8.933,
     1  0.000,  0.107,  0.792,  1.183,  3.050,  4.798,  6.694,  8.264,
     2  9.729, 10.695, 10.684, 10.141,  5.184, -9.582,-14.563,
     1  0.000,  0.121,  0.822,  1.296,  3.347,  5.346,  7.318,  9.209,
     2 10.805, 11.781, 11.777, 11.023,  5.439,-11.696,-17.527,
     1  0.000,  0.167,  1.012,  1.749,  4.480,  7.227,  9.894, 12.438,
     2 14.393, 15.532, 15.376, 14.176,  6.611,-17.117,-25.415,
     1  0.000,  0.168,  0.866,  1.787,  4.557,  7.517, 10.542, 13.362,
     2 15.440, 16.541, 16.507, 15.466,  8.334,-15.380,-23.944,
     1  0.000,  0.088,  0.284,  1.291,  3.353,  5.460,  7.553,  9.314,
     2 10.509, 11.088, 11.172, 10.863,  8.516, -3.449, -8.521,
     1  0.000,  0.050,  0.132,  1.356,  3.313,  4.892,  6.129,  7.413,
     2  8.748,  9.985, 10.887, 10.611,  8.904, -0.966, -7.358,
     1  0.000,  0.032,  0.097,  1.038,  2.522,  3.830,  5.052,  6.342,
     2  7.590,  8.736,  9.611,  9.197,  7.499, -0.105, -7.330,
     1  0.000,  0.022,  0.079,  0.852,  2.053,  3.069,  4.132,  5.360,
     2  6.701,  7.966,  8.962,  8.594,  6.897,  0.987, -8.651,
     1  0.000,  0.017,  0.067,  0.722,  1.742,  2.597,  3.482,  4.600,
     2  5.899,  7.247,  8.304,  8.027,  6.447,  1.063, -7.544,
     1  0.000,  0.013,  0.075,  0.627,  1.515,  2.272,  3.030,  4.062,
     2  5.264,  6.674,  7.723,  7.546,  6.061,  1.296, -7.449,
     1  0.000,  0.012,  0.149,  0.555,  1.342,  2.031,  2.707,  3.633,
     2  4.787,  6.111,  7.253,  7.074,  5.645,  1.500, -7.483,
     1  0.000,  0.009,  0.152,  0.485,  1.184,  1.814,  2.429,  3.274,
     2  4.444,  5.760,  7.051,  6.796,  5.266,  1.528, -7.140,
     1  0.000,  0.008,  0.148,  0.432,  1.064,  1.649,  2.232,  3.014,
     2  4.185,  5.533,  6.894,  6.609,  4.991,  1.628, -7.220/
C
C*** AXIAL FORCE COEFFICIENT
C
C    MACH ACROSS: .6 .85 .95 1.05 1.2 1.6 2 2.4 2.8 3.2 3.6 4 4.5 5
C
       DATA TCA/
     A  0.3430,  0.5380,  0.6850,  0.8320,  0.8090,  0.7420,  0.7400,
     B  0.7390,  0.7280,  0.7110,  0.6970,  0.6850,  0.6750,  0.6650/
C
C*** AXIAL FORCE DERIVATIVE OF ALPHAP (PER DEG)
C
       DATA TCAA/
     A -0.0168, -0.0226, -0.0245, -0.0288, -0.0290, -0.0070, -0.0065,
     B -0.0061, -0.0056, -0.0052, -0.0048, -0.0046, -0.0043, -0.0041/
C
C*** AXIAL FORCE DERIVATIVE OF EFFECTIVE FIN DEFLECTIONS (PER DEG**2)
C
       DATA TCAD/
     A  0.0058,  0.0102,  0.0110,  0.0144,  0.0134,  0.0085,  0.0064,
     B  0.0051,  0.0043,  0.0038,  0.0034,  0.0030,  0.0026,  0.0024/
C
C*** POWER OFF CORRECTION OF CA0
C
       DATA TCAOFF/
     A  0.0940,  0.0910,  0.1070,  0.1440,  0.1330,  0.1190,  0.1010,
     B  0.0850,  0.0720,  0.0600,  0.0520,  0.0450,  0.0420,  0.0380/
C
C*** NORMAL FORCE DERIVATIVE OF ELEVATOR (PER DEG)
C
       DATA TCNDQ/
     1  0.137,  0.134,  0.139,  0.153,  0.163,  0.147,  0.111,  0.102,
     2  0.067,  0.055,  0.057,  0.047,  0.052,  0.022,  0.022,
     1  0.157,  0.153,  0.159,  0.175,  0.187,  0.176,  0.149,  0.142,
     2  0.114,  0.102,  0.101,  0.088,  0.074,  0.021,  0.022,
     1  0.169,  0.165,  0.171,  0.188,  0.201,  0.193,  0.169,  0.164,
     2  0.139,  0.127,  0.124,  0.109,  0.085,  0.022,  0.022,
     1  0.212,  0.208,  0.215,  0.236,  0.252,  0.246,  0.224,  0.220,
     2  0.196,  0.181,  0.175,  0.154,  0.114,  0.025,  0.026,
     1  0.187,  0.183,  0.190,  0.206,  0.219,  0.219,  0.210,  0.210,
     2  0.199,  0.188,  0.179,  0.159,  0.111,  0.019,  0.020,
     1  0.112,  0.112,  0.113,  0.117,  0.120,  0.118,  0.115,  0.109,
     2  0.101,  0.092,  0.082,  0.069,  0.042,  0.018,  0.019,
     1  0.082,  0.082,  0.083,  0.086,  0.090,  0.091,  0.089,  0.088,
     2  0.086,  0.084,  0.077,  0.068,  0.054,  0.022,  0.023,
     1  0.064,  0.064,  0.065,  0.067,  0.070,  0.073,  0.077,  0.074,
     2  0.074,  0.073,  0.074,  0.068,  0.060,  0.032,  0.033,
     1  0.053,  0.053,  0.054,  0.055,  0.058,  0.060,  0.064,  0.069,
     2  0.068,  0.067,  0.069,  0.071,  0.056,  0.046,  0.047,
     1  0.045,  0.045,  0.046,  0.047,  0.049,  0.052,  0.056,  0.061,
     2  0.065,  0.063,  0.064,  0.068,  0.058,  0.036,  0.036,
     1  0.039,  0.039,  0.040,  0.041,  0.043,  0.046,  0.050,  0.054,
     2  0.062,  0.061,  0.061,  0.064,  0.057,  0.038,  0.038,
     1  0.035,  0.035,  0.035,  0.037,  0.039,  0.042,  0.045,  0.050,
     2  0.056,  0.061,  0.059,  0.062,  0.058,  0.044,  0.044,
     1  0.031,  0.031,  0.031,  0.033,  0.034,  0.038,  0.042,  0.047,
     2  0.052,  0.063,  0.058,  0.060,  0.058,  0.041,  0.042,
     1  0.028,  0.027,  0.028,  0.029,  0.031,  0.035,  0.039,  0.044,
     2  0.050,  0.063,  0.058,  0.058,  0.058,  0.043,  0.044/
C
C*** PITCHING MOMENT DERIVATIVE OF ELEVATOR (PER DEG)
C
       DATA TCLMDQ/
C *** IGNITION CG
     1 -1.052, -1.027, -1.062, -1.169, -1.250, -1.127, -0.849, -0.780,
     2 -0.515, -0.417, -0.434, -0.359, -0.401, -0.166, -0.169,
     1 -1.200, -1.171, -1.214, -1.334, -1.427, -1.347, -1.137, -1.086,
     2 -0.873, -0.783, -0.770, -0.671, -0.562, -0.163, -0.165,
     1 -1.293, -1.263, -1.309, -1.436, -1.536, -1.474, -1.295, -1.251,
     2 -1.061, -0.972, -0.945, -0.833, -0.650, -0.164, -0.167,
     1 -1.634, -1.597, -1.654, -1.811, -1.935, -1.889, -1.722, -1.691,
     2 -1.504, -1.394, -1.347, -1.187, -0.879, -0.193, -0.195,
     1 -1.475, -1.442, -1.492, -1.621, -1.724, -1.726, -1.656, -1.657,
     2 -1.565, -1.479, -1.410, -1.254, -0.876, -0.153, -0.155,
     1 -0.893, -0.891, -0.901, -0.931, -0.955, -0.944, -0.913, -0.866,
     2 -0.806, -0.731, -0.655, -0.554, -0.334, -0.147, -0.149,
     1 -0.652, -0.651, -0.658, -0.683, -0.716, -0.727, -0.713, -0.703,
     2 -0.690, -0.671, -0.610, -0.543, -0.430, -0.177, -0.180,
     1 -0.510, -0.509, -0.515, -0.533, -0.556, -0.579, -0.615, -0.591,
     2 -0.587, -0.585, -0.590, -0.540, -0.480, -0.258, -0.263,
     1 -0.420, -0.419, -0.424, -0.439, -0.458, -0.480, -0.512, -0.550,
     2 -0.542, -0.531, -0.548, -0.568, -0.448, -0.369, -0.376,
     1 -0.358, -0.356, -0.362, -0.374, -0.391, -0.413, -0.443, -0.485,
     2 -0.520, -0.502, -0.510, -0.539, -0.458, -0.282, -0.287,
     1 -0.312, -0.310, -0.316, -0.327, -0.342, -0.366, -0.396, -0.432,
     2 -0.495, -0.484, -0.480, -0.508, -0.453, -0.298, -0.304,
     1 -0.277, -0.275, -0.279, -0.290, -0.305, -0.329, -0.360, -0.397,
     2 -0.447, -0.480, -0.465, -0.489, -0.463, -0.346, -0.352,
     1 -0.243, -0.241, -0.246, -0.257, -0.272, -0.298, -0.328, -0.369,
     2 -0.414, -0.495, -0.456, -0.474, -0.464, -0.326, -0.332,
     1 -0.217, -0.215, -0.220, -0.231, -0.247, -0.275, -0.306, -0.349,
     2 -0.395, -0.499, -0.457, -0.460, -0.462, -0.341, -0.347/
C
C*** PITCHING MOMENT DAMPING DERVATIVE (PER DEG)
C
       DATA TCLMQ/
C *** IGNITION CG
     A-23.1551,-20.4505,-19.0051,-18.3272,-11.1299, -8.6025, -7.0830,
     B -6.0708, -5.4161, -4.9576, -4.6164, -4.3528, -4.0976, -3.9001/
C
C*** ROLLING MOMENT DERIVATIVE OF ALPHA**2 FOR NONZERO PHIP (PER DEG**2)
C
       DATA TCLLAP/
     1 0.0000,-.0012,-.0017,-.0007,-.0010,-.0010,-.0010,-.0009,
     2 -.0007,-.0006,-.0004,-.0003,-.0002,0.0000,0.0000,
     1 0.0000,-.0014,-.0016,-.0008,-.0011,-.0011,-.0010,-.0009,
     2 -.0008,-.0006,-.0005,-.0004,-.0003,-.0001,0.0000,
     1 0.0000,-.0016,-.0017,-.0009,-.0011,-.0012,-.0011,-.0010,
     2 -.0008,-.0007,-.0006,-.0005,-.0003,-.0001,0.0000,
     1 0.0000,-.0021,-.0020,-.0011,-.0014,-.0015,-.0014,-.0012,
     2 -.0011,-.0009,-.0008,-.0007,-.0004,-.0001,0.0000,
     1 0.0000,-.0021,-.0018,-.0012,-.0014,-.0014,-.0013,-.0012,
     2 -.0011,-.0009,-.0008,-.0007,-.0005,-.0001,0.0000,
     1 0.0000,-.0013,-.0008,-.0011,-.0012,-.0011,-.0010,-.0009,
     2 -.0008,-.0007,-.0006,-.0005,-.0003,-.0001,0.0000,
     1 0.0000,-.0008,-.0003,-.0010,-.0011,-.0010,-.0008,-.0007,
     2 -.0006,-.0006,-.0005,-.0005,-.0003,-.0001,0.0000,
     1 0.0000,-.0006,-.0002,-.0009,-.0009,-.0008,-.0007,-.0006,
     2 -.0005,-.0005,-.0005,-.0004,-.0003,-.0001,0.0000,
     1 0.0000,-.0003,-.0002,-.0007,-.0008,-.0006,-.0005,-.0005,
     2 -.0005,-.0005,-.0004,-.0004,-.0003,-.0001,0.0000,
     1 0.0000,-.0003,-.0002,-.0006,-.0007,-.0006,-.0005,-.0004,
     2 -.0004,-.0004,-.0004,-.0003,-.0003,-.0001,0.0000,
     1 0.0000,-.0002,-.0002,-.0005,-.0006,-.0005,-.0004,-.0004,
     2 -.0004,-.0004,-.0004,-.0003,-.0002,-.0001,0.0000,
     1 0.0000,-.0002,-.0004,-.0005,-.0005,-.0004,-.0004,-.0003,
     2 -.0003,-.0003,-.0003,-.0003,-.0002,-.0001,0.0000,
     1 0.0000,-.0001,-.0004,-.0004,-.0005,-.0004,-.0003,-.0003,
     2 -.0003,-.0003,-.0003,-.0003,-.0002,-.0001,0.0000,
     1 0.0000,-.0001,-.0004,-.0004,-.0004,-.0004,-.0003,-.0003,
     2 -.0002,-.0003,-.0003,-.0003,-.0002,-.0001,0.0000/
C
C*** ROLLING MOMENT DERIVATIVE OF AILERON (PER DEG)
C
       DATA TCLLDP/
     1  0.084,  0.085,  0.088,  0.090,  0.084,  0.066,  0.052,  0.038,
     2  0.029,  0.028,  0.024,  0.028,  0.026,  0.010,  0.010,
     1  0.096,  0.097,  0.100,  0.103,  0.098,  0.084,  0.074,  0.063,
     2  0.056,  0.054,  0.049,  0.049,  0.034,  0.009,  0.009,
     1  0.103,  0.105,  0.109,  0.111,  0.107,  0.095,  0.086,  0.077,
     2  0.070,  0.067,  0.060,  0.058,  0.037,  0.008,  0.008,
     1  0.129,  0.132,  0.136,  0.139,  0.137,  0.126,  0.118,  0.107,
     2  0.098,  0.093,  0.084,  0.077,  0.049,  0.009,  0.009,
     1  0.119,  0.121,  0.126,  0.130,  0.130,  0.124,  0.120,  0.114,
     2  0.107,  0.102,  0.093,  0.083,  0.050,  0.006,  0.006,
     1  0.075,  0.075,  0.075,  0.074,  0.073,  0.071,  0.068,  0.063,
     2  0.058,  0.052,  0.045,  0.037,  0.021,  0.009,  0.009,
     1  0.057,  0.057,  0.058,  0.059,  0.059,  0.059,  0.058,  0.057,
     2  0.055,  0.052,  0.046,  0.042,  0.029,  0.012,  0.012,
     1  0.046,  0.046,  0.047,  0.048,  0.049,  0.052,  0.052,  0.050,
     2  0.050,  0.050,  0.049,  0.043,  0.038,  0.020,  0.019,
     1  0.039,  0.039,  0.040,  0.041,  0.042,  0.045,  0.048,  0.049,
     2  0.048,  0.048,  0.049,  0.049,  0.037,  0.030,  0.029,
     1  0.034,  0.034,  0.035,  0.036,  0.037,  0.040,  0.043,  0.047,
     2  0.048,  0.046,  0.048,  0.050,  0.039,  0.023,  0.023,
     1  0.030,  0.030,  0.031,  0.032,  0.033,  0.036,  0.039,  0.044,
     2  0.047,  0.044,  0.046,  0.049,  0.040,  0.025,  0.025,
     1  0.027,  0.027,  0.028,  0.029,  0.031,  0.033,  0.036,  0.041,
     2  0.045,  0.045,  0.045,  0.048,  0.041,  0.030,  0.030,
     1  0.024,  0.024,  0.025,  0.027,  0.028,  0.031,  0.034,  0.038,
     2  0.044,  0.048,  0.044,  0.047,  0.041,  0.029,  0.028,
     1  0.022,  0.022,  0.023,  0.025,  0.026,  0.029,  0.033,  0.037,
     2  0.044,  0.049,  0.044,  0.046,  0.041,  0.031,  0.030/
C
C*** ROLLING MOMENT DAMPING DERIVATIVE
C
       DATA TCLLP/
     1 -0.156, -0.158, -0.163, -0.167, -0.157, -0.122, -0.097, -0.071,
     2 -0.053, -0.052, -0.045, -0.053, -0.048, -0.019, -0.019,
     1 -0.178, -0.181, -0.186, -0.190, -0.183, -0.157, -0.137, -0.118,
     2 -0.105, -0.101, -0.091, -0.090, -0.063, -0.016, -0.016,
     1 -0.191, -0.195, -0.202, -0.206, -0.199, -0.176, -0.160, -0.142,
     2 -0.129, -0.124, -0.112, -0.107, -0.069, -0.015, -0.015,
     1 -0.239, -0.244, -0.252, -0.258, -0.254, -0.234, -0.218, -0.199,
     2 -0.183, -0.173, -0.156, -0.143, -0.090, -0.016, -0.016,
     1 -0.221, -0.225, -0.234, -0.242, -0.241, -0.231, -0.223, -0.212,
     2 -0.199, -0.189, -0.173, -0.154, -0.094, -0.012, -0.012,
     1 -0.138, -0.139, -0.139, -0.138, -0.136, -0.132, -0.125, -0.117,
     2 -0.107, -0.096, -0.084, -0.069, -0.039, -0.017, -0.017,
     1 -0.105, -0.106, -0.107, -0.109, -0.110, -0.109, -0.107, -0.105,
     2 -0.102, -0.096, -0.085, -0.078, -0.053, -0.022, -0.022,
     1 -0.086, -0.086, -0.088, -0.089, -0.091, -0.096, -0.097, -0.094,
     2 -0.093, -0.093, -0.090, -0.079, -0.070, -0.036, -0.036,
     1 -0.072, -0.073, -0.075, -0.076, -0.078, -0.083, -0.089, -0.092,
     2 -0.088, -0.089, -0.091, -0.091, -0.069, -0.056, -0.055,
     1 -0.063, -0.064, -0.065, -0.067, -0.069, -0.074, -0.080, -0.087,
     2 -0.088, -0.085, -0.088, -0.092, -0.072, -0.043, -0.042,
     1 -0.056, -0.056, -0.058, -0.060, -0.062, -0.067, -0.073, -0.082,
     2 -0.088, -0.082, -0.085, -0.091, -0.074, -0.047, -0.046,
     1 -0.050, -0.051, -0.052, -0.054, -0.057, -0.062, -0.068, -0.075,
     2 -0.084, -0.084, -0.083, -0.088, -0.076, -0.056, -0.055,
     1 -0.045, -0.045, -0.047, -0.049, -0.052, -0.057, -0.063, -0.071,
     2 -0.082, -0.089, -0.081, -0.086, -0.075, -0.054, -0.053,
     1 -0.040, -0.041, -0.043, -0.046, -0.048, -0.054, -0.061, -0.068,
     2 -0.081, -0.091, -0.081, -0.085, -0.075, -0.058, -0.056/
C
C*** VEHICLE REFERENCE VALUES
C
      REFA= .1963*(1.-.9071*OPTMET)
      REFL= .5/(1.+2.28*OPTMET)
C
C*** INITIALIZE AERODYNAMIC DERIVATIVES FOR AUTOPILOT
C
      CALL G2
      CALL A2I
      CALL A1
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
C         1     TRCVEL     C1    MINIMUM CLOSING VELOCITY - ft/s
C         2     TRMACH     G2    MINIMUM MACH NUMBER
C         3     TRDYNM     G2    MINIMUM DYNAMIC PRESSURE - lbf/ft^2
C         4     TRLOAD     A1    MINIMUM LOAD FACTOR - g's
C         5     TRALP      G3    MAXIMUM TOTAL INCIDENCE ANGLE - rad
C         6     TRTHT      S1    MAXIMUM PITCH GIMBAL ANGLE - rad
C         7     TRTHTD     S1    MAXIMUM PITCH GIMBAL RATE - rad/s
C         8     TRPHID     S1    MAXIMUM ROLL GIMBAL RATE  - rad/s
C         9     TRATE      S1    MAXIMUM TRACKING RATE - rad/s
C
      TRCVEL= 0.
      TRMACH= .8
      TRDYNM= 10.E+3
      TRLOAD=3.
      TRALP=1.
      TRTHT=1.57
      TRPHID=14.
      TRTHTD=10.
      TRATE=1.
C
      TRCODE=0.
      TMCODE=0.
C
      RETURN
      END
C******************************************************************
      SUBROUTINE A1
C******************************************************************
C***  *
C***  * Aerodynamic Module
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Calculates aerodynamic coefficients from look-up tables.
C***  * (2) Converts them into body axes
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 950125 Created by Peter Zipfel
C***  * 960118 3-DIM table look up (interpolates cg location), PZi
C***  * 960411 Corrected fin drag calculation. DEFF. PZi
C***  * 970314 Account for CG travel by moment arm correction, TAO
C***  *
C***  *************************************************************
C
C*** COMMON LOCATIONS ARE 1200-1299
C
      COMMON C(3510)
C
      COMMON/AERO/TCN(210),TCNP(210),TCLM(210),TCLMP(210),TCYP(210)
     &,TCLNP(210),TCA(14),TCAA(14),TCAD(14),TCAOFF(14),TCNDQ(210)
     &,TCLMDQ(210),TCLMQ(14),TCLLAP(210),TCLLDP(210),TCLLP(210)
     &,ARGX(14),ARGXY(29),NX,NXY(2)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1265),ALPLIMX)
C
C ALPLIMX = D Maximum total alpha permissible - deg
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2000),T)
C
C*** FROM INITIALIZATION MODULE
C
      EQUIVALENCE (C(1203),REFL)
      EQUIVALENCE (C(1204),REFA)
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1285),TRLOAD)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0206),VMACH)
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(0340),ALPPX)
      EQUIVALENCE (C(0343),PHIP)
      EQUIVALENCE (C(0907),ALIMIT)
      EQUIVALENCE (C(1119),DPX)
      EQUIVALENCE (C(1120),DQX)
      EQUIVALENCE (C(1121),DRX)
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1314),XCGREF)
      EQUIVALENCE (C(1320),XCG)
      EQUIVALENCE (C(1636),DVBE)
      EQUIVALENCE (C(1710),PPX)
      EQUIVALENCE (C(1711),QQX)
      EQUIVALENCE (C(1712),RRX)
C
C AGRAV= E Gavity acceleration (9.806635 m/s^2)
C VMACH= O Mach number of missile
C PDYNMC= O Dynamic pressure - Pa
C ALPPX= O Total angle of attack - deg
C PHIP= O Aerodynamic roll angle - rad
C ALIMIT= D Total structural acceleration limiter - g's
C DPX= O Roll control deflection - deg
C DQX= O Pitch control deflection - deg
C DRX= O Yaw control deflection - deg
C MPROP= D =0: Motor off, =1:Motor on
C VMASS= O Vehicle mass - kg
C XCGREF= I Launch CG aft of vehicle nose - m
C XCG= G Vehicle CG aft of vehicle nose - m
C DVBE= I/O Vehicle speed - m/s
C PPX= O Body roll angular velocity in body axes - deg/s
C QQX= O Body pitch angular velocity in body axes - deg/s
C RRX= O Body yaw angular velocity in body axes - deg/s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1220),CA)
      EQUIVALENCE (C(1221),CY)
      EQUIVALENCE (C(1222),CN)
      EQUIVALENCE (C(1223),CLL)
      EQUIVALENCE (C(1224),CLM)
      EQUIVALENCE (C(1225),CLN)
      EQUIVALENCE (C(1267),GMAX)
C
C CA = O Axial force coefficient
C CY = O Side force coefficient
C CN = O Normal force coefficient
C CLL = O Rolling moment coefficient
C CLM = O Pitching moment coefficient
C CLN = O Yawing moment coefficient
C GMAX = G Max maneuverability limited by ALPLIMX - g's
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1236),CA0)
      EQUIVALENCE (C(1237),CAA)
      EQUIVALENCE (C(1238),CAD)
      EQUIVALENCE (C(1234),CYP)
      EQUIVALENCE (C(1230),CN0)
      EQUIVALENCE (C(1231),CNP)
      EQUIVALENCE (C(1239),CNDQ)
      EQUIVALENCE (C(1242),CLLAP)
      EQUIVALENCE (C(1244),CLLP)
      EQUIVALENCE (C(1243),CLLDP)
      EQUIVALENCE (C(1232),CLM0)
      EQUIVALENCE (C(1233),CLMP)
      EQUIVALENCE (C(1241),CLMQ)
      EQUIVALENCE (C(1240),CLMDQ)
      EQUIVALENCE (C(1235),CLNP)
      EQUIVALENCE (C(1252),CNA)
      EQUIVALENCE (C(1253),CYA)
      EQUIVALENCE (C(1254),CLMA)
      EQUIVALENCE (C(1255),CLNA)
      EQUIVALENCE (C(1266),GAVAIL)
C
C CA0 = G Axial force coeff at zero incidence
C CAA = G Axial force coeff alpha derivative - 1/deg
C CAD = G Axial force coeff of control surface drag - 1/deg^2
C CYP = G Side force coeff of roll attitude -
C CN0 = G Normal force coeff as function of Mach and alpha
C CNP = G Normal force coeff of roll attitude -
C CNDQ = G Normal force coeff of pitch control deflection - 1/deg
C CLLAP = G Rolling moment coeff of incidence angle - 1/deg^2
C CLLP = G Rolling moment coeff of roll damping - 1/deg
C CLLDP = G Rolling moment coeff of roll control deflec - 1/deg
C CLM0 = G Pitching moment coeff as a function of Mach and alpha
C CLMP = G Pitching moment coeff of roll attitude -
C CLMQ = G Pitching moment coeff pitch damping - 1/deg
C CLMDQ = G Pitching moment coeff of pitch control deflec - 1/deg
C CLNP = G Yawing moment coeff of roll attitude -
C CNA = G Normal force coeff in aeroballistic axes -
C CYA = G Side force coeff in aeroballistic axes -
C CLMA = G Pitching moment coeff in aeroballistic axes -
C CLNA = G Yawing moment coeff in aeroballistic axes -
C GAVAIL = G Maneuver headroom - g's
C
C*** TRANSFORM CONTROL COMMANDS AND BODY RATES, BODY --> AEROBALLISTIC AXES
C
      CPHIP=COS(PHIP)
      SPHIP=SIN(PHIP)
      DQAX=DQX*CPHIP-DRX*SPHIP
      DRAX=DQX*SPHIP+DRX*CPHIP
C
      QQAX=QQX*CPHIP-RRX*SPHIP
      RRAX=QQX*SPHIP+RRX*CPHIP
C
      ALPPAX=ABS(ALPPX)
C
C*** AXIAL FORCE COEFFICIENT
C
      VMACHI=VMACH
      CALL TABLE(VMACHI,ARGX,TCA,NX,CA0)
      VMACHI=VMACH
      CALL TABLE(VMACHI,ARGX,TCAA,NX,CAA)
      VMACHI=VMACH
      CALL TABLE(VMACHI,ARGX,TCAD,NX,CAD)
      VMACHI=VMACH
      CALL TABLE(VMACHI,ARGX,TCAOFF,NX,CAOFF)
C
      DEFF=(ABS(DQAX)+ABS(DRAX))/2.
      CA=CA0+CAA*ALPPX+CAD*DEFF**2.+(1-MPROP)*CAOFF
C
C*** SIDE FORCE COEFFICIENT IN AEROBALLISTIC AXES
C
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCYP,NXY,CYP)
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCNDQ,NXY,CYDR)
C
      S4PHI=SIN(4.*PHIP)
      CYA=CYP*S4PHI+CYDR*DRAX
C
C*** NORMAL FORCE COEFFICIENT IN AEROBALLISTIC AXES
C
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCN,NXY,CN0)
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCNP,NXY,CNP)
      CNDQ=CYDR
C
      S2PHI=(SIN(2.*PHIP))**2
      CNA=CN0+CNP*S2PHI+CNDQ*DQAX
C
C** ROLLING MOMENT COEFFICIENT
C
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCLLAP,NXY,CLLAP)
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCLLP,NXY,CLLP)
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCLLDP,NXY,CLLDP)
C
      CLL=CLLAP*ALPPX**2*S4PHI+CLLP*PPX*REFL/(2.*DVBE)+CLLDP*DPX
C
C*** PITCHING MOMENT COEFFICIENT IN AEROBALLISTIC AXES
C
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCLM,NXY,CLM0)
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCLMP,NXY,CLMP)
      VMACHI=VMACH
      CALL TABLE(VMACHI,ARGX,TCLMQ,NX,CLMQ)
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCLMDQ,NXY,CLMDQ)
C
      CLMAREF=CLM0+CLMP*S2PHI+CLMQ*QQAX*REFL/(2.*DVBE)+CLMDQ*DQAX
      CLMA=CLMAREF-CNA*(XCGREF-XCG)/REFL
C
C*** YAWING MOMENT COEFFICIENT IN AEROBALLISTIC AXES
C
      ALPHAI=ALPPAX
      VMACHI=VMACH
      CALL TABL2(ALPHAI,VMACHI,ARGXY,TCLNP,NXY,CLNP)
C
      CLNR=CLMQ
      CLNDR=CLMDQ
      CLNAREF=CLNP*S4PHI+CLNR*RRAX*REFL/(2.*DVBE)+CLNDR*DRAX
      CLNA=CLNAREF-CYA*(XCGREF-XCG)/REFL
C
C*** FORCE COEFFICIENTS IN BODY AXES
C
      CY=CYA*CPHIP-CNA*SPHIP
      CN=CYA*SPHIP+CNA*CPHIP
C
C*** MOMENT COEFFICIENT IN BODY AXES
C
      CLM=CLMA*CPHIP+CLNA*SPHIP
      CLN=-CLMA*SPHIP+CLNA*CPHIP
C
C*** SUBROUTINE CALL FOR DERIVATIVE CALCULATIONS
 
      CALL A1DER(CNDQ,CLMDQ,CLMQ,CLLP,CLLDP)
C
C***  CALCULATE LOAD FACTOR AVAILABLE
C
      ALPMAX=ALPLIMX
      VMACHI=VMACH
      CALL TABL2(ALPMAX,VMACHI,ARGXY,TCN,NXY,CN0MX)
C
      ALMX=CN0MX*PDYNMC*REFA
      WEIGHT=VMASS*AGRAV
      GMAX=ALMX/WEIGHT
      IF(GMAX.GE.ALIMIT)GMAX=ALIMIT
      ALOAD=CN0*PDYNMC*REFA
      GG=ALOAD/WEIGHT
      GAVAIL=GMAX-GG
C
C*** RUN TERMINATION IF MAX G CAPABILITY IS LESS THAN TRLOAD
C
      IF(GMAX.LT.TRLOAD) TRCODE=4.
C
      RETURN
      END
C******************************************************************
      SUBROUTINE A1DER(CNDQ,CLMDQ,CLMQ,CLLP,CLLDP)
C***  *
C***  * Aerodynamic Derivaties
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Calculates Non-dimensional Derivatives (radians)
C***  * (2) Calculates Dimensional Derivaties (rad, ft, sec)
C***  * (3) Calculates the Airframe Roots
C***  *
C***  * Input:
C***  *        CNDQ=Normal force control derivative - 1/deg
C***  *        CLMDQ=Pitch control derivative - 1/deg
C***  *        CLMQ=Pitch damping derivative - 1/deg
C***  *        CLLP=Roll damping derivative - 1/deg
C***  *        CLLDP=Roll control derivative - 1/deg
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 950125 Created by Peter Zipfel
c***  * 950726 MFREEZE disabled (PZi)
C***  * 951011 TVC augmented derivatives (PZi)
C***  *
C***  *************************************************************
C
      COMMON C(3510)
C
      COMMON/AERO/TCN(210),TCNP(210),TCLM(210),TCLMP(210),TCYP(210)
     &,TCLNP(210),TCA(14),TCAA(14),TCAD(14),TCAOFF(14),TCNDQ(210)
     &,TCLMDQ(210),TCLMQ(14),TCLLAP(210),TCLLDP(210),TCLLP(210)
     &,ARGX(14),ARGXY(29),NX,NXY(2)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1265),ALPLIMX)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2000),T)
C
C*** INPUT FROM INITIALIZATION MODULE
C
      EQUIVALENCE (C(1203),REFL)
      EQUIVALENCE (C(1204),REFA)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0206),VMACH)
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(0340),ALPPX)
      EQUIVALENCE (C(0901),MFREEZE)
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1313),THRUST)
      EQUIVALENCE (C(1314),XCGREF)
      EQUIVALENCE (C(1320),XCG)
      EQUIVALENCE (C(1321),AI11)
      EQUIVALENCE (C(1322),AI33)
      EQUIVALENCE (C(1636),DVBE)
C
C CRAD= E Conversion from radians to degree (57.29577951)
C VMACH= O Mach number of missile
C PDYNMC= O Dynamic pressure - Pa
C ALPPX= O Total angle of attack - deg
C MFREEZE= D =0:Unfreeze and print; =1:Freeze; increment for more
C VMASS= O Vehicle mass - kg
C THRUST= O Rocket thrust parallel to vehicle centerline - N
C XCGREF= I Launch CG aft of vehicle nose - m
C XCG= G Vehicle CG aft of vehicle nose - m
C AI11= O Roll Moment of Inertia - kg*m^2
C AI33= O Pitch/Yaw moment of Inertia - kg*m^2
C DVBE= I/O Vehicle speed - m/s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1245),DNA)
      EQUIVALENCE (C(1246),DND)
      EQUIVALENCE (C(1247),DMA)
      EQUIVALENCE (C(1248),DMQ)
      EQUIVALENCE (C(1249),DMD)
      EQUIVALENCE (C(1250),DLP)
      EQUIVALENCE (C(1251),DLD)
C
C DNA = O Normal force slope derivative - m/s^2
C DND = O Pitch control force derivative - m/s^2
C DMA = O Pitch moment derivative - 1/s^2
C DMQ = O Pitch damping derivative - 1/s
C DMD = O Pitch control derivative - 1/s^2
C DLP = O Roll damping derivative - 1/s
C DLD = O Roll control derivative - 1/s^2
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1259),STMARG)
      EQUIVALENCE (C(1260),REALQ1)
      EQUIVALENCE (C(1261),REALQ2)
      EQUIVALENCE (C(1262),WNQ)
      EQUIVALENCE (C(1263),ZETQ)
      EQUIVALENCE (C(1264),REALP)
      EQUIVALENCE (C(1268),PQREAL)
C
C STMARG = G Static margin (+stable, -unstable) - caliber
C REALQ1 = G First real root of airframe pitch dynamics - rad/s
C REALQ2 = G Second real root of airframe pitch dynamics - rad/s
C WNQ = G Natural frequency of airframe pitch dynamics - rad/s
C ZETQ = G Damping of airframe pitch dynamics - NA
C REALP = G Real root of airframe roll dynamics - rad/s
C PQREAL = G Real part or mean value (real roots) of airf dyn - rad/s
C
C*** NON-DIMENSIONAL DERIVATIVES (IN RADIANS)
C*** PERFORM CALCULATIONS ONLY AS LONG AS ALPPX < ALPMAX
C
      IF(ALPPX.GE.(ALPLIMX-3.)) GOTO 100
C
      ALPP=ALPPX+3.
      IF(ALPP.LT.3.)ALPP=3.
      ALPM=ALPPX-3.
      IF(ALPM.LT.0.)ALPM=0.
C
      ALPPI=ALPP
      VMACHI=VMACH
      CALL TABL2(ALPPI,VMACHI,ARGXY,TCN,NXY,CN0P)
      ALPMI=ALPM
      VMACHI=VMACH
      CALL TABL2(ALPMI,VMACHI,ARGXY,TCN,NXY,CN0M)
C
      DUM=(CN0P-CN0M)
      IF(DUM.NE.0.)CNA=CRAD*DUM/(ALPP-ALPM)
C
      CND=CRAD*CNDQ
C
      ALPPI=ALPP
      VMACHI=VMACH
      CALL TABL2(ALPPI,VMACHI,ARGXY,TCLM,NXY,CLM0P)
      ALPMI=ALPM
      VMACHI=VMACH
      CALL TABL2(ALPMI,VMACHI,ARGXY,TCLM,NXY,CLM0M)
C
      CMA=CRAD*(CLM0P-CLM0M)/(ALPP-ALPM)-CNA*(XCGREF-XCG)/REFL
C
      CMQ=CRAD*CLMQ
      CMD=CRAD*CLMDQ
      CLP=CRAD*CLLP
      CLD=CRAD*CLLDP
C
C*** DIMENSIONAL DERIVATIVES
C
      DUMN=PDYNMC*REFA/VMASS
      DNA=DUMN*CNA
      DND=DUMN*CND
C
      DUMM=PDYNMC*REFA*REFL/AI33
      DMA=DUMM*CMA
      DMQ=DUMM*(REFL/(2.*DVBE))*CMQ
      DMD=DUMM*CMD
C
      DUML=PDYNMC*REFA*REFL/AI11
      DLP=DUML*(REFL/(2.*DVBE))*CLP
      DLD=DUML*CLD
C
C*** STATIC MARGIN
C
      STMARG=-CMA/CNA
C
C*** MANEUVER PLANE ROOTS
C
      A11=DMQ
      A12=DMA/DNA
      A21=DNA
      A22=-DNA/DVBE
C
      ARG=(A11+A22)**2-4.*(A11*A22-A12*A21)
      IF(ARG.GE.0.) THEN
         WNQ=0.
         ZETQ=0.
         DUM=A11+A22
         REALQ1=(DUM+SQRT(ARG))/2.
         REALQ2=(DUM-SQRT(ARG))/2.
         PQREAL=(REALQ1+REALQ2)/2.
      ELSE
         REALQ1=0.
         REALQ2=0.
         WNQ=SQRT(A11*A22-A12*A21)
         ZETQ=-(A11+A22)/(2.*WNQ)
         PQREAL=-ZETQ*WNQ
      ENDIF
C
C*** ROLL RATE ROOT
C
      REALP=DLP
C
 100  RETURN
      END
C******************************************************************
      SUBROUTINE A2I
C******************************************************************
C***  *
C***  * The initialization module for the Propulsion Module A2.
C***  *
C***  * Configuration: Boost rocket motor
C***  *               Initial mass of vehicle 6.301 slugs
C***  *               Initial cg location 60.46 inches from nose
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Initializes vehicle mass properties
C***  *
C***  * MODIFICATION HISTORY
C***  * 960228 Created by TAO
C***  *
C***  *************************************************************
C
C*** COMMON LOCATIONS ARE 1300 - 1399
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0053),OPTMET)
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1314),XCGREF)
      EQUIVALENCE (C(1321),AI11)
      EQUIVALENCE (C(1322),AI33)
C
      VMASS=6.301*(1.+13.59*OPTMET)
      XCGREF=(60.46/12.)*(1.-.6952*OPTMET)
      AI11=.227*(1.+.356*OPTMET)
      AI33=43.70*(1.+.356*OPTMET)
C
      RETURN
      END
C******************************************************************
      SUBROUTINE A2
C******************************************************************
C***  *
C***  * Propulsion Module.
C***  *
C***  * This module performs the following functions:
C***  *
C***  * 1) Calculates vehicle mass properties
C***  * 2) Calculates the rocket thrust at altitude
C***  *
C***  * MODIFICATION HISTORY
C***  * 951003 Created by TAO
C***  *
C***  *************************************************************
C
C*** COMMON LOCATIONS ARE 1300 - 1399
C
      COMMON C(3510)
C
      DIMENSION PROTIM(13),TTHRSL(13),TWGT(13),TCGLOC(13),TMIPIT(13)
     &,TMIROL(13)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1300),MPROP)
C
C MPROP = D =0: Motor off, =1:Motor on
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0053),OPTMET)
      EQUIVALENCE (C(2000),T)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0202),PRESS)
      EQUIVALENCE (C(0901),MFREEZE)
C
C PRESS= O Atmospheric pressure - Pa
C MFREEZE= D =0:Unfreeze and print; =1:Freeze; increment for more
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1313),THRUST)
      EQUIVALENCE (C(1320),XCG)
      EQUIVALENCE (C(1321),AI11)
      EQUIVALENCE (C(1322),AI33)
C
C VMASS = O Vehicle mass - kg
C THRUST = O Rocket thrust parallel to vehicle centerline - N
C XCG   = G Vehicle CG aft of vehicle nose - m
C AI11  = O Roll Moment of Inertia - kg*m^2
C AI33  = O Pitch/Yaw moment of Inertia - kg*m^2
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1331),WEIGHT)
C
C WEIGHT = G Vehicle weight - N
C
C*** PARAMETERS
C
      PSL=2116.+(99208.*OPTMET)
      AEXIT=.1351*(1.- .9071*OPTMET)
C
C PSL =Atmospheric pressure at sea level - lbf/ft^2
C AEXIT =Nozzle exit area - ft^2
C
C*** PROPULSION DATA TTHRSL =THRUST AT SEA LEVEL(LB),TWGT=VEHICLE WEIGHT (LB)
C    TMIPIT =PITCH MOMENT OF INERTIA (SG*FT^2), TMIROL =ROLL MOI (SG*FT^2)
C    TCGLOC =CG LOCATION (INCH FROM NOSE)
      DATA PROTIM/0.,.05,.269,.538,.807,1.076,1.345,
     A   1.614,1.883,2.152,2.421,2.690,6.000/
      DATA TTHRSL/0.,7141.,7747.,8083.,8109.,7970.,7796.,
     A   7630.,7463.,7272.,7031.,0.,0./
      DATA TWGT/202.73,200.70,196.10,187.80,178.85,171.10,162.77,
     A   154.71,146.96,139.52,131.55,124.11,124.11/
      DATA TCGLOC/60.46,60.39,59.32,58.18,57.04,55.87,54.69,
     A   53.42,52.12,50.77,49.47,48.05,48.05/
      DATA TMIPIT/44.11,44.07,42.75,41.44,40.13,38.96,37.76,
     A   36.68,35.61,34.60,33.67,32.72,32.72/
      DATA TMIROL/.227,.227,.221,.216,.210,.203,.196,
     A   .189,.180,.170,.160,.149,.149/
C
      DATA NX/13/
C
C*** FUEL MASS, ROCKET MASS, AND THRUST
C
      IF (MPROP.EQ.1) THEN
         TIME=T
         CALL TABLE(TIME,PROTIM,TTHRSL,NX,THRSL)
         TSL=THRSL*(1.+3.448*OPTMET)
         THRUST=TSL+(PSL-PRESS)*AEXIT
         TIME=T
         CALL TABLE(TIME,PROTIM,TWGT,NX,WEIGHT)
         VMASS=WEIGHT*(1.+13.59*OPTMET)/32.174
         TIME=T
         CALL TABLE(TIME,PROTIM,TCGLOC,NX,XCGINCH)
         XCG=(XCGINCH/12.)*(1.-.6952*OPTMET)
         TIME=T
         CALL TABLE(TIME,PROTIM,TMIPIT,NX,AI33)
         AI33=AI33*(1.+.356*OPTMET)
         TIME=T
         CALL TABLE(TIME,PROTIM,TMIROL,NX,AI11)
         AI11=AI11*(1.+.356*OPTMET)
         IF(T.GT.2.69)MPROP=0
      ELSE
         THRUST=0.
      ENDIF
C
      RETURN
      END
C******************************************************************
      SUBROUTINE C1
C******************************************************************
C***  *
C***  * Guidance Module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) MGUID= 0: No Guidance
C***  *            2: Look Angle Guidance (Implemented in C2)
C***  *            3: Midcourse (Pro-Nav based on INS information)
C***  *               Set by input or in Module S1 if break lock occured
C***  *            6: Terminal (Pro-Nav based on LOS rates only)
C***  *               Set in Module S1 when MSEEK=4
C***  *
C***  * (2) Receives targeting data (MNAV=3) from the AI radar and
C***  *     extrapolates target postion to current time
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 950125 Created by Peter Zipfel
C***  * 950206 Change in switching logic PZ
C***  * 980212 Added MNAV=0 statement after update is received, PZi
C***  * 980625 RT-CADAC: EPCHTA equivalenced, PZi
C***  *
C******************************************************************
C
C*** COMMON LOCATIONS ARE 800-899
C
      COMMON C(3510)
C
      DIMENSION ST1CEL(3),VT1CEL(3),VT1EL(3),DUM3(3)
     +,ST1ELC(3),VT1ELC(3),ST1BLC(3),SBELC(3),UT1BLC(3)
     +,TBLC(3,3),UT1BBC(3),VBELC(3),VT1BLC(3),DUM9(3,3),WOELC(3)
     +,AAPNB(3),DUM4(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0800),MGUID)
C
C MGUID = D/G =0:None, =2:LAG, =3:Pro-Nav, =6:Comp Pro-Nav
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(2000),T)
C
C*** RT-CADAC
C
      EQUIVALENCE (C(0805),EPCHTA)
C
C EPCHTA = R Epoch of target update - s
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0500),MNAV)
      EQUIVALENCE (C(0511),ST1CEL(1))
      EQUIVALENCE (C(0514),VT1CEL(1))
      EQUIVALENCE (C(0703),VBELC(1))
      EQUIVALENCE (C(0715),TBLC(1,1))
      EQUIVALENCE (C(0776),SBELC(1))
C
C VT1EL= S Velocity of T1 wrt earth in local level coord - m/s
C MNAV= O =0: Reset, =1: Tracking, =2: Measurement, =3:Update
C ST1CEL= O Target position measured by AI radar - m
C VT1CEL= O Target velocity measured by AI radar - m/s
C VBELC= O Computed body vel in earth coor - m/s
C TBLC= O Comp trans matrix of body wrt earth coor - None
C SBELC= O Computed pos of body wrt earth reference point - m
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0802),ANCOM)
      EQUIVALENCE (C(0803),ALCOM)
      EQUIVALENCE (C(0840),WOELC(1))
      EQUIVALENCE (C(0848),UT1BLC(1))
C
C ANCOM = O Normal acceleration command - g's
C ALCOM = O Lateral acceleration command - g's
C WOELC(3) = O LOS rate computed from INS data - rad/s
C UT1BLC(3) = O LOS unit vector from INS data - NA
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0843),TGOC)
      EQUIVALENCE (C(0844),DT1BC)
      EQUIVALENCE (C(0845),DVT1BC)
      EQUIVALENCE (C(0846),PSIOBCX)
      EQUIVALENCE (C(0847),THTOBCX)
      EQUIVALENCE (C(0855),ST1ELC(1))
      EQUIVALENCE (C(0858),ST1BLC(1))
C
C TGOC = G Time-to-go, computed - s
C DT1BC = G Distance-to-target, computed - m
C DVT1BC = G Closing speed, computed - m/s
C PSIOBCX = G Yaw LOS angle wrt missile - deg
C THTOBCX = G Pitch LOS angle wrt missile - deg
C ST1ELC(3) = G Target location, extrapol onboard missile - m
C ST1BLC(3) = G target wrt Missile position, extrapolated - m
C
C***     TARGET DATA RECEIPT AND EXTRAPOLATION
C
C980212 Assure that MNAV is an integer and add MNAV=0
      IF(MNAV.EQ.3) THEN
	   MNAV=0
C980212 End
         EPCHTA=T
         CALL MATEQL(DUM4,ST1CEL,3,1)
         CALL MATEQL(VT1ELC,VT1CEL,3,1)
      END IF
C
      DTIMEX=T-EPCHTA
      CALL MATCON(DUM3,DTIMEX,VT1ELC,3,1)
      CALL MATADD(ST1ELC,DUM4,DUM3,3,1)
      CALL MATSUB(ST1BLC,ST1ELC,SBELC,3,1)
C
      IF(MGUID.EQ.3) THEN
C
C***     INS MIDCOURSE
C
C***     LINE OF SIGHT KINEMATICS
C
         CALL MATABS(DT1BC,ST1BLC,3)
C
C***     UNIT LOS VECTOR
C
         DUM1=1./DT1BC
         CALL MATCON(UT1BLC,DUM1,ST1BLC,3,1)
         CALL MATMUL(UT1BBC,TBLC,UT1BLC,3,3,1)
C
C***     LOS ANGLES WRT MISSILE BODY
C
         CALL MATPOL(DUM,PSIOBC,THTOBC,UT1BBC)
         THTOBCX=THTOBC*CRAD
         PSIOBCX=PSIOBC*CRAD
C
C***     RELATIVE VELOCITY
C
         CALL MATSUB(VT1BLC,VT1ELC,VBELC,3,1)
C
C***     CLOSING VELOCITY
C
         CALL MATSCA(DUM2,UT1BLC,VT1BLC,3)
         DVT1BC=ABS(DUM2)
C
C***     TIME TO GO
C
         TGOC=DT1BC/DVT1BC
C
C***     INERTIAL LOS RATES IN LOCAL COORDINATES
C
         CALL MATSKS(DUM9,UT1BLC)
         CALL MATMUL(DUM3,DUM9,VT1BLC,3,3,1)
         CALL MATCON(WOELC,DUM1,DUM3,3,1)
C
C***     MIDCOURSE GUIDANCE
C
         CALL C1MID(AAPNB,WOELC,DVT1BC,PSIOBC,THTOBC,TBLC)
C
         ANCOM=-AAPNB(3)/AGRAV
         ALCOM=AAPNB(2)/AGRAV
C
      ENDIF
C
C*** TERMINAL GUIDANCE
C
      IF(MGUID.EQ.6) THEN
C
         CALL C1TERM(ALCOM,ANCOM)
C
      ENDIF
C
      RETURN
      END
C******************************************************************
      SUBROUTINE C1MID(AAPNB,WOEL,DVBO,PSIOB,THTOB,TBL)
C******************************************************************
C***  *
C***  * Midcourse Guidance Law
C***  *
C***  * Proportional Navigation using full state information
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Calculates acceleration commands from inertial LOS rates
C***  *     and closing speed
C***  *
C***  * Argument Output:
C***  *
C***  *          AAPNB(3)=Acceleration command - m/s^2
C***  *
C***  * Argument Input:
C***  *
C***  *          WOEL(3)=Inertial LOS rate - rad/s
C***  *          DVBO=Closing speed - m/s
C***  *          PSIOB=Yaw LOS angle - rad
C***  *          THTOB=Pitch LOS angle - rad
C***  *          TBL(3,3)=Transf matrix of body wrt local level coord
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 950125 Created by Peter Zipfel
C***  *
C***  *************************************************************
C
      COMMON C(3510)
C
      DIMENSION AAPNB(3),UOBL(3),WOEL(3),TBL(3,3),DUM3(3),DUM9(3,3)
     +,APNL(3),UOBB(3),TLB(3,3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0801),GNAV)
C
C*** PROPORTIONAL NAVIGATION
C
      CALL MATCAR(UOBB,1.,PSIOB,THTOB)
      CALL MATTRA(TLB,TBL,3,3)
      CALL MATMUL(UOBL,TLB,UOBB,3,3,1)
      CALL MATSKS(DUM9,WOEL)
      CALL MATMUL(DUM3,DUM9,UOBL,3,3,1)
      DUM1=GNAV*DVBO
      CALL MATCON(APNL,DUM1,DUM3,3,1)
      CALL MATMUL(AAPNB,TBL,APNL,3,3,1)
C
      RETURN
      END
 
C******************************************************************
      SUBROUTINE C1TERM(ALCOM,ANCOM)
C******************************************************************
C***  *
C***  * Terminal Guidance Law
C***  *
C***  * Proportional Navigation using LOS rate only
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Calculates acceleration commands based on:
C***  *         (a) LOS rates
C***  *         (b) Velocity decay term
C***  *
C***  * (2) Limits acceleration commands by circular limiter
C***  *
C***  * Argument Output:
C***  *
C***  *          ALCOM=Lateral acceleration command - g's
C***  *          ANCOM=Normal acceleration command - g's
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 941122 Created by Peter Zipfel
C***  * 990802 Caffa equation (corrected) implemented with g-bias, PZi
C***  * 000322 Corrected gravity bias, PZi
C***  *
C***  *************************************************************
C
      COMMON C(3510)
C
       DIMENSION FSPCB(3),VT1EL(3),ST1EL(3),SBEL(3),VBEL(3),SBT1L(3)
     &,VBT1L(3),TBLC(3,3),GRAVL(3),GRAVB(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0053),OPTMET)
      EQUIVALENCE (C(0801),GNAV)
C
C GNAV = D Navigation gain -
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(0205),GRAV)
      EQUIVALENCE (C(0447),THTPB)
      EQUIVALENCE (C(0448),PSIPB)
      EQUIVALENCE (C(0487),SIGDPY)
      EQUIVALENCE (C(0488),SIGDPZ)
      EQUIVALENCE (C(0715),TBLC(1,1))
      EQUIVALENCE (C(0734),FSPCB(1))
      EQUIVALENCE (C(1267),GMAX)
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1282),TRCVEL)
      EQUIVALENCE (C(1619),SBEL(1))
      EQUIVALENCE (C(1633),VBEL(1))
	EQUIVALENCE (C(2000),T)
C
C AGRAV= E Gavity acceleration (9.806635 m/s^2)
C VT1EL= S Velocity of T1 wrt earth in local level coord - m/s
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C GRAV= O Gravity acceleration - m/s^2
C THTPB= O Pitch pointing angle - rad
C PSIPB= O Yaw pointing angle - rad
C SIGDPY= O Pitch sight line spin - rad/s
C SIGDPZ= O Yaw sight line spin - rad/s
C TBLC= O Comp trans matrix of body wrt earth coor - None
C FSPCB= O Computed specific force on body - N/kg
C GMAX= G Max maneuverability limited by ALPLIMX - g's
C TRCODE= I Termination code number
C TRCVEL= D Minimum closing speed - m/s
C SBEL= S Vehicle pos. wrt point E in local level axes - m
C VBEL= O Vehicle velocity in local level axes - m/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0804),GN)
      EQUIVALENCE (C(0806),APNY)
      EQUIVALENCE (C(0807),APNZ)
      EQUIVALENCE (C(0808),ADELY)
      EQUIVALENCE (C(0809),ADELZ)
      EQUIVALENCE (C(0810),ALL)
      EQUIVALENCE (C(0811),ANN)
C
C GN = G Guidance Gain - m/s
C APNY = G Pronav acceleration along P2 axis - m/s^2
C APNZ = G Pronav acceleration along P3 axis - m/s^2
C ADELY = G Vehicle longit accel correction term along P2 - m/s^2
C ADELZ = G Vehicle longit accel correction term along P3 - m/s^2
C ALL = G Vehicle lateral accel before limiting - m/s^2
C ANN = G Vehicle normal accel before limiting - m/s^2
C
C*** NAV GAIN TABLE LOOK-UP
C
C    NOT IMPLEMENTED INSTEAD FIXED NAV GAIN INPUT
C
C*** CIRCULAR LIMITER TABLE LOOK-UP
C
C    NOT IMPLEMENTED; INSTEAD CLOSING VELOCITY CALCULATION AS INTERIM FIX
C
      CALL MATSUB(SBT1L,SBEL,ST1EL,3,1)
      CALL MATABS(DBT1,SBT1L,3)
      CALL MATSUB(VBT1L,VBEL,VT1EL,3,1)
      CALL MATSCA(DUM,SBT1L,VBT1L,3)
      DCVEL=ABS(DUM/DBT1)
C
C*** TERMINATION CONDITION
C
      IF(DBT1.LE.(3280./(1.+2.28*OPTMET))) THEN
         IF(DCVEL.LT.TRCVEL) TRCODE=1.
      ENDIF
C
C*** MISSILE ACCELERATION CORRECTION TERM (in g's)
C
      ADELY=FSPCB(1)*TAN(PSIPB)/(AGRAV)
      ADELZ=FSPCB(1)*TAN(THTPB)/(COS(PSIPB)*AGRAV)
C
C*** GRAVITATIONAL BIAS TERM
C
      CALL VECVEC(GRAVL,0.,0.,1.) !changed from GRAV to 1
	CALL MATMUL(GRAVB,TBLC,GRAVL,3,3,1)
C
C*** ACCELERATION COMMANDS  ALONG BODY AXES IN M/S^2
C
      GN=GNAV*DCVEL
      APNY=GN*SIGDPZ/(COS(PSIPB)*AGRAV)
      APNZ=GN*(+SIGDPZ*TAN(THTPB)*TAN(PSIPB)+SIGDPY/COS(THTPB))/AGRAV
      ALL=APNY+ADELY-GRAVB(2)
      ANN=APNZ+ADELZ+GRAVB(3)
C
C*** CIRCULAR LIMITER
C
      AA=SQRT(ALL*ALL+ANN*ANN)
      IF(AA.GT.GMAX) AA=GMAX
      IF(AMAX1(ABS(ANN),ABS(ALL)).LT.1E-10) THEN
         PHI=0.
      ELSE
         PHI=ATAN2(ANN,ALL)
      ENDIF
      ALCOM=AA*COS(PHI)
      ANCOM=AA*SIN(PHI)
C
      RETURN
      END
C******************************************************************
      SUBROUTINE C2I
C******************************************************************
C***  * Autopilot Initialization Module
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Initializes the state variables
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 900-999
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
C*** STORAGE OF STATE VARIABLE LOCATIONS
C
      ILOC=914
      DO I=1,2
         IPL(NIP)=ILOC
         IPLV(NIP)=ILOC+1
         ILOC=ILOC+2
         NIP=NIP+1
      ENDDO
C
      RETURN
      END
C******************************************************************
      SUBROUTINE C2
C******************************************************************
C***  * Autopilot Module
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Selects Rate Loop for pitch and yaw (MAUT=1)
C***  * (2) Selects Look Angle Guidance (LAG) for pitch and yaw (MAUT=2)
C***  * (3) Selects Integral/Proportional Autopilot for pitch and yaw (MAUT=3)
C***  * (4) Selects Proportional Autopilot for pitch and yaw (MAUT=4)
C***  * (5) Selects Flight Path Angle Hold Autopilot for pitch and
C***  *     Proportional Autopilot for yaw (MAUT=6)
C***  * (6) Selects Roll Position Autopilot
C***  * (7) Freezes flight conditions for autopilot response.
C***  *     Frozen variables are: DVBE, VMACH, PDYNMC, VMASS, THRUST,
C***  *     AI11, AI33 and derivatives in subroutine A1DER.
C***  *     Invoked by setting  MFREEZE=1 and canceled by MFREEZE=0.
C***  *     Canceling Freeze will also cause print out of previous peak
C***  *     values and their times.
C***  *     For sequential maneuvers increment MFREEZE by "one"
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  * 950929 Added Look Angle Guidance subroutine C2LAG, PZi
C***  * 951214 Added Proportional pole placement subroutine C2PROP, PZi
C***  * 980403 Added C2INTS, more efficient C2INT, PZi
C***  * 980403 Added C2RATE, Derived from C2LAG (compacted), PZi
C***  * 980814 Removed unused code for RT-CADAC, PZi
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 900-999
C
      COMMON C(3510)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2000),T)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0900),MAUT)
      EQUIVALENCE (C(0901),MFREEZE)
      EQUIVALENCE (C(0907),ALIMIT)
      EQUIVALENCE (C(0908),DQLIMX)
      EQUIVALENCE (C(0909),DRLIMX)
      EQUIVALENCE (C(0910),DPLIMX)
      EQUIVALENCE (C(0911),PHICOMX)
C
C MAUT = D =1:Rate; =2:LAG; =3:INT; =4:PROP; =5 FPATH; =6:GAM
C MFREEZE = D =0:Unfreeze and print; =1:Freeze; increment for more
C ALIMIT = D Total structural acceleration limiter - g's
C DQLIMX = D Pitch flap control limiter - deg
C DRLIMX = D Yaw flap control limiter - deg
C DPLIMX = D Roll command limiter - deg
C PHICOMX = D Commanded roll angle - deg
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0802),ANCOM)
      EQUIVALENCE (C(0803),ALCOM)
      EQUIVALENCE (C(1707),QQ)
      EQUIVALENCE (C(1709),RR)
C
C ANCOM= O Normal acceleration command - g's
C ALCOM= O Lateral acceleration command - g's
C QQ= S Body pitch angular velocity - rad/s
C RR= S Body yaw angular velocity - rad/s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0919),DPCX)
      EQUIVALENCE (C(0920),DQCX)
      EQUIVALENCE (C(0921),DRCX)
C
C DPCX = O Roll flap command deflection - deg
C DQCX = O Pitch flap command deflection - deg
C DRCX = O Yaw flap command deflection - deg
C
C*** DIAGNOSTICS
C
      IF(MAUT.EQ.0) RETURN
C
C*** RATE AUTOPILOT
C
      IF(MAUT.EQ.1) CALL C2RATE(DQCX,DRCX)
C
C*** STRUCTURAL TOTAL ACCELERATION LIMITER: ALIMIT
C
      AA=SQRT(ALCOM**2+ANCOM**2)
      IF(AA.GT.ALIMIT) AA=ALIMIT
      IF(AMAX1(ABS(ANCOM),ABS(ALCOM)).LT.1E-10) THEN
         PHI=0.
      ELSE
         PHI=ATAN2(ANCOM,ALCOM)
      ENDIF
      ALCOM=AA*COS(PHI)
      ANCOM=AA*SIN(PHI)
C
C*** CALL ACCELERATION AUTOPILOT FOR PITCH AND YAW PLANE
C
      IF(MAUT.EQ.3) CALL C2INTS(DQCX,DRCX,ANCOM,ALCOM)
C
C*** CALL PROP ACCEL AUTOPILOT FOR YAW AND GAMMA AUTOPILOT FOR PITCH
C
      IF(MAUT.EQ.6)THEN
         CALL C2INTS(DQCX,DRCX,ANCOM,ALCOM)
      ENDIF
C
C*** CONTROL COMMAND LIMITING
C
      IF(ABS(DQCX).GE.DQLIMX) DQCX=SIGN(DQLIMX,DQCX)
      IF(ABS(DRCX).GE.DRLIMX) DRCX=SIGN(DRLIMX,DRCX)
C
C*** CALL ROLL POSITION AUTOPILOT
C
      CALL C2ROLL(DPCX,PHICOMX)
C
      IF(ABS(DPCX).GE.DPLIMX) DPCX=SIGN(DPLIMX,DPCX)
C
      RETURN
      END
C******************************************************************
      SUBROUTINE C2RATE(DQCX,DRCX)
C******************************************************************
C***  * Rate Feedback Autopilot
C***  * Employs pole placement techniques
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Calculates rate gyro feedback gain
C***  * (3) Ouputs actuator fin contol commands
C***  *
C***  * Argument Output:
C***  *          DQCX=Pitch control flap command deflection - deg
C***  *          DRCX=Yaw control flap command deflection - deg
C***  *
C***  * MODIFICATION HISTORY
C***  * 980403 Derived from C2LAG (compacted), PZi
C***  *
C***  **************************************************************
C
      COMMON C(3510)
C
	DIMENSION WBECB(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0950),ZETLAGR)
      EQUIVALENCE (C(0953),RATELIMX)
C
C ZETLAGR = D Desired damping of closed rate loop ND
C RATELIMX = D Rate command limiter - deg/s
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0706),WBECB(1))
      EQUIVALENCE (C(0730),DVBEC)
      EQUIVALENCE (C(1245),DNA)
      EQUIVALENCE (C(1246),DND)
      EQUIVALENCE (C(1247),DMA)
      EQUIVALENCE (C(1248),DMQ)
      EQUIVALENCE (C(1249),DMD)
C
C WBECB= O Computed ang vel of body wrt earth - rad/s
C DVBEC= O Computed body speed wrt earth - m/s
C DNA= O Normal force slope derivative - m/s^2
C DND= O Pitch control force derivative - m/s^2
C DMA= O Pitch moment derivative - 1/s^2
C DMQ= O Pitch damping derivative - 1/s
C DMD= O Pitch control derivative - 1/s^2
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0960),ZRATE)
      EQUIVALENCE (C(0961),GRATE)
      EQUIVALENCE (C(0962),WNLAGR)
C
C ZRATE = G Zero pole of rate TR.FCT. - 1/rad
C GRATE = G Feedback gain of rate loop - ND
C WNLAGR = G Nat freq of closed rate loop - rad/s
C
C *** PARAMETERS OF OPEN LOOP ANGULAR RATE TRANSFER FUNCTION
C
      ZRATE=DNA/DVBEC-DMA*DND/(DVBEC*DMD)
      AA=DNA/DVBEC-DMQ
      BB=-DMA-DMQ*DNA/DVBEC
C
C*** FEEDBACK GAIN OF RATE LOOP GIVEN DESIRED CLOSED LOOP ZETLAGR
C
      DUM1=(AA-2.*ZETLAGR**2*ZRATE)
      DUM2=AA**2-4.*ZETLAGR**2*BB
      RADIX=DUM1**2-DUM2
      IF(RADIX.LT.0.)THEN
      ELSE
         GRATE=(-DUM1+SQRT(RADIX))/(-DMD)
      ENDIF
C
C*** NATURAL FREQUENCY OF CLOSED RATE LOOP
C
      DUM3=GRATE*DMD*ZRATE
      RADIX=BB+DUM3
      IF(RADIX.LT.0.)THEN
      ELSE
         WNLAGR=SQRT(RADIX)
      ENDIF
C
C*** COMMANDED PITCH FIN CONTROL DQCX
C
      DQCX=CRAD*GRATE*WBECB(2)
C
C*** COMMANDED YAW FIN CONTROL DRCX
C
      DRCX=CRAD*GRATE*WBECB(3)
C
      RETURN
      END
C******************************************************************
      SUBROUTINE C2INTS(DQCX,DRCX,ANCOM,ALCOM)
C******************************************************************
C***  * Integral/Proportional Acceleration Autopilot for Pitch and
C***  * Yaw plane - Short Version - (MAUT=3)
C***  *
C***  * Employs pole placement technique. Faster version than C2INT
C***  *   (no matrix inversion required)
C***  * Feedback signals are: body rate (gyro) and acceleration (accel)
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Calculates two feedback and one feed-foreward gains
C***  *     based on input of dominant closed loop conjugate complex
C***  *     roots
C***  * (2) Calculates the commanded pitch and control deflections
C***  *
C***  * Argument Output
C***  *          DQCX=Pitch flap command deflection - deg
C***  *          DRCX=Yaw flap command deflection - deg
C***  *
C***  * Argument Input:
C***  *          ANCOM=Pitch acceleration command - g's
C***  *          ALCOM=Yaw acceleration command - g's
C***  *
C***  * MODIFICATION HISTORY
C***  * 980403 Created by Peter Zipfel
C***  * 980520 Correction of GAINFB(2) calculation, PZi
C***  *
C***  **************************************************************
C
      COMMON C(3510)
C
      DIMENSION GAINFB(3),FSPCB(3),WBECB(3)
C
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(0945),ISETC2)
C
C ISETC2 = I Flag to print freeze variables
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(2001),TSTAGE)
      EQUIVALENCE (C(2866),ICOOR)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0901),MFREEZE)
      EQUIVALENCE (C(0946),FACTWACL)
      EQUIVALENCE (C(0947),FACTZACL)
C
C FACTWACL = D Fact assoc with closed loop natural frequency
C FACTZACL = D Fact assoc with closed loop damping
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(0706),WBECB(1))
      EQUIVALENCE (C(0734),FSPCB(1))
      EQUIVALENCE (C(1245),DNA)
      EQUIVALENCE (C(1246),DND)
      EQUIVALENCE (C(1247),DMA)
      EQUIVALENCE (C(1248),DMQ)
      EQUIVALENCE (C(1249),DMD)
      EQUIVALENCE (C(1636),DVBE)
C
C PDYNMC= O Dynamic pressure - Pa
C WBECB= O Computed ang vel of body wrt earth - rad/s
C FSPCB= O Computed specific force on body - N/kg
C DNA= O Normal force slope derivative - m/s^2
C DND= O Pitch control force derivative - m/s^2
C DMA= O Pitch moment derivative - 1/s^2
C DMQ= O Pitch damping derivative - 1/s
C DMD= O Pitch control derivative - 1/s^2
C DVBE= I/O Vehicle speed - m/s
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0914),YYD)
      EQUIVALENCE (C(0915),YY)
      EQUIVALENCE (C(0916),ZZD)
      EQUIVALENCE (C(0917),ZZ)
C
C YY = S Yaw feed-foreward integration variable- m/s
C ZZ = S Pitch feed-foreward integration variable- m/s
C XTHT = S Intermediate pitch - rad
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0904),WACL)
      EQUIVALENCE (C(0905),ZACL)
      EQUIVALENCE (C(0924),GAINFB(1))
      EQUIVALENCE (C(0933),FSPB2M)
      EQUIVALENCE (C(0934),FSPB2MT)
      EQUIVALENCE (C(0935),FSPB3M)
      EQUIVALENCE (C(0936),FSPB3MT)
      EQUIVALENCE (C(0937),QQXM)
      EQUIVALENCE (C(0938),QQXMT)
      EQUIVALENCE (C(0939),RRXM)
      EQUIVALENCE (C(0940),RRXMT)
      EQUIVALENCE (C(0941),DQCXM)
      EQUIVALENCE (C(0942),DQCXMT)
      EQUIVALENCE (C(0943),DRCXM)
      EQUIVALENCE (C(0944),DRCXMT)
C
C WACL = G Nat freq of accel close loop complex pole - rad/s
C ZACL = G Damping of accel close loop complex pole -
C GAINFB(3) = G Feedback gain of rate, accel and control
C FSPB2M = G Max pitch accel transient - m/s^2
C FSPB2MT = G Stagetime at max pitch accel transient - s
C FSPB3M = G Max yaw accel transient - m/s^2
C FSPB3MT = G Stagetime at max yaw accel transient - s
C QQXM = G Max pitch rate transient - deg/s
C QQXMT = G Stagetime at max pitch rate transient - s
C RRXM = G Max yaw rate transient - deg/s
C RRXMT = G Stagetime at max yaw rate transient - s
C DQCXM = G Max pitch flap transient - deg
C DQCXMT = G Stagetime at max pitch flap transient - s
C DRCXM = G Max yaw flap transient - deg
C DRCXMT = G Stagetime at max yaw flap transient - s
C
C*** FEEDBACK GAINS BASED ON NATURAL FREQUENCY AND DAMPING OF CONJUGATE
C* * COMPLEX POLES OF DESIRED CLOSED LOOP RESPONSE. ZACL (DAMPING) IS FIXED INPU
C* * WACL(NATURAL FREQUENCY) IS A FUNCTION OF DYNAMIC PRESSURE
C*** ***  SO5A CONCEPT PECULIAR VALUES FOR WACL AND ZACL ADOPTED FOR SR1S ***
C*** ***  This subroutine C2INTS adopted the WACL and ZACL values from C2INT
C*** ***  and added selection of closed loop real pole to be PACL=10
	FACTWACL=-.1
	FACTZACL=0.
C
      WACL=(.02*SQRT(PDYNMC)+4.)*(FACTWACL+1.)
      ZACL=(-7.E-4*SQRT(PDYNMC)+1.14)*(FACTZACL+1.)
	PACL=9.
C
C*** GAIN CALCULATION
C
	GAINFB(3)=WACL**2*PACL/(DNA*DMD)
	GAINFB(2)=(2.*ZACL*WACL+PACL+DMQ-DNA/DVBE)/DMD
	GAINFB(1)=(WACL**2+2.*ZACL*WACL*PACL+DMA+DMQ*DNA/DVBE
C20MAY98     &-GAINFB(2)*DND*DNA/DVBE)/(DNA*DMD)
     &-GAINFB(2)*DMD*DNA/DVBE)/(DNA*DMD)
C
C*** PITCH LOOP ACCELERATION AUTOPILOT. PITCH CONTROL COMMAND
C
      ZZD=AGRAV*ANCOM+FSPCB(3)
	DQC=-GAINFB(1)*(-FSPCB(3))-GAINFB(2)*WBECB(2)+GAINFB(3)*ZZ
      DQCX=DQC*CRAD
C
C*** YAW LOOP ACCELERATION AUTOPILOT. YAW CONTROL COMMAND
C
      YYD=AGRAV*ALCOM-FSPCB(2)
      DRC=-GAINFB(1)*FSPCB(2)-GAINFB(2)*WBECB(3)+GAINFB(3)*YY
      DRCX=DRC*CRAD
C
      RETURN
      END
C******************************************************************
      SUBROUTINE C2ROLL(DPCX,PHICOMX)
C******************************************************************
C***  * Roll Position Autopilot
C***  *
C***  * Employs pole placement technique
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Calculates gains
C***  * (2) Calculates commanded roll flap
C***  *
C***  * Argument Output:
C***  *          DPCX=Roll flap command deflection- deg
C***  *
C***  * Argument Input:
C***  *          PHICOMX=Roll control command - deg
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  * 951018 Corrected sign error in GKP calc (PZi)
C***  *
C***  **************************************************************
C
      COMMON C(3510)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0912),WRCL)
      EQUIVALENCE (C(0913),ZRCL)
C
C WRCL = D Natural freq of roll closed loop complex pole - rad/s
C ZRCL = D Damping of roll closed loop pole -
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0339),PHIBLX)
      EQUIVALENCE (C(1250),DLP)
      EQUIVALENCE (C(1251),DLD)
      EQUIVALENCE (C(1705),PP)
C
C PHIBLX= I/G Rolling angle of vehicle - deg
C DLP= O Roll damping derivative - 1/s
C DLD= O Roll control derivative - 1/s^2
C PP= S Body roll angular velocity - rad/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0931),GKP)
      EQUIVALENCE (C(0932),GKPHI)
C
C GKP = G Gain of roll rate feedback - s
C GKPHI = G Gain of roll angle feedback -
C
C*** FEEDBACK GAINS
C
      GKP=(2.*ZRCL*WRCL+DLP)/DLD
      GKPHI=WRCL**2/DLD
CC      GKP=2.*WRCL/DLD
CC      GKPHI=GKP**2*DLD/4.
C
C*** ROLL POSITION CONTROL
C
      EPHI=GKPHI*(PHICOMX-PHIBLX)/CRAD
      DPC=EPHI-GKP*PP
      DPCX=DPC*CRAD
C
      RETURN
      END
C*******************************************************************
      SUBROUTINE G2
C*******************************************************************
C***  * Atmosphere and Gravity Module
C***  * ISA International Standard Atmosphere (Prof. Stuemke)
C***  * This module performs the following functions:
C***  *
C***  * 1) Calculates the atmospheric properties
C***  * 2) Calculates the gravitational acceleration
C***  * 3) Calculates the vehicle Mach number
C***  *
C***  *    SI units
C***  *
C***  * MODIFICATION HISTORY
C***  * 931007 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 0100 - 0199
C
      COMMON C(3510)
C
C*** INPUT FROM EXECUTIVE ROUTINE
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(2866),ICOOR)
C
C REARTH = E Radius of Earth - m
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0901),MFREEZE)
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1283),TRMACH)
      EQUIVALENCE (C(1284),TRDYNM)
      EQUIVALENCE (C(1636),DVBE)
      EQUIVALENCE (C(1639),HBE)
C
C MFREEZE= D =0:Unfreeze and print; =1:Freeze; increment for more
C MGUID= D/G =0:None, =2:LAG, =3:Pro-Nav, =6:Aug Pro-Nav
C TRCODE= I Termination code number
C TRMACH= D Minimum Mach number
C TRDYNM= D Minimum dynamic pressure - Pa
C DVBE= I/O Vehicle speed - m/s
C HBE= O Height above ground - m
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
C VMACH = O Mach number of missile
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
C*** ALTITUDE ABOVE EARTH
C
      RAD=REARTH+HBE
C
C*** CALCULATE THE GRAVITY ACCELERATION
C
      GRAV=G*EMASS/RAD**2
C
C*** ATMOSPH DENSITY, SONIC SPEED AND ROCKET MACH NUMBER
C
      IF (HBE.LT.11000.) THEN
        TEMPK=288.15-0.0065*HBE
        PRESS=101325.*(TEMPK/288.15)**5.2559
      ELSE
        TEMPK=216.
        PRESS=22630.*EXP(-0.00015769*(HBE-11000.))
      ENDIF
C
      RHO=PRESS/(R*TEMPK)
      VSOUND=SQRT(1.4*R*TEMPK)
C
      VMACH=ABS(DVBE/VSOUND)
C
      PDYNMC=0.5*RHO*DVBE**2
C
C*** TERMINATION CODITIONS
C
      IF(MGUID.EQ.6) THEN
         IF(VMACH.LE.TRMACH) TRCODE=2.
         IF(PDYNMC.LE.TRDYNM) TRCODE=3.
      ENDIF
C
C*** FREEZE VARIABLES FOR AUTOPILOT RESPONSE CALCULATIONS
C
      IF(MFREEZE.EQ.0) THEN
         MFREEZEM=0
      ELSE
         IF(MFREEZE.NE.MFREEZEM) THEN
            MFREEZEM=MFREEZE
            VMACHF=VMACH
            PDYNMCF=PDYNMC
         ENDIF
         VMACH=VMACHF
         PDYNMC=PDYNMCF
      ENDIF
C
      RETURN
      END
C******************************************************************
      SUBROUTINE S1I
C******************************************************************
C***  *
C***  * Initialization of the Seeker Module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * Identifies state variables for integration
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 950125 Created by Peter Zipfel
C***  * 950206 Elimination of seeker initialization PZ
C***  *
C***  *************************************************************
C
C*** COMMON LOCATIONS ARE 400-499
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
C*** INITIALIZE STATE VARIABLES
C
      ILOC=419
      DO I=1,6
         IPL(NIP)=ILOC
         IPLV(NIP)=ILOC+1
         ILOC=ILOC+2
         NIP=NIP+1
      ENDDO
C
      RETURN
      END
C******************************************************************
      SUBROUTINE S1
C******************************************************************
C***  *
C***  * Seeker Module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) MSEEK=0: No seeker
C***  *          =2: Seeker enabled (input,or set internally if brake-lock occur
C***  *          =3: Acquis. mode (set internally, when missile is within RACQ)
C***  *          =4: Seeker lock-on (set internally, when DTIMAC has elapsed)
C***  *          =5: Seeker within blind range (set internally). Output held con
C***  *
C***  * (2) MS1DYN=0: Kinematic seeker
C***  *           =1: Dynamic seker
C***  *
C***  * (3) Sets MGUID=6(terminal guidance) when seeker is locked on (MSEEK=4)
C***  *     Sets MGUID=3(midcourse guidance) when seeker brakes lock (MSEEK=2)
C***  *
C***  * (4) Value of RACQ determines wether Seeker is locked on before or after
C***  *     For LOBL, the time delay DTIMAC represents the seeker lock-out time
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 950125 Created by Peter Zipfel
C***  * 950206 Restructuring of logic PZ
C***  * 980613 For RT-CADAC equivalenced ISETS1 and EPCHAC, PZi
C***  *
C***  *************************************************************
C
C*** COMMON LOCATIONS ARE 400-499
C
C
      COMMON C(3510)
C
      DIMENSION SBT1L(3),TT1L(3,3),SBT1T1(3),SBEL(3),ST1EL(3),THB(3,3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0400),MSEEK)
      EQUIVALENCE (C(0401),MS1DYN)
      EQUIVALENCE (C(0407),RACQ)
      EQUIVALENCE (C(0408),DTIMAC)
      EQUIVALENCE (C(0431),FOVYAW)
      EQUIVALENCE (C(0432),FOVPP)
      EQUIVALENCE (C(0433),FOVPN)
C
C MSEEK = D/G =2:Enable, =3:Acquisition, =4:Lock
C MS1DYN = D =0: Kinemtic, =1:Dynamic
C RACQ = D Acquisition range - m
C DTIMAC = D Time duration to acquire target - s
C FOVYAW = D Half yaw field-of-view at acquisition - rad
C FOVPP = D Half positive pitch field-of-view at acquis. - rad
C FOVPN = D Half negative pitch field-of-view at acquis. - rad
C
C*** RT-CADAC
C
      EQUIVALENCE (C(0402),ISETS1)
      EQUIVALENCE (C(0403),EPCHAC)
C
C ISETS1 = R Seeker flag
C EPCHAC = R Epoch of start of seeker acquisition - s
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(0130),TT1L(1,1))
      EQUIVALENCE (C(1619),SBEL(1))
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(2000),T)
C
C CRAD= E Conversion from radians to degree (57.29577951)
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C TT1L= O Transf Matrix of T1 body coord wrt local level coord
C SBEL= S Vehicle pos. wrt point E in local level axes - m
C MGUID= D/G =0:None, =2:LAG, =3:Pro-Nav, =6:Aug Pro-Nav
C
C*** STATE VARIABLE INITIALIZATION
C
      EQUIVALENCE (C(0420),WLQ1)
      EQUIVALENCE (C(0422),WLQ)
      EQUIVALENCE (C(0424),WLR1)
      EQUIVALENCE (C(0426),WLR)
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0447),THTPB)
      EQUIVALENCE (C(0448),PSIPB)
      EQUIVALENCE (C(0487),SIGDPY)
      EQUIVALENCE (C(0488),SIGDPZ)
C
C THTPB = O Pitch pointing angle - rad
C PSIPB = O Yaw pointing angle - rad
C SIGDPY = O Pitch sight line spin - rad/s
C SIGDPZ = O Yaw sight line spin - rad/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0462),THB(1,1))
      EQUIVALENCE (C(0471),TIMEAC)
      EQUIVALENCE (C(0472),PSIOT1)
      EQUIVALENCE (C(0473),THTOT1)
      EQUIVALENCE (C(0485),THTPBX)
      EQUIVALENCE (C(0486),PSIPBX)
      EQUIVALENCE (C(0494),SBT1L(1))
C
C THB(3,3) = I/G TM of head axes wrt body axes
C TIMEAC = G Time duration of seeker acquisition phase - s
C PSIOT1 = G Azimuth of sensor LOS in target axes - rad
C THTOT1 = G Elevation of sensor LOS in target axes - rad
C THTPBX = G Pitch pointing angle - deg
C PSIPBX = G Yaw pointing angle - deg
C SBT1L(3) = G Position of missile wrt target - m
C
C*** TARGET ASPECT ANGLES
C
      CALL MATSUB(SBT1L,SBEL,ST1EL,3,1)
      CALL MATMUL(SBT1T1,TT1L,SBT1L,3,3,1)
      CALL MATPOL(DBT1,PSIOT1,THTOT1,SBT1T1)
C
      IF(MSEEK.EQ.2) THEN
C
C***    ACQUISITION RANGE
C
         ISETS1=1
         IF(DBT1.LT.RACQ) MSEEK=3
      END IF
C
      IF(MSEEK.EQ.3) THEN
C
         IF(ISETS1.EQ.1) THEN
C
C***       INITIALIZATION OF HEAD WRT BODY T.M., STATE VARIABLES AND
C***       ACQUISITION TIME COUNTER
C
            CALL S1KIN(THTPB,PSIPB,SIGDY,SIGDZ,SBT1L,DBT1)
            CALL UTHPB(THTHB,PHIHB,PSIPB,THTPB)
            CALL UTTHB(THB,THTHB,PHIHB)
            WLQ=THTPB
            WLR=PSIPB
            WLQ1=SIGDY
            WLR1=SIGDZ
            ISETS1=0
            EPCHAC=T
         END IF
C
C***    ACQUISITION (FOR DYNAMIC SEEKER THE TARGET MUST BE IN FIELD-OF-VIEW)
C
         IF(MS1DYN.EQ.1) THEN
            CALL S1DYN(MSEEK,MGUID,THTPB,PSIPB,SIGDY,SIGDZ,EHZ,EHY
     &                 ,THB,SBT1L,DBT1)
            IF(ABS(EHZ).LE.FOVYAW.AND.ABS(EHY).LE.FOVPP)THEN
               TIMEAC=T-EPCHAC
               IF(TIMEAC.GT.DTIMAC) MSEEK=4
            END IF
         ELSE
            CALL S1KIN(THTPB,PSIPB,SIGDY,SIGDZ,SBT1L,DBT1)
            TIMEAC=T-EPCHAC
            IF(TIMEAC.GT.DTIMAC) MSEEK=4
         END IF
      END IF
C
C*** SEEKER LOCK-ON
C
      IF(MSEEK.EQ.4) THEN
C
         MGUID=6
C
         IF(MS1DYN.EQ.1) THEN
C
C*** DYNAMIC SEEKER
C
            CALL S1DYN(MSEEK,MGUID,THTPB,PSIPB,SIGDY,SIGDZ,EHZ,EHY
     &                 ,THB,SBT1L,DBT1)
         ELSE
C
C*** KINEMATIC SEEKER
C
            CALL S1KIN(THTPB,PSIPB,SIGDY,SIGDZ,SBT1L,DBT1)
         END IF
C
C*** LOS RATE OUTPUT TO GUIDANCE MODULE
C
         SIGDPY=SIGDY
         SIGDPZ=SIGDZ
C
      END IF
C
      THTPBX=THTPB*CRAD
      PSIPBX=PSIPB*CRAD
C
C*** SG ANIMATION ANIMATION VARIABLE
C
      FOVPN=-FOVPP
C
      RETURN
      END
C******************************************************************
      SUBROUTINE S1DYN(MSEEK,MGUID,THTPB,PSIPB,SIGDY,SIGDZ,EHZ,EHY
     &                 ,THB,SBT1L,DBT1)
C******************************************************************
C***  *
C***  * Dynamic seeker module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Given true target relative geometry it determines inertial
C***  *     LOS rates in pitch and yaw, corrupted by these errors:
C***  *         Target Scintillation
C***  *         Blur, pixel quatization and bias
C***  *         Gimbal dynamics, quantization and bias
C***  *
C***  * (2) Determines Aimpoint off-set from computer determined seeker axis
C***  *     in Focal Plane (F.P.) array EAPH(3)
C***  *
C***  * (3) Models Kalman Filter dynamics (generates inertial LOS rates)
C***  *
C***  * (4) Models strap-down gyro feedback and gimbal kinematics
C***  *
C***  * (5) Allows for aimpoint selection and correction.
C***  *
C***  * Argument Output:
C***  *                 MSEEK: If break lock occured reset to 2 (acquisition)
C***  *                 MGUID: If break lock occured reset to 3 (midcourse)
C***  *                 THTPB=Pitch pointing angle - rad
C***  *                 PSIPB=Yaw pointing angle - rad
C***  *                 SIGDY=Pitch sight line spin - rad/s
C***  *                 SIGDZ=Yaw sight line spin - rad/s
C***  *                 EHZ=Yaw seeker error angle - rad
C***  *                 EHY=Pitch seeker error angle - rad
C***  *
C***  * Argument Input:
C***  *                 THB(3,3)=Transf matrix of head wrt body axes (Initializa
C***  *                 SBT1L(3)=Position of missile wrt target - m
C***  *                 DBT1=Distance between missile and target - m
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 950125 Created by Peter Zipfel
C***  * 950206 Restructuring of Logic PZ
C***  * 980626 For RT-CADAC equivalenced IBREAK, PZi
C***  *
C***  *************************************************************
C
      COMMON C(3510)
C
      DIMENSION THB(3,3),TBL(3,3),SBT1L(3),EAHH(3),EPHH(3)
     +,WBECB(3),THL(3,3),SBT1H(3),SAT1H(3),SABH(3)
     +,TPB(3,3),TPH(3,3),THP(3,3),U1PP(3),U1PH(3),U1HH(3),EAPP(3)
     +,EAPH(3),WBEP(3),TBH(3,3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0406),DBLIND)
      EQUIVALENCE (C(0411),GK)
      EQUIVALENCE (C(0412),ZETAK)
      EQUIVALENCE (C(0413),WNK)
      EQUIVALENCE (C(0415),BIAST)
      EQUIVALENCE (C(0416),RANDT)
      EQUIVALENCE (C(0417),BIASP)
      EQUIVALENCE (C(0418),RANDP)
      EQUIVALENCE (C(0489),BIASEH)
      EQUIVALENCE (C(0490),RANDEH)
C
C DBLIND = D Blind range - m
C GK = D K.F. gain - 1/s
C ZETAK = D K.F. damping
C WNK = D K.F. natural frequency - rad/s
C BIAST = D Pitch gimbal bias errors - rad
C RANDT = D Pitch gimbal random errors - rad
C BIASP = D Roll gimbal bias error - rad
C RANDP = D Roll gimbal bias error - rad
C BIASEH = D Image blur and pixel bias errors - rad
C RANDEH = D Image blur and pixel random errors - rad
C
C*** RT-CADAC
C
      EQUIVALENCE (C(0404),IBREAK)
C
C IBREAK = R Flag for seeker break-lock ND
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0320),TBL(1,1))
      EQUIVALENCE (C(0706),WBECB(1))
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1287),TRTHT)
      EQUIVALENCE (C(1289),TRTHTD)
      EQUIVALENCE (C(1290),TRPHID)
      EQUIVALENCE (C(1291),TRATE)
C
C CRAD= E Conversion from radians to degree (57.29577951)
C TBL= O Transf matrix of body wrt local level axes
C WBECB= O Computed ang vel of body wrt earth - rad/s
C TRCODE= I Termination code number
C TRTHT= D Maximum pitch gimbal angle - rad
C TRTHTD= D Maximum pitch gimbal rate - rad/s
C TRPHID= D Maximum roll gimbal rate - rad/s
C TRATE= D Maximum tracking rate - rad/s
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0419),WLQ1D)
      EQUIVALENCE (C(0420),WLQ1)
      EQUIVALENCE (C(0421),WLQD)
      EQUIVALENCE (C(0422),WLQ)
      EQUIVALENCE (C(0423),WLR1D)
      EQUIVALENCE (C(0424),WLR1)
      EQUIVALENCE (C(0425),WLRD)
      EQUIVALENCE (C(0426),WLR)
      EQUIVALENCE (C(0427),WLQ2D)
      EQUIVALENCE (C(0428),WLQ2)
      EQUIVALENCE (C(0429),WLR2D)
      EQUIVALENCE (C(0430),WLR2)
C
C WLQ1 = S Pitch sight line spin - rad/s
C WLQ = S Pitch pointing rate - rad/s
C WLR1 = S Yaw sight line spin - rad/s
C WLR = S Yaw pointing rate - rad/s
C WLQ2 = S Second state variable in K.F. - rad/s^2
C WLR2 = S Second state variable in K.F. - rad/s^2
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0434),DBA)
      EQUIVALENCE (C(0445),EPY)
      EQUIVALENCE (C(0446),EPZ)
      EQUIVALENCE (C(0449),THTHB)
      EQUIVALENCE (C(0450),PHIHB)
      EQUIVALENCE (C(0451),SIGDBY)
      EQUIVALENCE (C(0452),SIGDBZ)
      EQUIVALENCE (C(0453),TPB(1,1))
      EQUIVALENCE (C(0476),EAHH(1))
      EQUIVALENCE (C(0479),EPHH(1))
      EQUIVALENCE (C(0482),EAPH(1))
      EQUIVALENCE (C(0491),PSIHLX)
      EQUIVALENCE (C(0492),THTHLX)
      EQUIVALENCE (C(0493),PHIHLX)
C
C DBA = G Distance between active seeker and its aimpoint - m
C EPY = G Error of pointing in pitch - rad
C EPZ = G Error of pointing in yaw - rad
C THTPB = G Pitch pointing angle - rad
C PSIPB = G Yaw pointing angle - rad
C THTHB = G Head pitch angle - rad
C PHIHB = G Head roll angle - rad
C SIGDBY = G Pitch inertial LOS rate - rad/s
C SIGDBZ = G Yaw inertial LOS rate - rad/s
C TPB(3,3) = I/G TM of pointing axes wrt body axes
C EAHH(3) = G Aimpoint displacement wrt center of F.P. - rad
C EPHH(3) = G Computer pointing error of sensor wrt center of F.P. - rad
C EAPH(3) = G Aimpoint to computer pointing displacement - rad
C PSIHLX = G Yaw angle of CL seeker wrt local axes - deg
C THTHLX = G Pitch angle of CL seeker wrt local axes - deg
C PHIHLX = G Roll angle of CL seeker wrt local axes - deg
C
C*** AIMPOINT MODULATION
C
      CALL MATMUL(THL,THB,TBL,3,3,3)
      CALL MATMUL(SBT1H,THL,SBT1L,3,3,1)
      CALL S1AIMP(SAT1H,THL,DBT1)
      CALL MATSUB(SABH,SAT1H,SBT1H,3,1)
C
C*** ERROR ANGLES
C
      EY=ATAN2(-SABH(3),SABH(1))
      EZ=ATAN2(SABH(2),SABH(1))
      EHY=EY+BIASEH+RANDEH
      EHZ=EZ+BIASEH+RANDEH
      EAHH(1)=0.
      EAHH(2)=EHZ
      EAHH(3)=-EHY
      CALL MATTRA(TBH,THB,3,3)
      CALL MATMUL(TPH,TPB,TBH,3,3,3)
      CALL MATTRA(THP,TPH,3,3)
      U1PP(1)=1.
      U1PP(2)=0.
      U1PP(3)=0.
      CALL MATMUL(U1PH,THP,U1PP,3,3,1)
      CALL MATEQL(U1HH,U1PP,3,1)
      CALL MATSUB(EPHH,U1PH,U1HH,3,1)
      CALL MATSUB(EAPH,EAHH,EPHH,3,1)
      CALL MATMUL(EAPP,TPH,EAPH,3,3,1)
      EPZ=EAPP(2)
      EPY=-EAPP(3)
C
C*** SIGHT LINE SPIN ESTIMATOR (KALMAN FILTER REPRESENTED BY 2nD ORDER LAG)
C
      WSQ=WNK**2
      GG=GK*WSQ
      WLQ1D=WLQ2
      WLQ2D=GG*EPY-2.*ZETAK*WNK*WLQ1D-WSQ*WLQ1
C
      WLR1D=WLR2
      WLR2D=GG*EPZ-2.*ZETAK*WNK*WLR1D-WSQ*WLR1
C
C*** OUTPUT TO GUIDANCE MODULE: LOS RATES IN POINTING AXES
C
      SIGDY=WLQ1
      SIGDZ=WLR1
C
C*** LOOK ANGLE CONTROL
C
      CALL MATMUL(WBEP,TPB,WBECB,3,3,1)
      WLQD=(WLQ1-WBEP(2))
      THTPB=WLQ
      THTPBD=WLQD
C
      WLRD=(WLR1-WBEP(3))
      PSIPB=WLR
      PSIPBD=WLRD
C
      CALL UTHPB(THTHBC,PHIHBC,PSIPB,THTPB)
      CALL MAT2TR(TPB,PSIPB,THTPB)
C
C*** GIMBAL DYNAMICS
C
      THTHB=THTHBC+BIAST+RANDT
      PHIHB=PHIHBC+BIASP+RANDP
      CALL UTTHB(THB,THTHB,PHIHB)
C
C*** BREAK LOCK AND BLIND RANGE CHECKS
C
      IF(MSEEK.EQ.4)THEN
         IBREAK=0
         PHIHBD=-THTPBD*SIN(PSIPB)
         EH=SQRT(EHY*EHY+EHZ*EHZ)
         IF(ABS(THTHB).GT.TRTHT)THEN
            TRCODE=6.
            IBREAK=1
         ELSEIF(ABS(THTPBD).GT.TRTHTD)THEN
            TRCODE=7.
            IBREAK=1
         ELSEIF(ABS(PHIHBD).GT.TRPHID)THEN
            TRCODE=8.
            IBREAK=1
         ELSEIF(EH.GT.TRATE)THEN
            TRCODE=9.
            IBREAK=1
         END IF
         IF(IBREAK.EQ.1)THEN
           MSEEK=2
           MGUID=3
         END IF
         IF(DBT1.LT.DBLIND) MSEEK=5
      END IF
C
C*** DIAGNOSTIC VARIABLES: SEEKER PYRAMID DISPLAY ON SG ANIMATION
C
      DBA=DBT1
      IF(MSEEK.EQ.3)DBA=DBT1
 
C*** EULER ANGLES OF SEEKER PYRMID
 
      IF(ABS(THL(1,3)).LT.1.) THTHL=ASIN(-THL(1,3))
      CTHTHL=COS(THTHL)
      DUM1=THL(1,1)/CTHTHL
      DUM2=THL(3,3)/CTHTHL
      IF(ABS(DUM1).LT.1.) PSIHL=ACOS(DUM1)*SIGN(1.,THL(1,2))
      IF(ABS(DUM2).LT.1.) PHIHL=ACOS(DUM2)*SIGN(1.,THL(2,3))
C
      PSIHLX=CRAD*PSIHL
      THTHLX=CRAD*THTHL
      PHIHLX=CRAD*PHIHL
C
      RETURN
      END
C******************************************************************
      SUBROUTINE S1KIN(THTPB,PSIPB,SIGDY,SIGDZ,SBT1L,DBT1)
C******************************************************************
C***  *
C***  * Kinematic seeker module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1)  Calculates error free LOS rates and angles
C***  *
C***  * (2)  Also used to initialize the dynamic seeker subroutine
C***  *
C***  * Argument Output:
C***  *                 THTPB=Pitch pointing angle - rad
C***  *                 PSIPB=Yaw pointing angle - rad
C***  *                 SIGDY=Pitch sight line spin - rad/s
C***  *                 SIGDPZ=Yaw sight line spin - rad/s
C***  *
C***  * Argument Input:
C***  *                 SBT1L(3)=Position of missile wrt target - m
C***  *                 DBT1=Distance between missile and target - m
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 941122 Created by Peter Zipfel
C***  *
C***  *************************************************************
C
      COMMON C(3510)
C
      DIMENSION VT1EL(3),SBT1L(3),VBEL(3),TBL(3,3),TPB(3,3),ST1BL(3)
     +,ST1BB(3),UT1BL(3),VT1BL(3),DUM9(3,3),DUM3(3),WOEL(3),WOEB(3)
     +,WOEP(3)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0320),TBL(1,1))
      EQUIVALENCE (C(1633),VBEL(1))
C
C CRAD= E Conversion from radians to degree (57.29577951)
C VT1EL= S Velocity of T1 wrt earth in local level coord - m/s
C TBL= O Transf matrix of body wrt local level axes
C VBEL= O Vehicle velocity in local level axes - m/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0474),DVBT1C)
C
C DVBT1C = G Closing velocity computed by INS - m/s
C
C*** LOS KINEMATICS
C
      CALL MATCON(ST1BL,-1.,SBT1L,3,1)
      CALL MATMUL(ST1BB,TBL,ST1BL,3,3,1)
      DUM1=1./DBT1
      CALL MATCON(UT1BL,DUM1,ST1BL,3,1)
C
C*** RELATIVE VELOCITY
C
      CALL MATSUB(VT1BL,VT1EL,VBEL,3,1)
C
C*** CLOSING VELOCITY
C
      CALL MATSCA(DUM2,UT1BL,VT1BL,3)
      DVBT1C=ABS(DUM2)
C
C*** LOS RATE OUTPUT IN POINTING AXES
C
      CALL MATSKS(DUM9,UT1BL)
      CALL MATMUL(DUM3,DUM9,VT1BL,3,3,1)
      CALL MATCON(WOEL,DUM1,DUM3,3,1)
      CALL MATMUL(WOEB,TBL,WOEL,3,3,1)
C
      CALL MATPOL(DUM1,PSIPB,THTPB,ST1BB)
      THTPBX=THTPB*CRAD
      PSIPBX=PSIPB*CRAD
      CALL MAT2TR(TPB,PSIPB,THTPB)
      CALL MATMUL(WOEP,TPB,WOEB,3,3,1)
      SIGDY=WOEP(2)
      SIGDZ=WOEP(3)
C
      RETURN
      END
C******************************************************************
      SUBROUTINE S1AIMP(SAT1H,THL,DBT1)
C******************************************************************
C***  *
C***  * Aimpoint selection and corruption subroutine
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Introduces aimpoint tracking errors
C***  *
C***  * (2) Introduces hot spot jitter and bias errors
c***  *
c***  * Both are initiated at distance DAIM from the target
C***  *
C***  * Argument Output:
C***  *
C***  *          SAT1H(3)=Aimpoint error in head axes (focal plane array)
C***  *
C***  * Argument Input:
C***  *
C***  *          THL(3,3)=Tran Matrix of head wrt local level axes
C***  *          DBT1=Distance of vehicle wrt target T1 - m
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***   941122 Created by Peter Zipfel
C***  *
C***  *
C***  *************************************************************
C
      COMMON C(3510)
C
      DIMENSION BIASAI(3),BIASSC(3),RANDSC(3),TT1L(3,3),TLT1(3,3)
     +,SAT1H(3),THL(3,3),SAT1T1(3),THT1(3,3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0435),DAIM)
      EQUIVALENCE (C(0436),BIASAI(1))
      EQUIVALENCE (C(0439),BIASSC(1))
      EQUIVALENCE (C(0442),RANDSC(1))
C
C DAIM = D Dist from targ to initiate aimpoint mode - m
C BIASAI(3) = D Bias error of aimpoint mode in target coor - m
C BIASSC(3) = D Bias error of hot spot mode in target coor - m
C RANDSC(3) = D Random error of hot spot mode in targ coor - m
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0130),TT1L(1,1))
C
C TT1L= O Transf Matrix of T1 body coord wrt local level coord
C
      CALL MATTRA(TLT1,TT1L,3,3)
      CALL MATMUL(THT1,THL,TLT1,3,3,3)
C
      IF(DBT1.LT.DAIM) THEN
C
C***     AIMPOINT UPDATE
C
         CALL MATMUL(SAT1H,THT1,BIASAI,3,3,1)
      ELSE
C
C***     HOT SPOT MODE
C
         CALL MATADD(SAT1T1,BIASSC,RANDSC,3,1)
         CALL MATMUL(SAT1H,THT1,SAT1T1,3,3,1)
      END IF
C
      RETURN
      END
C******************************************************************
      SUBROUTINE UTHPB(THTHB,PHIHB,PSIPB,THTPB)
C******************************************************************
C***  *
C***  * Utility subroutine for angle conversions
C***  *
C***  * This subroutine performs the following function:
C***  *
C***  * Converts pointing angles (computer) to head angles (gimbals)
C***  *
C***  * Argument Output:
C***  *
C***  *          THTHB=Gimbal head pitch angle - rad
C***  *          PHIHB=Gimbal roll angle - rad
C***  *
C***  * Argument Input:
C***  *
C***  *          PSIPB=Yaw computer pointing angle - rad
C***  *          THTPB=Pitch computer pointing angle - rad
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 941122 Created by Peter Zipfel
C***  *
C***  *************************************************************
C
C*** ANGLES THTHB AND PHIHB FROM ANGLES PSIPB AND THTPB
C
      REAL*8 DTHTPB,DPSIPB,DTHTHB,DPHIHB,SINPSI,TANTHT
C
      DTHTPB=THTPB
      DPSIPB=PSIPB
      DTHTHB=DACOS(DCOS(DTHTPB)*DCOS(DPSIPB))
      THTHB=DTHTHB
      SINPSI=DSIN(DPSIPB)
      TANTHT=DTAN(DTHTPB)
      IF(DMAX1(DABS(SINPSI),DABS(TANTHT)).LT.1.E-10) THEN
        DPHIHB=0.
      ELSE
        DPHIHB=DATAN2(SINPSI,TANTHT)
      ENDIF
      PHIHB=DPHIHB
C
      RETURN
      END
C******************************************************************
      SUBROUTINE UTTHB(THB,THT,PHI)
C******************************************************************
C***  *
C***  * Utility subroutine for THB Transformation matrix
C***  *
C***  * This subroutine performs the following function:
C***  *
C***  * Calculates T.M. of head axes wrt body axes
C***  *
C***  * Argument Output
C***  *
C***  *          THB=Transformation matrix of head angles wrt missile body axes
C***  *
C***  * Argument Input:
C***  *
C***  *          THTHB=Gimbal head pitch angle - rad
C***  *          PHIHB=Gimbal roll angle - rad
C***  *
C***  * MODIFICATION HISTORY
C***  *
C***  * 941122 Created by Peter Zipfel
C***  *
C***  *************************************************************
C
      DIMENSION THB(3,3)
C
      THB(1,1)=COS(THT)
      THB(3,1)=SIN(THT)
      THB(2,2)=COS(PHI)
      THB(2,3)=SIN(PHI)
      THB(1,2)=THB(3,1)*THB(2,3)
      THB(1,3)=-THB(3,1)*THB(2,2)
      THB(3,2)=-THB(1,1)*THB(2,3)
      THB(3,3)=THB(1,1)*THB(2,2)
      THB(2,1)=0.
C
      RETURN
      END
C******************************************************************
      SUBROUTINE A3
C******************************************************************
C***  * Force Module A3
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Calculates the non-gravitational forces
C***  * (2) Calculates the aerodynamic moments
C***  *
C***  * MODIFICATION HISTORY
C***  * 931117 Created by Peter Zipfel
C***  * 980814 For RT-CADAC (C3 deleted) thrust included directly, PZi
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 1400 - 1499
C
      COMMON C(3510)
C
      DIMENSION FAPB(3),FMB(3)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(1203),REFL)
      EQUIVALENCE (C(1204),REFA)
      EQUIVALENCE (C(1220),CA)
      EQUIVALENCE (C(1221),CY)
      EQUIVALENCE (C(1222),CN)
      EQUIVALENCE (C(1223),CLL)
      EQUIVALENCE (C(1224),CLM)
      EQUIVALENCE (C(1225),CLN)
      EQUIVALENCE (C(1313),THRUST)
C
C PDYNMC= O Dynamic pressure - Pa
C REFL= I Reference length for moment derivatives - m
C REFA= I Reference area for aero coefficients - m^2
C CA= O Axial force coefficient
C CY= O Side force coefficient
C CN= O Normal force coefficient
C CLL= O Rolling moment coefficient
C CLM= O Pitching moment coefficient
C CLN= O Yawing moment coefficient
C THRUST= O Rocket thrust parallel to vehicle centerline - N
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
      FAPB(1)=-PDYNMC*REFA*CA+THRUST
      FAPB(2)=PDYNMC*REFA*CY
      FAPB(3)=-PDYNMC*REFA*CN
C
C*** AERODYNAMIC AND PROPULSIVE MOMENTS
C
      FMB(1)=PDYNMC*REFA*REFL*CLL
      FMB(2)=PDYNMC*REFA*REFL*CLM
      FMB(3)=PDYNMC*REFA*REFL*CLN
C
      RETURN
      END
C******************************************************************
      SUBROUTINE C4I
C******************************************************************
C***  * Actuator Initialization Module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Initializes the state variables for second
C***  *     order actuator dynamics
C***  *
C***  * MODIFICATION HISTORY
C***  * 940819 Created by Peter Zipfel
C***  *
C***  **************************************************************
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
C NIP = E The number of variables being integrated
C IPL(100) = E The location of the state variable derivative
C IPLV(100) = E The location of the sate variable
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1100),MACT)
C
C MACT = D  =0:no dynamics, =2:second order
C
C*** STORAGE OF VECTOR STATE VARIABLE LOCATIONS
C
      IF(MACT.GT.0) THEN
         ILOC=1130
         DO I=0,3
            IPL(NIP)=ILOC+I
            IPLV(NIP)=ILOC+I+4
            NIP=NIP+1
         ENDDO
      ENDIF
C
      IF(MACT.GT.1) THEN
         ILOC=1138
         DO I=0,3
            IPL(NIP)=ILOC+I
            IPLV(NIP)=ILOC+I+4
            NIP=NIP+1
         ENDDO
      ENDIF
C
      RETURN
      END
C******************************************************************
      SUBROUTINE C4
C******************************************************************
C***  * Actuator Module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Converts from control deflections to fin deflections
C***  * (2) Calls actuator dynamic subroutine
C***  *     MACT=0 No dynamics with position limiting
C***  *         =2 Second order dynamics with rate limiting
C***  * (3) Limits fins excursions and converts back to control deflections
C***  *
C***  * MODIFICATION HISTORY
C***  * 940819 Created by Peter Zipfel
C***  * 950304 (1) Changed postion limiter in first order dynamics (PZi)
C***  *        (2) Added second order dynamics with rate limiter (PZi)
C***  * 960926 Changed 3 to 4 in: CALL MATEQL(DELX,DELCX,3,1), PZi
C***  * 980225 Renamed C4FIRST to C4FRST, PZi
C***  * 980814 Deleted C4FRST	 for RT-CADAC, PZi
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 1100-1199
C
      COMMON C(3510)
C
      DIMENSION DELCX(4),DELX(4)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1100),MACT)
      EQUIVALENCE (C(1102),DLIMX)
C
C MACT = D  =0:no dynamics, =2:second order
C DLIMX = D Control fin limiter - deg
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0919),DPCX)
      EQUIVALENCE (C(0920),DQCX)
      EQUIVALENCE (C(0921),DRCX)
C
C DPCX= O Roll flap command deflection - deg
C DQCX= O Pitch flap command deflection - deg
C DRCX= O Yaw flap command deflection - deg
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1119),DPX)
      EQUIVALENCE (C(1120),DQX)
      EQUIVALENCE (C(1121),DRX)
C
C DPX = O Roll control deflection - deg
C DQX = O Pitch control deflection - deg
C DRX = O Yaw control deflection - deg
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1122),DELX(1))
      EQUIVALENCE (C(1147),DELCX(1))
C
C DELX(4) = G Four fin output position - deg
C DELCX(4) = G Four fin position command - deg
C
C*** CONVERSION TO FOUR FIN DEFLECTIONS
C
      DELCX(1)=-DPCX+DQCX-DRCX
      DELCX(2)=-DPCX+DQCX+DRCX
      DELCX(3)=+DPCX+DQCX-DRCX
      DELCX(4)=+DPCX+DQCX+DRCX
C
C*** NO ACTUATOR DYNAMICS
C
      IF(MACT.EQ.0.) THEN
         CALL MATEQL(DELX,DELCX,4,1)
         DO I=1,4
            IF(ABS(DELX(I)).GT.DLIMX) DELX(I)=SIGN(DLIMX,DELX(I))
         ENDDO
      ENDIF
C
C*** CALL SECOND ORDER ACTUATOR
C
      IF(MACT.EQ.2)CALL C4SCND(DELX,DELCX)
C
C*** CONVERSION TO CONTROL DEFLECTIONS
C
      DPX=(-DELX(1)-DELX(2)+DELX(3)+DELX(4))/4.
      DQX=(+DELX(1)+DELX(2)+DELX(3)+DELX(4))/4.
      DRX=(-DELX(1)+DELX(2)-DELX(3)+DELX(4))/4.
C
      RETURN
      END
 
C******************************************************************
      SUBROUTINE C4SCND(DELX,DELCX)
C******************************************************************
C***  * Second Order Actuator Model
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Models second order lags of all four control fins
C***  * (2) Limits fin positions
C***  * (3) Limits fin rates
C***  *
C***  * Argument Output:
C***  *          DELX(4)=Four actuator fin deflections - deg
C***  * Argument Input:
C***  *          DELCX(4)=Four actuator fin commands - deg
C***  *
C***  * MODIFICATION HISTORY
C***  * 950405 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
      COMMON C(3510)
C
      DIMENSION DX(4),DXD(4),DELCX(4),DELX(4),DDXD(4),DDX(4)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1102),DLIMX)
      EQUIVALENCE (C(1104),DDLIMX)
      EQUIVALENCE (C(1105),WNACT)
      EQUIVALENCE (C(1106),ZETACT)
      EQUIVALENCE (C(2000),T)
C
C DLIMX = D Control fin limiter - deg
C DDLIMX = D Control fin rate limiter - deg/s
C WNACT = D Natural frequency of actuator - rad/s
C ZETACT = D Damping of actuator - ND
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1130),DXD(1))
      EQUIVALENCE (C(1134),DX(1))
      EQUIVALENCE (C(1138),DDXD(1))
      EQUIVALENCE (C(1142),DDX(1))
C
C DXD(4) = S Four fin position derivative - deg/s
C DX(4) = S Four fin position - deg
C DDXD(4) = S Four fin rate derivative - deg/s^2
C DDX(4) = S Four fin rate - deg/s
 
C
C*** SECOND ORDER LAG
C
      DO I=1,4
C*** FIN POSITION LIMITER
         IF(ABS(DX(I)).GT.DLIMX) THEN
            DX(I)=SIGN(DLIMX,DX(I))
            IF(DX(I)*DDX(I).GT.0.) DDX(I)=0.
         ENDIF
C*** FIN RATE LIMITER
         IFLAG=0
         IF(ABS(DDX(I)).GT.DDLIMX) THEN
            IFLAG=1
            DDX(I)=SIGN(DDLIMX,DDX(I))
         ENDIF
C*** STATE INTEGRATION
         DXD(I)=DDX(I)
         EDX=DELCX(I)-DX(I)
         DDXD(I)=WNACT*WNACT*EDX-2.*ZETACT*WNACT*DXD(I)
C*** SET RATE DERIVATIVE TO ZERO
         IF(IFLAG.EQ.1.AND.DDX(I)*DDXD(I).GT.0.) DDXD(I)=0.
      ENDDO
C
      CALL MATEQL(DELX,DX,4,1)
C
      RETURN
      END
C******************************************************************
      SUBROUTINE D1I
C******************************************************************
C***  * The initialization module for the Newton Module D1.
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Define State variables VBEB, VBEL
C***  * (2) Compute initial values of VBEB
C***  * (3) Initialize the state vector VBEL
C***  * (4) Initialization of runs is governed by MINIT
C***  *     (Initialization of specific engagement modes is carried out in G1I)
C***  *
C***  *     MINIT=|MINS|MINC|MINM|
C***  *
C***  *            MINS=0 No sweep runs
C***  *                 1 Sweep runs (not implemented)
C***  *
C***  *                 MINC=0 Free choice of center
C***  *                      1 Shooter centered
C***  *                      2 Target centered
C***  *
C***  *                      MINM=Modes of engagements
C***  *
C***  *      General Mode of Engagement
C***  *
C***  *       MINIT=000 General shooter and target initialization
C***  *                 Missile initialized by SBEL, DVBE, PSIBLX, THTBLX, PHIBL
C***  *                                      ALPHA0X, BETA0X
C***  *
C***  *      Specific Modes of Engagements (see G1I):
C***  *
C***  *       MINIT=_12 LAR-2 Pre-Merge Engagement
C***  *             _13 LAR-3 One Circle Fight
C***  *             _14 LAR-4 Two Circle Fight
C***  *             _15 LAR-5 Lufbery Circle Fight
C***  *
C***  *       MINIT=_21 LAR-1 Attack Zone
C***  *             _22 Head-on Circle Engmt (UK)
C***  *             _23 Chase Circle Engmt (UK)
C***  *             _24 Twin Circle Engmt (UK)
C***  *
C***  * MODIFICATION HISTORY
C***  * 931117 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 1600-1699
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100),VBEB(3),SBEL(3),VBEL(3)
     &,TBL(3,3),TLB(3,3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1600),MINIT)
      EQUIVALENCE (C(1619),SBEL(1))
      EQUIVALENCE (C(1636),DVBE)
      EQUIVALENCE (C(1637),ALPHA0X)
      EQUIVALENCE (C(1638),BETA0X)
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
C*** INITIALIZATIONS
C
      EQUIVALENCE (C(1613),VBEB(1))
      EQUIVALENCE (C(1633),VBEL(1))
      EQUIVALENCE (C(1639),HBE)
      EQUIVALENCE (C(1642),GNDTCK)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0320),TBL(1,1))
C
C TBL= O Transf matrix of body wrt local level axes
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
      IF(MINIT.GT.0) RETURN
C
C*** INITIALIZATION OF VBEB
C
      ALPHA0=ALPHA0X/CRAD
      BETA0=BETA0X/CRAD
      SALP=SIN(ALPHA0)
      CALP=COS(ALPHA0)
      SBET=SIN(BETA0)
      CBET=COS(BETA0)
C
      VBEB(1)=CALP*CBET*DVBE
      VBEB(2)=SBET*DVBE
      VBEB(3)=SALP*CBET*DVBE
C
C*** INITIALIZATION OF VBEL
C
      CALL MATTRA(TLB,TBL,3,3)
      CALL MATMUL(VBEL,TLB,VBEB,3,3,1)
C
C*** INITIALIZE HBE
C
      HBE=-SBEL(3)
	GNDTCK=0.
      RETURN
      END
C******************************************************************
      SUBROUTINE D1
C******************************************************************
C***  * Newton Module D1
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Integrates vehicle velocity in body axes
C***  * (2) Integrates vehicle position in local level axes
C***  *
C***  * MODIFICATION HISTORY
C***  * 931117 Created by Peter Zipfel
C***  * 980413 Added groundtrack diagnostic: GNDTCK, PZi
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 1600-1699
C
      COMMON C(3510)
C
      DIMENSION VBEBD(3),VBEB(3),SBELD(3),SBEL(3),WBEB(3),DUM33(3,3)
     &,ATB(3),FAPB(3),FSPB(3),DUM4(3),GRAVL(3),GRAVB(3),TBL(3,3)
     &,TLB(3,3),VBEL(3),SBELM(3),DUM3(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2866),ICOOR)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0205),GRAV)
      EQUIVALENCE (C(0320),TBL(1,1))
      EQUIVALENCE (C(0901),MFREEZE)
      EQUIVALENCE (C(1309),VMASS)
      EQUIVALENCE (C(1410),FAPB(1))
      EQUIVALENCE (C(1713),WBEB(1))
C
C GRAV= O Gravity acceleration - m/s^2
C TBL= O Transf matrix of body wrt local level axes
C MFREEZE= D =0:Unfreeze and print; =1:Freeze; increment for more
C VMASS= O Vehicle mass - kg
C FAPB= O Aerodynamic and propulsive forces in body axes - N
C WBEB= G Angular vel of veh wrt earth in body axes - rad/s
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
C
C FSPB(3) = O Specific force in body axes - m/s^2
C VBEL(3) = O Vehicle velocity in local level axes - m/s
C DVBE = I/O Vehicle speed - m/s
C HBE = O Height above ground - m
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1640),PSIVLX)
      EQUIVALENCE (C(1641),THTVLX)
      EQUIVALENCE (C(1642),GNDTCK)
      EQUIVALENCE (C(1644),AN)
      EQUIVALENCE (C(1645),AL)
      EQUIVALENCE (C(1646),ATB)
C
C PSIVLX = G Heading angle - deg
C THTVLX = G Vertical flight path angle - deg
C GNDTCK = G Ground track - m
C AN = G Normal specific force component - g's
C AL = G Lateral specific force component - g's
C ATB = G Tangential accel, solving Newton's Law - m/s^2
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
C*** DIAGNOSTIC: ACCELERATIONS ACIEVED
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
C******************************************************************
      SUBROUTINE D2I
C******************************************************************
C***  * The initialization module for the Euler Module D2
C***  *
C***  * This module performs the following functions:
C***  *
C***  * Defines the loc of the state variables pp, qq, rr
C***  *
C***  * MODIFICATION HISTORY
C***  * 931110 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 1700 - 1799
C
      COMMON C(3510)
C
      DIMENSION  IPL(100), IPLV(100)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** STORAGE OF STATE VARIABLE LOCATIONS
C
      ILOC=1704
      DO I=1,3
         IPL(NIP)=ILOC
         IPLV(NIP)=ILOC+1
         ILOC=ILOC+2
         NIP=NIP+1
      ENDDO
C
      RETURN
      END
C*******************************************************************
      SUBROUTINE D2
C*******************************************************************
C***  * Euler Module
C***  *
C***  * This module performs the following functions:
C***  *
C***  * 1) Solves the Euler equations
C***  * 2) Calculates the body rates
C***  *
C***  * MODIFICATION HISTORY
C***  * 931110 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 1700 - 1799
C
      COMMON C(3510)
C
      DIMENSION FMB(3),WBEB(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2000),T)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1321),AI11)
      EQUIVALENCE (C(1322),AI33)
      EQUIVALENCE (C(1413),FMB(1))
C
C AI11= O Roll Moment of Inertia - kg*m^2
C AI33= O Pitch/Yaw moment of Inertia - kg*m^2
C FMB= O Aerodynamic and propulsive moments in body axes - N*m
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1704),PPD)
      EQUIVALENCE (C(1705),PP)
      EQUIVALENCE (C(1706),QQD)
      EQUIVALENCE (C(1707),QQ)
      EQUIVALENCE (C(1708),RRD)
      EQUIVALENCE (C(1709),RR)
C
C PP = S Body roll angular velocity - rad/s
C QQ = S Body pitch angular velocity - rad/s
C RR = S Body yaw angular velocity - rad/s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1710),PPX)
      EQUIVALENCE (C(1711),QQX)
      EQUIVALENCE (C(1712),RRX)
      EQUIVALENCE (C(1713),WBEB(1))
C
C PPX = O Body roll angular velocity in body axes - deg/s
C QQX = O Body pitch angular velocity in body axes - deg/s
C RRX = O Body yaw angular velocity in body axes - deg/s
C WBEB(3) = G Angular vel of veh wrt earth in body axes - rad/s
C
C*** DIAGNOSTICS
C
C
C*** ANGULAR VELOCITY RATES
C
      PPD=FMB(1)/AI11
      QQD=((AI33-AI11)*PP*RR+FMB(2))/AI33
      RRD=(-(AI33-AI11)*PP*QQ+FMB(3))/AI33
C
      PPX=PP*CRAD
      QQX=QQ*CRAD
      RRX=RR*CRAD
C
      CALL VECVEC(WBEB,PP,QQ,RR)
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE G1I
C***********************************************************************
C***  * Target Initialization Module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Identifies the state variables for integration
C***  * (2) Initializes the target and shooter velocity vectors
C***  * (3) Initializations for special engagements
C***  *     (a) Fixes Shooter initial position
C***  *     (b) Calculates shooter angle of attack
C***  *     (c) Calculates shooter Euler angles
C***  *     (d) Shooter velocity vector
C***  *     (e) Fixes target initial position
C***  *     (f) Calculates target Euler angles
C***  *     (g) Target velocity vector
C***  *     (h) Missile Euler angles = shooter's Euler angles, modified by
C***  *         shooter angle of attack and missile installed roll angle
C***  *     (i) Missile acceleration = shooter acceleration
C***  *     (k) Missile velocity = shooter velocity
C***  *     (l) Missile position = shooter position
C***  * (4) Initialization of specific engagements
C***  *     LAR-1 Attack Zone (MINIT=21)
C***  *     LAR-2 Pre-Merge Engagement  (MINIT=12)
C***  *     LAR-3 One Circle Fight (MINIT=13)
C***  *     LAR-4 Two Circle Fight (MINIT=14)
C***  *     LAR-5 Lufbery Circle (MINIT=15)
C***  *     CIRC-2 Chase Circle Engagement (UK) (MINIT=22)
C***  *     CIRC-3 Head-On Circle Engagement (UK) (MINIT=23)
C***  *     CIRC-4 Twin Circle Engagement (UK) (MINIT=24)
C***  *
C_______________________________________________________________________________
C                                          |                        ||
C   INITIALIZATION VARIABLES               |     SHOOTER CENTERED   ||      TARG
C__________________________________________|FREE|LAR2|LAR3|LAR4|LAR5||FREE|LAR1|
C MINIT = Engagement modes (see Module D1) | 10 | 12 | 13 | 14 | 15 || 20 | 21 |
C*** TARGET ***
C DVT1E=  Speed of target wrt earth(m/s)   |    |  I |  I |  I |  I ||    |  I |
C AN1C=   Normal load factor of target(g's)|    |  1.|  I |  I |  I ||    |  I |
C HT1E=   Height of target at launch(m)    |    |  I |  I |  I |  I ||    |  I |
C TAUHX=  Target aspect angle(deg)         |    |  I |  G |  G |  G ||    |  I |
C*** SHOOTER ***
C DVT2E=  Speed of shooter wrt earth(m/s)  |    |  I DVT1E|DVT1E  I ||    |  I |
C AN2C=   Normal load fact.of shooter(g's) |    |  1.|AN1C|AN1C|  I ||    |  1.|
C HT2E=   Height of shooter at launch(m)   |    |HT1E|HT1E|HT1E|  I ||    |HT1E|
C ALAMHX= Look angle of shooter(deg)       |    |  I |  I |  I |  I ||    |  G |
C SIGHX=  Slew angle of shooter (deg)      |    |  G |  G |  G |  G ||    |  I |
C RHL=    Horizontal launch range(m)       |    |  I |  I |  I |  I ||    |  I |
C RCOMB=  Combat radius for circle engmt(m)|    |  - |  - |  - |  - ||    |  - |
C AMUHX=  Maneuver angle(deg)              |    |  - |  - |  - |  - ||    |  - |
C WLOADT2=Wing loading of shooter(N/m^2)   |    | I/N| I/N| I/N| I/N||    | I/N|
C CLAT2=  Lift Slope of shooter(1/deg)     |    | I/N| I/N| I/N| I/N||    | I/N|
C ALPT2X= Shooter alpha(if=0:internal)(deg)|    | I/N| I/N| I/N| I/N||    | I/N|
C PHIBT2X=Missile carriage roll angle(deg) |    |  I |  I |  I |  I ||    |  I |
C_______________________________________________________________________________
C
C Code: I Initialize, G Diagnostic, I/N Initialize or not, - Not used, 'blank' n
C
C Notes: (1) MTARG=11 (always); (2) Entries with variable names may be given dif
C        (3) MINIT=0 General initialization of shooter, target, and missile (see
C***  *
C***  * MODIFICATION HISTORY
C***  * 941129 Created by Peter Zipfel
C***  * 950421 Added LAR and CIRCLE engagement initializations (PZi)
C***  * 950719 Trigonometry fix for LAR4 (TAO)
C***  * 971128 Added alpha initialization for cirle fights (PZi)
C***  *
C***  ******************************************************************
C
C*** TARGET MODULE INITIALIZATION
C
C*** COMMON LOCATIONS ARE 100-199
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100),VT1EL(3),VT2EL(3),ST2EL(3),TBT2(3,3)
     &,TT2L(3,3),TBL(3,3),ST1EL(3),AT2ET2(3),VBEBD(3),VBEB(3),SBELD(3)
     &,SBEL(3),VBEL(3),AT2EB(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0100),MTARG)
      EQUIVALENCE (C(0105),AN1C)
      EQUIVALENCE (C(0140),DVT1E)
      EQUIVALENCE (C(0141),PSIT1LX)
      EQUIVALENCE (C(0142),THTT1LX)
      EQUIVALENCE (C(0144),HT1E)
      EQUIVALENCE (C(0145),TAUHX)
      EQUIVALENCE (C(0153),AX2C)
      EQUIVALENCE (C(0155),AN2C)
      EQUIVALENCE (C(0157),AMUHX)
      EQUIVALENCE (C(0158),RCOMB)
      EQUIVALENCE (C(0159),SIGHX)
      EQUIVALENCE (C(0190),DVT2E)
      EQUIVALENCE (C(0191),PSIT2LX)
      EQUIVALENCE (C(0192),THTT2LX)
      EQUIVALENCE (C(0194),WLOADT2)
      EQUIVALENCE (C(0195),CLAT2)
      EQUIVALENCE (C(0196),PHIBT2X)
      EQUIVALENCE (C(0197),HT2E)
      EQUIVALENCE (C(0198),ALAMHX)
      EQUIVALENCE (C(0199),RHL)
C
C MTARG = D |MTARG|=|TARGET|SHOOTER|, =0:Fixed, =1:moving
C AN1C = D Commanded normal load factor of T1 - g's
C DVT1E = I/G Speed of T1 wrt earth - m/s
C PSIT1LX = I/G Heading angle of T1 velocity vector - deg
C THTT1LX = I/G Flight path angle of T1 celocity vector - deg
C HT1E = D Height of target at launch - m
C TAUHX = D/G Horizontal target aspect angle - deg
C AX2C = D Commanded accel along the T2 vel vec - g's
C AN2C = D Commanded normal load factor of T2 - g's
C AMUHX = D/G Horizontal maneuver angle of target wrt shooter - deg
C RCOMB = D Combat radius for circle engagements - m
C SIGHX = D/G Horizontal shooter slew angle - deg
C DVT2E = I/G Speed of T2 wrt earth - m/s
C PSIT2LX = I/G Heading angle of T2 velocity vector - deg
C THTT2LX = I/G Flight path angle of T2 celocity vector - deg
C WLOADT2 = D Wing loading of shooter - N/m^2
C CLAT2 = D Lift Slope of shooter - 1/deg
C PHIBT2X = D Missile carriage installation roll angle - deg
C HT2E = D Height of shooter at launch - m
C ALAMHX = D/G Look angle of shooter at launch - deg
C RHL = D/G Horizontal launch range - m
C
C*** INPUT FROM EXEC
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1600),MINIT)
C
C MINIT= D Initialization modes, see table in Module D1I
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(0101),PHIT1LCX)
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(0151),PHIT2LCX)
      EQUIVALENCE (C(0169),VT2EL(1))
      EQUIVALENCE (C(0175),ST2EL(1))
C
C*** OUTPUT TO OTHER MODULES FOR INITIALIZATION
C
      EQUIVALENCE (C(0337),PSIBLX)
      EQUIVALENCE (C(0338),THTBLX)
      EQUIVALENCE (C(0339),PHIBLX)
      EQUIVALENCE (C(1610),VBEBD(1))
      EQUIVALENCE (C(1613),VBEB(1))
      EQUIVALENCE (C(1616),SBELD(1))
      EQUIVALENCE (C(1619),SBEL(1))
      EQUIVALENCE (C(1636),DVBE)
      EQUIVALENCE (C(1639),HBE)
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0146),PSILAR4X)
      EQUIVALENCE (C(0178),ALPT2X)
      EQUIVALENCE (C(0179),ANUHX)
C
C PSILAR4X = G Shooter heading when pointed at target (LAR4) - deg
C ALPT2X = G/D Shooter alpha (if=0:calculated internally) - deg
C ANUHX = G Horizontal velocity angle of target wrt shooter - deg
C
      MTARG1=INT(MTARG/10.)
      MTARG2=MTARG-MTARG1*10.
C
      MINS=INT(MINIT/100.)
      MINC=INT((MINIT-MINS*100)/10.)
      MINM=MINIT-MINS*100-MINC*10
C
C*** INITIALIZATION OF STATE VARIABLE INTEGRATION
C
      IF(MTARG1.EQ.1) THEN
C
         IPL(NIP)=110
         IPLV(NIP)=111
         NIP=NIP+1
         IPL(NIP)=112
         IPLV(NIP)=113
         NIP=NIP+1
         IPL(NIP)=114
         IPLV(NIP)=115
         NIP=NIP+1
C
         ILOC=116
         DO I=0,2
            IPL(NIP)=ILOC+I
            IPLV(NIP)=ILOC+I+3
            NIP=NIP+1
         ENDDO
C
         ILOC=122
         DO I=0,2
            IPL(NIP)=ILOC+I
            IPLV(NIP)=ILOC+I+3
            NIP=NIP+1
         ENDDO
C
      ENDIF
C
      IF(MTARG2.EQ.1) THEN
C
         IPL(NIP)=160
         IPLV(NIP)=161
         NIP=NIP+1
         IPL(NIP)=162
         IPLV(NIP)=163
         NIP=NIP+1
         IPL(NIP)=164
         IPLV(NIP)=165
         NIP=NIP+1
C
         ILOC=166
         DO I=0,2
            IPL(NIP)=ILOC+I
            IPLV(NIP)=ILOC+I+3
            NIP=NIP+1
         ENDDO
C
         ILOC=172
         DO I=0,2
            IPL(NIP)=ILOC+I
            IPLV(NIP)=ILOC+I+3
            NIP=NIP+1
         ENDDO
	ENDIF
C
C*** FOR GENERAL INITIALIZATION (MINIT=0) BYPASS SHOOTER INITIALIZATIONS (MINIT>
C
      IF(MINIT.NE.0) THEN
C
C***    SHOOTER INITIALIZATION
C
C***    MISSILE ANGLE OF ATTACK CALCULATED	
         IF(ALPT2X.EQ.0.)THEN
C**     AIR DENSITY IN KG*M^3
            RHOT2=1.225*(1.+ST2EL(3)/41900.)**4
C**     ANGLE OF ATTACK
            ALPT2X=2.*AN2C/(RHOT2*DVT2E**2)*WLOADT2/CLAT2
         ENDIF
C
C**     SHOOTER POSITION
C
         IF(MINC.EQ.1) CALL VECVEC(ST2EL,0.,0.,-HT2E)
         IF(MINC.EQ.2)THEN
            IF(MINM.EQ.1)CALL VECVEC(ST2EL,RHL*COS(TAUHX/CRAD)
     &                               ,RHL*SIN(TAUHX/CRAD),-HT2E)
            IF(MINM.GE.2)CALL VECVEC(ST2EL,-RCOMB*SIN(AMUHX/CRAD)
     &                            ,RCOMB*(1.-COS(AMUHX/CRAD)),-HT2E)
         ENDIF
C**     SHOOTER ANGLES
C*      HORIZONTAL, SHOOTER CENTERED LAUNCH FOR  LAR-2
         IF(MINC.EQ.1.AND.MINM.EQ.2) THEN
            PSIT2LX=0.
            THTT2LX=0.
            PHIT2LX=0.
         ENDIF
C*      HORIZONTAL, SHOOTER CENTERED LAUNCH FOR  LAR-3, LAR-4, LAR-5
         IF(MINC.EQ.1.AND.MINM.GE.3) THEN
            PHIT2L=ATAN(AN2C)
            SPHIT2=SIN(PHIT2L)
            PHIT2LX=CRAD*PHIT2L
            PSIT2LX=-CRAD*ATAN(SPHIT2*TAN(ALPT2X/CRAD))
            THTT2LX=0.
            PHIT2LCX=PHIT2LX
         ENDIF
C*      FOR LAR4 ONLY: CALCULATE THAT HEADING ANGLE OF THE SHOOTER, PSILAR4,
C           AT WHICH SHOOTER AND TARGET VELOCITY VECTORS ARE POINTING AT EACH OT
C*          AT THIS POINT IN THE TRAJECTORY BOTH WILL CONVERT TO LEVEL FLIGHT.
         IF(MINC.EQ.1.AND.MINM.EQ.4) THEN
            GH=AN2C*SIN(PHIT2L)
            RHT=DVT2E**2./(GH*AGRAV)
            SIGH=(ALPT2X+ALAMHX)/CRAD
            DENOM2=RHL**2./4+RHT**2.-RHL*RHT*SIN(SIGH)
            DENOM=SQRT(DENOM2)
            ARGU=(RHT-RHL/2.*SIN(SIGH))/DENOM
            IF(ABS(ARGU).LE.1.) THEN
               ANGL1=ACOS((RHT-RHL/2.*SIN(SIGH))/DENOM)
            ELSE
               ANGL1=3.1416
            ENDIF
            IF(RHT.LE.DENOM) THEN
               ANGL2=ACOS(RHT/DENOM)
            ELSE
               ANGL2=-1.5708
            ENDIF
            ALPT2=ALPT2X/CRAD
C
            CON1=RHT*SIN(ALPT2)
            CON2=RHL-2.*RHT*COS(ALPT2)
            CONX=CON1/CON2
            ANGXI=ATAN(CONX)
            CON3=2.*SIN(ANGXI)
            CON4=CON3/SIN(ALPT2)
            IF(ABS(CON4).GT.1.) CON4=SIGN(1.,CON4)
            ANGZETA=COS(CON4)
            DELTAX=(1.5708+ANGXI-ANGZETA)*CRAD
C
            IF(ALAMHX.LE.70.) THEN
               PSILAR4=ANGL1-ANGL2-ALPT2
               PSILAR4X=PSILAR4*CRAD
            ELSE
               PSILAR4X=ALAMHX+DELTAX
            ENDIF
         ENDIF
C*      HORIZONTAL, TARGET CENTERED LAUNCH FOR LAR-1 AND THREE UK CIRCLES
         IF(MINC.EQ.2)THEN
            THTT2LX=0.
            IF(MINM.EQ.1)THEN
               PHIT2LX=0.
               PSIT2LX=-180.+TAUHX-SIGHX
               ALAMHX=SIGHX-ALPT2X
            ENDIF
            IF(MINM.EQ.2)THEN
               PHIT2LX=CRAD*ATAN(DVT2E**2/(AGRAV*RCOMB))
               AN2C=1./COS(PHIT2LX/CRAD)
               PSIT2LX=-AMUHX
               SIGHX=AMUHX/2.
               TAUHX=180-SIGHX
            ENDIF
            IF(MINM.GE.3)THEN
               PHIT2LX=-CRAD*ATAN(DVT2E**2/(AGRAV*RCOMB))
               AN2C=1./COS(PHIT2LX/CRAD)
               PSIT2LX=-180.-AMUHX
               SIGHX=AMUHX/2.
               TAUHX=-SIGHX
            ENDIF
            PHIT2LCX=PHIT2LX
         ENDIF
C***    MISSILE ANGLE OF ATTACK RE-CALCULATED FOR CIRCLE LOAD FACTORS	
         IF(ALPT2X.EQ.0.)THEN
C**     AIR DENSITY IN KG*M^3
            RHOT2=1.225*(1.+ST2EL(3)/41900.)**4
C**     ANGLE OF ATTACK
            ALPT2X=2.*AN2C/(RHOT2*DVT2E**2)*WLOADT2/CLAT2
         ENDIF
C***     TRANSFORMATION MATRIX OF INSTALLED MISSILE WRT SHOOTER
         CALP=COS(ALPT2X/CRAD)
         SALP=SIN(ALPT2X/CRAD)
         CPHI=COS(PHIBT2X/CRAD)
         SPHI=SIN(PHIBT2X/CRAD)
         TBT2(1,1)=CALP
         TBT2(1,2)=0.
         TBT2(1,3)=-SALP
         TBT2(2,1)=SPHI*SALP
         TBT2(2,2)=CPHI
         TBT2(2,3)=SPHI*CALP
         TBT2(3,1)=CPHI*SALP
         TBT2(3,2)=-SPHI
         TBT2(3,3)=CPHI*CALP
C**     SHOOTER TRANSFORMATION MATRIX
         CALL MAT3TR(TT2L,PSIT2LX/CRAD,0.,PHIT2LX/CRAD)
      ENDIF
C**  SHOOTER VELOCITY VECTOR
      PSIT2L=PSIT2LX/CRAD
      THTT2L=THTT2LX/CRAD
      CALL MATCAR(VT2EL,DVT2E,PSIT2L,THTT2L)
C
C*** FOR GENERAL INITIALIZATION (MINIT=0) BYPASS TARGET INITIALIZATIONS (MINIT>0
C
      IF(MINIT.NE.0) THEN
C
C***    TARGET INITIALIZATION
C
C**     TARGET POSITION
         IF(MINC.EQ.1)THEN
            ST1EL(1)=RHL*COS(ALAMHX/CRAD)
            ST1EL(2)=RHL*SIN(ALAMHX/CRAD)
            ST1EL(3)=-HT1E
         ENDIF
         IF(MINC.EQ.2)THEN
            CALL VECVEC(ST1EL,0.,0.,-HT1E)
            IF(MINM.GE.2)THEN
               RHL=2.*RCOMB*ABS(SIN(AMUHX/(CRAD*2.)))
            ENDIF
         ENDIF
C**     TARGET ANGLES
C*      HORIZONTAL, SHOOTER CENTERED LAUNCH  FOR LAR-2, LAR-3, LAR-4, LAR-5
         IF(MINC.EQ.1)THEN
            THTT1LX=0.
            IF(MINM.EQ.2)THEN
               PSIT1LX=180.+ALAMHX-TAUHX
               PHIT1LX=0.
               SIGHX=ALAMHX
            ENDIF
            IF(MINM.EQ.3)THEN
               PSIT1LX=ALPT2X+2.*ALAMHX-180.
               PHIT1LX=-CRAD*ATAN(AN1C)
               TAUHX=-(ALPT2X+ALAMHX)
               SIGHX=-TAUHX
            ENDIF
            IF(MINM.EQ.4)THEN
               PSIT1LX=180.-ALPT2X
               PHIT1LX=CRAD*ATAN(AN1C)
               TAUHX=ALPT2X+ALAMHX
               SIGHX=TAUHX
            ENDIF
            IF(MINM.EQ.5)THEN
               PSIT1LX=2.*ALAMHX+ALPT2X
               PHIT1LX=CRAD*ATAN(AN1C)
               TAUHX=180.-(ALPT2X+ALAMHX)
               SIGHX=180.-TAUHX
            ENDIF
         ENDIF
C*      HORIZONTAL, TARGET CENTERED LAUNCH FOR LAR-1 AND THREE UK CIRCLES
         IF(MINC.EQ.2)THEN
            PSIT1LX=0.
            THTT1LX=0.
            IF(MINM.GE.2)THEN
               AN1C=AN2C
               PHIT1LX=CRAD*ATAN(AN1C)
            ENDIF
            IF(MINM.EQ.4)PHIT1LX=-PHIT1LX
         ENDIF
         PHIT1LCX=PHIT1LX
C
C***    MANEUVER ANGLE
C
         ANUHX=PSIT1LX-PSIT2LX
      ENDIF
C
C*** INITIALIZATION OF TARGET VELOCITY VECTOR
C
      PSIT1L=PSIT1LX/CRAD
      THTT1L=THTT1LX/CRAD
      CALL MATCAR(VT1EL,DVT1E,PSIT1L,THTT1L)
C
C*** FOR GENERAL INITIALIZATION (MINIT=0) BYPASS MISSILE INITIALIZATIONS (MINIT>
C
      IF(MINIT.NE.0) THEN
C
C***    MISSILE INITIALIZATION
C
C**     MISSLE WRT LOCAL LEVEL TRANSFORMATION MATRIX
         CALL MATMUL(TBL,TBT2,TT2L,3,3,3)
C**     MISSILE EULER ANGLES FOR G3I MODULE
         IF(ABS(TBL(1,3)).LT.1.) THTBL=ASIN(-TBL(1,3))
         CTHTBL=COS(THTBL)
         DUM1=TBL(1,1)/CTHTBL
         DUM2=TBL(3,3)/CTHTBL
C GW ****
         IF(ABS(DUM1).LE.1.) PSIBL=ACOS(DUM1)*SIGN(1.,TBL(1,2))
         IF(ABS(DUM2).LE.1.) PHIBL=ACOS(DUM2)*SIGN(1.,TBL(2,3))
C    ****
C         IF(ABS(DUM1).LT.1.) PSIBL=ACOS(DUM1)*SIGN(1.,TBL(1,2))
C         IF(ABS(DUM2).LT.1.) PHIBL=ACOS(DUM2)*SIGN(1.,TBL(2,3))
         PSIBLX=CRAD*PSIBL
         THTBLX=CRAD*THTBL
         PHIBLX=CRAD*PHIBL
C**     MISSILE ACCELERATION FOR D1I MODULE
         CALL VECVEC(AT2ET2,AX2C,0.,-AN2C)
         CALL MATMUL(AT2EB,TBT2,AT2ET2,3,3,1)
         CALL MATCON(VBEBD,AGRAV,AT2EB,3,1)
C**     MISSILE VELOCITY FOR D1I (SIDESLIP ANGLE = O)
         CALL MATEQL(VBEL,VT2EL,3,1)
         CALL MATMUL(VBEB,TBL,VBEL,3,3,1)
         CALL MATEQL(SBELD,VBEL,3,1)
C**     MISSILE SPEED FOR A1
         CALL MATABS(DVBE,VBEL,3)
C**     MISSILE POSITION FOR D1I
         CALL MATEQL(SBEL,ST2EL,3,1)
         HBE=HT2E
      ENDIF
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE G1
C***********************************************************************
C***  * Target Module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * Simulates target and shooter trajectories
C***  *
C***  * MODIFICATION HISTORY
C***  * 941129 Created by Peter Zipfel
C***  *
C***  ******************************************************************
C
C    ________________________________________________________________
C   |                                                               |
C   |   TARGET MODULE FOR AIR-TO-AIR AND AIR-TO-GROUND INTERCEPTS   |
C   |                                                               |
C   |       MTARG=  MTARG1 | MTARG2                                 |
C   |               TARGET | SHOOTER                                |
C   |                                                               |
C   |                 0        0       BOTH FIXED                   |
C   |                 1        1       BOTH MOVING                  |
C   |                 0        1       SHOOTER MOVING ONLY          |
C   |                 1        0       TARGET MOVING ONLY           |
C   |                                                               |
C   |_______________________________________________________________|
C
C    The target and the shooter are modeled by the translational motions
C    of their respective center of mass: T1, T2 and their transformation
C    matrices TT1L, TT2L, of the target coordinates wrt the local earth
C    coordinates L. The target x-axis is parallel to its velocity vector,
C    the z-axis is parallel to the load factor vector but in the opposite
C    direction and the y-axis completes the right handed axis system.
C
C    The target maneuvers are generated by the commanded longitudinal
C    accelerations AX1C, the bank angle of the load factor plane
C    PHIT1LCX, and the normal load factor AN1C, contained in this
C    plane and normal to the target velocity vector VT1EL. The
C    maneuver occurs in the maneuver plane, subtended by the target
C    velocity vector and the total acceleration vector (including
C    gravitational acceleration). The normal load factor plane and the
C    maneuver plane are, in general, not the same.
C
C    To take into account the maneuver time lags, time constants can
C    be introduced into the bank angle, and the longitudinal and normal
C    load factor commands : TPHI1, TAX1, TAN1. For zero values, the
C    responses are instantaneous.
C
C    The target is initialized by its position ST1EL(3x1) and its
C    velocity vector, given by the speed DVT1E, azimuth PSIT1LX and
C    elevation THTT1LX. Target maneuvers can be combined by STAGEING
C    and introducing new values for PHIT1LCX, AX1C,and AN1C.
C
C    The Shooter is set up the same way. One could futher expand the
C    target and shooter modeling by using drag and thrust data to
C    calculate AX1C and/or AX2C. Futhermore, the bank angles PHIT1LCX,
C    PHIT2LCX and the load factors AN1C, AN2C could be tied to the
C    missile dynamics through an evasive guidance law. These
C    extensions, however, are application dependent and not introduced here.
C
C               STAIGHT &    CLIMB IN   HORIZONTAL   DIVING    MISSILE
C                 LEVEL    AFTERBURNER  45 DEG BANK  ESCAPE    LAUNCH
C
C PHIT1LCX  0101     0.       0.         45.          135.      0.
C TPHI1     0102     .8       0.         0.           0.        0.
C AX1C      0103     0.       3.         0.           3.        10.
C TAX1      0104     0.       0.         0.           0.        .2
C AN1C      0105     1.       1.064 (2)  1.414 (3)    5.        0.
C TAN1      0106     .8       0.         0.           0.        0.
C ST1EL1    0125     0.       0.         0.           0.        0.
C ST1EL2    0126     2000.    2000.      2000.        2000.     0.
C ST1EL3    0127     -6000.   -6000.     -6000.       -6000.    0.
C DVT1E     0140     300.     300.       300.         300.      10. (4)
C PSIT1LX   0141     180.     0.         0.           0.        90.
C THTT1LX   0142     0.       20.        0.           -30.      70.
C
C **************** STAGEING ******************************** STAGEING ***
C
C                 HORIZONTAL                                 BURNOUT
C                 7 G ESCAPE
C
C PHIT1LCX  0101     81.86 (1)
C AN1C      0105     7.
C
C AX1C      0103                                                0.
C
C
C REMARKS: (1) PHIT1LCX= ATAN(AN1C)
C          (2) AN1C= 1/COS(THTT1LX)
C          (3) AN1C= 1/COS(PHIT1LCX)
C          (4) Small initial velocity required.
C
C________________________________________________________________________
C
C*** COMMON LOCATIONS ARE 100-199
C
      COMMON C(3510)
C
      DIMENSION TT1L(3,3),VT1ELD(3),VT1EL(3),ST1ELD(3),ST1EL(3)
     &,TT2L(3,3),VT2ELD(3),VT2EL(3),ST2ELD(3),ST2EL(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0100),MTARG)
      EQUIVALENCE (C(0101),PHIT1LCX)
      EQUIVALENCE (C(0102),TPHI1)
      EQUIVALENCE (C(0103),AX1C)
      EQUIVALENCE (C(0104),TAX1)
      EQUIVALENCE (C(0105),AN1C)
      EQUIVALENCE (C(0106),TAN1)
      EQUIVALENCE (C(0151),PHIT2LCX)
      EQUIVALENCE (C(0152),TPHI2)
      EQUIVALENCE (C(0153),AX2C)
      EQUIVALENCE (C(0154),TAX2)
      EQUIVALENCE (C(0155),AN2C)
      EQUIVALENCE (C(0156),TAN2)
C
C MTARG = D |MTARG|=|TARGET|SHOOTER|, =0:Fixed, =1:moving
C PHIT1LCX = D Commanded bank angle of normal load plane of T1 - deg
C TPHI1 = D Time lag bank angle of normal load plane of T1- s
C AX1C = D Commanded accel along the T1 vel vec - g's
C TAX1 = D Time lag of longitudinal accel of T1 - s
C AN1C = D Commanded normal load factor of T1 - g's
C TAN1 = D Time lag of normal load of T1 - s
C PHIT2LCX = D Commanded bank angle of normal load plane of T2 - deg
C TPHI2 = D Time lag bank angle of normal load plane of T2- s
C AX2C = D Commanded accel along the T2 vel vec - g's
C TAX2 = D time lag of longitudinal accel of T2 - s
C AN2C = D Commanded normal load factor of T2 - g's
C TAN2 = D Time lag of normal load of T2 - s
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0052),CRAD)
C
C CRAD= E Conversion from radians to degree (57.29577951)
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0110),PHIT1LD)
      EQUIVALENCE (C(0111),PHIT1L)
      EQUIVALENCE (C(0112),AX1D)
      EQUIVALENCE (C(0113),AX1)
      EQUIVALENCE (C(0114),AN1D)
      EQUIVALENCE (C(0115),AN1)
      EQUIVALENCE (C(0116),VT1ELD(1))
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0122),ST1ELD(1))
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(0160),PHIT2LD)
      EQUIVALENCE (C(0161),PHIT2L)
      EQUIVALENCE (C(0162),AX2D)
      EQUIVALENCE (C(0163),AX2)
      EQUIVALENCE (C(0164),AN2D)
      EQUIVALENCE (C(0165),AN2)
      EQUIVALENCE (C(0166),VT2ELD(1))
      EQUIVALENCE (C(0169),VT2EL(1))
      EQUIVALENCE (C(0172),ST2ELD(1))
      EQUIVALENCE (C(0175),ST2EL(1))
C
C PHIT1L = S Bank angle of normal load factor plane of T1 - rad
C AX1 = S Acceleration along the target velocity vector of T1 - g's
C AN1 = S Normal load factor (normal to velocity vector) of T1 - g's
C VT1EL(3) = S Velocity of T1 wrt earth in local level coord - m/s
C ST1EL(3) = S Position of T1 wrt earthref point E in L coor - m
C PHIT2L = S Bank angle of normal load factor plane of T2 - rad
C AX2 = S Acceleration along the target velocity vector of T2 - g's
C AN2 = S Normal load factor (normal to velocity vector) of T2 - g's
C VT2EL(3) = S Velocity of T2 wrt earth in local level coord - m/s
C ST2EL(3) = S Position of T2 wrt earthref point E in L coor - m
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0130),TT1L(1,1))
      EQUIVALENCE (C(0180),TT2L(1,1))
C
C TT1L(3,3) = O Transf Matrix of T1 body coord wrt local level coord
C TT2L(3,3) = O Transf Matrix of T2 body coord wrt local level coord
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0139),PHIM1LX)
      EQUIVALENCE (C(0140),DVT1E)
      EQUIVALENCE (C(0141),PSIT1LX)
      EQUIVALENCE (C(0142),THTT1LX)
      EQUIVALENCE (C(0143),PHIT1LX)
      EQUIVALENCE (C(0179),ANUHX)
      EQUIVALENCE (C(0189),PHIM2LX)
      EQUIVALENCE (C(0190),DVT2E)
      EQUIVALENCE (C(0191),PSIT2LX)
      EQUIVALENCE (C(0192),THTT2LX)
      EQUIVALENCE (C(0193),PHIT2LX)
C
C PHIM1LX = G Bank angle of maneuver plane of T1 - deg
C DVT1E = I/G Speed of T1 wrt earth - m/s
C PSIT1LX = I/G Heading angle of T1 velocity vector - deg
C THTT1LX = I/G Flight path angle of T1 celocity vector - deg
C PHIT1LX = G Bank angle of normal load factor plane of T1 - deg
C ANUHX =   G Horizontal velocity angle of target wrt shooter - deg
C PHIM2LX = G Bank angle of maneuver plane of T2 - deg
C DVT2E = I/G Speed of T2 wrt earth - m/s
C PSIT2LX = I/G Heading angle of T2 velocity vector - deg
C THTT2LX = I/G Flight path angle of T2 celocity vector - deg
C PHIT2LX = G Bank angle of normal load factor plane of T2 - deg
C
      MTARG1=INT(MTARG/10.)
      MTARG2=MTARG-MTARG1*10.
C
      IF(MTARG1.EQ.1) THEN
         PHIT1LC=PHIT1LCX/CRAD
         CALL G1TARG(TT1L,PHIT1LD,AX1D,AN1D,VT1ELD,ST1ELD,PHIM1LX
     &   ,DVT1E,PSIT1LX,THTT1LX
     &   ,VT1EL,PHIT1L,PHIT1LC,AX1,AX1C,AN1,AN1C,TPHI1,TAX1,TAN1)
      PHIT1LX=PHIT1L*CRAD
      ENDIF
C
      IF(MTARG2.EQ.1) THEN
         PHIT2LC=PHIT2LCX/CRAD
         CALL G1TARG(TT2L,PHIT2LD,AX2D,AN2D,VT2ELD,ST2ELD,PHIM2LX
     &   ,DVT2E,PSIT2LX,THTT2LX
     &   ,VT2EL,PHIT2L,PHIT2LC,AX2,AX2C,AN2,AN2C,TPHI2,TAX2,TAN2)
      PHIT2LX=PHIT2L*CRAD
      ENDIF
C
C*** HORIZONTAL VELOCITY ANGLE OF TARGET WRT SHOOTER
C
      ANUHX=PSIT1LX-PSIT2LX
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G1TARG(TTL,PHITLD,AXD,AND,VTELD,STELD,PHIMLX
     &,DVTE,PSITLX,THTTLX
     &,VTEL,PHITL,PHITLC,AX,AXC,AN,ANC,TPHI,TAX,TAN)
C
C OUTPUT ARGUMENTS:
C        TTL(3*3)  =Transformation matrix of target frame wrt local earth
C        PHITLD    =Derivative of PHITL
C        AXD       =Derivative of AX
C        AND       =Derivative of AN
C        VTELD(3*1)=Derivative of VTEL
C        STELD(3*1)=Derivative of STEL
C        PHIMLX    =Bank angle of Maneuver Plane
C        DVTE      =Target speed - m/s
C        PSITLX    =Heading angle of target velocity vector - deg
C        THTTLX    =Flight path angle of target velocity vector - deg
C
C INPUT  ARGUMENTS:
C        VTEL(3*1) =Velocity of target in local earth coord. - m/s
C        PHITL     =Bank angle of normal load factor plane - rad
C        PHITLC    =Commanded PHITL - rad
C        AX        =Acceleration along the target velocity vector - g's
C        AXC       =Commanded AX - g's
C        AN        =Normal load factor (normal to velocity vector) - g's
C        ANC       =Commanded AN - g's
C        TPHI      =Time lag of bank angle of load factor plane - s
C        TAX       =Time lag of acceleration along velocity vector - s
C        TAN       =Time lag of normal load factor - s
C
C NOTE: (1) PSITLX,THTTLX,PHITLX are the Euler angles of TTL transf.matrix
C       (2) PHITLX and PHIMLX are the bank angles of the vehicle symmetry
C           plane (load factor plane) and the maneuver plane, respectively.
C            They are, in general, not the same.
C
      COMMON C(3510)
C
      DIMENSION VTEL(3),VTELD(3),STELD(3),AAT(3)
     &,TVL(3,3),TTV(3,3),TTL(3,3),TLT(3,3),GL(3),AAL(3)
     &,UVL(3),DUM33(3,3),EE(3,3),PROJ(3,3),APROJ(3),UPROJ(3),UL3(3)
C
C     INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
C
C*** TRANSFORMATION MATRIX TTL
C
      CALL MATPOL(DVTE,PSITL,THTTL,VTEL)
      CALL MAT2TR(TVL,PSITL,THTTL)
      CALL MATUNI(TTV,3)
      CPHITL=COS(PHITL)
      SPHITL=SIN(PHITL)
      TTV(2,2)=CPHITL
      TTV(3,3)=CPHITL
      TTV(2,3)=SPHITL
      TTV(3,2)=-SPHITL
      CALL MATMUL(TTL,TTV,TVL,3,3,3)
C
C*** DELAY OF INPUT COMMAND
C
      IF(TPHI.NE.0.) THEN
         PHITLD=(PHITLC-PHITL)/TPHI
      ELSE
         PHITL=PHITLC
      ENDIF
      IF(TAX.NE.0.) THEN
         AXD=(AXC-AX)/TAX
      ELSE
         AX=AXC
      ENDIF
      IF(TAN.NE.0.) THEN
         AND=(ANC-AN)/TAN
      ELSE
         AN=ANC
      ENDIF
C
C*** STATE INTEGRATION
C
      CALL VECVEC(AAT,AGRAV*AX,0.,-AGRAV*AN)
      CALL VECVEC(GL,0.,0.,AGRAV)
      CALL MATTRA(TLT,TTL,3,3)
      CALL MATMUL(AAL,TLT,AAT,3,3,1)
      CALL MATADD(VTELD,AAL,GL,3,1)
      CALL MATEQL(STELD,VTEL,3,1)
C
C*** DIAGNOSTICS: MANEUVER PLANE BANK ANGLE: PHIML
C
      CALL VECUVC(UVL,VTEL(1),VTEL(2),VTEL(3))
      CALL MATMUL(DUM33,UVL,UVL,3,1,3)
      CALL MATUNI(EE,3)
      CALL MATSUB(PROJ,EE,DUM33,3,3)
      CALL MATMUL(APROJ,PROJ,VTELD,3,3,1)
      CALL VECUVC(UL3,0.,0.,1.)
      CALL MATMUL(UPROJ,PROJ,UL3,3,3,1)
      CALL VECANG(PHIML,APROJ,UPROJ)
      PHIMLX=PHIML*CRAD
      PSITLX=PSITL*CRAD
      THTTLX=THTTL*CRAD
C
      RETURN
      END
C******************************************************************
      SUBROUTINE G3I
C******************************************************************
C***  * The initialization module for the Kinematic Module G3.
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Initialization of the quaternions as state variables
C***  * (2) Initialization of the quaternions, given the Euler angles.
C***  * (3) Initialization of direction cosine matrix TBL(3x3)
C***  *
C***  * MODIFICATION HISTORY
C***  * 931112 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 300-399
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100),TBL(3,3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INPUT
C
      EQUIVALENCE (C(0337),PSIBLX)
      EQUIVALENCE (C(0338),THTBLX)
      EQUIVALENCE (C(0339),PHIBLX)
C
C*** INITIALIZATION
C
      EQUIVALENCE (C(0311),Q0)
      EQUIVALENCE (C(0313),Q1)
      EQUIVALENCE (C(0315),Q2)
      EQUIVALENCE (C(0317),Q3)
      EQUIVALENCE (C(0320),TBL(1,1))
C
C*** STORAGE OF STATE VARIABLE LOCATIONS
C
      ILOC=310
      DO I=1,4
         IPL(NIP)=ILOC
         IPLV(NIP)=ILOC+1
         ILOC=ILOC+2
         NIP=NIP+1
      ENDDO
C
C*** QUATERNION INITIALIZATION
C
      SPSI=SIN(PSIBLX/(2.*CRAD))
      CPSI=COS(PSIBLX/(2.*CRAD))
      STHT=SIN(THTBLX/(2.*CRAD))
      CTHT=COS(THTBLX/(2.*CRAD))
      SPHI=SIN(PHIBLX/(2.*CRAD))
      CPHI=COS(PHIBLX/(2.*CRAD))
C
      Q0=CPSI*CTHT*CPHI+SPSI*STHT*SPHI
      Q1=CPSI*CTHT*SPHI-SPSI*STHT*CPHI
      Q2=CPSI*STHT*CPHI+SPSI*CTHT*SPHI
      Q3=-CPSI*STHT*SPHI+SPSI*CTHT*CPHI
C
C*** INITIALIZATION OF DIRECTION COSINE MATRIX TBL
C
      CALL MAT3TR(TBL,PSIBLX/CRAD,THTBLX/CRAD,PHIBLX/CRAD)
C
      RETURN
      END
C******************************************************************
      SUBROUTINE G3
C******************************************************************
C***  * Kinematic Module
C***  *
C***  * This module performs the following functions:
C***  *
C***  * (1) Solves the quaternion differential equations
C***  * (2) Calculates the direction cosine matrix of body wrt to earth
C***  *     coordinate system
C***  * (3) Calculates incidence angles
C***  *
C***  * MODIFICATION HISTORY
C***  * 931112 Created by Peter Zipfel
C***  *
C***  **************************************************************
C
C*** COMMON LOCATIONS ARE 300-399
C
      COMMON C(3510)
C
      DIMENSION TBL(3,3),TLB(3,3),UBL(3,3),VBEB(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1286),TRALP)
      EQUIVALENCE (C(1613),VBEB(1))
      EQUIVALENCE (C(1705),PP)
      EQUIVALENCE (C(1707),QQ)
      EQUIVALENCE (C(1709),RR)
      EQUIVALENCE (C(2000),T)
C
C TRCODE= I Termination code number
C TRALP= D Maximum total angle of attack - rad
C VBEB= S Vehicle velocity in body axes - m/s
C PP= S Body roll angular velocity - rad/s
C QQ= S Body pitch angular velocity - rad/s
C RR= S Body yaw angular velocity - rad/s
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0310),Q0D)
      EQUIVALENCE (C(0311),Q0)
      EQUIVALENCE (C(0312),Q1D)
      EQUIVALENCE (C(0313),Q1)
      EQUIVALENCE (C(0314),Q2D)
      EQUIVALENCE (C(0315),Q2)
      EQUIVALENCE (C(0316),Q3D)
      EQUIVALENCE (C(0317),Q3)
C
C Q0 = S Quaternion, 0-th component
C Q1 = S Quaternion, 1-th component
C Q2 = S Quaternion, 2-th component
C Q3 = S Quaternion, 3-th component
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0320),TBL(1,1))
      EQUIVALENCE (C(0340),ALPPX)
      EQUIVALENCE (C(0341),PHIPX)
      EQUIVALENCE (C(0342),ALPP)
      EQUIVALENCE (C(0343),PHIP)
C
C TBL(3,3) = O Transf matrix of body wrt local level axes
C ALPPX = O Total angle of attack - deg
C PHIPX = O Aerodynamic roll angle - deg
C ALPP = O Total angle of attack - rad
C PHIP = O Aerodynamic roll angle - rad
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0334),PSIBL)
      EQUIVALENCE (C(0335),THTBL)
      EQUIVALENCE (C(0336),PHIBL)
      EQUIVALENCE (C(0337),PSIBLX)
      EQUIVALENCE (C(0338),THTBLX)
      EQUIVALENCE (C(0339),PHIBLX)
      EQUIVALENCE (C(0344),ALPHAX)
      EQUIVALENCE (C(0345),BETAX)
      EQUIVALENCE (C(0346),ERQ)
      EQUIVALENCE (C(0347),ETBL)
      EQUIVALENCE (C(0348),TLB(1,1))
C
C PSIBL = G Yawing angle of vehicle - rad
C THTBL = G Pitching angle of vehicle - rad
C PHIBL = G Rolling angle of vehicle - rad
C PSIBLX = I/G Yawing angle of vehicle - deg
C THTBLX = I/G Pitching angle of vehicle - deg
C PHIBLX = I/G Rolling angle of vehicle - deg
C ALPHAX = G Angle of attack - deg
C BETAX = G Side slip angle - deg
C ERQ = G Error of quaternion nonorthogonality
C ETBL = G Error of direction cosine matrix nonorthogonality
C TLB(3,3) = G T.M. of local level wrt body axes
C
C*** PARAMETER
C
	PARAMETER(CK=.5)
C
C CK = D Quaternion orthonormality factor
C
C*** INTEGRATE QUATERNIONS
C
      ERQ=1.-(Q0**2+Q1**2+Q2**2+Q3**2)
      Q0D=0.5*(-PP*Q1-QQ*Q2-RR*Q3)+CK*ERQ*Q0
      Q1D=0.5*(PP*Q0+RR*Q2-QQ*Q3)+CK*ERQ*Q1
      Q2D=0.5*(QQ*Q0-RR*Q1+PP*Q3)+CK*ERQ*Q2
      Q3D=0.5*(RR*Q0+QQ*Q1-PP*Q2)+CK*ERQ*Q3
C
C*** TRANSFORMATION MATRIX OF BODY WRT LOCAL LEVEL COORDINATE AXES
C
      TBL(1,1)=Q0**2+Q1**2-Q2**2-Q3**2
      TBL(1,2)=2.*(Q1*Q2+Q0*Q3)
      TBL(1,3)=2.*(Q1*Q3-Q0*Q2)
      TBL(2,1)=2.*(Q1*Q2-Q0*Q3)
      TBL(2,2)=Q0**2-Q1**2+Q2**2-Q3**2
      TBL(2,3)=2.*(Q2*Q3+Q0*Q1)
      TBL(3,1)=2.*(Q1*Q3+Q0*Q2)
      TBL(3,2)=2.*(Q2*Q3-Q0*Q1)
      TBL(3,3)=Q0**2-Q1**2-Q2**2+Q3**2
C
C*** TBL ORTHOGONALITY CHECK
C
      CALL MATTRA(TLB,TBL,3,3)
      CALL MATMUL(UBL,TLB,TBL,3,3,3)
      ETBL=SQRT((UBL(1,1)-1.)**2+(UBL(2,2)-1.)**2 +(UBL(3,3)-1.)**2)
C
C*** EULER ANGLES
C
      IF(ABS(TBL(1,3)).LT.1.) THTBL=ASIN(-TBL(1,3))
      CTHTBL=COS(THTBL)
      DUM1=TBL(1,1)/CTHTBL
      DUM2=TBL(3,3)/CTHTBL
      IF(ABS(DUM1).LE.1.) PSIBL=ACOS(DUM1)*SIGN(1.,TBL(1,2))
      IF(ABS(DUM2).LE.1.) PHIBL=ACOS(DUM2)*SIGN(1.,TBL(2,3))
C
      PSIBLX=CRAD*PSIBL
      THTBLX=CRAD*THTBL
      PHIBLX=CRAD*PHIBL
C
C*** INCIDENCE ANGLES
C
      ALPHA=ATAN2(VBEB(3),VBEB(1))
      CALL MATABS(DVBE,VBEB,3)
      BETA=ASIN(VBEB(2)/DVBE)
      DUM=VBEB(1)/DVBE
      IF(ABS(DUM).GE.1.) DUM=SIGN(1.,DUM)
      ALPP=ACOS(DUM)
      IF(VBEB(2).EQ.0..AND.VBEB(3).EQ.0.)THEN
         PHIP=0.
      ELSE
         PHIP=ATAN2(VBEB(2),VBEB(3))
      ENDIF
C
C*** TERMINATION CONDITION
C
      IF(ALPP.GT.TRALP) TRCODE=5.
C
      ALPHAX=ALPHA*CRAD
      BETAX=BETA*CRAD
      ALPPX=ALPP*CRAD
      PHIPX=PHIP*CRAD
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE G4
C***********************************************************************
C***  * End-Of-Run Subroutine.
C***  *
C***  * This subroutine performs the following functions:
C***  * (1) Stops run at closest point of approach
C***  * (2) Calculates intercept plane. The plane is normal to the relative
C***  *     velocity vector and contains the target center of mass.
C***  *     Its x-axis (which lies in the plane) is obtained by rotating
C***  *     from the nose of the target through PSIPT1X and THTPT1X.
C***  *     Its y-axis remains in the target x,y-plane.
C***  * (3) Interpolates to calculate miss distance in intercept plane
C***  * (4) Interpolates intercept time
C***  * (5) Calculates navigation error at intercept plane. Navigation
C***  *     error is taken at the time the seeker goes blind (MSEEK=5)
C***  *     and projected into the intercept plane.
C***  * (6) Calculates guidance error in intercept plane
C***  * (7) Calculates the intercept geometry for SHAZAM Pk post processing.
C***  * (8) Monte Carlo run: for every sample run the intercept plane
C***  *     will be oriented slightly different. Better use two M.C. runs.
C***  *     First run establishes the mean orientation angles PSIPT1X,THTPT1X
C***  *     of the intercept plane. Then use MTERM=2 to input these angles for
C***  *     the actual M.C. run.
C***  *
C***  *     MTERM= 0 Internally calculated intercept plane, no SHAZAM output
C***  *            1 Internally calculated intercept plane, with SHAZAM output
C***  *            2 Intercept plane defined by PSIPT1X, THTPT1X input,
C***  *               with SHAZAM output
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  * 951205 Added nav and guidance errors in intercept plane, PZi
C***  * 980415 Introduced LCONV= 3 and 4, PZi
C***  *
C***  ******************************************************************
C
C*** COMMON LOCATIONS ARE 1750-1799
C
      CHARACTER*100 TITLE
      COMMON C(3510)
      COMMON /HCOM/TITLE
C
      DIMENSION VT1EL(3),ST1EL(3),TT1L(3,3),EXX(8),SBEL(3),VBEL(3)
     &,SBTP(3),SBTCP(3),STCTP(3),SBT1L(3),VBT1L(3),VBT1T1(3)
     &,TPT1(3,3),SBT1P(3),TT1P(3,3),SBTT(3),TPL(3,3),TLP(3,3)
     &,SBELM(3),SBBML(3),SBBMP(3),STBMP(3),DUM3(3),SBTL(3)
     &,STCTL(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1750),MSTOP)
      EQUIVALENCE (C(1751),MTERM)
C
C MSTOP = D =0:No stop, =1:Stop sim when TRcond is violated
C MTERM = G/D >0:SHAZAM, <2:intercept plane, =2:i-plane input
C DEC = I Auxiliary variable initialized to one
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(2020),LCONV)
      EQUIVALENCE (C(2664),DER)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(0130),TT1L(1,1))
      EQUIVALENCE (C(0141),PSIT1LX)
      EQUIVALENCE (C(0142),THTT1LX)
      EQUIVALENCE (C(0143),PHIT1LX)
      EQUIVALENCE (C(0206),VMACH)
      EQUIVALENCE (C(0339),PHIBLX)
      EQUIVALENCE (C(0400),MSEEK)
      EQUIVALENCE (C(0666),EXX(1))
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(0900),MAUT)
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1281),TMCODE)
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1619),SBEL(1))
      EQUIVALENCE (C(1633),VBEL(1))
      EQUIVALENCE (C(1636),DVBE)
      EQUIVALENCE (C(1640),PSIVLX)
      EQUIVALENCE (C(1641),THTVLX)
C
C VT1EL= S Velocity of T1 wrt earth in local level coord - m/s
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C TT1L= O Transf Matrix of T1 body coord wrt local level coord
C PSIT1LX= I/G Heading angle of T1 velocity vector - deg
C THTT1LX= I/G Flight path angle of T1 celocity vector - deg
C PHIT1LX= G Bank angle of normal load factor plane of T1 - deg
C VMACH= O Mach number of missile
C PHIBLX= I/G Rolling angle of vehicle - deg
C MSEEK= D/G =2:Enable, =3:Acquisition, =4:Lock
C MGUID= D/G =0:None, =2:LAG, =3:Pro-Nav, =6:Aug Pro-Nav
C MAUT= D =1:Rate; =2:LAG; =3:INT; =4:PROP; =5 FPATH; =6:GAM
C TRCODE= I Termination code number
C TMCODE= I Dummy variable initialized to zero
C MPROP= D =0: Motor off, =1:Motor on
C SBEL= S Vehicle pos. wrt point E in local level axes - m
C VBEL= O Vehicle velocity in local level axes - m/s
C DVBE= I/O Vehicle speed - m/s
C PSIVLX= G Heading angle - deg
C THTVLX= G Vertical flight path angle - deg
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1752),PSIPT1X)
      EQUIVALENCE (C(1753),THTPT1X)
      EQUIVALENCE (C(1754),SBTP(1))
      EQUIVALENCE (C(1757),SBTCP(1))
      EQUIVALENCE (C(1760),STCTP(1))
      EQUIVALENCE (C(1768),EVENT)
      EQUIVALENCE (C(1772),TRCOND)
      EQUIVALENCE (C(1773),DBT)
      EQUIVALENCE (C(1774),DBTC)
      EQUIVALENCE (C(1775),DTCT)
      EQUIVALENCE (C(1776),DBT1)
      EQUIVALENCE (C(1794),SBTT(1))
C
C PSIPT1X = G/D Yaw angle of intercept plane from target nose - deg
C THTPT1X = G/D Pitch angle of intercept plane - deg
C SBTP(3) = G Miss distance in intercept plane and coor. - m
C SBTCP(3) = G Guidance & control error in intercept plane - m
C STCTP(3) = G Navigation error in intercept plane - m
C EVENT = G =|MSEEK|MGUID|MAUT|MPROP| Event flags
C TRCOND = G Termination condition code (filing in from left)
C DBT = G Miss magnitude in intercept plane - m
C DBTC = G Guidance & control miss mag. in intercept plane - m
C DTCT = G Navigation miss magnitude in intercept plane - m
C DBT1 = G Distance of missile wrt target - m
C SBTT(3) = G Closest point to target in targ coord - m
C
C*** EVENT MARKER
C
      DATA DEC/1./
C
      EVENT=REAL(MSEEK*1000+MGUID*100+MAUT*10+MPROP)
C
C*** DISTANCE TO TARGET
C
      CALL MATSUB(SBT1L,SBEL,ST1EL,3,1)
      CALL MATABS(DBT1,SBT1L,3)
C
C*** STOP RUN ON TERMINATION CONDITIONS
C
      IF(TRCODE.NE.TMCODE) THEN
         TRCOND=TRCOND+TRCODE*DEC
         DEC=DEC*10.
         TMCODE=TRCODE
         IF(MSTOP.EQ.1.AND.DBT1.GT.50.) THEN
            LCONV=4
	      DEC=1.
            WRITE(6,8)TRCOND,T,DBT1
         ENDIF
      END IF
C
      IF(DBT1.LT.50.) THEN
C
C***     CLOSING SPEED
C
         CALL MATSUB(VBT1L,VBEL,VT1EL,3,1)
         CALL MATSCA(DUM,SBT1L,VBT1L,3)
         CVEL=DUM/DBT1
C
C***     T.M. TPL OF INTERCEPT PLANE WRT LOCAL LEVEL
C
         IF(MTERM.LT.2)THEN
            CALL MATMUL(VBT1T1,TT1L,VBT1L,3,3,1)
            CALL MATPOL(DVBT1,PSIYT1,THTYT1,VBT1T1)
            PSIPT1X=PSIYT1*CRAD
            THTPT1X=THTYT1*CRAD-90.
         ENDIF
         CALL MAT2TR(TPT1,PSIPT1X/CRAD,THTPT1X/CRAD)
         CALL MATMUL(TPL,TPT1,TT1L,3,3,3)
         CALL MATMUL(SBT1P,TPL,SBT1L,3,3,1)
C
C***        END-OF-RUN CALCULATIONS
C
         IF(CVEL.GT.0.)THEN
C
C***        MISS DISTANCE IN INTERCEPT PLANE (INTERPOLATED)
C
            CALL MATSUB(SBBML,SBEL,SBELM,3,1)
            CALL MATMUL(SBBMP,TPL,SBBML,3,3,1)
            CALL MATSUB(STBMP,SBBMP,SBT1P,3,1)
            WW=STBMP(3)/SBBMP(3)
            CALL MATCON(DUM3,WW,SBBMP,3,1)
            CALL MATSUB(SBTP,DUM3,STBMP,3,1)
            DBT=SQRT(SBTP(1)*SBTP(1)+SBTP(2)*SBTP(2))
            CALL MATTRA(TT1P,TPT1,3,3)
            CALL MATMUL(SBTT,TT1P,SBTP,3,3,1)
            CALL MATTRA(TLP,TPL,3,3)
            CALL MATMUL(SBTL,TLP,SBTP,3,3,1)
C
C***        NAVIGATION ERROR IN INTERCEPT PLANE (NOT INTERPOLATED)
C           (ONLY CALCULATED FOR THE STRAP-DOWN SEEKER CONCEPT SO5A)
C
            DO I=1,3
               STCTL(I)=-EXX(I)
            ENDDO
            CALL MATMUL(STCTP,TPL,STCTL,3,3,1)
            DTCT=SQRT(STCTP(1)*STCTP(1)+STCTP(2)*STCTP(2))
C
C***        GUIDANCE & CONTROL ERROR IN INTERCEPT PLANE (NOT INTERPOLATED)
C
            CALL MATSUB(SBTCP,SBTP,STCTP,3,1)
            DBTC=SQRT(SBTCP(1)*SBTCP(1)+SBTCP(2)*SBTCP(2))
C
C***        TIME AT INTERCEPT
C
            EPCHIM=TM+WW*DER
C
            LCONV=2
	      DEC=1.
C
            WRITE(6,99) TITLE
            WRITE(6,1) EPCHIM,TRCOND
            WRITE(6,2) DBT,SBTT(1),SBTT(2),SBTT(3)
            WRITE(6,3) DBT,SBTP(1),SBTP(2),PSIPT1X
            WRITE(6,4) DBTC,SBTCP(1),SBTCP(2),THTPT1X
            WRITE(6,5) DTCT,STCTP(1),STCTP(2),VMACH
            WRITE(6,6) DVBE,PSIVLX,THTVLX,PHIBLX
            WRITE(6,7) DVBT1,PSIT1LX,THTT1LX,PHIT1LX
C
            IF(MTERM.GT.0) CALL G4SHAZ(SBTL,VBT1L)
C
            GOTO 100
C
         ENDIF
C
C***     SAVE VALUES FOR NEXT INTEGRATION CYCLE
C
         TM=T
         CALL MATEQL(SBELM,SBEL,3,1)
C
         GOTO 100
C
      ENDIF
C
      DBT1M=DBT1
C
      IF(SBEL(3).GE.0.) THEN
C
C***     IMPACT IN HORIZONTAL PLANE
C
         LCONV=3
	   DEC=1.
         WRITE (6,10)
      ENDIF
C
 99   FORMAT(1H0,' ',A)
 1    FORMAT(' INTERCEPT TIME =',1PG12.4,
     +'LIMITER VIOLATIONS TRCOND =',1PG12.4)
 2    FORMAT('    DBT=',1PG12.5,'  SBTT1=',1PG12.5,'  SBTT2='
     +,1PG12.5,'  SBTT3=',1PG12.5)
 3    FORMAT('    DBT=',1PG12.5,'  SBTP1=',1PG12.5,'  SBTP2='
     +,1PG12.5,' PSIPTX=',1PG12.5)
 4    FORMAT('   DBTC=',1PG12.5,' SBTCP1=',1PG12.5,' SBTCP2='
     +,1PG12.5,' THTPTX=',1PG12.5)
 5    FORMAT('   DTCT=',1PG12.5,' STCTP1=',1PG12.5,' STCTP2='
     +,1PG12.5,'  VMACH=',1PG12.5)
 6    FORMAT('   DVBE=',1PG12.5,' PSIVLX=',1PG12.5,' THTVLX='
     +,1PG12.5,' PHIBLX=',1PG12.5)
 7    FORMAT('   DVBT=',1PG12.5,' PSITLX=',1PG12.5,' THTTLX='
     +,1PG12.5,' PHITLX=',1PG12.5)
 8    FORMAT(' *** MSTOP:',' TRCOND =',F4.0,' TIME ='
     +,1PG14.4,' DBT1 =',1PG12.5)
 10   FORMAT(' *** IMPACT OCCURRED IN THE HORIZONTAL PLANE ***')
C
 100  RETURN
      END
C***********************************************************************
      SUBROUTINE G4SHAZ(SBTL,VBT1L)
C***********************************************************************
C***  * Parameters for program SHAZAM Warhead Effectiveness Calculations
C***  * Ref: AFATL-TR-86-32
C***  *
C***  * This subroutine performas the following functions
C***  * (1) Miss distance in plane of closest approach
C***  * (2) Relative missile velocity aspect angles
C***  *
C***  * Argument Input:
C***  *          SBTL(3)=Miss distance components in local axes - m
C***  *          VBT1L(3)=Velocity of missile wrt target in local axes - m
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  *
C***  ******************************************************************
C
      COMMON C(3510)
      PARAMETER(PI=3.14159)
C
      DIMENSION VT1EL(3),VBT1L(3),VBEL(3),VBEU(3),VBT1U(3),TUL(3,3)
     +,TZU(3,3),TZL(3,3),SBTL(3),SHJZ(3),VBT1T1(3)
     +,TT1L(3,3),TYT1(3,3),TYL(3,3),SHJY(3)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0111),PHIT1L)
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0130),TT1L(1,1))
      EQUIVALENCE (C(1633),VBEL(1))
      EQUIVALENCE (C(1639),HBE)
C
C CRAD= E Conversion from radians to degree (57.29577951)
C PHIT1L= S Bank angle of normal load factor plane of T1 - rad
C VT1EL= S Velocity of T1 wrt earth in local level coord - m/s
C TT1L= O Transf Matrix of T1 body coord wrt local level coord
C VBEL= O Vehicle velocity in local level axes - m/s
C HBE= O Height above ground - m
C
C*** OUPUT TO SHAZAM
C
      EQUIVALENCE (C(1764),ASPAZ)
      EQUIVALENCE (C(1765),ASPEL)
      EQUIVALENCE (C(1766),AZINTX)
      EQUIVALENCE (C(1767),ELINTX)
      EQUIVALENCE (C(1770),YSS)
      EQUIVALENCE (C(1771),ZSS)
      EQUIVALENCE (C(1777),DYRB)
      EQUIVALENCE (C(1778),DZRB)
      EQUIVALENCE (C(1779),ASPAZX)
      EQUIVALENCE (C(1780),ASPELX)
C
C ASPAZ = G Az angle of missile vel vec wrt targ vel vec - rad
C ASPEL = G El angle of missile vel vec wrt targ vel vec - rad
C AZINTX = G Az angle of rel vel vec wrt T1 x-axis - rad
C ELINTX = G El angle of rel vel vec wrt T1 x-axis - rad
C YSS = G Y-miss in plane of closest approach - m
C ZSS = G Z-miss in plane of closest approach - m
C DYRB = G Y-miss in plane of closest approach - m
C DZRB = G Z-miss in plane of closest approach - m
C ASPAZX = G Az angle of missile vel vec wrt targ vel vec - deg
C ASPELX = G El angle of missile vel vec wrt targ vel vec - deg
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1781),VBEU(1))
      EQUIVALENCE (C(1784),VBT1U(1))
      EQUIVALENCE (C(1788),PSIUL)
      EQUIVALENCE (C(1789),THTUL)
      EQUIVALENCE (C(1790),PSIVU)
      EQUIVALENCE (C(1791),THTVU)
      EQUIVALENCE (C(1792),PSIZU)
      EQUIVALENCE (C(1793),THTZU)
      EQUIVALENCE (C(1797),PSIUTX)
      EQUIVALENCE (C(1798),THTUTX)
C
C VBEU(3) = G Missile vel in target vel coor - m/s
C VBT1U(3) = G Missile rel vel in target vel coor - m/s
C PSIUL = G Yaw target vel angle - rad
C THTUL = G Pitch target vel angle - rad
C PSIVU = G Yaw missile vel vec wrt vel plane angle - rad
C THTVU = G Pitch missile vel vec wrt vel plane angle - rad
C PSIZU = G Yaw rel vel vec wrt vel plane angle - rad
C THTZU = G Pitch rel vel vec wrt vel plane angle - rad
C PSIUTX = G Yaw of rel vel vec wrt target axes - deg
C THTUTX = G Pitch of rel vel vec wrt target axes - deg
C
C*** CALCULATION OF ASPECT ANGLES
C
      CALL MATPOL(DVT1E,PSIUL,THTUL,VT1EL)
      CALL MAT2TR(TUL,PSIUL,THTUL)
      CALL MATMUL(VBEU,TUL,VBEL,3,3,1)
      CALL MATPOL(DVBE,PSIVU,THTVU,VBEU)
      ASPAZ=-SIGN((PI-ABS(PSIVU)),PSIVU)
      ASPEL=-THTVU
      ASPAZX=ASPAZ*CRAD
      ASPELX=ASPEL*CRAD
C
      CALL MATMUL(VBT1T1,TT1L,VBT1L,3,3,1)
      CALL MATPOL(DVBT1,PSIYT1,THTYT1,VBT1T1)
      ELINT=THTYT1
      AZINT=SIGN((PI-ABS(PSIYT1)),PSIYT1)
      ELINTX=ELINT*CRAD
      AZINTX=AZINT*CRAD
      PSIUTX=PSIYT1*CRAD
      THTUTX=THTYT1*CRAD
C
C*** MISS DISTANCE IN PLANE OF CLOSEST APPROACH
C
      CALL MATMUL(VBT1U,TUL,VBT1L,3,3,1)
      CALL MATPOL(DVBT1,PSIZU,THTZU,VBT1U)
      CALL MAT2TR(TZU,PSIZU,THTZU)
      CALL MATMUL(TZL,TZU,TUL,3,3,3)
      CALL MATMUL(SHJZ,TZL,SBTL,3,3,1)
      YSS=-SHJZ(2)
      ZSS=-SHJZ(3)
C
      CALL MAT2TR(TYT1,PSIYT1,THTYT1)
      CALL MATMUL(TYL,TYT1,TT1L,3,3,3)
      CALL MATMUL(SHJY,TYL,SBTL,3,3,1)
      DYRB=-SHJY(2)
      DZRB=-SHJY(3)
C
      WRITE(6,1)
 1    FORMAT(' *** SHAZAM COMPATIBLE OUTPUT ***')
      WRITE(6,2)DVT1E,HBE,PSIUTX,THTUTX
 2    FORMAT('   DVTE=',1PG12.5,'    HBE=',1PG12.5,' PSIUTX='
     +,1PG12.5,' THTUTX=',1PG12.5)
      WRITE(6,3)YSS,ZSS,ASPAZX,ASPELX
 3    FORMAT('    YSS=',1PG12.5,'    ZSS=',1PG12.5,' ASPAZX='
     +,1PG12.5,' ASPELX=',1PG12.5)
      WRITE(6,4)DYRB,DZRB,AZINTX,ELINTX
 4    FORMAT('   DYRB=',1PG12.5,'   DZRB=',1PG12.5,' AZINTX='
     +,1PG12.5,' ELINTX=',1PG12.5)
      RETURN
      END
C***********************************************************************
      SUBROUTINE S2
C***********************************************************************
C***  * Acquisition Radar Module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Measures target position and velocity wrt
C***  *     the shooter aircraft in local coordinates
C***  *     (at measurement epoch)
C***  * (2) Sends target update to the guidance processor
C***  *     (at update epoch)
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  * 980212 Removed MNAV=0 statement (Set to zero in C1), PZi
C***  *
C***  ******************************************************************
C
C*** COMMON LOCATIONS ARE 500-599
C
      COMMON C(3510)
C
      DIMENSION VT1EL(3),VT2EL(3),ST2T1L(3),ST1CEL(3),
     +VT1CEL(3),ST2EL(3),EVT1EL(3),ST1EL(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0501),NTAG)
      EQUIVALENCE (C(0502),DTIMTU)
      EQUIVALENCE (C(0503),DTIMUP)
      EQUIVALENCE (C(0504),BIASTD)
      EQUIVALENCE (C(0505),RANDTD)
      EQUIVALENCE (C(0506),BIASTA)
      EQUIVALENCE (C(0507),RANDTA)
      EQUIVALENCE (C(0508),BIASTE)
      EQUIVALENCE (C(0509),RANDTE)
      EQUIVALENCE (C(0517),EVT1EL(1))
C
C NTAG = D/G =0: No AI update, =1: Start AI update, number of updates
C DTIMTU = D Time duration to determine target position - s
C DTIMUP = D Time interval between updates - s
C BIASTD = D Range to target measuremet bias - m
C RANDTD = D Range to target measuremet noise - m
C BIASTA = D Azimuth to target measurement bias - rad
C RANDTA = D Azimuth to target measurement noise - rad
C BIASTE = D Elevation to target measurement bias - rad
C RANDTE = D Elevation to target measurement noise - rad
C EVT1EL(3) = D AI velocity error - m/s
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(0169),VT2EL(1))
      EQUIVALENCE (C(0175),ST2EL(1))
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(2000),T)
C
C VT1EL= S Velocity of T1 wrt earth in local level coord - m/s
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C VT2EL= S Velocity of T2 wrt earth in local level coord - m/s
C ST2EL= S Position of T2 wrt earthref point E in L coor - m
C MGUID= D/G =0:None, =2:LAG, =3:Pro-Nav, =6:Aug Pro-Nav
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0500),MNAV)
      EQUIVALENCE (C(0511),ST1CEL(1))
      EQUIVALENCE (C(0514),VT1CEL(1))
C
C MNAV = O =0: Reset, =1: Tracking, =2: Measurement, =3:Update
C ST1CEL(3) = O Target position measured by AI radar - m
C VT1CEL(3) = O Target velocity measured by AI radar - m/s
C
C*** ENTER AI RADAR ONLY IF MISSILE IS NOT IN TERMINAL GUIDANCE
C
      IF(MGUID.EQ.6) RETURN
C
C*** SHOOTER WRT TARGET GEOMETRY (UNCORRUPTED)
C
      CALL MATSUB(ST2T1L,ST2EL,ST1EL,3,1)
      CALL MATPOL(DT2T1,AZT2T1,ELT2T1,ST2T1L)
C
C*** RESET NAVIGATION MODE
C
C12FEB98      MNAV=0
C
      IF(NTAG.EQ.0) RETURN
C
C*** RADAR IN TRACKING MODE
C
      IF(NTAG.EQ.1) THEN
C
C***    RADAR BEGINS TRACKING
C
         ISET1=0
         EPCHTA=T
C
      END IF
C
      IF(T.GE.EPCHTA.AND.ISET1.EQ.0) THEN
C
C***    TARGET MEASUREMENT EPOCH
C
         MNAV=2
         ISET1=1
         ISET2=0
         NTAG=NTAG+1
         EPCHUP=T+DTIMTU
C
C***    TARGET MEASUREMENT
C
         DT2T1R=DT2T1+BIASTD+RANDTD
         AZT2TR=AZT2T1+BIASTA+RANDTA
         ELT2TR=+ELT2T1+BIASTE+RANDTE
C
         CALL MATCAR(ST2T1L,DT2T1R,AZT2TR,ELT2TR)
C
         CALL MATSUB(ST1CEL,ST2EL,ST2T1L,3,1)
         CALL MATADD(VT1CEL,VT1EL,EVT1EL,3,1)
      END IF
C
      IF(T.GE.EPCHUP.AND.ISET2.EQ.0) THEN
C
C***    UPDATE EPOCH
C
         MNAV=3
         ISET1=0
         ISET2=1
         EPCHTA=T+DTIMUP-DTIMTU
C
      END IF
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE S4I
C***********************************************************************
C***  *  Initialization of INS Module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Identifies variables for integration
C***  * (2) Initializes INS error states using the Cholesky method
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  * 980923 Modified integration to conform to CHKINT, PZi
C***  *
C***  ******************************************************************
C
C*** COMMON LOCATIONS ARE 700-799
C
      COMMON C(3510)
C
      DIMENSION IPL(100),VBELC(3),ST1EL(3),SBEL(3)
     +,VBEL(3),IPLV(100),EVBE(3),ESTTC(3),SBELC(3)
     +,RECE(3),PP0(9,9),APP0(9,9),GAUSS(9)
     +,XX0(9),DUM9(9)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0700),MINS)
      EQUIVALENCE (C(0701),FRAX)
C
C MINS = D INS mode. =0:ideal INS; =1:with INS errors
C FRAX = D Fractn to modify init INS err state: XXO=XXO(1+FRAX)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(1619),SBEL(1))
      EQUIVALENCE (C(1633),VBEL(1))
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C SBEL= S Vehicle pos. wrt point E in local level axes - m
C VBEL= O Vehicle velocity in local level axes - m/s
C NIP= E The number of variables being integrated
C IPL= E The location of the state variable derivative
C IPLV= E The location of the sate variable
C
C*** INITIALIZATION OUTPUT
C
      EQUIVALENCE (C(0703),VBELC(1))
      EQUIVALENCE (C(0740),RECE(1))
      EQUIVALENCE (C(0746),EVBE(1))
      EQUIVALENCE (C(0752),ESTTC(1))
      EQUIVALENCE (C(0776),SBELC(1))
C
C VBELC(3) = O Computed body vel in earth coor - m/s
C RECE(3) = O INS tilt errors - rad
C EVBE(3) = O INS vel error - m/s
C ESTTC(3) = O INS pos error - m
C SBELC(3) = O Computed pos of body wrt earth reference point - m
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
          IPL(NIP)=ILOC+I
	    IPLV(NIP)=ILOC+I+3
          NIP=NIP+1
       ENDDO
C
       ILOC=743
       DO I=0,2
          IPL(NIP)=ILOC+I
	    IPLV(NIP)=ILOC+I+3
          NIP=NIP+1
       ENDDO
C
       ILOC=749
       DO I=0,2
          IPL(NIP)=ILOC+I
	    IPLV(NIP)=ILOC+I+3
          NIP=NIP+1
       ENDDO
C
      IF(MINS.EQ.0) THEN
C
C***     OUTPUT WITHOUT INS ERRORS
C
         CALL MATEQL(SBELC,SBEL,3,1)
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
      ENDIF
C
      RETURN
      END
C***********************************************************************
      SUBROUTINE S4
C***********************************************************************
C***  * INS Module
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) MINS = 0: Perfect INS
C***  *          = 1: Transfer alignment, gyro, accel, and INS errors
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  *
C***  ******************************************************************
C
C*** COMMON LOCATIONS ARE 700-799
C
      COMMON C(3510)
C
      DIMENSION FSPLC(3),FSPCB(3),FSPB(3),TLB(3,3),EWBEL(3),EWBEB(3)
     +,RECED(3),RECE(3),RERE(3,3),DUM2(3,3),TLLC(3,3)
     +,TBL(3,3),TBLC(3,3),TLCB(3,3),VBEL(3),VBELC(3),EF(3)
     +,DUM3(3),WBEB(3),WBECB(3),EFSPB(3),EFSPLC(3),EVBED(3)
     +,ESTTC(3),ESTTCD(3),EVBE(3)
     +,SBELC(3),ST1EL(3),SBEL(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0700),MINS)
      EQUIVALENCE (C(0727),TANLAT)
C
C MINS = D INS mode=0, No INS; =1, INS only
C TANLAT = I Tangent of latitude angle of body at release - None
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0051),REARTH)
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(1630),FSPB(1))
      EQUIVALENCE (C(1619),SBEL(1))
      EQUIVALENCE (C(1633),VBEL(1))
      EQUIVALENCE (C(1636),DVBE)
      EQUIVALENCE (C(0320),TBL(1,1))
      EQUIVALENCE (C(0348),TLB(1,1))
      EQUIVALENCE (C(1713),WBEB(1))
      EQUIVALENCE (C(2000),T)
C
C REARTH= E Radius of Earth - m
C CRAD= E Conversion from radians to degree (57.29577951)
C AGRAV= E Gavity acceleration (9.806635 m/s^2)
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C FSPB= O Specific force in body axes - m/s^2
C SBEL= S Vehicle pos. wrt point E in local level axes - m
C VBEL= O Vehicle velocity in local level axes - m/s
C DVBE= I/O Vehicle speed - m/s
C TBL= O Transf matrix of body wrt local level axes
C TLB= G T.M. of local level wrt body axes
C WBEB= G Angular vel of veh wrt earth in body axes - rad/s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0703),VBELC(1))
      EQUIVALENCE (C(0706),WBECB(1))
      EQUIVALENCE (C(0715),TBLC(1,1))
	EQUIVALENCE (C(0728),THTBLC)
	EQUIVALENCE (C(0729),THTBLCX)
      EQUIVALENCE (C(0730),DVBEC)
      EQUIVALENCE (C(0731),THTVLC)
      EQUIVALENCE (C(0732),THTVLCX)
      EQUIVALENCE (C(0733),PSIVLCX)
      EQUIVALENCE (C(0734),FSPCB(1))
      EQUIVALENCE (C(0776),SBELC(1))
C
C VBELC(3) = O Computed vel of body in earth coor - m/s
C WBECB(3) = O Computed ang vel of body wrt earth - rad/s
C TBLC(3,3) = O Comp trans matrix of body wrt earth coor - None
C DVBEC = O Computed body speed wrt earth - m/s
C THTBLC = O Computed pitch angle - rad
C THTBLCX = O Computed pitch angle - deg
C THTVLC = O Computed vertical flight path angle - rad
C THTVLCX = O Computed vertical flight path angle - deg
C PSIVLCX = O Computed heading angle - deg
C FSPCB(3) = O Computed specific force on body - N/kg
C SBELC(3) = O Computed pos of missile wrt earth ref point - m
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
C EVBE(3) = S INS velocity errors - m/s
C ESTTC(3) = S INS position errors - m
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0724),EWBEB(1))
      EQUIVALENCE (C(0773),EFSPB(1))
C
C EWBEB(3) = G Error in angular vel of body wrt earth - rad/s
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
         CALL MATEQL(VBELC,VBEL,3,1)
         DVBEC=DVBE
C
      ELSE
C
C***     OUTPUT WITH INS ERRORS
C
C***     CALL INSTRUMENT MEASUREMENTS
C
         CALL S4ACCL(FSPCB,EFSPB)
         CALL S4GYRO(EWBEB)
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
         CALL MATMUL(TBLC,TBL,TLLC,3,3,3)
C
C***     VELOCITY AND POSITION ERROR EQUATIONS
C
         CALL MATTRA(TLCB,TBLC,3,3)
         CALL MATMUL(FSPLC,TLCB,FSPCB,3,3,1)
         CALL MATMUL(DUM3,RERE,FSPLC,3,3,1)
         CALL MATMUL(EFSPLC,TLCB,EFSPB,3,3,1)
         CALL MATSUB(EF,EFSPLC,DUM3,3,1)
C
         EVBED(1)=EF(1)
         EVBED(2)=EF(2)
         EVBED(3)=EF(3)+2.*AGRAV*ESTTC(3)/REARTH
         CALL MATEQL(ESTTCD,EVBE,3,1)
C
C***    POS OF MISSILE WRT EARTH REF POINT, SBELC.
C       SENT TO GUIDANCE MODULE C1.
C
         CALL MATADD(SBELC,ESTTC,SBEL,3,1)
C
C*** INS VELOCITY VECTORS
C
         CALL MATADD(VBELC,EVBE,VBEL,3,1)
         CALL MATABS(DVBEC,VBELC,3)
C
      ENDIF
C
C*** COMPUTED FLIGHT PATH ANGLES
C
      IF(VBELC(1).EQ.0..AND.VBELC(2).EQ.0.) THEN
         PSIVLC=0.
         THTVLC=0.
      ELSE
         PSIVLC=ATAN2(VBELC(2),VBELC(1))
         THTVLC=ATAN2(-VBELC(3),SQRT(VBELC(1)**2+VBELC(2)**2))
      ENDIF
      PSIVLCX=PSIVLC*CRAD
      THTVLCX=THTVLC*CRAD
C
C*** COMPUTED PITCH ANGLE
C
      IF(ABS(TBLC(1,3)).LT.1.) THTBLC=ASIN(-TBLC(1,3))
	THTVLCX=THTVLC*CRAD
C
      RETURN
      END
C******************************************************************
      SUBROUTINE S4GYRO(EWBEB)
C******************************************************************
C***  * Gyro subroutine
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Introduces gyro errors: Random walk
C***  *                             Gyro cluster misalignment
C***  *                             Scale factor error
C***  *                             Bias error
C***  *                             Mass unbalence
C***  * (2) Outputs gyro measurements of body angular velocities
C***  *
C***  * Argument Output:
C***  *
C***  *          EWBEB(3)=Three gyro errors - rad/s
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  *
C***  **************************************************************
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
      EQUIVALENCE (C(1713),WBEB(1))
      EQUIVALENCE (C(1630),FSPB(1))
      EQUIVALENCE (C(2664),DER)
      EQUIVALENCE (C(2000),T)
C
C WBEB= G Angular vel of veh wrt earth in body axes - rad/s
C FSPB= O Specific force in body axes - m/s^2
C DER= E Integration step size - s
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
C********************************************************************
      SUBROUTINE S4ACCL(FSPCB,EFSPB)
C******************************************************************
C***  * Accelerometer subroutine
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Introduces accelerometer errors: Bias
C***  *                                      Random bias
C***  *                                      Scale factor error
C***  *                                      Accelerometer cluster misalignment
C***  * (2) Outputs acceleration measurements in body axes
C***  *
C***  * Argument Output
C***  *
C***  *          FSPBCB(3)=Three accelerometer outputs, in body axes - m/s^2
C***  *          EFSPB(3)=Three accel output errors - m/s^2
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  *
C***  **************************************************************
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
C EWALKA(3) = D Accel random bias - m/s2
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1630),FSPB(1))
      EQUIVALENCE (C(2000),T)
C
C FSPB= O Specific force in body axes - m/s^2
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
C******************************************************************
      SUBROUTINE S4ALT
C******************************************************************
C***  * Altimeter subroutine
C***  *
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Intruduces altimeter errors: Bias
C***  *                                  Random
C***  * (2) Outputs altimeter measurement above sea level
C***  *
C***  * MODIFICATION HISTORY
C***  * 920128 Created by Peter Zipfel
C***  *
C***  *************************************************************
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
      EQUIVALENCE (C(1639),HBE)
C
C HBE= O Height above ground - m
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0702),HBEM)
      EQUIVALENCE (C(0714),EHBE)
C
C HBEM = O Computed body alt above SL - m
C EHBE = O Altimeter error - m
C
      EHBE=+BIASAL+RANDAL
      HBEM=HBE+EHBE
C
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
	SUBROUTINE D4I
	RETURN
	END
	SUBROUTINE D4
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
	SUBROUTINE S2I
	RETURN
	END
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
