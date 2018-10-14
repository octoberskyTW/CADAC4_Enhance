C**********************************************************************
C*********************** MODULES AND DUMMY FOR SRAAM5 *****************
C**********************************************************************
C*** *
C*** * Short range air-to-air missile program
C*** * Member of CADAC4-Family of pseudo-5 DoF programs
C*** *
C*** * Calling Sequence:
C*** *  G1   TARGET
C*** *  G2   ENVIRON
C*** *  S1   SENSOR
C*** *  S2   AI RADAR
C*** *  S4   INS
C*** *  C1   GUIDANCE
C*** *  C2   AUTOPILOT
C*** *  A1   AERO
C*** *  A2   PROPULSION
C*** *  A3   FORCES
C*** *  D1   NEWTON
C*** *  D2   ROTATIONS
C*** *
C*** * MODIFICATION HISTORY
C*** * 000418 Version 1,0 Created by Peter Zipfel
C*** *
C**********************************************************************
C**********************************************************************
      SUBROUTINE A1I
C**********************************************************************
C*** * The initialization module for the Aerodynamic Module A1.
C*** * Reserved C(3510) locations are 1200-1299
C*** * This module performs the following functions:
C*** *
C*** * (1) Provides the aerodynamic tables of design SRAAM5
C*** *     Ref Area AREA= 0.0182 M^2, diameter = 0.1524 m
C*** *     Length = 2.95 m. Forward strakes and tail control
C*** * (2) Defines the run stopping conditions
C*** *
C*** * MODIFICATION HISTORY
C*** * 950718 Created by Tom O'Grady
C*** * 970415 New Tables for design SRAAM5 - Tom O'Grady
C*** *
C*** *****************************************************************
C
      COMMON C(3510)
C
      COMMON/AERO1/ATBLON,ATBLOF,TNTBL1,TNTBL2,ATBLSF,AMXTBL
C
      DIMENSION ATBLON(462),ATBLOF(154),TNTBL1(462),TNTBL2(154),
     & ATBLSF(616),AMXTBL(112)
C
C***  INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0053),OPTMET)
C
C***  INITIALIZATIONS
C
      EQUIVALENCE (C(1203),CNALP)
      EQUIVALENCE (C(1204),CYBET)
      EQUIVALENCE (C(1205),CMDEL)
      EQUIVALENCE (C(1210),PHIPP)
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1281),TMCODE)
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
C PHIPP = I/G Initialize aerodynamic roll angle to zero - rad
C TRCODE = I Termination code number
C TMCODE = I Dummy variable initialized to zero
C TRCVEL = D Minimum closing speed - ft/s
C TRMACH = D Minimum Mach number
C TRDYNM = D Minimum dynamic pressure - lbf/ft^2
C TRLOAD = D Minimum load capacity - g's
C TRALP = D Maximum total angle of attack - rad
C TRTHT = D Maximum pitch gimbal angle - rad
C TRTHTD = D Maximum pitch gimbal rate - rad/s
C TRPHID = D Maximum roll gimbal rate - rad/s
C TRATE = D Maximum tracking rate - rad/s
C
C*** TRIMMED AERODYNAMIC COEFFICIENTS IN WIND AXES
C
C    THE FOLLOWING REFERENCE DIMENSIONS WERE USED
C    LENGTH=116.35 IN, DIAMETER=6.0 IN, REFERENCE AREA=0.196 SQ FT
C    CG LOCATION AT 60.46 IN (LAUNCH); 48.05 IN (BURNOUT)
C
C    MACH NO ACROSS:.6 .85 .95 1.05 1.2 1.6 2 2.4 2.8 3.2 3.6 4 4.5 5
C    ANGLE OF ATTACK DOWN: 0 3 6 10 15 20 25 30 35 40 45
C
C*** THRUST ON DRAG COEFFICIENT FOR BURNOUT CG (48.05 IN)
C
      DATA(ATBLON(N),N=1,154)/
     C 0.351,0.547,0.695,0.841,0.818,0.751,0.746,0.744,0.733,0.715,
     X 0.697,0.686,0.675,0.665,
     C 0.365,0.565,0.713,0.860,0.837,0.769,0.763,0.760,0.747,0.728,
     X 0.710,0.698,0.687,0.678,
     C 0.423,0.626,0.773,0.921,0.899,0.822,0.813,0.806,0.794,0.774,
     X 0.756,0.756,0.747,0.739,
     C 0.573,0.748,0.895,1.048,1.035,0.976,0.960,1.010,1.016,0.999,
     X 0.981,0.964,0.949,0.930,
     C 0.821,1.028,1.176,1.340,1.350,1.541,1.598,1.604,1.574,1.526,
     X 1.464,1.421,1.385,1.355,
     C 1.225,1.434,1.600,1.822,2.363,2.632,2.600,2.537,2.410,2.312,
     X 2.231,2.171,2.122,2.102,
     C 1.702,1.998,2.874,3.479,3.900,4.158,3.967,3.715,3.589,3.455,
     X 3.363,3.321,3.307,3.304,
     C 2.221,3.818,4.401,5.350,5.827,6.128,5.563,5.299,5.139,5.018,
     X 5.005,4.999,5.012,5.032,
     C 2.786,5.631,6.263,7.538,7.896,8.171,7.610,7.268,7.137,7.094,
     X 7.026,6.963,6.927,6.927,
     C 3.760,7.647,8.377,9.781,10.31,10.60,9.974,9.576,9.409,9.373,
     X 9.370,9.341,9.305,9.242,
     C 7.527,10.09,10.58,12.36,12.81,13.51,12.49,12.01,11.90,11.90,
     X 11.90,11.90,11.90,11.90/
C
C*** THRUST ON DRAG COEFFICIENT FOR MIDBURN CG (53.61 IN)
C
 
      DATA(ATBLON(N),N=155,308)/
     C 0.351,0.550,0.698,0.845,0.821,0.750,0.746,0.744,0.733,0.715,
     X 0.697,0.685,0.675,0.665,
     C 0.367,0.567,0.715,0.864,0.841,0.772,0.766,0.762,0.751,0.732,
     X 0.714,0.702,0.691,0.681,
     C 0.429,0.636,0.782,0.927,0.905,0.835,0.826,0.819,0.807,0.787,
     X 0.770,0.771,0.764,0.757,
     C 0.591,0.772,0.917,1.066,1.055,1.013,0.998,1.054,1.061,1.044,
     X 1.025,1.007,0.992,0.974,
     C 0.866,1.083,1.227,1.389,1.403,1.641,1.702,1.705,1.674,1.618,
     X 1.556,1.511,1.475,1.446,
     C 1.311,1.532,1.696,1.911,2.473,2.817,2.771,2.697,2.569,2.456,
     X 2.369,2.302,2.248,2.223,
     C 1.847,2.149,3.044,3.636,4.064,4.420,4.194,3.927,3.796,3.635,
     X 3.527,3.459,3.419,3.395,
     C 2.428,4.053,4.627,5.557,6.023,6.446,5.838,5.530,5.354,5.189,
     X 5.125,5.059,5.013,4.982,
     C 3.048,5.902,6.520,7.564,8.096,8.527,7.883,7.460,7.278,7.184,
     X 7.124,7.059,7.015,6.991,
     C 4.066,7.899,8.596,9.775,10.41,10.92,10.14,9.697,9.633,9.546,
     X 9.486,9.419,9.379,9.363,
     C 7.747,10.09,10.58,12.05,12.63,13.57,12.68,12.29,12.28,12.21,
     X 12.16,12.10,12.07,12.06/
C
C*** THRUST ON DRAG COEFFICIENT FOR LAUNCH CG (60.46 IN)
C
      DATA(ATBLON(N),N=309,462)/
     C 0.351,0.549,0.697,0.846,0.821,0.749,0.746,0.744,0.733,0.715,
     X 0.698,0.686,0.675,0.665,
     C 0.369,0.570,0.717,0.864,0.842,0.778,0.773,0.770,0.758,0.739,
     X 0.721,0.710,0.699,0.689,
     C 0.442,0.651,0.797,0.944,0.921,0.859,0.851,0.847,0.837,0.816,
     X 0.801,0.809,0.804,0.799,
     C 0.625,0.809,0.953,1.100,1.093,1.086,1.076,1.157,1.178,1.164,
     X 1.151,1.134,1.122,1.104,
     C 0.947,1.180,1.323,1.475,1.500,1.870,1.966,1.992,1.978,1.921,
     X 1.857,1.809,1.772,1.741,
     C 1.502,1.716,1.874,2.075,2.718,3.308,3.291,3.242,3.117,2.990,
     X 2.900,2.829,2.773,2.749,
     C 2.173,2.443,3.424,4.015,4.511,5.240,5.025,4.761,4.656,4.467,
     X 4.349,4.276,4.228,4.199,
     C 2.858,4.625,5.226,6.150,6.699,7.638,6.998,6.688,6.547,6.366,
     X 6.314,6.236,6.175,6.135,
     C 3.564,6.720,7.352,8.578,8.996,10.05,9.385,8.340,8.806,8.707,
     X 8.667,8.597,8.547,8.522,
     C 4.730,8.941,9.655,10.98,11.54,12.75,11.93,11.47,11.47,11.38,
     X 11.34,11.26,11.22,11.21,
     C 8.858,11.33,11.82,13.53,13.94,15.65,14.72,14.29,14.36,14.29,
     X 14.25,14.18,14.12,14.10/
C
C*** THRUST OFF DRAG COEFFICIENT FOR BURNOUT CG (48.05 IN)
C
      DATA(ATBLOF(N),N=1,154)/
     C 0.444,0.639,0.802,0.986,0.951,0.869,0.847,0.829,0.805,0.776,
     X 0.749,0.730,0.717,0.703,
     C 0.459,0.656,0.819,1.005,0.970,0.887,0.865,0.844,0.819,0.789,
     X 0.762,0.743,0.729,0.716,
     C 0.517,0.717,0.879,1.065,1.030,0.941,0.914,0.891,0.865,0.834,
     X 0.808,0.800,0.789,0.778,
     C 0.517,0.838,1.000,1.190,1.167,1.092,1.059,1.094,1.086,1.059,
     X 1.033,1.008,0.990,0.968,
     C 0.912,1.116,1.279,1.480,1.480,1.655,1.696,1.685,1.644,1.584,
     X 1.514,1.464,1.426,1.392,
     C 1.313,1.521,1.701,1.958,2.489,2.744,2.696,2.616,2.477,2.369,
     X 2.280,2.213,2.161,2.138,
     C 1.786,2.080,2.970,3.609,4.020,4.266,4.058,3.792,3.653,3.509,
     X 3.409,3.361,3.345,3.338,
     C 2.301,3.897,4.491,5.474,5.941,6.231,5.650,5.372,5.202,5.071,
     X 5.050,5.038,5.048,5.065,
     C 2.863,5.704,6.350,7.656,8.004,8.269,7.692,7.337,7.195,7.214,
     X 7.254,7.318,7.396,7.478,
     C 3.830,7.718,8.459,9.891,10.41,10.69,10.08,9.818,9.720,9.720,
     X 9.720,9.720,9.720,9.720,
     C 7.593,9.846,10.65,12.44,12.91,13.57,12.80,12.33,12.21,12.20,
     X 12.20,12.20,12.20,12.20/
C
C*** THRUST ON LIFT COEFFICIENT FOR BURNOUT CG (48.05 IN)
C
      DATA(TNTBL1(N),N=1,154)/
     C 0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.429,0.427,0.426,0.432,0.455,0.451,0.391,0.335,0.299,0.271,
     X 0.254,0.239,0.225,0.216,
     C 0.947,0.905,0.896,0.903,0.920,0.869,0.759,0.677,0.628,0.582,
     X 0.569,0.669,0.681,0.691,
     C 1.602,1.370,1.360,1.371,1.446,1.476,1.313,1.587,1.649,1.634,
     X 1.617,1.569,1.528,1.465,
     C 2.118,2.065,2.059,2.096,2.231,3.161,3.287,3.255,3.140,2.985,
     X 2.807,2.677,2.566,2.475,
     C 2.854,2.811,2.854,3.020,4.524,5.365,5.145,4.897,4.540,4.265,
     X 4.063,3.905,3.771,3.708,
     C 3.496,3.632,5.174,6.034,6.969,7.391,6.820,6.227,5.920,5.589,
     X 5.374,5.238,5.144,5.080,
     C 3.997,6.418,7.118,8.340,9.800,9.188,8.082,7.503,7.162,6.853,
     X 6.715,6.572,6.450,6.358,
     C 4.433,8.290,8.921,10.34,10.92,10.20,9.189,8.514,8.183,7.930,
     X 7.751,7.553,7.384,7.251,
     C 5.320,9.802,10.41,11.72,12.40,10.94,9.791,9.027,8.800,8.609,
     X 8.454,8.376,8.299,8.263,
     C 8.867,10.86,11.20,12.60,13.06,11.79,10.75,10.25,9.983,9.869,
     X 9.642,9.565,9.529,9.487/
C
C*** THRUST ON LIFT COEFFICIENT FOR MIDBURN CG (53.61 IN)
C
      DATA(TNTBL1(N),N=155,308)/
     C 0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.475,0.476,0.475,0.482,0.506,0.501,0.434,0.373,0.333,0.302,
     X 0.283,0.266,0.252,0.241,
     C 1.051,1.007,0.998,1.007,1.023,0.965,0.844,0.753,0.700,0.648,
     X 0.634,0.744,0.758,0.768,
     C 1.778,1.524,1.517,1.531,1.610,1.637,1.456,1.759,1.828,1.811,
     X 1.792,1.739,1.694,1.624,
     C 2.352,2.297,2.296,2.341,2.483,3.497,3.637,3.602,3.476,3.306,
     X 3.109,2.965,2.842,2.741,
     C 3.171,3.127,3.183,3.375,5.037,5.940,5.701,5.431,5.034,4.734,
     X 4.510,4.337,4.189,4.121,
     C 3.881,4.042,5.782,6.765,7.795,8.217,7.595,6.938,6.597,6.239,
     X 6.006,5.866,5.776,5.715,
     C 4.433,7.192,8.006,9.421,11.38,10.28,9.070,8.443,8.070,7.749,
     X 7.626,7.504,7.406,7.334,
     C 4.922,9.390,10.15,11.82,12.47,11.57,10.47,9.763,9.427,9.237,
     X 9.119,9.001,8.910,8.846,
     C 5.956,11.30,12.07,13.67,14.45,12.59,11.47,10.82,10.65,10.46,
     X 10.34,10.22,10.12,10.06,
     C 10.38,13.07,13.49,15.36,15.90,13.25,12.15,11.59,11.48,11.30,
     X 11.17,11.03,10.92,10.84/
C
C*** THRUST ON LIFT COEFFICIENT FOR LAUNCH CG (60.46 IN)
C
      DATA(TNTBL1(N),N=309,462)/
     C 0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.550,0.551,0.551,0.560,0.586,0.579,0.502,0.432,0.387,0.351,
     X 0.329,0.310,0.293,0.281,
     C 1.213,1.165,1.158,1.170,1.184,1.113,0.974,0.871,0.809,0.751,
     X 0.734,0.859,0.874,0.886,
     C 2.053,1.766,1.761,1.780,1.864,1.885,1.676,2.020,2.097,2.077,
     X 2.054,1.993,1.941,1.860,
     C 2.716,2.657,2.663,2.721,2.871,4.000,4.151,4.107,3.958,3.762,
     X 3.536,3.371,3.229,3.113,
     C 3.646,3.610,3.686,3.919,5.808,6.759,6.471,6.151,5.692,5.348,
     X 5.090,4.891,4.720,4.641,
     C 4.436,4.658,6.681,7.845,8.989,9.306,8.572,7.805,7.397,6.992,
     X 6.723,6.562,6.461,6.392,
     C 5.056,8.285,9.256,10.95,12.00,11.59,10.18,9.442,8.983,8.613,
     X 8.463,8.325,8.219,8.142,
     C 5.613,10.84,11.76,13.81,14.46,13.01,11.71,10.87,10.44,10.22,
     X 10.07,9.932,9.830,9.776,
     C 6.797,13.10,14.06,16.07,16.86,14.17,12.82,12.05,11.80,11.58,
     X 11.42,11.28,11.17,11.10,
     C 11.97,15.28,15.85,18.24,18.72,15.00,13.66,13.01,12.81,12.60,
     X 12.44,12.29,12.18,12.10/
C
C*** THRUST OFF LIFT COEFFICIENT FOR BURNOUT CG (48.05 IN)
C
      DATA(TNTBL2(N),N=1,154)/
     C 0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.425,0.423,0.421,0.425,0.448,0.445,0.385,0.331,0.295,0.267,
     X 0.251,0.236,0.223,0.214,
     C 0.937,0.896,0.885,0.888,0.906,0.856,0.748,0.668,0.621,0.576,
     X 0.564,0.664,0.677,0.687,
     C 1.586,1.354,1.342,1.346,1.423,1.455,1.295,1.572,1.637,1.624,
     X 1.608,1.561,1.521,1.458,
     C 2.093,2.041,2.032,2.059,2.196,3.130,3.261,3.233,3.121,2.969,
     X 2.794,2.665,2.555,2.465,
     C 2.822,2.780,2.818,2.971,4.479,5.324,5.110,4.868,4.516,4.244,
     X 4.045,3.890,3.756,3.694,
     C 3.457,3.593,5.129,5.974,6.913,7.340,6.778,6.191,5.890,5.564,
     X 5.352,5.219,5.127,5.063,
     C 3.950,6.373,7.065,8.269,9.756,9.109,8.032,7.460,7.126,6.822,
     X 6.689,6.550,6.430,6.339,
     C 4.378,8.239,8.860,10.26,10.84,10.13,9.131,8.465,8.142,7.895,
     X 7.721,7.527,7.361,7.229,
     C 5.261,9.742,10.34,11.62,12.29,10.78,9.700,8.992,8.662,8.550,
     X 8.404,8.326,8.249,8.213,
     C 8.800,10.80,11.12,12.49,12.98,11.74,10.70,10.20,9.933,9.819,
     X 9.592,9.515,9.479,9.437/
C
C*** DRAG CORRECTION AT  ALTITUDE= -100 FT
C
      DATA(ATBLSF(N),N=1,154)/
     C 0.002,0.002,0.002,0.001,0.001,0.001,0.001,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.002,0.002,0.002,0.001,0.001,0.001,0.001,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.002,0.002,0.002,0.001,0.001,0.001,0.001,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.002,0.002,0.001,0.001,0.001,0.001,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.002,0.002,0.001,0.001,0.001,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.001,0.001,0.001,0.001,0.000,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.001,0.001,0.001,0.001,0.000,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.001,0.001,0.001,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.001,0.001,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.001,0.001,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000,
     C 0.001,0.001,0.000,0.000,0.000,0.000,0.000,0.000,0.000,0.000,
     X 0.000,0.000,0.000,0.000/
C
C*** DRAG CORRECTION AT  ALTITUDE= 20 KFT
C
      DATA(ATBLSF(N),N=155,308)/
     C -.004,-.017,-.020,-.019,-.017,-.014,-.012,-.011,-.008,-.003,
     X 0.000,0.000,0.000,0.000,
     C -.001,-.017,-.020,-.019,-.017,-.013,-.011,-.009,-.006,-.002,
     X 0.001,0.001,0.000,0.000,
     C 0.009,-.013,-.016,-.015,-.011,-.004,-.005,-.011,-.009,-.006,
     X -.004,0.000,0.000,0.000,
     C 0.050,0.040,0.035,0.038,0.029,-.018,-.023,-.011,-.008,-.004,
     X 0.000,0.000,0.000,0.000,
     C 0.194,0.072,0.011,-.028,-.042,-.013,-.012,-.011,-.008,-.004,
     X 0.000,0.000,0.000,0.000,
     C 0.275,-.040,-.070,-.117,-.015,-.013,-.011,-.011,-.009,-.005,
     X -.001,0.001,0.000,0.000,
     C 0.055,-.162,-.017,-.017,-.015,-.013,-.011,-.009,-.009,-.006,
     X -.002,0.000,0.000,0.000,
     C -.110,-.013,-.015,-.015,-.014,-.013,-.011,-.009,-.009,-.006,
     X -.003,0.000,0.000,0.000,
     C -.243,-.013,-.014,-.014,-.012,-.012,-.010,-.009,-.008,-.006,
     X -.004,-.001,0.001,0.001,
     C -.843,-.012,-.013,-.013,-.011,-.011,-.009,-.009,-.008,-.007,
     X -.004,-.003,0.000,0.001,
     C 0.004,-.012,-.011,-.012,-.010,-.010,-.009,-.007,-.007,-.006,
     X -.005,-.004,-.002,0.000/
C
C*** DRAG CORRECTION AT  ALTITUDE= 40 KFT
C
      DATA(ATBLSF(N),N=309,462)/
     C 0.014,-.009,-.017,-.020,-.022,-.022,-.019,-.015,-.009,-.005,
     X -.002,-.002,-.002,-.003,
     C 0.021,-.009,-.017,-.020,-.022,-.021,-.017,-.011,-.006,-.002,
     X 0.001,0.001,0.000,-.001,
     C 0.039,-.005,-.011,-.014,-.012,-.003,-.001,0.001,0.004,0.006,
     X 0.005,-.002,-.002,-.003,
     C 0.115,0.082,0.078,0.085,0.078,0.054,0.035,-.014,-.009,-.005,
     X -.001,-.001,-.002,-.003,
     C 0.365,0.231,0.211,0.225,0.191,-.021,-.018,-.014,-.009,-.005,
     X -.002,-.002,-.002,-.003,
     C 0.615,0.406,0.345,0.318,-.020,-.020,-.017,-.014,-.010,-.006,
     X -.002,-.001,-.001,-.002,
     C 0.913,0.490,-.014,-.018,-.019,-.019,-.017,-.013,-.010,-.006,
     X -.003,-.001,-.001,-.002,
     C 1.177,-.007,-.013,-.016,-.018,-.018,-.016,-.013,-.010,-.007,
     X -.004,-.001,-.001,-.002,
     C 1.380,-.007,-.012,-.015,-.016,-.016,-.015,-.012,-.010,-.006,
     X -.004,-.002,0.000,0.000,
     C 1.077,-.007,-.011,-.014,-.014,-.015,-.013,-.012,-.010,-.007,
     X -.004,-.003,0.000,0.000,
     C 0.004,-.006,-.010,-.012,-.013,-.013,-.012,-.010,-.009,-.007,
     X -.005,-.004,-.002,0.000/
C
C*** DRAG CORRECTION AT  ALTITUDE= 60 KFT
C
      DATA(ATBLSF(N),N=463,616)/
     C 0.032,0.003,-.008,-.013,-.015,-.014,-.012,-.009,-.006,-.002,
     X 0.000,-.001,-.002,-.004,
     C 0.040,0.002,-.008,-.013,-.015,-.013,-.012,-.006,-.002,0.000,
     X 0.003,0.002,0.001,0.000,
     C 0.063,0.006,-.003,-.006,-.005,0.005,0.010,0.013,0.016,0.019,
     X 0.019,-.001,-.002,-.003,
     C 0.149,0.092,0.087,0.100,0.101,0.103,0.098,-.009,-.006,-.002,
     X 0.001,-.001,-.002,-.003,
     C 0.398,0.294,0.295,0.344,0.343,-.013,-.011,-.009,-.005,-.003,
     X 0.000,-.001,-.002,-.003,
     C 0.732,0.646,0.639,0.713,-.014,-.012,-.010,-.009,-.006,-.003,
     X 0.000,0.000,-.001,-.002,
     C 1.261,1.076,-.007,-.011,-.014,-.012,-.010,-.008,-.006,-.003,
     X -.001,0.001,-.001,-.001,
     C 1.913,0.002,-.006,-.010,-.013,-.012,-.010,-.008,-.006,-.004,
     X -.001,0.001,0.000,-.001,
     C 2.681,0.001,-.006,-.009,-.011,-.010,-.009,-.008,-.006,-.003,
     X -.002,0.000,0.001,0.000,
     C 3.124,0.001,-.006,-.009,-.010,-.010,-.008,-.007,-.006,-.004,
     X -.002,0.000,0.001,0.001,
     C 0.025,0.001,-.004,-.008,-.009,-.008,-.007,-.006,-.005,-.004,
     X -.002,-.001,0.000,0.002/
C
C*** MAXIMUM TRIMMED ANGLE OF ATTACK
C
      DATA(AMXTBL(N),N=  1,112)/
     X 41.65,35.86,35.10,33.95,33.73,35.56,34.74,33.42,32.52,31.16,
     X 30.02,28.93,27.90,27.11,
     X 41.75,37.00,36.27,34.95,35.06,37.85,37.21,35.92,35.66,33.65,
     X 32.57,31.48,30.46,29.69,
     X 41.85,38.24,37.46,36.20,36.45,40.08,39.74,38.58,37.74,36.38,
     X 35.37,34.34,33.41,32.65,
     X 41.95,39.57,38.59,37.59,38.03,42.65,42.61,41.67,40.96,39.88,
     X 39.00,37.95,37.19,36.64,
     X 42.27,41.03,40.38,39.47,39.75,45.00,45.00,44.99,44.77,43.95,
     X 43.28,42.43,41.76,41.30,
     X 44.32,42.67,42.11,40.65,41.60,45.00,45.00,45.00,45.00,45.00,
     X 45.00,45.00,45.00,45.00,
     X 45.00,44.42,43.96,43.28,43.63,45.00,45.00,45.00,45.00,45.00,
     X 45.00,45.00,45.00,45.00,
     X 45.00,45.00,45.00,44.10,45.00,45.00,45.00,45.00,45.00,45.00,
     X 45.00,45.00,45.00,45.00/
C
C*** INITIALIZATION OF DERIVATIVES FOR AUTOPILOT
C
      CNALP=8.
      CYBET=-8.
      CMDELT=78.
C
C*** INITIALIZE COUNTERS
C
      PHIPP=0.0
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
C         1     TRCVEL     C1    MINIMUM CLOSING VELOCITY - FT/S
C         2     TRMACH     G2    MINIMUM MACH NUMBER
C         3     TRDYNM     G2    MINIMUM DYNAMIC PRESSURE - LBF/FT^2
C         4     TRLOAD     A1    MINIMUM LOAD FACTOR - G'S
C         5     TRALP      A1    MAXIMUM TOTAL INCIDENCE ANGLE - RAD
C         6     TRTHT      S1    MAXIMUM PITCH GIMBAL ANGLE - RAD
C         7     TRTHTD     S1    MAXIMUM PITCH GIMBAL RATE - RAD/S
C         8     TRPHID     S1    MAXIMUM ROLL GIMBAL RATE  - RAD/S
C         9     TRATE      S1    MAXIMUM TRACKING RATE - RAD/S
C
      TRCVEL=300.*(1.-.695*OPTMET)
      TRMACH=.8
      TRDYNM=200.*(1.+46.881*OPTMET)
      TRLOAD=3.
      TRALP=1.
      TRTHT=1.57
      TRPHID=14.
      TRTHTD=10
      TRATE=1.
C
      TRCODE=0.
      TMCODE=0.
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A1
C**********************************************************************
C*** * Aerodynamic Module
C*** * Reserved C(3510) locations are 1200-1299
C*** * This module performs the following functions:
C*** *
C*** * (1) Determines aerodynamic coefficients in flight path axes.
C*** * (2) Converts them into body axes
c*** * (3) Looks up max angle of attack
C*** * (4) Determines max g's permissible and g available
C*** * (5) Calculates lift and drag force
C*** *
C*** * MODIFICATION HISTORY
C*** * 950718 Created by Tom O'Grady
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      COMMON/AERO1/ATBLON,ATBLOF,TNTBL1,TNTBL2,ATBLSF,AMXTBL
C
      DIMENSION NXY(2),COFAR2(25),NXYZ(3),COFAR3(28),MXY(2),COFALP(22),
     1MXYZ(3),COFIR3(29),ATBLON(11,14,3),ATBLOF(11,14),TNTBL1(11,14,3),
     1TNTBL2(11,14),ATBLSF(11,14,4),AMXTBL(8,14)
C
C*** INITIALIZATIONS
C
      EQUIVALENCE (C(1210),PHIPP)
      EQUIVALENCE (C(1285),TRLOAD)
      EQUIVALENCE (C(1286),TRALP)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0053),OPTMET)
      EQUIVALENCE (C(0054),AGRAV)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(0206),VMACH)
      EQUIVALENCE (C(0908),ALPHA)
      EQUIVALENCE (C(0909),BETA)
      EQUIVALENCE (C(0910),ANPLIM)
      EQUIVALENCE (C(0979),MFREEZE)
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1330),AMASS)
      EQUIVALENCE (C(1332),XCGIN)
      EQUIVALENCE (C(1403),AREA)
      EQUIVALENCE (C(1615),HBE)
C
C PDYNMC= O Dynamic pressure - Pa
C VMACH= O Mach number of missile
C ALPHA= O Angle of attack - rad
C BETA= O Sideslip angle - rad
C ANPLIM= D Pos pitch accel limiter - g's
C MFREEZE= D =0:Unfreeze and print; =1:Freeze; increment for more
C MPROP= O Propulsion mode flag =0: Off, =1: On
C AMASS= O Mass of missile - kg
C XCGIN= O CG location aft of vehicle nose - in
C AREA= D Reference area for aerodynamic data - m^2
C HBE= O Height of vehicle above sea level - m
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1200),CA)
      EQUIVALENCE (C(1201),CN)
      EQUIVALENCE (C(1202),CY)
      EQUIVALENCE (C(1203),CNALP)
      EQUIVALENCE (C(1204),CYBET)
      EQUIVALENCE (C(1205),CMDEL)
      EQUIVALENCE (C(1267),GMAX)
      EQUIVALENCE (C(1280),TRCODE)
C
C CA = O Axial trimmed force coefficient - NA
C CN = O Normal trimed force coefficient - NA
C CY = O Side force coefficient - NA
C GMAX = O Maximum g's within permissable ALPMAX - g's
C TRCODE = O Code numbers for stopping conditions
C CNALP = O Normal force derivative - 1/rad
C CYBET = O Side force derivative (carry neg sign) - 1/rad
C CMDEL = O Control moment derivative at trim - 1/rad
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1221),CDT)
      EQUIVALENCE (C(1222),CSFC)
      EQUIVALENCE (C(1223),CD)
      EQUIVALENCE (C(1275),ALPMAX)
      EQUIVALENCE (C(1294),GAVAIL)
      EQUIVALENCE (C(1295),ALIFT)
      EQUIVALENCE (C(1296),ADRAG)
C
C CDT = G Drag coeff without skin friction -
C CSFC = G Skin friction coefficient -
C CD = G Drag foefficient -
C ALPMAX = G Maximum permissible total angle of attack - deg
C GAVAIL = G Additional available g's for maneuvering - g's
C ALIFT = G Lift force - lbf
C ADRAG = G Drag force - lbf
C
      DATA COFAR2/.6,.85,.95,1.05,1.2,1.6,2.,2.4,2.8,3.2,3.6,4.,
     1 4.5,5.,0.,3.,6.,10.,15.,20.,25.,30.,35.,40.,45./
      DATA COFAR3/.6,.85,.95,1.05,1.2,1.6,2.,2.4,2.8,3.2,3.6,4.,
     1 4.5,5.,0.,3.,6.,10.,15.,20.,25.,30.,35.,40.,45.,
     1 48.05,53.61,60.46/
      DATA COFIR3/.6,.85,.95,1.05,1.2,1.6,2.,2.4,2.8,3.2,3.6,4.,
     1 4.5,5.,0.,3.,6.,10.,15.,20.,25.,30.,35.,40.,45.,
     1 -100., 20000., 40000., 60000./
      DATA COFALP/.6,.85,.95,1.05,1.2,1.6,2.,2.4,2.8,3.2,3.6,4.,
     1 4.5,5.,48.05,49.47,50.77,52.12,53.42,54.69,55.87,57.04/
      DATA NXY/14,11/
      DATA NXYZ/14,11,3/
      DATA MXY/14,8/
      DATA MXYZ/14,11,4/
C
C*** CONVERT TO ROLL COORD
C
      ALPHAP=ACOS(COS(ALPHA)*COS(BETA))
C
      IF(ABS(ALPHAP).GT.TRALP) TRCODE=5.
C
      DUM1=TAN(BETA)
      DUM2=SIN(ALPHA)
      PHIPP=SIGN(1.570796,BETA)
      IF(ABS(DUM2).GT.1.E-10) PHIPP=ATAN2(DUM1,DUM2)
C
C*** TABLE ENTRIES
C
      ALPHAPX=ABS(ALPHAP*CRAD)
      HBESK=HBE*(1.+2.281*OPTMET)
C
      IF(XCGIN.GT.57.04) THEN
         ALPMAX=45.
      ELSE
         VMACHI=VMACH
         XCGINI=XCGIN
         CALL TABL2(VMACHI,XCGINI,COFALP,AMXTBL,MXY,ALPMAX)
      ENDIF
      IF(ALPMAX.GT.0..AND.ALPHAPX.GT.ALPMAX) ALPHAPX=ALPMAX
C
      VMACHI=VMACH
      ALPHAPXI=ALPHAPX
      HBESKI=HBESK
      CALL TABL3(VMACHI,ALPHAPXI,HBESKI,COFIR3,ATBLSF,MXYZ,CSFC)
C
      IF(MPROP.EQ.0) THEN
         VMACHI=VMACH
         ALPHAPXI=ALPHAPX
         CALL TABL2(VMACHI,ALPHAPXI,COFAR2,ATBLOF,NXY,CDT)
         CD=CDT+CSFC
      ELSE
         VMACHI=VMACH
         ALPHAPXI=ALPHAPX
         XCGINI=XCGIN
         CALL TABL3(VMACHI,ALPHAPXI,XCGINI,COFAR3,ATBLON,NXYZ,CDT)
         CD=CDT+CSFC
      ENDIF
C
      IF(MPROP.EQ.0) THEN
         VMACHI=VMACH
         ALPHAPXI=ALPHAPX
         CALL TABL2(VMACHI,ALPHAPXI,COFAR2,TNTBL2,NXY,CLT)
      ELSE
         VMACHI=VMACH
         ALPHAPXI=ALPHAPX
         XCGINI=XCGIN
         CALL TABL3(VMACHI,ALPHAPXI,XCGINI,COFAR3,TNTBL1,NXYZ,CLT)
      ENDIF
C
C*** AERO DERIVATIVES FOR AUTOPILOT. PARTICULAR TO SRAAM5 CONCEPT
C
      ALPX=ABS(CRAD*ALPHA)
      BETX=ABS(CRAD*BETA)
      IF(ALPX.LE.10.)THEN
         CNALP=CRAD*(0.123+0.013*ALPX)
      ELSE
         CNALP=CRAD*.06*ALPX**.625
      ENDIF
      IF(BETX.LE.10.)THEN
         CYBET=-CRAD*(0.123+0.013*BETX)
      ELSE
         CYBET=-CRAD*.06*BETX**.625
      ENDIF
C
C*** FORCE COEFFICIENTS IN BODY AXES
C
      CA=CD*COS(ALPHAPX/CRAD)-CLT*SIN(ALPHAPX/CRAD)
      CN=CD*SIN(ALPHAPX/CRAD)+CLT*COS(ALPHAPX/CRAD)
      IF(ALPHAP.LT.0.) CN=-CN
      CY=-CN*SIN(PHIPP)
      CN=CN*COS(PHIPP)
C
C***  CALCULATE MAX G AND LOAD FACTOR AVAILABLE
C
      IF(MPROP.EQ.0) THEN
         VMACHI=VMACH
         ALPMAXI=ALPMAX
         CALL TABL2(VMACHI,ALPMAXI,COFAR2,ATBLOF,NXY,CDMAX)
         CDMX=CDMAX+CSFC
      ELSE
         VMACHI=VMACH
         ALPMAXI=ALPMAX
         XCGINI=XCGIN
         CALL TABL3(VMACHI,ALPMAXI,XCGINI,COFAR3,ATBLOF,NXYZ,CDMAX)
         CDMX=CDMAX+CSFC
      ENDIF
      IF(MPROP.EQ.0) THEN
         VMACHI=VMACH
         ALPMAXI=ALPMAX
         CALL TABL2(VMACHI,ALPMAXI,COFAR2,TNTBL2,NXY,CLMX)
      ELSE
         VMACHI=VMACH
         ALPMAXI=ALPMAX
         XCGINI=XCGIN
         CALL TABL3(VMACHI,ALPMAXI,XCGINI,COFAR3,TNTBL1,NXYZ,CLMX)
      ENDIF
      CNMX=CDMX*SIN(ALPMAX/CRAD)+CLMX*COS(ALPMAX/CRAD)
C
      ALMX=CNMX*PDYNMC*AREA
      WEIGHT=AMASS*AGRAV
      GMAX=ALMX/WEIGHT
      IF(GMAX.LT.TRLOAD) TRCODE=4.
      IF(GMAX.GT.ANPLIM) GMAX=ANPLIM
      ALTOT=CLT*PDYNMC*AREA
      AFAC=ALTOT/WEIGHT
      GAVAIL=GMAX-AFAC
C
C***  AERO LIFT AND DRAG COMPUTATIONS
C
      ALIFT=CLT*PDYNMC*AREA*(1.+3.45*OPTMET)
      ADRAG=CD*PDYNMC*AREA*(1.+3.45*OPTMET)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE A2
C**********************************************************************
C*** * Propulsion Module A2.
C*** * Reserved C(3510) locations are 1300-1399
C*** * (English units (default) or SI units (OPTMET=1.))
C*** * Configuration: Boost rocket motor
C*** *                Initial weight of vehicle 202.73 lbf
C*** *                Initial cg location 5.038 ft from nose
C*** *
C*** * MPROP=0: Motor off
C*** *      =1: Motor on
C*** *
C*** * This module performs the following functions:
C*** *
C*** * 1) Calculates vehicle mass
C*** * 2) Calculates the rocket thrust at altitude
C*** * 3) Calculates vehicle CG location during motor burn
C*** *
C*** * MODIFICATION HISTORY
C*** * 950718 Created by Tom O'Grady
C*** * 970415 Modified for SRAAM5 design - Tom O'Grady
C*** *
C*** *****************************************************************
C
C*** COMMON LOCATIONS ARE 1300-1399
C
      COMMON C(3510)
C
      DIMENSION PROTIM(13),THRUST(13),WGT(13),CGLOC(13)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0053),OPTMET)
      EQUIVALENCE (C(2000),T)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0202),PRESS)
      EQUIVALENCE (C(0979),MFREEZE)
C
C PRESS= O Atmospheric pressure - Pa
C MFREEZE= D =0:Unfreeze and print; =1:Freeze; increment for more
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1330),AMASS)
      EQUIVALENCE (C(1332),XCGIN)
C
C MPROP = O Propulsion mode flag =0: Off, =1: On
C FTHALT = O Thrust at altitude - N
C AMASS = O Mass of missile - kg
C XCGIN = O CG location aft of vehicle nose - in
C
C*** ROCKET MOTOR DATA
C
C    THRUST in lbf, WGT in lb, CGLOC in inch
C
      DATA PROTIM/0.,.05,.269,.538,.807,1.076,1.345,
     A   1.614,1.883,2.152,2.421,2.690,6.000/
      DATA THRUST/0.,7141.,7747.,8083.,8109.,7970.,7796.,
     A   7630.,7463.,7272.,7031.,0.,0./
      DATA WGT/202.73,200.70,196.10,187.80,178.85,171.10,162.77,
     A   154.71,146.96,139.52,131.55,124.11,124.11/
      DATA CGLOC/60.46,60.39,59.32,58.18,57.04,55.87,54.69,
     A   53.42,52.12,50.77,49.47,48.05,48.05/
      DATA NX/13/
C
      AEXIT=0.1351*(1.-.9071*OPTMET)
C
C*** DETERMINE THRUST, MASS, AND CG LOCATION
C
      IF(T.LT.2.69)THEN
         MPROP=1
         CALL TABLE(T,PROTIM,THRUST,NX,FTHSL)
         CALL TABLE(T,PROTIM,WGT,NX,WEIGHT)
         CALL TABLE(T,PROTIM,CGLOC,NX,XCGIN)
         AMASS=WEIGHT*(1.+13.59*OPTMET)/32.174
         ALTCOR=((2116.+99208.*OPTMET)-PRESS)*AEXIT
         FTHALT=FTHSL*(1.+3.45*OPTMET)+ALTCOR
      ELSE
         MPROP=0
         FTHALT=0.
      ENDIF
C
      RETURN
      END
 
C**********************************************************************
      SUBROUTINE C1
C**********************************************************************
C*** * Guidance Module
C*** * Reserved C(3510) locations are 800-899
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) MGUID= 0: No Guidance
C*** *            3: Midcourse (Pro-Nav based on INS information)
C*** *               Set by input or in Module S1 if break lock occured
C*** *            6: Terminal (Pro-Nav based on LOS rates only)
C*** *               Set in Module S1 when MSEEK=4
C*** *
C*** * (2) Receives targeting data (MNAV=3) from the AI radar and
C*** *     extrapolates target postion to current time
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** * 950206 Change in switching logic (PZi)
C*** * 950720 Adapted for CADAC4 from CADAC6 (PZi)
C*** * 980212 Added MNAV=0 statement after update is received, PZi
C*** * 980625 RT-CADAC: EPCHTA equivalenced, PZi
C*** *
C*** ******************************************************************
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
C MGUID = D/G =0:No guidance, =3:Midcourse, =6:Terminal
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
C UT1BLC(3) = O LOS unit vector from INS data - none
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
C**********************************************************************
      SUBROUTINE C1MID(AAPNB,WOEL,DVBO,PSIOB,THTOB,TBL)
C**********************************************************************
C*** * Midcourse Guidance Law
C*** * Proportional Navigation using full state information
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Calculates acceleration commands from inertial LOS rates
C*** *     and closing speed
C*** *
C*** * Argument Output:
C*** *
C*** *          AAPNB(3)=Acceleration command - m/s^2
C*** *
C*** * Argument Input:
C*** *
C*** *          WOEL(3)=Inertial LOS rate - rad/s
C*** *          DVBO=Closing speed - m/s
C*** *          PSIOB=Yaw LOS angle - rad
C*** *          THTOB=Pitch LOS angle - rad
C*** *          TBL(3,3)=Transf matrix of body wrt local level coord
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** *
C*** ******************************************************************
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
 
C**********************************************************************
      SUBROUTINE C1TERM(ALCOM,ANCOM)
C**********************************************************************
C***  * Terminal Guidance Law
C***  * Proportional Navigation using LOS rate only
C***  * This subroutine performs the following functions:
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
C***  * 941122 Created by Peter Zipfel
C***  *
C***  *****************************************************************
C
      COMMON C(3510)
C
       DIMENSION FSPCB(3),VT1EL(3),ST1EL(3),SBEL(3),VBEL(3),SBT1L(3)
     &,VBT1L(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0801),GNAV)
C
C GNAV = D Navigation gain
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0119),VT1EL(1))
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(0447),THTPB)
      EQUIVALENCE (C(0448),PSIPB)
      EQUIVALENCE (C(0487),SIGDPY)
      EQUIVALENCE (C(0488),SIGDPZ)
      EQUIVALENCE (C(0734),FSPCB(1))
      EQUIVALENCE (C(1267),GMAX)
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1282),TRCVEL)
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1605),VBEL(1))
C
C AGRAV= E Gavity acceleration (9.806635 m/s^2)
C VT1EL= S Velocity of T1 wrt earth in local level coord - m/s
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C THTPB= O Pitch pointing angle - rad
C PSIPB= O Yaw pointing angle - rad
C SIGDPY= O Pitch sight line spin - rad/s
C SIGDPZ= O Yaw sight line spin - rad/s
C FSPCB= O Computed specific force on body - N/kg
C GMAX= O Maximum g's within permissable ALPMAX - g's
C TRCODE= O Code numbers for stopping conditions -
C TRCVEL= D Minimum closing speed - ft/s
C SBEL= O Position of vehicle wrt earth ref point E - m
C VBEL= O Velocity of vehicle wrt earth - m/s
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
C ALL = G Lateral accel comand, unlimited - g's
C ANN = G Normal accel command, unlimited - g's
C
      CALL MATSUB(SBT1L,SBEL,ST1EL,3,1)
      CALL MATABS(DBT1,SBT1L,3)
      CALL MATSUB(VBT1L,VBEL,VT1EL,3,1)
      CALL MATSCA(DUM,SBT1L,VBT1L,3)
      DCVEL=ABS(DUM/DBT1)
C
C*** TERMINATION CONDITION
C
      IF(DBT1.LE.1000.) THEN
         IF(DCVEL.LT.TRCVEL) TRCODE=1.
      ENDIF
C
C*** MISSILE ACCELERATION CORRECTION TERM
C
      ADELY=SIN(PSIPB)*FSPCB(1)
      ADELZ=SIN(THTPB)*COS(PSIPB)*FSPCB(1)
C
C*** ACCELERATION COMMANDS  ALONG BODY AXES IN M/S^2
C
      GN=GNAV*DCVEL
      APNY=GN*SIGDPZ
      APNZ=GN*SIGDPY
      CTHTHB=ABS(COS(THTPB)*COS(PSIPB))
      ALL=(APNY+ADELY)/(CTHTHB*AGRAV)
      ANN=(APNZ+ADELZ)/(CTHTHB*AGRAV)
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
 
C**********************************************************************
      SUBROUTINE C2I
C**********************************************************************
C*** * Autopilot Initialization Module
C*** * Reserved C(3510) locations are 900-999
C*** * This module performs the following functions:
C*** *
C*** * (1) Initializes the state variables
C*** *
C*** * MODIFICATION HISTORY
C*** * 850912 Created by Peter Zipfel
C*** * 920429 PI-Autopilot added (PZi)
C*** * 960208 Added MAUTA switch and old autopilot C2ACCP,C2ACCY (ABa)
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0900),MAUT)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1700),MTURN)
C
C MTURN= D =0:skid-to-turn, =1:bank-to-turn
C
C*** INITIALIZE STATE VARIABLES
C
         ILOC=942
         DO I=1,4
            IPL(NIP)=ILOC
            IPLV(NIP)=ILOC+1
            ILOC=ILOC+2
            NIP=NIP+1
         ENDDO
C
         ILOC=950
         DO I=1,2
            IPL(NIP)=ILOC
            IPLV(NIP)=ILOC+1
            ILOC=ILOC+2
            NIP=NIP+1
         ENDDO
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C2
C**********************************************************************
C*** * Autopilot Module
C*** * Reserved C(3510) locations are 900-999
C*** * This module performs the following functions:
C*** *
C*** * (1) Selects the autopilot modes
C*** *
C*** * MAUT=|MAUTA|MAUTL|MAUTP|
C*** *
C*** *       MAUTA=1 -not implemented-
C*** *       MAUTA=0 Use new autopilot
C*** *
C*** *                 SKID-TO-TURN (MTURN=0)
C*** *
C*** *             MAUTL=1  Beta hold
C*** *                  =4  Yaw acceleration hold
C*** *
C*** *                   MAUTP=1 Alpha hold
C*** *                        =4 Pitch acceleration hold
C*** *
C*** * (2) Limits the total angle of attack to ALPPLIM
C*** * (3) The baseline acceleration hold autopilot is a proportional loop
C*** *     with gain GR as input. To invoke the proportional-integral loop
C*** *     (P-I) TA and GACP must be provided. GR will be calculated
C*** *     internally.
C*** * (4) The rate loop (inner) has an aerodynamic and thrust vector branch. A
C*** *     apportions the contribution of TVC.  For APTVC=0 (no TVC, all aero
C*** *     control) the following data must be provided: CNALP, ALPPLIM, CMDEL,
C*** *     RLEN, GA, PARM, AIZ. With TVC (0<APTVC<1) the inputs TVCLIM and GP
C*** *     must be added.
C*** * (5) The rate loop can be simplified by only specifying the rate loop
C*** *     time constant TR > 0. Then all detailed rate loop calculations are
C*** *     replaced by a simple first order lag transfer function. If the P-I
C*** *     autopilot mode is selected the rate loop must be in this simplified
C*** *     form.
C*** * (6) Freezes flight conditions for autopilot response.
C*** *     Frozen variables are: DVBE, VMACH, PDYNMC, VMASS, THRUST,
C*** *     CNALP, CYBET, CMDELT.
C*** *     Invoked by setting  MFREEZE=1 and canceled by MFREEZE=0.
C*** *     Canceling Freeze will also cause print out of previous peak
C*** *     values and their times.
C*** *     For sequential maneuvers increment MFREEZE by one integer
C*** *     NOTE: Freeze capability only for skid-to-turn accel autopilot
C*** *
C*** * MODIFICATION HISTORY
C*** * 850912 Created by Peter Zipfel
C*** * 920429 PI-Autopilot added (PZi)
C*** * 950726 Added Freeze capability (PZi)
C*** * 960311 Added Third digit (MAUTA) to use old or new autopilot (GAB)
C*** * 980624 Renamed C2PITCH to C2PTCH, (for RT-CADAC), PZi
C*** * 980815 Removed unnecessary subroutines for RT-CADAC, PZi
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
      EQUIVALENCE (C(0902),THTVLC)
      EQUIVALENCE (C(0903),TTHT)
      EQUIVALENCE (C(0904),FACTTH)
      EQUIVALENCE (C(0905),GTHT)
      EQUIVALENCE (C(0906),BETAC)
      EQUIVALENCE (C(0907),ALPHAC)
      EQUIVALENCE (C(0910),ANPLIM)
      EQUIVALENCE (C(0911),ANNLIM)
      EQUIVALENCE (C(0915),ALLIM)
      EQUIVALENCE (C(0917),WPC)
      EQUIVALENCE (C(0919),GH)
      EQUIVALENCE (C(0920),GV)
      EQUIVALENCE (C(0921),HCOM)
      EQUIVALENCE (C(0924),PHIBVC)
      EQUIVALENCE (C(0962),HDTLIM)
      EQUIVALENCE (C(0963),PSIVLC)
      EQUIVALENCE (C(0964),TPSI)
      EQUIVALENCE (C(0965),FACTPS)
      EQUIVALENCE (C(0966),GPSI)
      EQUIVALENCE (C(0967),TA)
      EQUIVALENCE (C(0972),GACP)
      EQUIVALENCE (C(0973),FACTGACP)
      EQUIVALENCE (C(0974),FACTTR)
      EQUIVALENCE (C(0976),ALPPLIM)
      EQUIVALENCE (C(0979),MFREEZE)
      EQUIVALENCE (C(0995),GR)
      EQUIVALENCE (C(0997),TR)
C
C MAUT = D Two digit flag |lateral|pitch|; see table in C2 Module
C ALPHAC = D Commanded angle of attack; use with I1=1 - rad
C THTVLC = D Commanded flight path angle; use with I1=2 - rad
C TTHT = D Time const of flight path angle hold autop - s
C FACTTH = D Factor for lag time const shaping network
C GTHT = D Gain of flight path angle hold autop - 1/s
C BETAC = D Commmanded side slip angle; use with I2=1 - rad
C ANPLIM = D Pos pitch accel limiter - g's
C ANNLIM = D Neg pitch accel limiter - g's
C ALLIM = D Yaw accel limiter - g's
C WPC = D Commanded roll rate; use with I2=6 - rad/s
C GH = D Gain of altitude hold loop - g/m
C GV = D Gain of altitude rate loop - g/(m/s)
C HCOM = D Commanded altitude - m
C PHIBVC = D Commanded roll angle; use with I2=1; - rad
C HDTLIM = D Altitude rate limiter - m/s
C PSIVLC = D Commanded heading angle; use with I2=2 - rad
C TPSI = D Time const of lateral flt path angle hold autop - s
C FACTPS = D Factor for lag time const shaping network
C GPSI = D Gain of lateral flight path angle hold autop - 1/s
C ALPPLIM = D Total angle of attack limiter - rad
C MFREEZE = D =0:Unfreeze and print; =1:Freeze; increment for more
C TA = D Ratio of prop/integral gains. If > 0, P-I engaged.
C GACP = D Root locus gain of accel loop - rad/s2
C FACTGACP = D Factor to modify GACP - ND
C FACTTR = D Factor to modify TR - ND
C GR = D/G Rate gain (D: P-loop, G: P-I loop) (rad/s)/(m/s)
C TR = D/G Rate loop time const (if > 0: simple rate loop)
C
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(2001),TSTAGE)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(0702),HBEM)
      EQUIVALENCE (C(0703),VBELC(1))
      EQUIVALENCE (C(0734),FSPCB(1))
      EQUIVALENCE (C(0776),SBELC(1))
      EQUIVALENCE (C(0802),ANCOM)
      EQUIVALENCE (C(0803),ALCOM)
      EQUIVALENCE (C(1203),CNALP)
      EQUIVALENCE (C(1204),CYBET)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1330),AMASS)
      EQUIVALENCE (C(1403),AREA)
      EQUIVALENCE (C(1641),DVBE)
      EQUIVALENCE (C(1643),PSIVL)
      EQUIVALENCE (C(1645),THTVL)
      EQUIVALENCE (C(1700),MTURN)
C
C PDYNMC= O Dynamic pressure - Pa
C HBEM= O Computed body alt above SL - m
C VBELC= O Computed body vel in earth coor - m/s
C FSPCB= O Computed specific force on body - N/kg
C SBELC= O Computed pos of body wrt earth reference point - m
C ANCOM= O Normal acceleration command - g's
C ALCOM= O Lateral acceleration command - g's
C CNALP= O Normal force derivative - 1/rad
C CYBET= O Side force derivative (carry neg sign) - 1/rad
C FTHALT= O Thrust at altitude - N
C AMASS= O Mass of missile - kg
C AREA= D Reference area for aerodynamic data - m^2
C DVBE= S Speed of vehicle wrt earth - m/s
C PSIVL= S Horizontal flight path angle - rad
C THTVL= S Vertical flight path angle - rad
C MTURN= D =0:skid-to-turn, =1:bank-to-turn
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0908),ALPHA)
      EQUIVALENCE (C(0909),BETA)
      EQUIVALENCE (C(0980),ALPHAP)
      EQUIVALENCE (C(0981),PHIP)
      EQUIVALENCE (C(0926),PHIBV)
      EQUIVALENCE (C(0927),ALPHAX)
      EQUIVALENCE (C(0928),BETAX)
C
C ALPHA = O Angle of attack - rad
C BETA = O Sideslip angle - rad
C ALPHAP = O Total angle of attack - rad
C PHIP = O Aerodynamic roll angle - rad
C PHIBV = O Bank angle - rad
C ALPHAX = O Angle of attack - deg
C BETAX = O Sideslip angle - deg
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0950),XID)
      EQUIVALENCE (C(0951),XI)
      EQUIVALENCE (C(0952),YID)
      EQUIVALENCE (C(0953),YI)
C
C XI = S Integral feedback, pitch loop - rad/s
C YI = S Integral feedback, lateral loop - rad/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0918),ALPHAPX)
C
C ALPHAPX = G Total angle of attack -  deg
C
      MAUTA=INT(MAUT/100.)
      MAUTL=INT((MAUT-MAUTA*100)/10.)
      MAUTP=MAUT-MAUTA*100-MAUTL*10
C
C***************************************************
C***  SPECIFIC AUTOPILOT PARAMETERS FOR SRAAM5 CONCEPT
C
      TR=(-2.0E-7*PDYNMC+.22)*(1.+FACTTR)
      GACP=(2.0E-3*PDYNMC)**.575*(1.+FACTGACP)
C990505	TA=1.2
C
C*** END OF SPECIFIC AUTOPILOT PARAMETERS
C***************************************************
C
C
C*** ANGLE OF ATTACK HOLD
C
      IF(MAUTP.EQ.1) THEN
         ALPH=ALPHAC
      ENDIF
C
C*** PITCH ACCELERATION HOLD
C
      IF(MAUTP.EQ.4) THEN
         IF(MAUTA.EQ.0) THEN
            IF(ANCOM.GT.ANPLIM) ANCOM=ANPLIM
            IF(ANCOM.LT.ANNLIM) ANCOM=ANNLIM
            ABECZ=-ANCOM*AGRAV
            EP=(ABECZ-FSPCB(3))
            TIP=DVBE*AMASS/(PDYNMC*AREA*CNALP)
            IF(TA.GT.0.)THEN
               GR=GACP*TIP*TR/DVBE
               GI=GR/TA
               XID=GI*EP
               PITCH=-(EP*GR+XI)
            ELSE
               PITCH=-EP*GR
            ENDIF
            CALL C2PTCH(ALPH,PITCH)
         ENDIF
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
            BETT=BETAC
         ENDIF
C
C***     YAW ACCELERATION HOLD
C
         IF(MAUTL.EQ.4) THEN
            IF(MAUTA.EQ.0) THEN
               IF(ALCOM.GT.ALLIM) ALCOM=ALLIM
               IF(ALCOM.LT.-ALLIM) ALCOM=-ALLIM
               ABECY=ALCOM*AGRAV
               EY=(ABECY-FSPCB(2))
               TIY=DVBE*AMASS/(-PDYNMC*AREA*CYBET)
               IF(TA.GT.0.)THEN
                  GR=GACP*TIY*TR/DVBE
                  GI=GR/TA
                  YID=GI*EY
                  YAW=(EY*GR+YI)
               ELSE
                  YAW=EY*GR
               ENDIF
               CALL C2YAW(BETT,YAW)
            ENDIF
         ENDIF
      ENDIF
C
C*** TOTAL ANGLE OF ATTACK LIMITER
C
	ALPHAP=ACOS(COS(ALPH)*COS(BETT))
      IF(ALPHAP.GT.ALPPLIM) ALPHAP=ALPPLIM
      IF(ALPHAP.LT.1.E-10) THEN
         PHIP=0.
      ELSE
         DUM1=TAN(BETT)
         DUM2=SIN(ALPH)
         PHIP=ATAN2(DUM1,DUM2)
      ENDIF
      ALPHA=ATAN(COS(PHIP)*TAN(ALPHAP))
      BETA=ASIN(SIN(PHIP)*SIN(ALPHAP))
C
      ALPHAPX=CRAD*ALPHAP
      ALPHAX=CRAD*ALPHA
      BETAX=CRAD*BETA
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE C2PTCH(ALPH,PITCH)
C**********************************************************************
C*** * Pitch rate loop
C*** * This module performs the following functions:
C*** *
C*** * (1) Pitch rate dynamics for aerodynamic control
C*** * (2) Pitch rate dynamics for TVC control
C*** * (3) Option for simple first order rate loop if TR>0
C*** *
C*** * Argument Output
C*** *          ALPH=Angle of attack - rad
C*** *
C*** * Argument Input
C*** *          PITCH=Pitch rate command - rad/s
C*** *
C*** * MODIFICATION HISTORY
C*** * 850912 Created by Peter Zipfel
C*** *
C*** *****************************************************************
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0977),ALPLIM)
      EQUIVALENCE (C(0985),ALNLIM)
      EQUIVALENCE (C(0978),TVCLIM)
      EQUIVALENCE (C(0989),RLENG)
      EQUIVALENCE (C(0990),APTVC)
      EQUIVALENCE (C(0991),GA)
      EQUIVALENCE (C(0992),GP)
      EQUIVALENCE (C(0993),PARM)
      EQUIVALENCE (C(0994),AIZ)
      EQUIVALENCE (C(0997),TR)
      EQUIVALENCE (C(0999),FLPLIM)
C
C ALPLIM = D Positive angle of attack limiter - rad
C ALNLIM = D Negative angle of attack limiter - rad
C TVCLIM = D Nozzle gimbal limiter - rad
C RLENG = D Ref length of control moment derivative - m
C APTVC = D TVC apportionment factor; =0: aero control only
C GA = D Gain of aero control - rad/(rad/s)
C GP = D Gain of TVC control - rad/(rad/s)
C PARM = D Nozzle moment arm - m
C AIZ = D Pitch moment of inertia - kg*m^2
C TR = D =0: Internally cal-, >0: rate loop time const - 1/s
C FLPLIM = D Limiter of control surfaces - rad
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(1203),CNALP)
      EQUIVALENCE (C(1205),CMDEL)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1330),AMASS)
      EQUIVALENCE (C(1403),AREA)
      EQUIVALENCE (C(1641),DVBE)
C
C PDYNMC= O Dynamic pressure - Pa
C CNALP= O Normal force derivative - 1/rad
C CMDEL= O Control moment derivative at trim - 1/rad
C FTHALT= O Thrust at altitude - N
C AMASS= O Mass of missile - kg
C AREA= D Reference area for aerodynamic data - m^2
C DVBE= S Speed of vehicle wrt earth - m/s
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0942),ALPD)
      EQUIVALENCE (C(0943),ALP)
      EQUIVALENCE (C(0944),RATEPD)
      EQUIVALENCE (C(0945),RATEP)
C
C ALP = S Intermediate angle-of-attack
C RATEP = S Pitch rate - rad/s
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
C AERMP = G Aero pitch control moment - N*m
C TVCMP = G TVC pitch control moment - N*m
C ETA = G Pitch nozzle angle - rad
C DELQ = G Pitch control surface angle - rad
C TIP = G Incidence lag time constant - s
C TRCALC = G Calculated rate loop time constant - s
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
      TIP=DVBE*AMASS/(PDYNMC*AREA*CNALP+FTHALT)
      ALPD=(TIP*RATEP-ALP)/TIP
      ALPH=ALP
C
C*** LIMIT ALPHA TO MAX VALUE IN AERO TABLES
C
 
	IF(ALPH.GT.ALPLIM)ALPH=ALPLIM
	IF(ALPH.LT.-ALNLIM)ALPH=-ALNLIM
 
      RETURN
      END
C**********************************************************************
      SUBROUTINE C2YAW(BETT,YAW)
C**********************************************************************
C*** * Yaw rate loop
C*** * This module performs the following functions:
C*** *
C*** * (1) Yaw rate dynamics for aerodynamic control
C*** * (2) Yaw rate dynamics for TVC control
C*** * (3) Option for simple first order rate loop if TR>0
C*** *
C*** * Argument Output
C*** *          BETT=Side slip angle - rad
C*** *
C*** * Argument Input
C*** *          YAW=Yaw rate command - rad/s
C*** *
C*** * MODIFICATION HISTORY
C*** * 850912 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(0978),TVCLIM)
	EQUIVALENCE (C(0975),BETLIM)
      EQUIVALENCE (C(0989),RLENG)
      EQUIVALENCE (C(0990),APTVC)
      EQUIVALENCE (C(0991),GA)
      EQUIVALENCE (C(0992),GP)
      EQUIVALENCE (C(0993),PARM)
      EQUIVALENCE (C(0994),AIZ)
      EQUIVALENCE (C(0997),TR)
      EQUIVALENCE (C(0999),FLPLIM)
C
C TVCLIM = D Nozzle gimbal limiter - rad
C BETLIM = D Limiter of side slip angle - rad
C RLENG = D Ref length of control moment derivative - m
C APTVC = D TVC apportionment factor; =0: aero control only
C GA = D Gain of aero control - rad/(rad/s)
C GP = D Gain of TVC control - rad/(rad/s)
C PARM = D Nozzle moment arm - m
C AIZ = D Pitch moment of inertia - kg*m^2
C TR = D =0: Internally calcul-, >0: rate loop time const - 1/s
C FLPLIM = D Limiter of control surfaces - rad
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(1204),CYBET)
      EQUIVALENCE (C(1205),CMDEL)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1330),AMASS)
      EQUIVALENCE (C(1403),AREA)
      EQUIVALENCE (C(1641),DVBE)
C
C T= E Time since launch - s
C PDYNMC= O Dynamic pressure - Pa
C CYBET= O Side force derivative (carry neg sign) - 1/rad
C CMDEL= O Control moment derivative at trim - 1/rad
C FTHALT= O Thrust at altitude - N
C AMASS= O Mass of missile - kg
C AREA= D Reference area for aerodynamic data - m^2
C DVBE= S Speed of vehicle wrt earth - m/s
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(0946),BETD)
      EQUIVALENCE (C(0947),BET)
      EQUIVALENCE (C(0948),RATEYD)
      EQUIVALENCE (C(0949),RATEY)
C
C BET = S Intermediate side slip angle - rad
C RATEY = S Yaw rate - rad/s
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0934),AERMY)
      EQUIVALENCE (C(0935),TVCMY)
      EQUIVALENCE (C(0940),ZETA)
      EQUIVALENCE (C(0941),DELR)
C
C AERMY = G Aero yaw control moment - N*m
C TVCMY = G TVC yaw control moment - N*m
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
      TIY=DVBE*AMASS/(-PDYNMC*AREA*CYBET+FTHALT)
      BETD=-(TIY*RATEY+BET)/TIY
      BETT=BET
C
C*** LIMIT BETA TO MAX VALUE IN AERO TABLES
C
 
	IF(BETT.GT.BETLIM)BETT=BETLIM
	IF(BETT.LT.-BETLIM)BETT=-BETLIM
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE S1I
C**********************************************************************
C*** * Initialization of the Seeker Module
C*** * Reserved C(3510) locations are 400-499
C*** * This subroutine performs the following functions:
C*** *
C*** * Identifies state variables for integration
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** * 950206 Elimination of seeker initialization PZ
C*** * 950720 Adapted for CADAC4 from CADAC6 (PZi)
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
C**********************************************************************
      SUBROUTINE S1
C**********************************************************************
C*** * Seeker Module
C*** * Reserved C(3510) locations are 400-499
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) MSEEK=0: No seeker
C*** *          =2: Seeker enabled (input,or set internally if brake-lock occur
C*** *          =3: Acquis. mode (set internally, when missile is within RACQ)
C*** *          =4: Seeker lock-on (set internally, when DTIMAC has elapsed)
C*** *          =5: Seeker within blind range (set internally). Output held con
C*** *
C*** * (2) MS1DYN=0: Kinematic seeker
C*** *           =1: Dynamic seker
C*** *
C*** * (3) Sets MGUID=6(terminal guidance) when seeker is locked on (MSEEK=4)
C*** *     Sets MGUID=3(midcourse guidance) when seeker brakes lock (MSEEK=2)
C*** *
C*** * (4) Value of RACQ determines wether Seeker is locked on before or after
C*** *     For LOBL, the time delay DTIMAC represents the seeker lock-out time
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** * 950206 Restructuring of logic PZ
C*** * 950720 Adapted for CADAC4 from CADAC6 (PZi)
C*** * 980613 RT-CADAC:equivalenced ISETS1 and EPCHAC, PZi
C*** *
C*** ******************************************************************
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
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(2000),T)
C
C CRAD= E Conversion from radians to degree (57.29577951)
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C TT1L= O Transf Matrix of T1 body coord wrt local level coord
C MGUID= D/G =0:No guidance, =3:Midcourse, =6:Terminal
C SBEL= O Position of vehicle wrt earth ref point E - m
C T= E Time since launch - s
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
CC            IF(ABS(EHZ).LE.FOVYAW.AND.ABS(EHY).LE.FOVPP)THEN
               TIMEAC=T-EPCHAC
               IF(TIMEAC.GT.DTIMAC) MSEEK=4
CC            END IF
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
C**********************************************************************
      SUBROUTINE S1DYN(MSEEK,MGUID,THTPB,PSIPB,SIGDY,SIGDZ,EHZ,EHY
     &                 ,THB,SBT1L,DBT1)
C**********************************************************************
C*** * Dynamic seeker module
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Given true target relative geometry it determines inertial
C*** *     LOS rates in pitch and yaw, corrupted by these errors:
C*** *         Target Scintillation
C*** *         Blur, pixel quatization and bias
C*** *         Gimbal dynamics, quantization and bias
C*** * (2) Determines Aimpoint off-set from computer determined seeker axis
C*** *     in Focal Plane (F.P.) array EAPH(3)
C*** * (3) Models Kalman Filter 2nd order dynamics (which generates inertial LO
C*** * (4) Models strap-down gyro feedback and gimbal kinematics
C*** * (5) Allows for aimpoint selection and correction.
C*** *
C*** * Argument Output:
C*** *                 MSEEK: If break lock occured reset to 2 (acquisition)
C*** *                 MGUID: If break lock occured reset to 3 (midcourse)
C*** *                 THTPB=Pitch pointing angle - rad
C*** *                 PSIPB=Yaw pointing angle - rad
C*** *                 SIGDY=Pitch sight line spin - rad/s
C*** *                 SIGDZ=Yaw sight line spin - rad/s
C*** *                 EHZ=Yaw seeker error angle - rad
C*** *                 EHY=Pitch seeker error angle - rad
C*** *
C*** * Argument Input:
C*** *                 THB(3,3)=Transf matrix of head wrt body axes (Initializa
C*** *                 SBT1L(3)=Position of missile wrt target - m
C*** *                 DBT1=Distance between missile and target - m
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** * 950206 Restructuring of Logic (PZi)
C*** * 980626 RT-CADAC Equivalenced IBREAK, PZi
C*** *
C*** *****************************************************************
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
      EQUIVALENCE (C(0706),WBECB(1))
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1287),TRTHT)
      EQUIVALENCE (C(1289),TRTHTD)
      EQUIVALENCE (C(1290),TRPHID)
      EQUIVALENCE (C(1291),TRATE)
      EQUIVALENCE (C(1701),TBL(1,1))
C
C CRAD= E Conversion from radians to degree (57.29577951)
C WBECB= O Computed ang vel of body wrt earth - rad/s
C TRCODE= O Code numbers for stopping conditions -
C TRTHT= D Maximum pitch gimbal angle - rad
C TRTHTD= D Maximum pitch gimbal rate - rad/s
C TRPHID= D Maximum roll gimbal rate - rad/s
C TRATE= D Maximum tracking rate - rad/s
C TBL= O Trans matrix of body wrt local earth axes
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
C WLQ2 = S Second state variable in K.F. - rad/s2
C WLR2 = S Second state variable in K.F. - rad/s2
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
      EQUIVALENCE (C(0497),TRACKR)
      EQUIVALENCE (C(0498),THTPBD)
      EQUIVALENCE (C(0499),PHIHBD)
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
C TRACKR = G Seeker tracking rate - rad/s
C THTPBD = G Seeker pitch gimbal rate - rad/s
C PHIHBD = G Seeker roll gimbal rate - rad/s
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
C*** SIGHT LINE SPIN ESTIMATOR ( KALMAN FILTER REPRESENTED BY 2ND ORDER LAG)
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
         TRACKR=SQRT(EHY*EHY+EHZ*EHZ)
         IF(ABS(THTHB).GT.TRTHT)THEN
            TRCODE=6.
            IBREAK=1
         ELSEIF(ABS(THTPBD).GT.TRTHTD)THEN
            TRCODE=7.
            IBREAK=1
         ELSEIF(ABS(PHIHBD).GT.TRPHID)THEN
            TRCODE=8.
            IBREAK=1
         ELSEIF(TRACKR.GT.TRATE)THEN
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
C*** DIAGNOSTIC VARIABLES: SEEKER VOLUME (BEAM) DISPLAY ON SG ANIMATION
C
      DBA=DBT1
      IF(MSEEK.EQ.3)DBA=DBT1
 
C*** EULER ANGLES OF SEEKER BEAM
 
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
C**********************************************************************
      SUBROUTINE S1KIN(THTPB,PSIPB,SIGDY,SIGDZ,SBT1L,DBT1)
C**********************************************************************
C*** * Kinematic seeker module
C*** * This subroutine performs the following functions:
C*** *
C*** * (1)  Calculates error free LOS rates and angles
C*** * (2)  Also used to initialize the dynamic seeker subroutine
C*** *
C*** * Argument Output:
C*** *                 THTPB=Pitch pointing angle - rad
C*** *                 PSIPB=Yaw pointing angle - rad
C*** *                 SIGDY=Pitch sight line spin - rad/s
C*** *                 SIGDZ=Yaw sight line spin - rad/s
C*** * Argument Input:
C*** *                 SBT1L(3)=Position of missile wrt target - m
C*** *                 DBT1=Distance between missile and target - m
C*** *
C*** * MODIFICATION HISTORY
C*** * 941122 Created by Peter Zipfel
C*** *
C*** ******************************************************************
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
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1701),TBL(1,1))
C
C CRAD= E Conversion from radians to degree (57.29577951)
C VT1EL= S Velocity of T1 wrt earth in local level coord - m/s
C VBEL= O Velocity of vehicle wrt earth - m/s
C TBL= O Trans matrix of body wrt local earth axes
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
C**********************************************************************
      SUBROUTINE S1AIMP(SAT1H,THL,DBT1)
C**********************************************************************
C*** * Aimpoint selection and corruption subroutine
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Introduces aimpoint tracking errors
C*** * (2) Introduces hot spot jitter and bias errors
c*** *
c*** * Both are initiated at distance DAIM from the target
C*** *
C*** * Argument Output:
C*** *          SAT1H(3)=Aimpoint error in head axes (focal plane array)
C*** * Argument Input:
C*** *          THL(3,3)=Tran Matrix of head wrt local level axes
C*** *          DBT1=Distance of vehicle wrt target T1 - m
C*** *
C*** * MODIFICATION HISTORY
C*** * 941122 Created by Peter Zipfel
C*** *
C*** ******************************************************************
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
C**********************************************************************
      SUBROUTINE UTHPB(THTHB,PHIHB,PSIPB,THTPB)
C**********************************************************************
C*** * Utility subroutine for angle conversions
C*** * This subroutine performs the following function:
C*** *
C*** * Converts pointing angles (computer) to head angles (gimbals)
C*** *
C*** * Argument Output:
C*** *          THTHB=Gimbal head pitch angle - rad
C*** *          PHIHB=Gimbal roll angle - rad
C*** * Argument Input:
C*** *          PSIPB=Yaw computer pointing angle - rad
C*** *          THTPB=Pitch computer pointing angle - rad
C*** *
C*** * MODIFICATION HISTORY
C*** * 941122 Created by Peter Zipfel
C*** *
C*** *****************************************************************
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
C**********************************************************************
      SUBROUTINE UTTHB(THB,THT,PHI)
C**********************************************************************
C*** * Utility subroutine for THB Transformation matrix
C*** * This subroutine performs the following function:
C*** *
C*** * Calculates T.M. of head axes wrt body axes
C*** *
C*** * Argument Output
C*** *          THB=Transformation matrix of head angles wrt missile body axes
C*** * Argument Input:
C*** *          THTHB=Gimbal head pitch angle - rad
C*** *          PHIHB=Gimbal roll angle - rad
C*** *
C*** * MODIFICATION HISTORY
C*** * 941122 Created by Peter Zipfel
C*** *
C*** ******************************************************************
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
C*********************************************************************
      SUBROUTINE A3
C*********************************************************************
C*** * Force Module A3
C*** * Reserved C(3510) locations are 1400-1499
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates the non-gravitational forces (specific force)
C*** * (2) Determines the TBV transformation matrix based on:
C*** *       Skid-to-Turn: ALPHA, BETA from autopilot
C*** *       Bank-to-Turn: ALPHA, PHIBV from autopilot
C*** *       (ALPHA is in the body plane of symmetry for both cases
C*** *
C*** * MODIFICATION HISTORY
C*** * 910311 Created by Peter Zipfel
C*** *
C*** *****************************************************************
C
      COMMON C(3510)
C
      DIMENSION FB(3),FSPB(3),TVB(3,3),TBV(3,3)
     +,FWB(3),FSPV(3),FAB(3),TBL(3,3)
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
      EQUIVALENCE (C(0207),PDYNMC)
      EQUIVALENCE (C(1200),CA)
      EQUIVALENCE (C(1201),CN)
      EQUIVALENCE (C(1202),CY)
      EQUIVALENCE (C(1301),FTHALT)
      EQUIVALENCE (C(1330),AMASS)
      EQUIVALENCE (C(1701),TBL(1,1))
      EQUIVALENCE (C(1710),TLB(1,1))
C
C AGRAV= E Gavity acceleration (9.806635 m/s^2)
C PDYNMC= O Dynamic pressure - Pa
C CA= O Axial trimmed force coefficient -
C CN= O Normal trimed force coefficient -
C CY= O Side force coefficient -
C FTHALT= O Thrust at altitude - N
C AMASS= O Mass of missile - kg
C TBL= O Trans matrix of body wrt local earth axes
C TLB= O Trans matrix of local earth wrt body axes
C
C*** OUTPUTS TO OTHER MODULES
C
      EQUIVALENCE (C(1400),FSPB(1))
      EQUIVALENCE (C(1413),TBV(1,1))
      EQUIVALENCE (C(1423),FSPV(1))
      EQUIVALENCE (C(1432),ABEL(1))
C
C FSPB(3) = O Specific force in body coor - N/kg
C TBV(3,3) = O Trans matrix of body wrt flight path axes
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
      CA=(1.0+FRACA)*CA
      CN=(1.0+FRACN)*CN
      CY=(1.0+FRACY)*CY
C
C*** AERODYNAMIC AND PROPULSION FORCES IN BODY COOR.
C
      FAB(1)=FTHALT-CA*PDYNMC*AREA
      FAB(2)=CY*PDYNMC*AREA
      FAB(3)=-CN*PDYNMC*AREA
C
C*** SPECIFIC FORCE IN BODY AXES
C
      CALL MATEQL(FB,FAB,3,1)
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
C*********************************************************************
      SUBROUTINE A3TRA(TBV)
C*********************************************************************
C*** * Transformation Matrix of Body wrt Flight Path Coordinates
C*** *
C*** * Argument Output:
C*** *  TBV(3,3)= Transformation matrix of body wrt flight path coor
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** *
C*** *****************************************************************
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
C ALPHA= O Angle of attack - rad
C BETA= O Sideslip angle - rad
C PHIBV= O Bank angle - rad
C MTURN= D =0:skid-to-turn, =1:bank-to-turn
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
C*********************************************************************
      SUBROUTINE D1I
C*********************************************************************
C*** * The initialization module for the Newton Module D1.
C*** * Reserved C(3510) locations are 1600-1699
C*** * This module performs the following functions:
C*** *
C*** * Computes initial values of TVL
C*** *
C*** * MODIFICATION HISTORY
C*** * 820318 Created by Peter Zipfel
C*** * 970428 Removed old initialization. All new init done in G1I - GAB
C*** *
C*** *****************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100),TVL(3,3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1643),PSIVL)
      EQUIVALENCE (C(1645),THTVL)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(2000),T)
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C*** INITIALIZATIONS
C
      EQUIVALENCE (C(1627),TVL(1,1))
C
C*** INITIALIZE STATE VARIABLES
C
      ILOC=1640
      DO I=1,3
         IPL(NIP)=ILOC
         IPLV(NIP)=ILOC+1
         ILOC=ILOC+2
         NIP=NIP+1
      ENDDO
      ILOC=1646
      DO I=0,2
         IPL(NIP)=ILOC+I
         IPLV(NIP)=ILOC+I+3
         NIP=NIP+1
      ENDDO
C
      CALL MAT2TR(TVL,PSIVL,THTVL)
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE D1
C**********************************************************************
C*** * Newton's Equations
C*** * Reserved C(3510) locations are 1600-1699
C*** * This module performs the following functions:
C*** *
C*** * (1) Solves differential equations for DVBE, PSIVL, THTVL, SBEL
C*** * (2) Calculates angular velocity of velocity frame wrt earth frame WVEV(3
C*** * (3) Calculates ground track
C*** *
C*** * MODIFICATION HISTORY
C*** * 820318 Created by Peter Zipfel
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION SBEL(3),SBELS(3),SBELSD(3),VBEL(3),FSPV(3),TVL(3,3)
     +,WVEV(3),DUM3(3),SBELM(3)
C
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(2866),ICOOR)
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(0979),MFREEZE)
      EQUIVALENCE (C(1423),FSPV(1))
C
C MFREEZE= D =0:Unfreeze and print; =1:Freeze; increment for more
C FSPV= O Specific force in flight path axes - N/kg
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1608),WVEV(1))
      EQUIVALENCE (C(1615),HBE)
      EQUIVALENCE (C(1627),TVL(1,1))
C
C SBEL(3) = O Position of vehicle wrt earth ref point E - m
C VBEL(3) = O Velocity of vehicle wrt earth - m/s
C WVEV(3) = O Angular vel of flight path frame wrt earth - rad/s
C HBE = O Height of vehicle above sea level - m
C TVL(3,3) = O Trans matrix of flight path wrt local earth axes
C
C*** STATE VARIABLES
C
      EQUIVALENCE (C(1640),DVBED)
      EQUIVALENCE (C(1641),DVBE)
      EQUIVALENCE (C(1642),PSIVLD)
      EQUIVALENCE (C(1643),PSIVL)
      EQUIVALENCE (C(1644),THTVLD)
      EQUIVALENCE (C(1645),THTVL)
      EQUIVALENCE (C(1646),SBELSD(1))
      EQUIVALENCE (C(1649),SBELS(1))
C
C DVBE = S Speed of vehicle wrt earth - m/s
C PSIVL = S Horizontal flight path angle - rad
C THTVL = S Vertical flight path angle - rad
C SBELS(3) = S State variable of vehicle position - m
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(1617),GNDTCK)
      EQUIVALENCE (C(1665),PSIVLX)
      EQUIVALENCE (C(1666),THTVLX)
      EQUIVALENCE (C(1669),HG)
      EQUIVALENCE (C(1671),DBE)
C
C GNDTCK = G Ground track - m
C PSIVLX = S Horizontal flight path angle - deg
C THTVLX = S Vertical flight path angle - deg
C HG = G Height of ground above sea level - m
C DBE = G Distance of vehicle from earth point E - m
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
      CALL MATEQL(SBELSD,VBEL,3,1)
      CALL MATEQL(SBEL,SBELS,3,1)
      CALL MATABS(DBE,SBEL,3)
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
C*** DISTANCE TRAVELLED ALONG GROUNDTRACK
C
      IF(ICOOR.GE.0) THEN
         CALL MATSUB(DUM3,SBEL,SBELM,3,1)
         DUM3(3)=0.
         CALL MATABS(DUM4,DUM3,3)
         GNDTCK=DUM4+GNDTCK
      END IF
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE D2I
C**********************************************************************
C*** * The initialization module for Module D2
C*** * Reserved C(3510) locations are 1700-1749
C*** * This module performs the following functions:
C*** *
C*** * Provides initial direction cosine matrix TBL and TLB
C*** *
C*** * MODIFICATION HISTORY
C*** * 910311 Created by Peter Zipfel
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
C TVL= O Trans matrix of flight path wrt local earth axes
C
C*** OUTPUT TO OTHER MODULES
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
C*** * Kinematic Rotation Module
C*** * Reserved C(3510) locations are 1700-1749
C*** * This module performs the following functions:
C*** *
C*** * (1) Calculates direction cosine matrix TBL
C*** * (2) Calculation of angular velocity of body wrt velocity frame WBVB
C*** *       Skid-to-Turn: ALP,ALPD,BETD from autopilot
C*** *       Bank-to-Turn: ALP,ALPD,PHD from autopilot
C*** * (3) Calcualtes body rates without solving Euler's equations
C*** * (4) Off-line calculation of Euler angles
C*** *
C*** * MODIFICATION HISTORY
C*** * 910311 Created by Peter Zipfel
C*** *
C*** ******************************************************************
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
      EQUIVALENCE (C(0942),ALPD)
      EQUIVALENCE (C(0943),ALP)
      EQUIVALENCE (C(0946),BETD)
      EQUIVALENCE (C(0958),PHD)
      EQUIVALENCE (C(1413),TBV(1,1))
      EQUIVALENCE (C(1608),WVEV(1))
      EQUIVALENCE (C(1627),TVL(1,1))
C
C CRAD= E Conversion from radians to degree (57.29577951)
C ALP= S Intermediate angle-of-attack
C TBV= O Trans matrix of body wrt flight path axes
C WVEV= O Angular vel of flight path frame wrt earth - rad/s
C TVL= O Trans matrix of flight path wrt local earth axes
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
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Identifies the state variables for integration
C*** * (2) Initializes the target and shooter velocity vectors
C*** * (3) Initializations for special engagements
C*** *     (a) Fixes Shooter initial position
C*** *     (b) Calculates shooter angle of attack
C*** *     (c) Calculates shooter Euler angles
C*** *     (d) Shooter velocity vector
C*** *     (e) Fixes target initial position
C*** *     (f) Calculates target Euler angles
C*** *     (g) Target velocity vector
C*** *     (h) Missile Euler angles = shooter's Euler angles, modified by
C*** *         shooter angle of attack and missile installed roll angle
C*** *     (i) Missile acceleration = shooter acceleration
C*** *     (k) Missile velocity = shooter velocity
C*** *     (l) Missile position = shooter position
C*** * (4) Initialization of specific engagements
C*** *     LAR-1 Attack Zone (MTARG=21)
C*** *     LAR-2 Pre-Merge Engagement  (MTARG=12)
C*** *     LAR-3 One Circle Fight (MTARG=13)
C*** *     LAR-4 Two Circle Fight (MTARG=14)
C*** *     LAR-5 Lufbery Circle (MTARG=15)
C*** *     CIRC-2 Chase Circle Engagement (UK) (MTARG=22)
C*** *     CIRC-3 Head-On Circle Engagement (UK) (MTARG=23)
C*** *     CIRC-4 Twin Circle Engagement (UK) (MTARG=24)
C*** *
C*** *
C_______________________________________________________________________________
C                                          |                        ||
C   INITIALIZATION VARIABLES               |     SHOOTER CENTERED   ||      TARG
C__________________________________________|FREE|LAR2|LAR3|LAR4|LAR5||FREE|LAR1|
C MTARG= |MTARGC|MTARGM|Engagement modes   | 00 | 12 | 13 | 14 | 15 || 00 | 21 |
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
C_______________________________________________________________________________
C
C Code: I Initialize, G Diagnostic, I/N Initialize or not, - Not used, 'blank' n
C
C Notes: (1) Entries in table with variable names may be given different values
C        (2) If MTARG=0 these special initializations are bypassed
C
C*** * MODIFICATION HISTORY
C*** * 941129 Created by Peter Zipfel
C*** * 950421 Added LAR and CIRCLE engagement initializations (PZi)
C*** * 950719 Fixed trig problem in LAR4 initialization (TAO)
C*** * 950721 Adapted for CADAC4 from CADAC6. use of MTARG and MINIT is changed
C*** * 970417 MINIT removed, PZi
C*** * 970428 Added CALLs to SWEEP subs with MINIT by pass, GAB
C*** * 970904 Corrected initialization of ALP and BET, PZi
C*** * 980818 Removed calls to SWEEP subroutines, PZi
C*** *
C*** ******************************************************************
C
      COMMON C(3510)
C
      DIMENSION IPL(100),IPLV(100),VT1EL(3),VT2EL(3),ST1EL(3),ST2EL(3)
     &,VBEL(3),SBEL(3),SBELS(3)
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
      EQUIVALENCE (C(0197),HT2E)
      EQUIVALENCE (C(0198),ALAMHX)
      EQUIVALENCE (C(0199),RHL)
      EQUIVALENCE (C(1600),MINIT)
C
C MTARG = D Shooter and target initialization (see table in code)
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
C HT2E = D Height of shooter at launch - m
C ALAMHX = D/G Look angle of shooter at launch - deg
C RHL = D/G Horizontal launch range - m
C MINIT = D =0:No sweep; =100:Sweep method. engaged
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
      EQUIVALENCE (C(2000),T)
C
C T= E Time since launch - s
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
      EQUIVALENCE (C(0943),ALP)
      EQUIVALENCE (C(0947),BET)
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1615),HBE)
      EQUIVALENCE (C(1641),DVBE)
      EQUIVALENCE (C(1643),PSIVL)
      EQUIVALENCE (C(1645),THTVL)
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1649),SBELS(1))
C
C*** DIAGNOSTICS
C
      EQUIVALENCE (C(0146),PSILAR4X)
      EQUIVALENCE (C(0178),ALPT2X)
      EQUIVALENCE (C(0179),ANUHX)
C
C PSILAR4X = G Shooter heading when pointed at target (LAR4) - deg
C ALPT2X = G/D Shooter alpha (if=0:calculated internally) - deg
C ANUHX =   G Horizontal velocity angle of target wrt shooter - deg
C
C*** DECOMPOSITION OF MTARG INTO MTARGC=DEFINING THE SHOOTER OR TARGET CENTER
C                                MTARGM=DEFINING THE MODE
      MTARGC=INT(MTARG/10.)
      MTARGM=MTARG-MTARGC*10.
C
C*** TARGET STATE VARIABLE INITIAIZATION
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
C*** SHOOTER STATE VARIABLE INITIALIZATION
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
C
C*** FOR GENERAL INITIALIZATION (MTARG=0) BYPASS INITIALIZATIONS
C
      IF(MTARG.NE.0) THEN
C
C***    SHOOTER INITIALIZATION
C
C**     SHOOTER POSITION
         IF(MTARGC.EQ.1) CALL VECVEC(ST2EL,0.,0.,-HT2E)
         IF(MTARGC.EQ.2)THEN
            IF(MTARGM.EQ.1)CALL VECVEC(ST2EL,RHL*COS(TAUHX/CRAD)
     &                               ,RHL*SIN(TAUHX/CRAD),-HT2E)
            IF(MTARGM.GE.2)CALL VECVEC(ST2EL,-RCOMB*SIN(AMUHX/CRAD)
     &                            ,RCOMB*(1.-COS(AMUHX/CRAD)),-HT2E)
         ENDIF
         IF(MTARGC.EQ.2.AND.MTARGM.GE.2)GOTO 10
         IF(ALPT2X.EQ.0.)THEN
C**     AIR DENSITY IN KG*M^3
            RHOT2=1.225*(1.+ST2EL(3)/41900.)**4
C**     ANGLE OF ATTACK
            ALPT2X=2.*AN2C/(RHOT2*DVT2E**2)*WLOADT2/CLAT2
         ENDIF
 10      CONTINUE
C**     SHOOTER ANGLES
C*      HORIZONTAL, SHOOTER CENTERED LAUNCH FOR  LAR-2
         IF(MTARGC.EQ.1.AND.MTARGM.EQ.2) THEN
            PSIT2LX=0.
            THTT2LX=0.
            PHIT2LX=0.
         ENDIF
C*      HORIZONTAL, SHOOTER CENTERED LAUNCH FOR  LAR-3, LAR-4, LAR-5
         IF(MTARGC.EQ.1.AND.MTARGM.GE.3) THEN
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
         IF(MTARGC.EQ.1.AND.MTARGM.EQ.4) THEN
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
            ANGZETA=ACOS(CON4)
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
         IF(MTARGC.EQ.2)THEN
            THTT2LX=0.
            IF(MTARGM.EQ.1)THEN
               PHIT2LX=0.
               PSIT2LX=-180.+TAUHX-SIGHX
               ALAMHX=SIGHX-ALPT2X
            ENDIF
            IF(MTARGM.EQ.2)THEN
               PHIT2LX=CRAD*ATAN(DVT2E**2/(AGRAV*RCOMB))
               AN2C=1./COS(PHIT2LX/CRAD)
               PSIT2LX=-AMUHX
               SIGHX=AMUHX/2.
               TAUHX=180-SIGHX
            ENDIF
            IF(MTARGM.GE.3)THEN
               PHIT2LX=-CRAD*ATAN(DVT2E**2/(AGRAV*RCOMB))
               AN2C=1./COS(PHIT2LX/CRAD)
               PSIT2LX=-180.-AMUHX
               SIGHX=AMUHX/2.
               TAUHX=-SIGHX
            ENDIF
            PHIT2LCX=PHIT2LX
         ENDIF
      ENDIF
C**  SHOOTER VELOCITY VECTOR
      PSIT2L=PSIT2LX/CRAD
      THTT2L=THTT2LX/CRAD
      CALL MATCAR(VT2EL,DVT2E,PSIT2L,THTT2L)
C
C*** FOR GENERAL INITIALIZATION (MTARG=0) BYPASS INITIALIZATIONS
C
      IF(MTARG.NE.0) THEN
C
C***    TARGET INITIALIZATION
C
C**     TARGET POSITION
         IF(MTARGC.EQ.1)THEN
            ST1EL(1)=RHL*COS(ALAMHX/CRAD)
            ST1EL(2)=RHL*SIN(ALAMHX/CRAD)
            ST1EL(3)=-HT1E
         ENDIF
         IF(MTARGC.EQ.2)THEN
            CALL VECVEC(ST1EL,0.,0.,-HT1E)
            IF(MTARGM.GE.2)THEN
               RHL=2.*RCOMB*ABS(SIN(AMUHX/(CRAD*2.)))
            ENDIF
         ENDIF
C**     TARGET ANGLES
C*      HORIZONTAL, SHOOTER CENTERED LAUNCH  FOR LAR-2, LAR-3, LAR-4, LAR-5
         IF(MTARGC.EQ.1)THEN
            THTT1LX=0.
            IF(MTARGM.EQ.2)THEN
c               PSIT1LX=0.0
               PSIT1LX=180.+ALAMHX-TAUHX
c               PSIT1LX=20.
               SIGHX=ALAMHX
            ENDIF
            IF(MTARGM.EQ.3)THEN
               PSIT1LX=ALPT2X+2.*ALAMHX-180.
               PHIT1LX=-CRAD*ATAN(AN1C)
               TAUHX=-(ALPT2X+ALAMHX)
               SIGHX=-TAUHX
            ENDIF
            IF(MTARGM.EQ.4)THEN
               PSIT1LX=180.-ALPT2X
               PHIT1LX=CRAD*ATAN(AN1C)
               TAUHX=ALPT2X+ALAMHX
               SIGHX=TAUHX
            ENDIF
            IF(MTARGM.EQ.5)THEN
               PSIT1LX=2.*ALAMHX+ALPT2X
               PHIT1LX=CRAD*ATAN(AN1C)
               TAUHX=180.-(ALPT2X+ALAMHX)
               SIGHX=180.-TAUHX
            ENDIF
         ENDIF
C*      HORIZONTAL, TARGET CENTERED LAUNCH FOR LAR-1 AND THREE UK CIRCLES
         IF(MTARGC.EQ.2)THEN
            PSIT1LX=0.
            THTT1LX=0.
CGAB*
            IF(MTARGM.EQ.1)THEN
               PHIT1LX=CRAD*ATAN(AN1C)
               IF(AN1C.LE.1.00) PHIT1LX=0.0
            ENDIF
CGAB*
            IF(MTARGM.GE.2)THEN
               AN1C=AN2C
               PHIT1LX=CRAD*ATAN(AN1C)
            ENDIF
            IF(MTARGM.EQ.4)PHIT1LX=-PHIT1LX
         ENDIF
         PHIT1LCX=PHIT1LX
C
C***    MANEUVER ANGLE
C
        ANUHX=PSIT1LX-PSIT2LX
C
      ENDIF
C
C*** INITIALIZATION OF TARGET VELOCITY VECTOR
C
      PSIT1L=PSIT1LX/CRAD
      THTT1L=THTT1LX/CRAD
      CALL MATCAR(VT1EL,DVT1E,PSIT1L,THTT1L)
C
C*** FOR GENERAL INITIALIZATION (MTARG=0) BYPASS INITIALIZATIONS
C
      IF(MTARG.NE.0) THEN
C
C***    MISSILE INITIALIZATION
C
C**     MISSILE INCIDENCE ANGLES
         ALP=ATAN(COS(-PHIT2LX/CRAD)*TAN(ALPT2X/CRAD))
         BET=ASIN(SIN(-PHIT2LX/CRAD)*SIN(ALPT2X/CRAD))
C**     MISSILE STATE VARIABLES
         DVBE=DVT2E
         PSIVL=PSIT2LX/CRAD
         THTVL=THTT2LX/CRAD
         CALL MATEQL(SBELS,ST2EL,3,1)
         CALL MATEQL(SBEL,ST2EL,3,1)
C
         CALL MATEQL(VBEL,VT2EL,3,1)
         HBE=HT2E
      ENDIF
C
      RETURN
      END
 
C**********************************************************************
      SUBROUTINE G1
C**********************************************************************
C*** * Target Module
C*** * Reserved C(3510) locations are 100-199
C*** * This subroutine performs the following functions:
C*** *
C*** * Simulates target and shooter trajectories
C*** *
C*** * MODIFICATION HISTORY
C*** * 941129 Created by Peter Zipfel
C*** *
C*** ******************************************************************
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
      COMMON C(3510)
C
      DIMENSION TT1L(3,3),VT1ELD(3),VT1EL(3),ST1ELD(3),ST1EL(3)
     &,TT2L(3,3),VT2ELD(3),VT2EL(3),ST2ELD(3),ST2EL(3)
C
C*** INPUT DATA
C
C      EQUIVALENCE (C(0100),MTARG)
      EQUIVALENCE (C(0101),PHIT1LCX)
      EQUIVALENCE (C(0102),TPHI1)
      EQUIVALENCE (C(0103),AX1C)
      EQUIVALENCE (C(0104),TAX1)
      EQUIVALENCE (C(0105),AN1C)
      EQUIVALENCE (C(0106),TAN1)
      EQUIVALENCE (C(0146),PSILAR4X)
      EQUIVALENCE (C(0151),PHIT2LCX)
      EQUIVALENCE (C(0152),TPHI2)
      EQUIVALENCE (C(0153),AX2C)
      EQUIVALENCE (C(0154),TAX2)
      EQUIVALENCE (C(0155),AN2C)
      EQUIVALENCE (C(0156),TAN2)
C
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
C*** INPUT FROM EXECUTIVE
C
      EQUIVALENCE (C(0052),CRAD)
      EQUIVALENCE (C(2000),T)
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
C*** CALL TO AIRCRAFT MOTION SUBROUTINE
C
      PHIT1LC=PHIT1LCX/CRAD
      CALL G1TARG(TT1L,PHIT1LD,AX1D,AN1D,VT1ELD,ST1ELD,PHIM1LX
     &,DVT1E,PSIT1LX,THTT1LX
     &,VT1EL,PHIT1L,PHIT1LC,AX1,AX1C,AN1,AN1C,TPHI1,TAX1,TAN1)
      PHIT1LX=PHIT1L*CRAD
C
      PHIT2LC=PHIT2LCX/CRAD
      CALL G1TARG(TT2L,PHIT2LD,AX2D,AN2D,VT2ELD,ST2ELD,PHIM2LX
     &,DVT2E,PSIT2LX,THTT2LX
     &,VT2EL,PHIT2L,PHIT2LC,AX2,AX2C,AN2,AN2C,TPHI2,TAX2,TAN2)
      PHIT2LX=PHIT2L*CRAD
C
C*** HORIZONTAL VELOCITY ANGLE OF TARGET WRT SHOOTER
C
      ANUHX=PSIT1LX-PSIT2LX
C      IF(PSIT2LX.GT.PSILAR4X)THEN
C        AN1C=0.0
C        AN2C=0.0
C      ENDIF
C
      RETURN
      END
C**********************************************************************
      SUBROUTINE G1TARG(TTL,PHITLD,AXD,AND,VTELD,STELD,PHIMLX
     &,DVTE,PSITLX,THTTLX
     &,VTEL,PHITL,PHITLC,AX,AXC,AN,ANC,TPHI,TAX,TAN)
C**********************************************************************
C*** * Generates target traces
C*** * Ouput Arguments:
C*** *        TTL(3*3)  =Transformation matrix of target frame wrt local earth
C*** *        PHITLD    =Derivative of PHITL
C*** *        AXD       =Derivative of AX
C*** *        AND       =Derivative of AN
C*** *        VTELD(3*1)=Derivative of VTEL
C*** *        STELD(3*1)=Derivative of STEL
C*** *        PHIMLX    =Bank angle of Maneuver Plane
C*** *        DVTE      =Target speed - m/s
C*** *        PSITLX    =Heading angle of target velocity vector - deg
C*** *        THTTLX    =Flight path angle of target velocity vector - deg
C*** * Input Arguments:
C*** *        VTEL(3*1) =Velocity of target in local earth coord. - m/s
C*** *        PHITL     =Bank angle of normal load factor plane - rad
C*** *        PHITLC    =Commanded PHITL - rad
C*** *        AX        =Acceleration along the target velocity vector - g's
C*** *        AXC       =Commanded AX - g's
C*** *        AN        =Normal load factor (normal to velocity vector) - g's
C*** *        ANC       =Commanded AN - g's
C*** *        TPHI      =Time lag of bank angle of load factor plane - s
C*** *        TAX       =Time lag of acceleration along velocity vector - s
C*** *        TAN       =Time lag of normal load factor - s
C*** *
C*** * NOTE: (1) PSITLX,THTTLX,PHITLX are the Euler angles of TTL transf.matrix
C*** *       (2) PHITLX and PHIMLX are the bank angles of the vehicle symmetry
C*** *           plane (load factor plane) and the maneuver plane, respectively.
C*** *            They are, in general, not the same.
C*** *
C*** * MODIFICATION HISTORY
C*** * 941129 Created by Peter Zipfel
C*** *
C**********************************************************************
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
      EQUIVALENCE (C(2000),T)
C
C*** TRANSFORMATION MATRIX TTL
C
      IF(T.LT.20.5)GOTO 999
      DUM988=988.0
  999 CONTINUE
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
C***********************************************************************
      SUBROUTINE G2
C***********************************************************************
C*** * Atmosphere and Gravity Module
C*** * Reserved C(3510) locations are 200-299
C*** * This module performs the following functions:
C*** *
C*** * 1) Calculates the atmospheric properties
C*** * 2) Calculates the gravitational acceleration
C*** * 3) Calculates the vehicle Mach number
C*** *
C*** * SI units
C*** *
C*** * MODIFICATION HISTORY
C*** * 931007 Created by Peter Zipfel
C*** * 970214 Adapted to CADAC4 from CADAC6, PZi
C*** *
C*** ******************************************************************
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
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(0979),MFREEZE)
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1283),TRMACH)
      EQUIVALENCE (C(1284),TRDYNM)
      EQUIVALENCE (C(1615),HBE)
      EQUIVALENCE (C(1641),DVBE)
C
C MGUID= D/G =0:No guidance, =3:Midcourse, =6:Terminal
C MFREEZE= D =0:Unfreeze and print; =1:Freeze; increment for more
C TRCODE= O Code numbers for stopping conditions -
C TRMACH= D Minimum Mach number
C TRDYNM= D Minimum dynamic pressure - lbf/ft^2
C HBE= O Height of vehicle above sea level - m
C DVBE= S Speed of vehicle wrt earth - m/s
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
C***********************************************************************
      SUBROUTINE G4
C***********************************************************************
C*** * End-Of-Run Subroutine.
C*** * Reserved C(3510) locations are 1750-1799
C*** *
C*** * This subroutine performs the following functions:
C*** * (1) Stops run at closest point of approach
C*** * (2) Calculates intercept plane. The plane is normal to the relative
C*** *     velocity vector and contains the target center of mass.
C*** *     Its x-axis (which lies in the plane) is obtained by rotating
C*** *     from the nose of the target through PSIPT1X and THTPT1X.
C*** *     Its y-axis remains in the target x,y-plane.
C*** * (3) Interpolates to calculate miss distance in intercept plane
C*** * (4) Interpolates intercept time
C*** * (5) Calculates the intercept geometry for SHAZAM Pk post processing.
C*** * (6) Monte Carlo run: for every sample run the intercept plane
C*** *     will be oriented slightly different. Better use two M.C. runs.
C*** *     First run establishes the mean orientation angles PSIPT1X,THTPT1X
C*** *     of the intercept plane. Then use MTERM=2 to input these angles for
C*** *     the actual M.C. run.
C*** *
C*** *     MTERM= 0 Internally calculated intercept plane, no SHAZAM output
C*** *            1 Internally calculated intercept plane, with SHAZAM output
C*** *            2 Intercept plane defined by PSIPT1X, THTPT1X input,
C*** *               with SHAZAM output
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** * 951205 Added nav and guidance errors in intercept plane, PZi
C*** * 970214 Adopted for CADAC4, PZi
C*** * 980528 Modified LCONV=2,3,4, PZi
C*** * 991108 Removed obsolete guidance and navigation print-out, PZi
C*** *
C*** ******************************************************************
C
      CHARACTER*100 TITLE
      COMMON C(3510)
      COMMON /HCOM/TITLE
C
      DIMENSION VT1EL(3),ST1EL(3),TT1L(3,3),EXX(8),SBEL(3),VBEL(3)
     &,SBTP(3),SBTCP(3),STCTP(3),SBT1L(3),VBT1L(3),VBT1T1(3)
     &,TPT1(3,3),SBT1P(3),TT1P(3,3),SBTT(3),TPL(3,3),TLP(3,3)
     &,SBELM(3),SBBML(3),SBBMP(3),STBMP(3),DUM3(3),SBTL(3)
C
C*** INPUT DATA
C
      EQUIVALENCE (C(1750),MSTOP)
      EQUIVALENCE (C(1751),MTERM)
C
C MSTOP = D =0:No stop, =1:Stop sim when TRcond is violated
C MTERM = G/D >0:SHAZAM, <2:intercept plane, =2:i-plane input
C DEC = I Auxiliary variable initialized to zero
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
      EQUIVALENCE (C(1732),PHIBLX)
      EQUIVALENCE (C(0400),MSEEK)
      EQUIVALENCE (C(0666),EXX(1))
      EQUIVALENCE (C(0800),MGUID)
      EQUIVALENCE (C(0900),MAUT)
      EQUIVALENCE (C(1280),TRCODE)
      EQUIVALENCE (C(1281),TMCODE)
      EQUIVALENCE (C(1300),MPROP)
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1641),DVBE)
      EQUIVALENCE (C(1665),PSIVLX)
      EQUIVALENCE (C(1666),THTVLX)
C
C VT1EL= S Velocity of T1 wrt earth in local level coord - m/s
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C TT1L= O Transf Matrix of T1 body coord wrt local level coord
C PSIT1LX= I/G Heading angle of T1 velocity vector - deg
C THTT1LX= I/G Flight path angle of T1 celocity vector - deg
C PHIT1LX= G Bank angle of normal load factor plane of T1 - deg
C VMACH= O Mach number of missile
C PHIBLX= G Euler roll angle of body wrt local earth axes - deg
C MSEEK= D/G =2:Enable, =3:Acquisition, =4:Lock
C MGUID= D/G =0:No guidance, =3:Midcourse, =6:Terminal
C MAUT= D Two digit flag |lateral|pitch|; see table in C2 Modul
C TRCODE= O Code numbers for stopping conditions -
C TMCODE= I Dummy variable initialized to zero
C MPROP= O Propulsion mode flag =0: Off, =1: On
C SBEL= O Position of vehicle wrt earth ref point E - m
C VBEL= O Velocity of vehicle wrt earth - m/s
C DVBE= S Speed of vehicle wrt earth - m/s
C PSIVLX= S Horizontal flight path angle - deg
C THTVLX= S Vertical flight path angle - deg
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
      MCTRL=INT(MAUT/100.)
      MAUTL=INT((MAUT-MCTRL*100)/10.)
      MAUTP=MAUT-MAUTL*10-MCTRL*100
      EVENT=10000.*MSEEK+1000.*MGUID+100.*MAUTL+10.*MAUTP
     &+1.*MPROP
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
         DEC=DEC*10.       ! rare problem of overflow during SWEEP
         TMCODE=TRCODE
         IF(MSTOP.EQ.1.AND.DBT1.GT.100.) THEN
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
 99   FORMAT(1X,' ',A)
 1    FORMAT('  INTERCEPT TIME =',1PG12.4,
     +'LIMITER VIOLATIONS TRCOND =',1PG12.4)
 2    FORMAT('    DBT=',1PG12.5,'  SBTT1=',1PG12.5,'  SBTT2='
     +,1PG12.5,'  SBTT3=',1PG12.5)
 3    FORMAT('    DBT=',1PG12.5,'  SBTP1=',1PG12.5,'  SBTP2='
     +,1PG12.5,' PSIPTX=',1PG12.5)
 6    FORMAT('   DVBE=',1PG12.5,' PSIVLX=',1PG12.5,' THTVLX='
     +,1PG12.5,' PHIBLX=',1PG12.5)
 7    FORMAT('   DVBT=',1PG12.5,' PSITLX=',1PG12.5,' THTTLX='
     +,1PG12.5,' PHITLX=',1PG12.5)
 8    FORMAT(' *** MSTOP:',' TRCOND =',F4.0,' TIME ='
     +,1PG14.4,' DBT1 =',1PG12.5)
 10   FORMAT (' *** IMPACT OCCURRED IN THE HORIZONTAL PLANE ***')
C
 100  RETURN
      END
C**********************************************************************
      SUBROUTINE G4SHAZ(SBTL,VBT1L)
C**********************************************************************
C*** * Parameters for program SHAZAM
C*** * This subroutine performas the following functions
C*** * (1) Miss distance in plane of closest approach
C*** * (2) Relative missile velocity aspect angles
C*** *
C*** * Argument Input:
C*** *          SBTL(3)=Miss distance components in local axes - m
C*** *          VBT1L(3)=Velocity of missile wrt target in local axes - m
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** *
C*** ******************************************************************
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
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1615),HBE)
C
C CRAD= E Conversion from radians to degree (57.29577951)
C PHIT1L= S Bank angle of normal load factor plane of T1 - rad
C VT1EL= S Velocity of T1 wrt earth in local level coord - m/s
C TT1L= O Transf Matrix of T1 body coord wrt local level coord
C VBEL= O Velocity of vehicle wrt earth - m/s
C HBE= O Height of vehicle above sea level - m
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
C**********************************************************************
      SUBROUTINE S2
C**********************************************************************
C*** * Acquisition Radar Module
C*** * Reserved C(3510) locations are 500-599
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Measures target position and velocity wrt
C*** *     the shooter aircraft in local coordinates
C*** *     (at measurement epoch)
C*** * (2) Sends target update to the guidance processor
C*** *     (at update epoch)
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** * 980212 Removed MNAV=0 statement (Set to zero in C1), PZi
C*** *
C*** ******************************************************************
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
C MGUID= D/G =0:No guidance, =3:Midcourse, =6:Terminal
C T= E Time since launch - s
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
C**********************************************************************
      SUBROUTINE S4I
C**********************************************************************
C*** *  Initialization of INS Module
C*** * Reserved C(3510) locations are 700-799
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Identifies variables for integration
C*** * (2) Initializes INS error states using the Cholesky method
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** * 950720 Adapted for CADAC4 from CADAC6 (PZi)
C*** *
C*** ******************************************************************
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
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(2561),NIP)
      EQUIVALENCE (C(2562),IPL(1))
      EQUIVALENCE (C(2867),IPLV(1))
C
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C SBEL= O Position of vehicle wrt earth ref point E - m
C VBEL= O Velocity of vehicle wrt earth - m/s
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
C*** INITIALIZE STATE VARIABLES
C
       LOCATION=737
       DO I=NIP,NIP+2
          IPL(I)=LOCATION
          IPLV(I)=LOCATION+3
          LOCATION=LOCATION+1
       ENDDO
       NIP=NIP+3
C
       LOCATION=743
       DO I=NIP,NIP+2
          IPL(I)=LOCATION
          IPLV(I)=LOCATION+3
          LOCATION=LOCATION+1
       ENDDO
       NIP=NIP+3
C
       LOCATION=749
       DO I=NIP,NIP+2
          IPL(I)=LOCATION
          IPLV(I)=LOCATION+3
          LOCATION=LOCATION+1
       ENDDO
       NIP=NIP+3
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
C**********************************************************************
      SUBROUTINE S4
C**********************************************************************
C*** * INS Module
C*** * Reserved C(3510) locations are 700-799
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) MINS = 0: Perfect INS
C*** *          = 1: Transfer alignment, gyro and INS errors
C*** * (2) Includes Targeting error
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
C*** * 950720 Adapted for CADAC4 from CADAC6 (PZi)
C*** *
C*** ******************************************************************
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
      EQUIVALENCE (C(0054),AGRAV)
      EQUIVALENCE (C(0125),ST1EL(1))
      EQUIVALENCE (C(1400),FSPB(1))
      EQUIVALENCE (C(1602),SBEL(1))
      EQUIVALENCE (C(1605),VBEL(1))
      EQUIVALENCE (C(1641),DVBE)
      EQUIVALENCE (C(1701),TBL(1,1))
      EQUIVALENCE (C(1710),TLB(1,1))
      EQUIVALENCE (C(1719),WBEB(1))
      EQUIVALENCE (C(2000),T)
C
C REARTH= E Radius of Earth - m
C AGRAV= E Gavity acceleration (9.806635 m/s^2)
C ST1EL= S Position of T1 wrt earthref point E in L coor - m
C FSPB= O Specific force in body coor - N/kg
C SBEL= O Position of vehicle wrt earth ref point E - m
C VBEL= O Velocity of vehicle wrt earth - m/s
C DVBE= S Speed of vehicle wrt earth - m/s
C TBL= O Trans matrix of body wrt local earth axes
C TLB= O Trans matrix of local earth wrt body axes
C WBEB= O Angular velocity of body wrt earth - rad/s
C T= E Time since launch - s
C
C*** OUTPUT TO OTHER MODULES
C
      EQUIVALENCE (C(0703),VBELC(1))
      EQUIVALENCE (C(0706),WBECB(1))
      EQUIVALENCE (C(0715),TBLC(1,1))
      EQUIVALENCE (C(0730),DVBEC)
      EQUIVALENCE (C(0734),FSPCB(1))
      EQUIVALENCE (C(0776),SBELC(1))
C
C VBELC(3) = O Computed vel of body in earth coor - m/s
C WBECB(3) = O Computed ang vel of body wrt earth - rad/s
C TBLC(3,3) = O Comp trans matrix of body wrt earth coor - None
C DVBEC = G Computed body speed wrt earth - m/s
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
         CALL MATUNI(TLLC,3)
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
      RETURN
      END
C**********************************************************************
      SUBROUTINE S4GYRO(EWBEB)
C**********************************************************************
C***  * Gyro subroutine
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
C***  *          EWBEB(3)=Three gyro errors - rad/s
C***  *
C***  * MODIFICATION HISTORY
C***  * 950125 Created by Peter Zipfel
C***  *
C***  *****************************************************************
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
      EQUIVALENCE (C(1400),FSPB(1))
      EQUIVALENCE (C(1719),WBEB(1))
      EQUIVALENCE (C(2664),DER)
      EQUIVALENCE (C(2000),T)
C
C FSPB= O Specific force in body coor - N/kg
C WBEB= O Angular velocity of body wrt earth - rad/s
C DER= E Integration step size - s
C T= E Time since launch - s
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
C*** * Accelerometer subroutine
C*** * This subroutine performs the following functions:
C*** *
C*** * (1) Introduces accelerometer errors: Bias
C*** *                                      Random bias
C*** *                                      Scale factor error
C*** *                                      Accelerometer cluster misalignment
C*** * (2) Outputs acceleration measurements in body axes
C*** *
C*** * Argument Output
C*** *          FSPBCB(3)=Three accelerometer outputs, in body axes - m/s^2
C*** *          EFSPB(3)=Three accel output errors - m/s^2
C*** *
C*** * MODIFICATION HISTORY
C*** * 950125 Created by Peter Zipfel
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
C EWALKA(3) = D Accel random bias - m/s2
C
C*** INPUT FROM OTHER MODULES
C
      EQUIVALENCE (C(1400),FSPB(1))
      EQUIVALENCE (C(2000),T)
C
C FSPB= O Specific force in body coor - N/kg
C T= E Time since launch - s
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
C***  * Altimeter subroutine
C***  * This subroutine performs the following functions:
C***  *
C***  * (1) Intruduces altimeter errors: Bias
C***  *                                  Random
C***  * (2) Outputs altimeter measurement above sea level
C***  *
C***  * MODIFICATION HISTORY
C***  * 920128 Created by Peter Zipfel
C***  *
C***  *****************************************************************
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
      EQUIVALENCE (C(1615),HBE)
C
C HBE= O Height of vehicle above sea level - m
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
 
C***********************************************************************
C*************************** DUMMY RETURNS *****************************
C***********************************************************************
 
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
 
