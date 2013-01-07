
#ifndef _PROJECTREADER_H
#define _PROJECTREADER_H

#define PROJECTDATASOLS_LENGTH 9
#define IDX_RADIUS 0
#define IDX_ANGLE 1
#define IDX_COILS 2
#define IDX_SELF_RESISTANCE 3
#define IDX_PARALLEL_RESISTANCE 4
#define IDX_MEAN 5
#define IDX_STD_DEVIATION 6
#define IDX_NOISEMIN 7
#define IDX_NOISEMAX 8

struct projectdata {
  // use 'readPendulumData' for these values
  // lengths of pendulum
  double l1a, l1b, l1, l2a, l2b;
  // positions of magnets
  double l1m, l2m;
  
  // range of unnormalized input values
  double inputrangemin, inputrangemax, noisefactor;
  double integralresetsamples, integralmax;
  
  // constants of differentials
  // these are read with 'readCalibrationData'
  double k1, k2, k3, k4, k5;
  
  // solenoids
  int solnum; // number of solenoids
  // array of solenoids (with length 'solnum'), where the second array index is:
  // read by 'readPendulumData':
  // 0: radius in meters
  // 1: angle in radians
  // 2: number of turns
  // 3: self resistance of the solenoid in ohm
  // 4: parallel resistance in ohm
  // read by 'readNormalisationData':
  // 5: zero level
  // 6: standard deviation
  // 7: noise minimum
  // 8: noise maximum
  double **sols;
};
typedef struct projectdata projectdata;

projectdata *readPendulumData(projectdata*, const char *filepath);
projectdata *readCalibrationData(projectdata*, const char *filepath);
projectdata *readNormalisationData(projectdata*, const char *filepath);

#endif // _PROJECTREADER_H

