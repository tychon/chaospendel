
#ifndef _PROJECTREADER_H
#define _PROJECTREADER_H

#define IDX_RADIUS 0
#define IDX_ANGLE 1
#define IDX_COILS 2
#define IDX_MEAN 3
#define IDX_STD_DEVIATION 4

struct projectdata {
  // use 'readPendulumData' for these values
  // lengths of pendulum
  double l1a, l1b, l1, l2a, l2b;
  // positions of magnets
  double l1m, l2m;
  
  // constants of differentials
  // these are read with 'readCalibrationData'
  double k1, k2, k3, k4, k5;
  
  // solenoids
  int solnum; // number of solenoids
  // array of solenoids (with length 'solnum'), where the second array index is:
  // 0: radius (read by 'readPendulumData')
  // 1: angle in radians (read by 'readPendulumData')
  // 2: number of turns (read by 'readPendulumData')
  // 3: zero level (read by 'readNormalisationData')
  // 4: standard deviation (read by 'readNormalisationData')
  double **sols;
};
typedef struct projectdata projectdata;

projectdata *readPendulumData(projectdata*, const char *filepath);
projectdata *readCalibrationData(projectdata*, const char *filepath);
projectdata *readNormalisationData(projectdata*, const char *filepath);

#endif // _PROJECTREADER_H

