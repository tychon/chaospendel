
#ifndef _PROJECTREADER_H
#define _PROJECTREADER_H

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
  // 2: zero level (read by 'readNormalisationData')
  // 3: standard deviation (read by 'readNormalisationData')
  // 4: normalization factor (??? TODO)
  double **sols;
};
typedef struct projectdata projectdata;

projectdata *readPendulumData(projectdata*, const char *filepath);
projectdata *readCalibrationData(projectdata*, const char *filepath);
projectdata *readNormalisationData(projectdata*, const char *filepath);

#endif // _PROJECTREADER_H

