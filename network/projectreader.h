
#ifndef _PROJECTREADER_H
#define _PROJECTREADER_H

struct projectdata {
  // lengths of pendulum
  double l1a, l1b, l1, l2a, l2b;
  // positions of magnets
  double l1m, l2m;
  
  // constants of differentials
  double k1, k2, k3, k4;
  
  // solenoids
  int solnum; // number of solenoids
  // array of solenoids (with length 'solnum'), where the second array index is:
  // 0: radius
  // 1: angle in radians
  // 3: zero level
  // 4: normalization factor
  double *sols[4];
};
typedef struct projectdata projectdata;

projectdata *readPendulumData(projectdata*, char *filepath);
projectdata *readCalibrationData(projectdata*, char *filepath);
projectdata *readZeroLevelData(projectdata*, char *filepath);

#endif // _PROJECTREADER_H

