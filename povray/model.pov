#include "colors.inc"
#include "textures.inc"
#include "solenoid/solenoid.inc"

#declare l1 = 7*4;
#declare l2 = 7*7;
#declare phi1 = 20;
#declare phi2 = 100;

camera {
  location <30, -l1-20, 0>
  look_at <0, 0, 0>
}

light_source {
  <-l2, l2, -120>
  color White
  area_light <5, 0, 0>, <0, 0, 5>, 10, 10
  adaptive 1
  jitter
}
light_source {
  <l2, l2, -120>
  color White
  area_light <5, 0, 0>, <0, 0, 5>, 10, 10
  adaptive 1
  jitter
}

plane {
  y, -l2-6
  pigment {
    checker
    color Black
    color White
    scale 100
  }
  finish {
    reflection 0.5
  }
}

// solenoids
#local theta = 0;
#while (theta < 360)
  object {
    Solenoid (4, 3, 6, 0.001)
    translate <0, l1, 0>
    rotate <0, 0, theta>
  }
  object {
    Solenoid (4, 3, 9, 0.0001)
    translate <0, l2, 0>
    rotate <0, 0, theta>
  }
  #local theta = theta + 30;
#end // while

// pendulum 1
cylinder {
  <0, 0, 0> <0, 0, -6> 1
  texture { Ruby_Glass }
}

box {
  <-2, 1, -1> <2, -l1-1, -2>
  texture { Brushed_Aluminum }
  rotate <0, 0, phi1>
}
box {
  <-2, 1, -4> <2, -l1-1, -5>
  texture { Brushed_Aluminum }
  rotate <0, 0, phi1>
}
cylinder {
  <0, 0, 0> <0, 0, -6> 1
  texture { Ruby_Glass }
  translate <0, -l1, 0>
  rotate <0, 0, phi1>
}

box {
  <-2, 1, -2.5> <2, -(l2-l1)-1, -3.5>
  texture { Brushed_Aluminum }
  rotate <0, 0, phi2>
  translate <0, -l1, 0>
  rotate <0, 0, phi1>
}

