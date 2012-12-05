#include "colors.inc"
#include "textures.inc"

#include "aluminium_profile.inc"

#declare depth = 15;
#declare depth_overspan = 10;

#declare solenoid_r1 = 10;
#declare solenoid_r2 = 27;
#declare solenoid_r3 = 46;

#declare phi1 = 10;
#declare phi2 = 100;

#macro toRad (deg) (deg / 360 * (2*pi)) #end

camera {
  location <70*cos(2*pi*clock), 50, ((-100)*abs(sin(2*pi*clock)))>
  look_at <0, 40, 0>
}
/* camera { // back view
  location <10, 30, 110>
  look_at <0, 10, 0>
} */
/* camera { // side view
  location <80, 50, 0>
  look_at <0, 25, 0>
} */

/*
// some fast lights
light_source {
  <-30, 10, -50>
  color White
}
light_source {
  <50, 50, -50>
  color White
}
*/

light_source {
  <-30, 10, -50>
  color White
  area_light <10, 0, 0>, <0, 0, 10>, 5, 5
  adaptive 1
  jitter
}
light_source {
  <60, 60, -50>
  color White
  area_light <10, 0, 0>, <0, 10, 0>, 5, 5
  adaptive 1
  jitter
}

/* light_source { // backlight
  <-30, 10, 50>
  color White
  area_light <10, 0, 0>, <0, 0, 10>, 5, 5
  adaptive 1
  jitter
} */

// */

//* // slows things down
plane {
  y, -4.5
  pigment {
    checker
    color Black
    color White
    scale 10
  }
  finish { reflection 0.5 }
}
// */

// pendulums
#declare pendulum1 =
box {
  <-2, 10, 0> <2, -30, 0.5>
  rotate <0, 0, phi1>
  translate <0, 50+3.18, 0>
  texture { Brushed_Aluminum }
}

object {
  pendulum1
  translate <0, 0, depth/2.0-1.5>
}
object {
  pendulum1
  translate <0, 0, depth/2.0+1>
}

box {
  <-2, 11, 0> <2, -19, 0.5>
  rotate <0, 0, phi2>
  translate <sin(toRad(phi1))*29.85, 50+3.18+cos(phi1/360*(2*pi))*(-29.85), depth/2.0-0.25>
  texture { Brushed_Aluminum }
}

cylinder {
  <0, 50+3.18, -10> <0, 50+3.18, depth+10> 0.8
  texture { Brushed_Aluminum }
}


////////
//// lower rect

// front
object {
  alprofile
  scale <1, 1, 100>
  rotate <0, 90, 0>
  translate <-50, -4.5, 0>
}
// back
object {
  alprofile
  scale <1, 1, 100>
  rotate <0, -90, 0>
  translate <50, -4.5, depth>
}
// left
object {
  alprofile
  scale <1, 1, depth+2*4.5+2*depth_overspan>
  translate <-50-4.5, -4.5, -depth_overspan-4.5>
}
// right
object {
  alprofile
  scale <1, 1, depth+2*4.5+2*depth_overspan>
  translate <50, -4.5, -depth_overspan-4.5>
}

///////
// lower rect angles
// back right
object {
  squareangle
  rotate <0, 0, 90>
  translate <50, -4.25, depth-4>
}
// front right
object {
  squareangle
  rotate <90, 0, 90>
  translate <50-4, -4.25, 0>
}
// back left
object {
  squareangle
  rotate <-90, 0, 90>
  translate <-50+4, -4.25, depth>
}
// back front
object {
  squareangle
  rotate <180, 0, 90>
  translate <-50, -4.25, 4>
}


//////////////
//// triangles

#declare rectsidesmall =
difference {
  object {
    alprofile
    scale <1, 1, 70.71+4.5>
  }
  box {
    <-0.1, -10, -10> <4.6, 0, 10>
    rotate <45, 0, 0>
    translate <0, 0, 4.5>
  }
  translate <0, -4.5, 0>
}
#declare rectsidelong =
difference {
  object {
    alprofile
    scale <1, 1, 70.71+2*4.5>  // <<<
  }
  box {
    <-0.1, -10, -10> <4.6, 0, 10>
    rotate <45, 0, 0>
    translate <0, 0, 4.5>
  }
  translate <0, -4.5, 0>
}

#declare twosides =
union {
  object {
    rectsidesmall
    rotate <0, 90, 45>
    translate <-50-6.36, 0, 0>
  }
  object {
    rectsidelong
    rotate <0, -90, -45>
    translate <50+6.36, 0, -4.5>
  }
  object {
    squareangle
    rotate <0, 90, 135>
    translate <2.83843, 50-2.83843, -0.25>
  }
}

object {
  twosides
}
object {
  twosides
  translate <0, 0, +4.5+depth>
}

object {
  squareangle
  translate <-50-4-0.25, 0, -4-4.5>
  //pigment {color Red }
}
object {
  squareangle
  translate <50+0.25, 0, -4-4.5>
  //pigment {color Red }
}
object {
  squareangle
  rotate <0, 180, 0>
  translate <50+4+0.25, 0, depth+4.5+4>
}
object {
  squareangle
  rotate <0, 180, 0>
  translate <-50-0.25, 0, depth+4.5+4>
}

///////////////
//// back plate

box {
  <-50, -4.5, depth+4.5> <50, 100, depth+4.5+0.5>
  pigment { Tan_Wood }
}

#declare solenoidbox =
union {
  box { <0, 4.5, 0>, <4.6, 6.7, 4> }
  box {
    0, <4.6, 4.5, 4>
    pigment { color Grey }
  }
  box {
    <1.25, 1.25, -0.5>, <3.35, 3.35, 4.2>
    texture { Rusty_Iron }
  }
  translate <-2.3, -2.25, 0>
}

#local theta = 0;
#while(theta < 360)
  object {
    solenoidbox
    pigment { color DarkGreen }
    translate <0, solenoid_r1, 0>
    rotate <0, 0, theta>
    translate <0, 50+3.18, depth>
  }
  #local theta = theta + 360/4;
#end // while

#local theta = 0;
#while(theta < 360)
  object {
    solenoidbox
    pigment { color DarkGreen }
    translate <0, solenoid_r2, 0>
    rotate <0, 0, theta+360/16>
    translate <0, 50+3.18, depth>
  }
  #local theta = theta + 360/8;
#end // while

#local theta = 0;
#while(theta < 360)
  object {
    solenoidbox
    pigment { color Red }
    translate <0, solenoid_r3, 0>
    rotate <0, 0, theta>
    translate <0, 50+3.18, depth>
  }
  #local theta = theta + 360/12;
#end // while


