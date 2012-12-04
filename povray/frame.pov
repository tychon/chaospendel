#include "colors.inc"
#include "textures.inc"

#include "aluminium_profile.inc"

camera {
  location <40, 40, -90>
  look_at <0, 25, 0>
}

light_source {
  <30, 30, -50>
  color White
  area_light <10, 0, 0>, <0, 10, 0>, 5, 5
  adaptive 1
  jitter
}
light_source {
  <-50, 10, -20>
  color White
  area_light <10, 0, 0>, <0, 10, 0>, 5, 5
  adaptive 1
  jitter
}


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


#declare depth = 20;
#declare depth_overspan = 10;

//////
// some marks
sphere {
  <0, 0, 0>, 1
  texture { Ruby_Glass }
}

sphere {
  <-50, 0, 0>, 1
  texture { Jade }
}
sphere {
  <50, 0, 0>, 1
  texture { Jade }
}
sphere {
  <-50, 0, depth>, 1
  texture { Jade }
}
sphere {
  <50, 0, depth>, 1
  texture { Jade }
}

sphere {
  <-50, 0, depth+4.5+depth_overspan>, 1
  texture { Copper_Metal }
}
sphere {
  <-50, 0, -4.5-depth_overspan>, 1
  texture { Copper_Metal }
}
sphere {
  <50, 0, depth+4.5+depth_overspan>, 1
  texture { Copper_Metal }
}
sphere {
  <50, 0, -4.5-depth_overspan>, 1
  texture { Copper_Metal }
}

cylinder {
  <0, 50+3.18, -10> <0, 50+3.18, depth+10> 0.8
  texture { Ruby_Glass }
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


// triangles

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
  translate <0, -4.5 0,>
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
  translate <0, -4.5 0,>
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
}

object {
  twosides
}
object {
  twosides
  translate <0, 0, +4.5+depth>
}

