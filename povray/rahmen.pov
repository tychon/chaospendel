#include "colors.inc"
#include "textures.inc"

#include "aluminium_profile.inc"

camera {
  location <50, 50, -50>
  look_at <0, 0, 0>
}

light_source {
  <50, 50, -50>
  color White
  area_light <10, 0, 0>, <0, 10, 0>, 30, 30
  adaptive 1
  jitter
}

plane {
  y, 0
  pigment {
    checker
    color Black
    color White
    scale 10
  }
  finish { reflection 0.5 }
}

object {
  alprofile
  scale <0, 0, 100>
  rotate <0, -90, 0>
  translate <50, 0, 0>
}

