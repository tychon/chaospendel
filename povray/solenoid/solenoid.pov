#include "colors.inc"
#include "textures.inc"
#include "solenoid.inc"

camera {
  location <0.7, 0.7, -1>
  look_at <0, 0, 0>
}

light_source {
  <0, 10, -8>
  color White
}

plane {
  y, -0.5
  pigment {
    checker
    color Black
    color White
  }
  finish {
    reflection 0.3
  }
}

object { Solenoid (1, 0.5, 10, 0.0001) }
object {
  Solenoid (1, 0.5, 20, 0.0001)
  translate <-1, 0, 0>
}

