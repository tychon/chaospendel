#include "colors.inc"
#include "textures.inc"

#declare alprofile =
difference {
  box { <0, 0, 0> <4.5, 4.5, 1> }
  // middle hole
  box { <1.65, 1.65, -0.1> <4.5-1.65, 4.5-1.65, 1.1> }
  // cutouts
  box { <1.8, -0.1, -0.1> <2.7, 1.5, 1.1> } // lower
  box { <3, 1.8, -0.1> <4.6, 2.7, 1.1> } // right
  box { <1.8, 3, -0.1> <2.7, 4.6, 1.1> } // upper
  box { <-0.1, 1.8, -0.1> <1.5, 2.7, 1.1> } // left
  // holes
  box { <0.15, 0.15, -0.1> <1.4, 1.4, 1.1> } // lower left
  box { <3.1, 0.15, -0.1> <4.4, 1.4, 1.1> } // lower right
  box { <3.1, 3.1, -0.1> <4.4, 4.4, 1.1> } // upper right
  box { <0.15, 3.1, -0.1> <1.4, 4.4, 1.1> } // upper left
  // textures
  texture { Chrome_Metal }
  finish { reflection 0.05 }
}

