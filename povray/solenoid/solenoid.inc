
// solenoid.inc
// Author: Hannes Riechert
// Date  : 2012-10-09
// Requires: textures.inc
/* This file defines a macro to create coils consisting of spheres.
 * These coils have a length (from <0, 0, 0> to <0, 0, length>)
 * and a total radius (including all of the wire).
 * n is the number of curls.
 * solWireStep is the distance moved along the z axis for every sphere.
 * example values:
 * solLength = 1;
 * solRadius = 0.5;
 * n = 10;
 * solWireStep = 0.0001;
 */

#macro Solenoid (length, rad, n, wireStep)
  union {
    #local wireMinRad = (length/n) /2;
    #local wireMajRad = rad - wireMinRad;
    
    #local x1 = wireMinRad;
    #local deltaTheta = 2*pi / (2*wireMinRad / wireStep);
    #local theta = 0;
    #while(x1 <= length-wireMinRad)
      sphere{ <wireMajRad*sin(theta), wireMajRad*cos(theta), x1>, wireMinRad }
      #local x1 = x1 + wireStep;
      #local theta = theta + deltaTheta;
    #end // while
    
    texture{Copper_Metal}
    finish {
      reflection 0.05
    }
  }
#end // macro

