
function cutted = periodic(X, xoff, xstart, xend)
  cutted = (X+xoff) / (xend-xstart);
endfunction

# X, Y, Z periodic in [0; 1]
function [QX, QY, QZ] = toTorus(X, Y, Z, R, r)
  alphaX = X .* 2 .* pi;
  alphaY = Y .* 2 .* pi;
  rZ = Z .* r;
  
  PX = rZ .* cos(alphaY);
  PY = 0;
  PZ = rZ .* sin(alphaY);
  
  QX = (PX+R) .* cos(alphaX);
  QY = (PX+R) .* sin(alphaX);
  QZ = PZ;
endfunction

function plotTorus(X, Y, Z, R, r)
  [BX, BY, BZ] = toTorus([0:0.001:1],[0:0.1:100],ones(1,1001), R, r);
  [X, Y, Z] = toTorus(X, Y, Z, R, r);
  plot3(X,Y,Z, "-r;;", BX, BY, BZ, ".b;;");
  axis([-R-r,R+r,-R-r,R+r,-R-r,R+r]);
endfunction


