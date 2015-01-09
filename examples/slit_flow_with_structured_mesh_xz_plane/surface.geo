// 2d box with structured mesh, meant for flow in xz (or rz, 1+3) coorindates

lc = 0.05;  // charateristic mesh length variable
rlength = 2.0; // length of box
zlength = 10.0; // length of box

// setup domain boundaries
Point(1) = {0, 0, 0, lc};
Point(2) = {rlength, 0, 0, lc} ;
Line(3) = {1,2};
Extrude {0,0,zlength} { Line{3}; Layers{ {10}, {1} }; Recombine;}
Physical Line("<outlet>") = {4};
Physical Line("<inlet>") = {3};
Physical Line("<centreline>") = {5};
Physical Line("<circumference>") = {6};
Physical Surface("<flow domain>") = {7};
