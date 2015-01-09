// 2d box with structured mesh

lc = 0.05;  // charateristic mesh length variable used for y spacing
xlength = 5.0; // length of box
ylength = 0.5; // width of box

// setup domain boundaries
Point(1) = {0, 0, 0, lc};
Point(2) = {0,  ylength, 0, lc} ;
Line(3) = {1,2};
Extrude {xlength,0,0} { Line{3}; Layers{ {10}, {1} }; Recombine;}
Physical Line("<outlet>") = {4};
Physical Line("<inlet>") = {3};
Physical Surface("<flow domain>") = {7};
Physical Line("<centreline>") = {5};
