// 2d box with structured mesh

lc = 0.025;  // charateristic mesh length variable used for y spacing
ylength = 1.0; // width of box
xlength = ylength; // length of box (now square cells in a square box)
nlayers = Floor(xlength/lc+1.e-10); // number of cells in the x direction is set to be the same as the number in the y direction

// setup domain boundaries
Point(1) = {0, 0, 0, lc};
Point(2) = {0,  ylength, 0, lc} ;
Line(3) = {1,2};
Extrude {xlength,0,0} { Line{3}; Layers{ {nlayers}, {1} }; Recombine;}
Physical Line("<outlet>") = {4};
Physical Line("<inlet>") = {3};
Physical Surface("<flow domain>") = {7};
Physical Line("<centreline>") = {5};
