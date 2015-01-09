// 2d box with structured mesh, nonuniform to resolve more detail within the double layer

lc = 0.025;  // charateristic mesh length variable used for y spacing
ylength = 1.0; // width of box
xlength = ylength; // length of box (now square cells in a square box)
nlayerx = Floor(xlength/lc+1.e-10); // number of cells in the x direction

// setup domain boundaries
Point(1) = {0, 0, 0, lc};
Point(2) = {0,  ylength, 0, lc/10} ;
Line(3) = {1,2};
Extrude {xlength,0,0} { Line{3}; Layers{ {nlayerx}, {1} }; Recombine;}
Physical Line("<outlet>") = {4};
Physical Line("<inlet>") = {3};
Physical Surface("<flow domain>") = {7};
Physical Line("<centreline>") = {5};
