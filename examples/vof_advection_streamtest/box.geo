// 2d box with structured mesh, now relative to an origin

//lc = 0.02;  // charateristic mesh length variable used for y spacing
lc = 0.1;  // charateristic mesh length variable used for y spacing
//lc = 0.05;  // charateristic mesh length variable used for y spacing
originx = 0.0;
originy = 0.0;
ylength = 4.0; // width of box
xlength = ylength; // length of box (now square cells in a square box)
nlayers = Floor(xlength/lc+1.e-10); // number of cells in the x direction is set to be the same as the number in the y direction

// setup domain boundaries
Point(1) = {originx, originy, 0, lc};
Point(2) = {originx, originy+ylength, 0, lc} ;
Line(3) = {1,2};
Extrude {xlength,0,0} { Line{3}; Layers{ {nlayers}, {1} }; Recombine;}
Physical Line("<east>") = {4};
Physical Line("<west>") = {3};
Physical Line("<south>") = {5};
Physical Line("<north>") = {5};
Physical Surface("<flow domain>") = {7};
