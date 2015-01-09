// 2d box with unstructured mesh, now relative to an origin

//lc = 0.02;  // charateristic mesh length variable used for y spacing
//lc = 0.1;  // charateristic mesh length variable used for y spacing
lc = 0.05;  // charateristic mesh length variable used for y spacing
originx = 0.0;
originy = 0.0;
ylength = 4.0; // width of box
xlength = ylength; // length of box (now square cells in a square box)

// setup domain boundaries
Point(1) = {originx, originy, 0, lc};
Point(2) = {originx, originy+ylength, 0, lc} ;
Point(3) = {originx+xlength, originy+ylength, 0, lc} ;
Point(4) = {originx+xlength, originy, 0, lc} ;
Line(3) = {1,2};
Line(4) = {2,3};
Line(5) = {3,4};
Line(6) = {4,1};
Physical Line("<west>") = {3};
Physical Line("<east>") = {5};
Physical Line("<south>") = {6};
Physical Line("<north>") = {4};
Line Loop(7) = {4, 5, 6, 3};
Plane Surface(8) = {7};
Physical Surface("<flow domain>") = {8};
