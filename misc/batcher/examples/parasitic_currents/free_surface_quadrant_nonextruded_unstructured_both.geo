// simple box with sidelengths (xbox,ybox)
// now still split mesh, with two axis of symmetry, from (0,0) to (xbox,ybox)

// this will bring in the variables and create the fluid mesh
Include "free_surface_quadrant_nonextruded_unstructured_fluid.geo";

// duplicate the fluid mesh with expansions too

Point(11) = {0, 0, 0, lc};
Point(12) = {+xbox, 0, 0, lc/2};
Point(13) = {+xbox, +ybox, 0, lc};
Point(14) = {0, +ybox, 0, lc};
Line(11) = {11,12} ;
Line(12) = {12,13} ;
Line(13) = {13,14} ;
Line(14) = {14,11} ;

Line Loop(11) = {11:14};
Plane Surface(11) = {11};

Periodic Surface 11 { 11:14 } = 1 { 1:4 };

Point(15) = {+xboxe, 0, 0, lc};
Point(16) = {+xboxe, +ybox, 0, lc};
Point(17) = {+xboxe, +yboxe, 0, lc};
Point(18) = {+xbox, +yboxe, 0, lc};
Point(19) = {0, +yboxe, 0, lc};

Line(15) = {12,15} ;
Line(16) = {15,16} ;
Line(17) = {16,17} ;
Line(18) = {17,18} ;
Line(19) = {18,19} ;
Line(20) = {19,14} ;

Line Loop(12) = {15:20,-13,-12};
Plane Surface(12) = {12};

Physical Surface("<free surface centre>") = {11} ;
Physical Line("<free surface centre south>") = {11};
Physical Line("<free surface centre east>") = {12};
Physical Line("<free surface centre north>") = {13};
Physical Line("<free surface centre west>") = {14};
Physical Surface("<free surface edge>") = {12};
Physical Line("<free surface north>") = {18,19};
Physical Line("<free surface south>") = {15};
Physical Line("<free surface east>") = {16,17};
Physical Line("<free surface west>") = {20};
