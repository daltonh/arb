// simple box with sidelengths (xbox,ybox)
// now still split mesh, with two axis of symmetry, from (0,0) to (xbox,ybox)
// and now structured too

Include "free_surface_quadrant_nonextruded_unstructured_variables.geo";

// fluid stuff only in this file

Point(1) = {0, 0, 0, lc};
Point(2) = {+xbox, 0, 0, lc};
Point(3) = {+xbox, +ybox, 0, lc};
Point(4) = {0, +ybox, 0, lc};
Line(1) = {1,2} ;
Line(2) = {2,3} ;
Line(3) = {3,4} ;
Line(4) = {4,1} ;

Line Loop(1) = {1:4};
Plane Surface(1) = {1};

Physical Surface("<fluid domain>") = {1} ;
Physical Line("<fluid south>") = {1};
Physical Line("<fluid east>") = {2};
Physical Line("<fluid north>") = {3};
Physical Line("<fluid west>") = {4};
