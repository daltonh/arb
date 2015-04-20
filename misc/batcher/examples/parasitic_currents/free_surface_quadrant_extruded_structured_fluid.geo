// simple box with sidelengths (xbox,ybox)
// now still split mesh, with two axis of symmetry, from (0,0) to (xbox,ybox)
// and now structured too

Include "free_surface_quadrant_extruded_structured_variables.geo";
//         free_surface_quadrant_extruded_structured_variables.geo

// fluid stuff only in this file

Point(101) = {0, 0, 0, lc};
Point(104) = {0, +ybox, 0, lc};
Line(104) = {104,101} ;
box_centre_free[] = Extrude {xbox, 0, 0} {
  Line{104}; Layers{nlayers}; Recombine;
} ;
Coherence;

Physical Surface("<fluid domain>") = {108} ;
Physical Line("<fluid south>") = {107};
Physical Line("<fluid east>") = {105};
Physical Line("<fluid north>") = {106};
Physical Line("<fluid west>") = {104};
