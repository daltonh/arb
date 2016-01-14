// simple box with sidelengths (xbox,ybox)
// now still split mesh, with two axis of symmetry, from (0,0) to (xbox,ybox)
// and now structured too

Include "free_surface_quadrant_extruded_structured_variables.geo";
//         free_surface_quadrant_extruded_structured_variables.geo

// fluid stuff only in this file

Point(1) = {0, 0, 0, lc};
Point(4) = {0, +ybox, 0, lc};
Line(4) = {4,1} ;
box_centre_free[] = Extrude {xbox, 0, 0} {
  Line{4}; Layers{nlayers}; Recombine;
} ;
Coherence;

Physical Surface("<fluid domain>") = {8} ;
Physical Line("<fluid south>") = {7};
Physical Line("<fluid east>") = {5};
Physical Line("<fluid north>") = {6};
Physical Line("<fluid west>") = {4};
