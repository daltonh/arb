// simple box with sidelengths (xbox,ybox)
// now still split mesh, with two axis of symmetry, from (0,0) to (xbox,ybox)
// and now structured too

Include "free_surface_quadrant_extruded_structured_variables.geo";
//         free_surface_quadrant_extruded_structured_variables.geo

// free surface stuff only in this file

Point(1) = {0, 0, 0, lc};
Point(4) = {0, +ybox, 0, lc};
Line(4) = {4,1} ;
box_centre_free[] = Extrude {xbox, 0, 0} {
  Line{4}; Layers{nlayers}; Recombine;
} ;

box_top_free[] =  Extrude {0, edge, 0} {
  Line{box_centre_free[3]}; Layers{nlayerse}; Recombine;
} ;

box_right_free[] =  Extrude {edge, 0, 0} {
  Line{box_centre_free[0],box_top_free[4]}; Layers{nlayerse}; Recombine;
} ;
Coherence;

Physical Surface("<free surface centre>") = {8} ;
Physical Surface("<free surface edge>") = {12,20,16};
Physical Line("<free surface north>") = {9,18};
Physical Line("<free surface south>") = {15};
Physical Line("<free surface east>") = {17,13};
Physical Line("<free surface west>") = {11};
Physical Line("<free surface centre south>") = {7};
Physical Line("<free surface centre east>") = {5};
Physical Line("<free surface centre north>") = {6};
Physical Line("<free surface centre west>") = {4};
