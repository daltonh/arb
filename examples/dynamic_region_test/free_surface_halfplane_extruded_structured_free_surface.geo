// simple box with sidelengths (xbox,ybox)
// now still split mesh, with two axis of symmetry, from (0,0) to (xbox,ybox)
// and now structured too

Include "free_surface_halfplane_extruded_structured_variables.geo";
//         free_surface_halfplane_extruded_structured_variables.geo

// free surface stuff only in this file

Point(1) = {0, -ybox, 0, lc};
Point(4) = {0, +ybox, 0, lc};
Line(4) = {4,1} ;
box_centre[] = Extrude {xbox, 0, 0} {
  Line{4}; Layers{nlayers}; Recombine;
} ;

box_top[] =  Extrude {0, edge, 0} {
  Line{box_centre[3]}; Layers{nlayerse}; Recombine;
} ;

box_bot[] =  Extrude {0, -edge, 0} {
  Line{box_centre[2]}; Layers{nlayerse}; Recombine;
} ;

box_right[] =  Extrude {edge, 0, 0} {
  Line{box_centre[0],box_top[4],box_bot[2]}; Layers{nlayerse}; Recombine;
} ;
Coherence;

Physical Surface("<free surface centre>") = {8} ;
Physical Surface("<free surface edge>") = {12,24,20,16,28};
Physical Line("<free surface north>") = {9,22};
Physical Line("<free surface south>") = {13,27};
Physical Line("<free surface east>") = {21,17,25};
Physical Line("<free surface west>") = {14,11};
Physical Line("<free surface centre south>") = {7};
Physical Line("<free surface centre east>") = {5};
Physical Line("<free surface centre north>") = {6};
Physical Line("<free surface centre west>") = {4};
