// simple box with sidelengths (xbox,ybox)
// now still split mesh, with two axis of symmetry, from (0,0) to (xbox,ybox)
// and now structured too

//Geometry.AutoCoherence=0;

lc = 0.125;
//the following is for batcher use
//lc = <<lc>>;
xbox = 2;
ybox = 2;
nlayers = Floor(xbox/lc+1.e-10);
dx=xbox/nlayers;
edge_nd = 10; // use 3 + <ls_phi_max> (1) + 3*<kernel_separation> (2) ~ 10 //
edge = edge_nd*dx;
xboxe = xbox+edge;
yboxe = ybox+edge;
nlayerse = edge_nd;

