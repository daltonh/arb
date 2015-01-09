// simple box with sidelengths (xbox,ybox)
// now split mesh, but with centreline on left

Geometry.AutoCoherence=0;

lc = 0.150;
xbox = 2;
ybox = 2;
edge_nd = 10; // use 3 + <ls_phi_max> (1) + 3*<kernel_separation> (2) ~ 10 //
edge = edge_nd*lc;
xboxe = xbox+edge;
yboxe = ybox+edge;

Point(1) = {0, -ybox, 0, lc};
Point(2) = {+xbox, -ybox, 0, lc};
Point(3) = {+xbox, +ybox, 0, lc};
Point(4) = {0, +ybox, 0, lc};

Point(5) = {0, -yboxe, 0, lc};
Point(6) = {+xboxe, -yboxe, 0, lc};
Point(7) = {+xboxe, +yboxe, 0, lc};
Point(8) = {0, +yboxe, 0, lc};

Line(1) = {1,2} ;
Line(2) = {2,3} ;
Line(3) = {3,4} ;
Line(4) = {4,1} ;

Line(5) = {5,6} ;
Line(6) = {6,7} ;
Line(7) = {7,8} ;
Line(8) = {8,4} ;
Line(9) = {1,5} ;

Line Loop(5) = {1,2,3,4} ;
Line Loop(6) = {5,6,7,8,-3,-2,-1,9} ;

Plane Surface(6) = {5} ;
Plane Surface(9) = {6};

Point(11) = {0, -ybox, 0, lc};
Point(12) = {+xbox, -ybox, 0, lc};
Point(13) = {+xbox, +ybox, 0, lc};
Point(14) = {0, +ybox, 0, lc};

Line(21) = {11,12} ;
Line(22) = {12,13} ;
Line(23) = {13,14} ;
Line(24) = {14,11} ;

Line Loop(7) = {21,22,23,24} ;
Plane Surface(11) = {7} ;

Periodic Surface 11 { 21, 22, 23, 24 } = 6 {1,2,3,4};

Physical Surface("<level set centre>") = {6} ;
Physical Line("<level set top>") = {7};
Physical Line("<level set bottom>") = {5};
Physical Line("<level set east>") = {6};
Physical Line("<level set west>") = {8,9};
Physical Line("<level set centre top>") = {3};
Physical Line("<level set centre bottom>") = {1};
Physical Line("<level set centre east>") = {2};
Physical Line("<level set centre west>") = {4};
Physical Surface("<level set edge>") = {9};

Physical Surface("<fluid domain>") = {11};
Physical Line("<fluid top>") = {23};
Physical Line("<fluid bottom>") = {21};
Physical Line("<fluid east>") = {22};
Physical Line("<fluid west>") = {24};



