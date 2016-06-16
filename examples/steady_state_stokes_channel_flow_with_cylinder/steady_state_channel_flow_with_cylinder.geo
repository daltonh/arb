// 2d_channel_with_cylinder

lc = 0.10;  // charateristic mesh length variable, coarser here than in tutorial_3

// setup domain boundaries
Point(1) = {0, 0, 0, lc/2};
Point(15) = {0.2, 0, 0, lc/4};
Point(2) = {2.2, 0,  0, lc} ;
Point(3) = {2.2, 0.41, 0, lc} ;
Point(16) = {0.2, 0.41, 0, lc/4};
Point(4) = {0,  0.41, 0, lc/2} ;

Line(1) = {1,15} ;
Line(15) = {15,2} ;
Line(2) = {2,3} ;
Line(3) = {3,16} ;
Line(16) = {16,4} ;
Line(4) = {4,1} ;

// create an elementary entity that is the domain boundary
Line Loop(5) = {1,15,2,3,16,4} ;

// create the physical entities for the inlet and output which become the arb regions
Physical Line("<inlet>") = {4};
Physical Line("<outlet>") = {2};

// create the cylinder
Point(5) = {0.2, 0.15, 0, lc/4};
Point(6) = {0.25, 0.2, 0, lc/4};
Point(7) = {0.2, 0.25, 0, lc/4};
Point(8) = {0.15, 0.2, 0, lc/4};
Point(9) = {0.2, 0.2, 0, lc/4};

Ellipse(7) = {5, 9, 9, 6};
Ellipse(8) = {6, 9, 9, 7};
Ellipse(9) = {7, 9, 9, 8};
Ellipse(10) = {8, 9, 9, 5};

// create an elementary entity that is the cylinder boundary
Line Loop(11) = {7, 8, 9, 10};

// create the physical entity for the cylinder boundary which becomes the arb region
Physical Line("<cylinder>") = {7, 8, 9, 10};

// all of the flow domain must be included as a physical entity to be output under gmsh
Plane Surface(12) = {5, 11};
Physical Surface("<flow domain>") = {12};
