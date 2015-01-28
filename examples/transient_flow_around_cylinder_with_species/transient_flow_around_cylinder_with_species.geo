// 2d_channel_with_cylinder
// actually modified from the mesh used in the turbulence problem, but without the boundary layers on the (now free-slip) walls

lc = 0.15;  // charateristic mesh length variable
lcf = 0.01;  // finer mesh length
lcbl = 0.005; // boundary layer finest cell thickness
// centre and radius of cylinder
xcylinder = 2.;
ycylinder = 0.0; // now on centreline
rcylinder = 0.2;
// size of housing box
xbox = 5;
ybox = 2;

// setup domain boundaries
Point(1) = {0, -ybox/2, 0, lc};
Point(15) = {xcylinder, -ybox/2, 0, lc/2};
Point(2) = {xbox, -ybox/2,  0, lc} ;
Point(3) = {xbox,    ybox/2, 0, lc} ;
Point(16) = {xcylinder, ybox/2, 0, lc/2};
Point(4) = {0,  ybox/2, 0, lc} ;

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
Point(5) = {xcylinder, ycylinder-rcylinder, 0, lcf};
Point(6) = {xcylinder+rcylinder, ycylinder, 0, lcf};
Point(7) = {xcylinder, ycylinder+rcylinder, 0, lcf};
Point(8) = {xcylinder-rcylinder, ycylinder, 0, lcf};
Point(9) = {xcylinder, ycylinder, 0, lcf};

Ellipse(7) = {5, 9, 9, 6};
Ellipse(8) = {6, 9, 9, 7};
Ellipse(9) = {7, 9, 9, 8};
Ellipse(10) = {8, 9, 9, 5};

Field[1] = BoundaryLayer;
Field[1].EdgesList = {7, 8, 9, 10};
Field[1].NodesList = {5, 6, 7, 8, 5};
//Field[1].EdgesList = {7, 8, 9, 10,16,3,1,15};
//Field[1].NodesList = {5, 6, 7, 8, 5,4,16,3,1,15,2};
Field[1].hfar = lc;
Field[1].hwall_n = lcbl;
Field[1].hwall_t = lcbl*5;
Field[1].thickness = 0.1;
Field[1].ratio = 1.2;
Field[1].Quads = 1;
BoundaryLayer Field = 1;

// create an elementary entity that is the cylinder boundary
Line Loop(11) = {7, 8, 9, 10};

// create the physical entity for the cylinder boundary which becomes the arb region
Physical Line("<cylinder>") = {7, 8, 9, 10};

// all of the flow domain must be included as a physical entity to be output under gmsh
Plane Surface(12) = {5, 11};
Physical Surface("<flow domain>") = {12};
