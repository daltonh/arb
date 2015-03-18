lc = 0.003;

Point(1) = {0, 0, 0, lc};

Point(2) = {.1, 0,  0, lc} ;
Point(3) = {.1, .3, 0, lc} ;
Point(4) = {0,  .3, 0, lc} ;

Line(1) = {1,2} ;
Line(2) = {3,2} ;
Line(3) = {3,4} ;
Line(4) = {4,1} ;

Line Loop(5) = {4,1,-2,3} ;

Physical Line("<inlet>") = {3};
Physical Line("<outlet>") = {1};
Point(5) = {0.05, 0.2, 0, lc/4};
Point(6) = {0.025, 0.15, 0, lc/4};
Point(7) = {0.05, 0.1, 0, lc/4};
Point(8) = {0.075, 0.15, 0, lc/4};
Point(9) = {0.05, 0.15, 0, lc/4};
Ellipse(7) = {5, 9, 9, 6};
Ellipse(8) = {5, 9, 9, 8};
Ellipse(9) = {7, 9, 9, 6};
Ellipse(10) = {7, 9, 9, 8};
Line Loop(11) = {8, -10, 9, -7};
Physical Line("<hole>") = {7, 9, 10, 8};
Plane Surface(12) = {5, 11};
Physical Surface("<the surface>") = {12};
