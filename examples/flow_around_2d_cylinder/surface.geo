 
L = 2.e+2;
B = 1.e+2;
lc = 10;
lc_small = 0.01;

Point(1) = {-L/2, 0, 0, lc};
Point(2) = {L/2, 0, 0, lc} ;
Point(3) = {L/2, B, 0, lc} ;
Point(4) = {-L/2, B, 0, lc} ;
Point(5) = {-0.5, 0, 0, lc_small} ;
Point(6) = {0, 0, 0, lc_small} ;
Point(7) = {0.5, 0, 0, lc_small} ;

Line(2) = {1,4};
Line(3) = {3,4};
Line(4) = {3,2};
Line(5) = {1, 5};
Line(6) = {7, 2};

Circle(7) = {7, 6, 5};


Physical Line("<inlet>") = {2};
Physical Line("<outlet>") = {4};
Physical Line("<walls>") = {3};



Physical Line("<centreline>") = {5, 6};
Physical Line("<sphere>") = {7};
Line Loop(10) = {3, -2, 5, -7, 6, -4};
Plane Surface(11) = {10};
Physical Surface("<system>") = {11};
