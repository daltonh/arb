// simple square box with sidelength 1 unit

lc = 0.200;

Point(1) = {0, 0, 0, lc};
Point(2) = {1, 0, 0, lc};
Point(3) = {1, 1, 0, lc};
Point(4) = {0, 1, 0, lc};

Line(1) = {1,2} ;
Line(2) = {2,3} ;
Line(3) = {3,4} ;
Line(4) = {4,1} ;

Line Loop(5) = {1,2,3,4} ;

Plane Surface(6) = {5} ;

Physical Surface("<surface>") = {6} ;
Physical Line("<top>") = {3};
Physical Line("<bottom>") = {1};
Physical Line("<east>") = {2};
Physical Line("<west>") = {4};
