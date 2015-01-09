// 2d box with unstructured mesh

lc = 0.05;  // charateristic mesh length variable
xlength = 5.0; // length of box
ylength = 0.5; // width of box

Point(1) = {0, 0, 0, lc};
Point(2) = {xlength, 0, 0, lc};
Point(3) = {xlength, ylength, 0, lc};
Point(4) = {0, ylength, 0, lc};

Line(1) = {1,2} ;
Line(2) = {2,3} ;
Line(3) = {3,4} ;
Line(4) = {4,1} ;

Line Loop(5) = {1,2,3,4} ;

Plane Surface(6) = {5} ;

Physical Line("<outlet>") = {2};
Physical Line("<inlet>") = {4};
Physical Surface("<flow domain>") = {6};
Physical Line("<centreline>") = {1};
