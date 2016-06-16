// Simple Rectangular flow field

lc = 1.0;  // charateristic mesh length variable
rr = 1.;
ll = 5.;

// setup domain boundaries
Point(1) = {rr, 0, 0, lc/2};
Point(2) = {0, rr, 0, lc/2};
Point(3) = {-rr, 0, 0, lc/2};
Point(4) = {0, -rr, 0, lc/2};

Point(5) = {0, 0, 0, lc/2}; // Center Point

//Create circle arcs
Ellipse(1) = {1,5,5,2};
Ellipse(2) = {2,5,5,3};
Ellipse(3) = {3,5,5,4};
Ellipse(4) = {4,5,5,1};

//Link 4 arcs into circle
Line Loop(5) = {1, 2, 3, 4};

Plane Surface(1) = {5}; //Define surface as circle interior

bits[] = Extrude {0, 0, 5.} { Surface{1}; };
Physical Surface("<inlet>") = { 1 };
Physical Surface("<outlet>") = { bits[0] };
Physical Volume("<flow domain>") = { bits[1] };
