// a structured mesh is created using the transfinite algorithm

//lc = 0.0125;
lc = 0.05;
//lc = 0.025;
// this is for batcher - the following string can be replaced in its entirety
//lc = <<lc>>;
xmax = 1;
ymax = 1;

Point(1) = {0, 0, 0, lc};
Point(2) = {xmax, 0, 0, lc};
Point(3) = {0, ymax, 0, lc};
Point(4) = {xmax, ymax, 0, lc};
Line(1) = {1, 2};
Line(2) = {2, 4};
Line(3) = {4, 3};
Line(4) = {3, 1};
Line Loop(5) = {3, 4, 1, 2};
Plane Surface(6) = {5};
Transfinite Surface {6} = {4, 2, 1, 3};
Recombine Surface {6};

Physical Line("<west>") = {4};
Physical Line("<east>") = {2};
Physical Line("<north>") = {3};
Physical Line("<south>") = {1};
Physical Surface("<gmshdomain>") = {6};
