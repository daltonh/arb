lc = 1.;
npoints = 50;

Point(1) = {0, 0, 0, lc};
Point(2) = {npoints, 0, 0, lc};
Line(1) = {1, 2};
Physical Point("<left>") = {1};
Physical Point("<right>") = {2};
Physical Line("<line domain>") = {1};
