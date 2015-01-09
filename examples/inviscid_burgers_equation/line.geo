lc = 0.01;

Point(1) = {0, 0, 0, lc};
Point(2) = {4, 0, 0, lc/2};
Line(1) = {1, 2};
Physical Point("<left>") = {1};
Physical Point("<right>") = {2};
Physical Line("<line domain>") = {1};
