lc = 0.1;

Point(1) = {-1.1, 0.3, 0, lc};
Point(2) = {0.1, 0.3, 0, lc/2};
Point(3) = {0.8, 0.3, 0, lc};
Line(1) = {1, 2};
Line(2) = {2, 3};
Physical Point("<left boundary>") = {1};
Physical Point("<centre>") = {2};
Physical Point("<right boundary>") = {3};
Physical Line("<left domain>") = {1};
Physical Line("<right domain>") = {2};
