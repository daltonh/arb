lc = 0.01;

Point(1) = {0, -1, 0, lc};
Point(2) = {0, 1, 0, lc};
Line(1) = {1, 2};
Physical Point("<bottom>") = {1};
Physical Point("<top>") = {2};
Physical Line("<line domain>") = {1};
