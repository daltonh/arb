// filtration channel
// Malavika Haribabu and daltonh
// 30/8/18

// geometry parameters
xlength=1.e-2; // 10 mm //
ylength=2.5e-3; // 2.5 mm full height //
extension_F=2*ylength; // entrance length for developed flow
extension_B=2*ylength; // entrance length for developed flow

// mesh parameters
lc=0.5; // mesh dimension, probably not used
y_min=1.e-5; // minimum y direction cell size
x_min=xlength/20; // minimum y direction cell size
del_x_e=1.0; // stretch ratio in the x dimension - not correctly implemented here
del_y_e=1.05; // stretch ratio in the y dimension

del_x_c=1/del_x_e;
del_y_c=1/del_y_e;
//m=Log(1-xlength*(1-del_x_e)/(2*x_min))/Log(del_x_e);
n=Log(1-ylength*(1-del_y_e)/(y_min))/Log(del_y_e);
//p=Log(1-extension*(1-del_x_e)/(2*x_min))/Log(del_x_e);
m=xlength/(x_min);
p_F=extension_F/(x_min);
p_B=extension_B/(x_min);
m_layer=Floor(m);
n_layer=Floor(n);
p_F_layer=Floor(p_F);
p_B_layer=Floor(p_B);

Point(1) = {0, 0, 0, lc};
Point(2) = {xlength, 0, 0, lc};
Point(3) = {xlength, ylength, 0, lc};
Point(4) = {0, ylength, 0, lc};
Point(5) = {-extension_F, 0, 0, lc};
Point(6) = {-extension_F, ylength, 0, lc};
Point(7) = {xlength+extension_B, 0, 0, lc};
Point(8) = {xlength+extension_B, ylength, 0, lc};
//Point(9) = {xlength/2, 0, 0, lc};
//Point(10) = {xlength/2, ylength, 0, lc};
//Point(11) = {-extension/2, 0, 0, lc};
//Point(12) = {-extension/2, ylength, 0, lc};
//Point(13) = {xlength+extension/2, 0, 0, lc};
//Point(14) = {xlength+extension/2, ylength, 0, lc};


//Line(1) = {1,4};
//Line(2) = {4,3};
//Line(3) = {2,3};
//Line(4) = {1,2};

Line(1) = {5, 6};
Line(2) = {6, 4};
Line(3) = {7, 8};
Line(4) = {5, 1};
Line(5) = {4, 3};
Line(6) = {1, 2};
Line(7) = {3, 8};
Line(8) = {2, 7};
//Line(9) = {10, 3};
//Line(10) = {9, 2};
//Line(11)= {11, 1};
//Line(12) = {12, 4};
//Line(13) = {13, 7};
//Line(14) = {14, 8};

Physical Line ("<inlet>")={1};
Physical Line ("<outlet>")={3};
Physical Line ("<centreline>")={5,2,7};
Physical Line ("<walls>")={4,8};
Physical Line ("<membrane>")={6};
Line Loop(14) = {1, 2,5, 7, -3, -8, -6,-4};
Plane Surface(15) = {14};
Physical Surface ("<feed_channel>")={15};
Transfinite Line {5, 6} = m_layer Using Progression del_x_e;
Transfinite Line {4, 2} = p_F_layer Using Progression del_x_e;
Transfinite Line {7, 8} = p_B_layer Using Progression del_x_e;
Transfinite Line {1, 3} = n_layer Using Progression del_y_e;
Transfinite Surface {15}={5,6,7,8}  Alternated;
Recombine Surface {15};
