// 2D mesh for particle separated from base which could be mirror or wall
// allows for inner particle to be included or not
// structured mesh around inner particle - note, gmsh bug (probably FIXME in BoundaryLayers.cpp) means mesh is now constructed in x/y rather than x/z plane
// daltonh, 160518

// set base variables
yc = 3.e0; // height of particle centre above base
rad = 0.5e0; // radius of particle (must be < yc)
domainsize = 10.e0; // height/radius of domain
innerparticle = 0; // whether to mesh particle as well
structuredout = 1; // whether to put structured mesh around the outside of the particle

// chose mesh resolution, by multiplying standard lc's with a multiplier
//lcmultiplier = 1.e0; // coarse
lcmultiplier = 4.e0; // super coarse for small file

// standard (coarse) mesh resolutions that are multiplied by lcmultiplier later
lcwall = 0.5e0; // mesh lengthscale at wall
lcparticle = 0.025e0; // mesh lengthscale at particle, in the absence of boundary layer
lcbase = 0.1e0; // mesh lengthscale at centre of the base
lcinner = 0.1e0; // mesh if the inner particle is meshed
blnstar = 2.0; // this number has to be greater than 1 and controls the number of cells in the boundary layer - the number of cells is this multiplied by the minimum number of elements required to span the boundary layer at the outer layer spacing
bltmax = 1.0; // this is the total thickness of the boundary layer, if there were an infinite number of cells in the layer
basemid = 2.; // radial position of a refinement point on the base

// hook in point to alter variables via batcher
//<<batchergeoreplacements>>

// ad hoc definitions
//lcmultiplier = 2.e0^(-4);
//domainsize = 20.e0;
//domainsize = 80.e0;
//lcwall = 2.e0;
//lcmultiplier = 0.125;
//lcmultiplier = 0.0625;

// calculate the mesh parameters for the unstructured mesh
lcwall = lcwall*domainsize/10.; // make lcwall normalised by that at 10.
lcwall = lcmultiplier*lcwall;
lcparticle = lcmultiplier*lcparticle;
lcbase = lcmultiplier*lcbase;
lcinner = lcmultiplier*lcinner;

Printf("lcwall = %g: lcparticle = %g: lcbase = %g: lcinner = %g: lcmultiplier = %g: domainsize = %g",
  lcwall,lcparticle,lcbase,lcinner,lcmultiplier,domainsize) ;

If (structuredout > 0)
  bllcouter = lcbase/2.;
  blr = 1. - bllcouter/bltmax;
  bln = Ceil(blnstar*bltmax/bllcouter);
  blt = bllcouter*((1.-blr^bln)/(1.-blr));
  bllcinner = bllcouter*(blr^(bln-1));
  lcparticle = lcbase*rad/(blt+rad);

  Printf("bllcouter = %g: bllcinner = %g: blr = %g: bln = %g: blt = %g: lcparticle = %g",
    bllcouter,bllcinner,blr,bln,blt,lcparticle) ;
EndIf

// setup outer domain
Point(1) = {0, 0, 0, lcbase}; // origin
Point(8) = {basemid, 0, 0, lcbase}; // origin
Point(2) = {domainsize, 0, 0, lcwall};
Point(3) = {0, domainsize, 0, lcwall};
Point(4) = {0, yc+rad, 0, lcparticle}; // particle top
Point(5) = {0, yc, 0, lcinner}; // particle centre
Point(6) = {rad, yc, 0, lcparticle}; // particle centre on radius
Point(7) = {0, yc-rad, 0, lcparticle}; // particle bottom

Line(1) = {1,8};
Line(8) = {8,2};
Circle(2) = {2, 1, 3};
Line(3) = {3,4};
Circle(4) = {4, 5, 6};
Circle(5) = {6, 5, 7};
Line(6) = {7,1};

If (structuredout > 0)
  Field[1] = BoundaryLayer;
  Field[1].EdgesList = {4, 5};
  Field[1].NodesList = {4, 6, 7};
//Field[1].IntersectMetrics = 0;
  Field[1].hfar = bllcouter;
  Field[1].hwall_n = bllcinner;
//Field[1].hwall_n_nodes = { lcparticle, lcparticle };
  Field[1].thickness = blt;
//Field[1].AnisoMax = 0.1;
  Field[1].ratio = 1/blr;
  Field[1].Quads = 1;
  BoundaryLayer Field = 1;
EndIf

Line Loop(7) = {1, 8, 2, 3, 4, 5, 6};
Plane Surface(8) = {7};

Physical Line("<base>") = {1,8};
Physical Line("<walls>") = {2};
Physical Line("<centreline>") = {3,6};
Physical Line("<particle>") = {4,5};

Physical Surface("<outer domain>") = {8};

// setup inner particle domain, if required
If (innerparticle > 0)

  Point(14) = {0, yc+rad, 0, lcparticle}; // particle top
  Point(15) = {0, yc, 0, lcinner}; // particle centre
  Point(16) = {rad, yc, 0, lcparticle}; // particle centre on radius
  Point(17) = {0, yc-rad, 0, lcparticle}; // particle bottom

  Line(10) = {14, 15};
  Line(11) = {15, 17};
  Circle(12) = {17, 15, 16};
  Circle(13) = {16, 15, 14};

  Periodic Line { 10, 11 } = { 4, 5 };

  Physical Line("<inner centreline>") = {10, 11};
  Physical Line("<inner particle>") = {12, 13};

  Line Loop(14) = {10, 11, 12, 13};
  Plane Surface(9) = {14};
  Physical Surface("<inner domain>") = {9};
EndIf

