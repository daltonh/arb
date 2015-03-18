" Vim syntax file for arb finite volume solver
" Language:     arb
" Version:      0.50
" Modified:     2015/03/12
" URL:          http://people.eng.unimelb.edu.au/daltonh/downloads/arb/
" Maintainer:   Christian Biscombe

" For version 5.x: Clear all syntax items
" For version 6.x: Quit if a syntax file is already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match

" common
syn match arbComment "#.*$" contains=arbTodo
syn region arbComment matchgroup=arbStatement start="^\s*\(BEGIN\|START\)_\(COMMENTS\=\|SKIP\)\>" end="^\s*\(END\|STOP\)_\(COMMENTS\=\|SKIP\)\>" contains=arbTodo
syn sync match arbCommentSync grouphere arbComment "^\s*\(BEGIN\|START\)_\(COMMENTS\=\|SKIP\)\>"
syn sync match arbCommentSync groupthere NONE "^\s*\(END\|STOP\)_\(COMMENTS\=\|SKIP\)\>"
syn region arbTodo start="\(FIXME\|TODO\|XXX\)" end="$" contained
syn match arbNumber "\<\d\+\.\=\d*\([DEde][-+]\=\d\+\>\)\="
syn match arbOperator "\(+\|-\|\*\|/\|\^\|=\)"
syn region arbName start="<" end=">" contains=arbSystemFlag,arbUserFlag contained nextgroup=arbUnit skipwhite
syn region arbUserVar start="<" end=">" contains=arbSystemFlag,arbUserFlag
syn region arbUserFlag start="<<" end=">>"
syn region arbUnit start="\[" end="\]" contained
syn region arbString start="'" end="'" contained
syn region arbString start=+"+ end=+"+ contained
syn region arbExpression start=+"+ end=+"+ contains=arbNumber,arbOperator,arbUserVar,arbUserFlag,arbStatement,arbDeprecated,arbSystemVar,arbSystemFlag,arbFunction,arbFortranFunction
syn match arbString2 "\w\+\S*" contained
syn match arbLogical "\.\(true\|false\)\." contained

" formerly in constants.in (some syntax common to equations.in)
syn keyword arbStatement DEFAULT_OPTIONS END GLUE_FACES NEWTRESTOL ON OVERRIDE_OPTIONS VERSION
syn keyword arbStatement MSH_FILE nextgroup=arbString,arbString2 skipwhite
syn match arbStatement "\<INCLUDE\(_FROM\|_ROOT\|_WORKING\)\=" nextgroup=arbString,arbString2 skipwhite
syn match arbStatement "\<\(GENERAL_\)\=REPLACEMENTS\>"
syn match arbStatement "\<\(C\(ANCEL\)\=\|D\(EFAULT\)\=\|EXTERNALS\=\|R\(EPLACE\)\=\|W\(ITH\)\=\)\>" nextgroup=arbString skipwhite
syn match arbStatement "\<\(ASSOCIATED WITH\|AT\|BOUNDARY OF\|COMMON\|COMPOUND\|DOMAIN OF\|GMSH\|INTERSECTION\|PART OF\|SURROUNDS\|UNION\|WITHIN BOX\)\>"
syn match arbStatement "\<\(NEWT\|TIME\)STEP\(ADDITIONAL\|DEBUGOUT\|MAX\|MIN\|OUT\|START\)\>"
syn match arbStatement "\<INFO_\(AUTHOR\|DATE\|DESCRIPTION\|FILENAME\|RUN\(DATE\|HOST\|VERSION\)\|TITLE\|VERSION\)\(+\|-\)\=" nextgroup=arbString skipwhite
syn region arbKernelStatement matchgroup=arbStatement start="^\s*KERNEL\(S\|_OPTIONS\=\)\=" end="$" contains=arbKernelOption,arbLogical,arbNumber,arbOperator,arbSystemFlag,arbUserFlag,arbDeprecated,arbComment keepend
syn region arbSolverStatement matchgroup=arbStatement start="^\s*SOLVER\(S\|_OPTIONS\=\)\=" end="$" contains=arbSolverOption,arbLogical,arbNumber,arbOperator,arbSystemFlag,arbUserFlag,arbComment keepend
syn keyword arbDeprecated DIMENSIONS READ_GMSH
syn match arbDeprecated "\<LINEAR_SOLVER\(\s\+\(DEFAULT\|HSL_MA28\|INTEL_PARDISO\(_OOC\)\=\|SUITESPARSE_UMF\)\)\=\>"

" formerly in equations.in (some syntax common to constants.in)
syn match arbStatement "\<\(CELL_\|FACE_\|NODE_\|NONE_\)\=\(CONDITION\|CONSTANT\|DERIVED\|EQUATION\|LOCAL\|NEWTIENT\|OUTPUT\|REGION\(_CONSTANT\|_LIST\)\=\|TRANSIENT\|UNKNOWN\|VARIABLE\)\>" nextgroup=arbName,arbNameError skipwhite
syn match arbDeprecated "\<\(CELL_\|FACE_\|NONE_\)\=\(\(IN\)\=DEPENDENT\|FIELD\)\>"
syn match arbStatement "\<\(\(NON\)\=NEWTIENT\|STEADY-\=STATE\|\(NON\)\=TRANSIENT\)_SIMULATION\>"
syn keyword arbStatement VARIABLE
syn match arbStatement "\<\(COMPOUND\|VARIABLE\)_OPTIONS\>"
syn keyword arbOption clearoptions magnitude negative nocheck positive reflect translate
syn match arbOption "\<\(\(centring\)\=\(mesh\)\=\|\(no\)\=\(compound\|component\)\=\)\(input\|output\)\>"
syn match arbOption "\<\(\(centring\)\=\(mesh\)\=\|\(no\)\=\)\(dat\|vtk\)output\>"
syn match arbOption "\<\(compound\|\(no\)\=component\)\=element\(node\(limited\)\=\)\=data\>"
syn match arbOption "\<\(no\)\=\(component\)\=\(output\|stepoutput\(noupdate\)\=\)\>"
syn match arbOption "\<\(bell\|convergence\|output\|stop\)condition\>"
syn match arbOption "\<\(dynamic\|static\)\=magnitude\(multiplier\)\=\>"
syn match arbOption "\<\(no\)\=deriv\(ative\)\=\>"
syn match arbOption "\<newtstep\(max\|min\)\>"
syn match arbOption "\<\(input\|output\)\(inverse\)\=scale\>"
syn keyword arbDeprecated check_minw kernel_method limit_mask_to_icell minimum_minw weight_separation_multiplier contained
syn match arbDeprecated "\<\(minimum\|maximum\)_\(boundary\|domain\)_separation\>" contained
syn match arbDeprecated "\<polynomial\(_average\)\=_order\>" contained
syn keyword arbKernelOption boundarynodeseparations checkminw hyperbolicb kernelmethod limitkernelmasktosharednodes minimumminw mls none optimisation separationmultipliedtrialkernels shiftboundaryweightcentre shifthyperbolicdistance simple weightseparationmultiplier contained
syn match arbKernelOption "\<\(partial\)\=hyperbolickernel\>" contained
syn match arbKernelOption "\<\(auto\)\=\(minimum\|maximum\)\(cell\)\=separation\>" contained
syn match arbKernelOption "\<polynomial\(average\|cell\)\=order\>" contained
syn keyword arbSolverOption backstepping default hslma28 intelpardiso intelpardisoooc lambdalimitfalseroot lambdalimitfalserootfactor lambdamin linearsolver none pardiso pardisoiterative stickylambda stickylambdaincrease suitesparse weightlargeequationerrors weightlargeequationerrorsfactor contained
syn match arbSystemVar "<\(all\|boundary\|domain\) \(cells\|faces\|nodes\)>" 
syn match arbSystemVar "<\(adjacentcellsignns\|adjacentface\(down\|other\|up\)cell\|boundaries\|celldx\(kernel\|max\|min\)\|cellvol\|\(central\|cross\)kernel\|\(nob\)\=celljfaces\|domain\|facearea\|facedx\(kernel\)\=\|face\(divop\|fromcelldirection\)\)>"
syn match arbSystemVar "<\(\(glue\|last\)face\|huge\(ish\)\=\|icell\|jface\|kernelsum\|knode\|limiter\(contgrad\|tolerance\)\(newtient\|transient\)delta\|newtres\|newtstep\|noloop\|pi\|separation\(centre\d*\)\=\|timestep\|tiny\(ish\)\=\|\(down\|up\)windfaceicells\)>"
syn match arbSystemVar "<\(\(noc\)\=adjacent\)\=\(cell\|face\|node\)\(icells\|knodes\)>"
syn match arbSystemVar "<cell\(kernel\(region\)\=\|d\=x\|to\(separation\)\=icellr\(eflect\|squared\)\=\)\[l=\(\d\|:\)\]>"
syn match arbSystemVar "<face\(dx\(up\|down\|unit\)\|kernel\(region\)\=\|norm\|reflect\|tang[12]\|toicellr\|x\)\[l=\(\d\|:\)\]>"
syn match arbSystemVar "<node\(kernel\(region\)\=\|x\)\[l=\(\d\|:\)\]>"
syn match arbSystemVar "<delta\[l=\(\d,\d\|:\)\]>"
syn match arbNameError "<\(all\|boundary\|domain\) \(cells\|faces\|nodes\)>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<\(adjacentcellsignns\|adjacentface\(down\|other\|up\)cell\|boundaries\|celldx\(kernel\|max\|min\)\|cellvol\|\(central\|cross\)kernel\|\(nob\)\=celljfaces\|domain\|facearea\|facedx\(kernel\)\=\|face\(divop\|fromcelldirection\)\)>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<\(\(glue\|last\)face\|huge\(ish\)\=\|icell\|jface\|kernelsum\|knode\|limiter\(contgrad\|tolerance\)\(newtient\|transient\)delta\|newtres\|newtstep\|noloop\|pi\|separation\(centre\d*\)\=\|timestep\|tiny\(ish\)\=\|\(down\|up\)windfaceicells\)>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<\(\(noc\)\=adjacent\)\=\(cell\|face\|node\)\(icells\|knodes\)>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<cell\(kernel\(region\)\=\|d\=x\|to\(separation\)\=icellr\(eflect\|squared\)\=\)\[l=\(\d\|:\)\]>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<face\(dx\(up\|down\|unit\)\|kernel\(region\)\=\|norm\|reflect\|tang[12]\|toicellr\|x\)\[l=\(\d\|:\)\]>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<node\(kernel\(region\)\=\|x\)\[l=\(\d\|:\)\]>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<delta\[l=\(\d,\d\|:\)\]>" contained nextgroup=arbUnit skipwhite
syn keyword arbFunction dot ddot heaviside signum contained
syn match arbFunction "\<\(cell\|face\|node\|none\)\(\(to\|from\)\(cell\|face\|node\|none\)\)\=\(ave\|boundangle\|delta\|div\(grad\)\=\|grad\|if\|limiter\|link\|magnitude\|max\|min\|newtonupdate\|product\|sum\)\>" nextgroup=arbFunctionBracket,arbFunctionBracket2 contained
syn match arbFunction "\<cellvof\(d\|phi\(adjust\|shape\)\)\>" nextgroup=arbFunctionBracket,arbFunctionBracket2 contained
syn keyword arbFunction facevofphi nextgroup=arbFunctionBracket,arbFunctionBracket2 contained
syn region arbFunctionBracket matchgroup=arbFunction start="\[" end="\]" contained contains=arbFunctionOption,arbSystemFlag,arbSystemVar,arbNumber,arbOperator,arbUserFlag,arbUserVar nextgroup=arbFunctionBracket2
syn region arbFunctionBracket2 start="(" end=")" contained contains=arbFunction,arbFunctionBracket,arbFunctionBracket2,arbFunctionParameter,arbFortranFunction,arbSystemFlag,arbSystemVar,arbNumber,arbOperator,arbUserFlag,arbUserVar
syn keyword arbFunctionOption advection box circle cube dxunit ellipse ellipsoid glueface highorder lastface lastfacenoglue linear lower noderivative rectangle reflect sphere square upper contained
syn match arbFunctionOption "\<sep\(aration\)\=cent\(er\|re\)\=\d*" contained
syn match arbFunctionOption "\<\(face\|max\(imum\)\=\|min\(imum\)\=\|no\|node\)separation\>" contained
syn match arbFunctionOption "\<\(exact\(piecewise\)\=\|linear\(one\|two\)\|parabolic\)\>" contained
syn match arbFunctionOption "\<\(adjacentcells\(evenweighting\|weighted\)\=\|\(down\|last\|other\|up\)cell\|\(\(non\)\=limited\)\=harmonic\(weighted\)\=\)\>" contained
syn match arbFunctionOption "\<l=[1-6:]" contained
syn keyword arbFunctionParameter d default dt expression faceseparationflag flux unknown contained
syn match arbFunctionParameter "\<\(from\|local\|remote\|to\)\=region\>" contained
syn match arbFunctionParameter "\<\(axis\|centre\|gradient\|normal\|size\)\[l=\(\d\|:\)\]" contained
syn match arbFunctionParameter "\<phi\(f\|\[r=1\]\|tol\)\=" contained
syn match arbSystemFlag "<<\(cartesian\|cylindrical\|\(radius\)\=dim[1-3]\|steadystate\|transient\)\(comment\|flag\)>>"
syn match arbSystemFlag "<<radius_\(c\|f\|n\)>>"
syn match arbSystemFlag "<<\(axial\|radial\)dim>>"
syn match arbSystemFlag "<<reflect=[1-3]>>"
syn keyword arbFortranFunction abs acos aimag aint anint asin atan atan2 ceiling char cmplx conjg cos cosh dim dprod exp floor ichar index int log log10 max min mod modulo nint sign sin sinh sqrt tan tanh contained

" Define the default highlighting.
" For version 5.7 and earlier: only when not done already
" For version 5.8 and later: only when an item doesn't have highlighting yet
if version >= 508 || !exists("did_arb_syn_inits")
  if version < 508
    let did_arb_syn_inits = 1
    command -nargs=+ HiLink hi link <args>
  else
    command -nargs=+ HiLink hi def link <args>
  endif
  HiLink arbComment Comment
  HiLink arbDeprecated Error
  HiLink arbFunction Function
  HiLink arbFunctionOption arbOption
  HiLink arbFunctionParameter Statement
  HiLink arbFortranFunction Function
  HiLink arbKernelOption arbOption
  HiLink arbSolverOption arbOption
  HiLink arbLogical Boolean
  HiLink arbName None
  HiLink arbNameError Error
  HiLink arbNumber Number
  HiLink arbOperator Operator
  HiLink arbOption Type
  HiLink arbStatement Statement
  HiLink arbString String
  HiLink arbString2 arbString
  HiLink arbSystemFlag arbSystemVar
  HiLink arbSystemVar Special
  HiLink arbTodo Todo
  HiLink arbUnit String
  HiLink arbUserFlag String
  HiLink arbUserVar None
  delcommand HiLink
endif

let b:current_syntax = "arb"
