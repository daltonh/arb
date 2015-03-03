" Vim syntax file for arb finite volume solver
" Language:     arb
" Version:      0.50
" URL:          http://www.chemeng.unimelb.edu.au/people/staff/daltonh/downloads/arb
" Modified:     2014/07/23

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
syn region arbComment matchgroup=arbStatement start="^\s*START_\(COMMENTS\=\|SKIP\)\>" end="\<\(END\|STOP\)_\(COMMENTS\=\|SKIP\)\>" contains=arbTodo
syn sync match arbCommentSync grouphere arbComment "\<START_\(COMMENTS\=\|SKIP\)\>"
syn sync match arbCommentSync groupthere NONE "\<\(END\|STOP\)_\(COMMENTS\=\|SKIP\)\>"
syn keyword arbTodo FIXME fixme TODO todo XXX xxx contained
syn match arbNumber "\<\d\+\.\=\d*\([DEde][-+]\=\d\+\>\)\="
syn match arbOperator "\(+\|-\|\*\|/\|\^\)"
syn match arbName "<[^>]*>" contained nextgroup=arbUnit skipwhite
syn match arbUserVar "<[^>]*>" " causes problems with REPLACE strings that have opening but not closing angle bracket
syn match arbUnit "\[[^\]]*\]" contained
syn match arbString "'[^']*'" contained
syn match arbString /"[^"]*"/ contained
syn match arbString2 "\w\+\S*" contained
syn match arbLogical "\.\(true\|false\)\." contained

" formerly in constants.in (some syntax common to equations.in)
syn keyword arbStatement DEFAULT_OPTIONS END GLUE_FACES NEWTRESTOL ON OVERRIDE_OPTIONS VERSION
syn keyword arbStatement MSH_FILE nextgroup=arbString,arbString2 skipwhite
syn keyword arbStatement EXTERNAL[S] nextgroup=arbString skipwhite
syn match arbStatement "\<\(GENERAL_\)REPLACEMENTS\>"
syn match arbStatement "\<INCLUDE\(_FROM\|_ROOT\|_WORKING\)\=" nextgroup=arbString,arbString2 skipwhite
syn match arbStatement "\<INFO_\(AUTHOR\|DATE\|DESCRIPTION\|FILENAME\|RUN\(DATE\|HOST\|VERSION\)\|TITLE\|VERSION\)\(+\|-\)\=" nextgroup=arbString skipwhite
syn match arbStatement "\<\(NEWT\|TIME\)STEP\(DEBUGOUT\|MAX\|MIN\|OUT\|START\)\>"
syn region arbKernelStatement matchgroup=arbStatement start="^\s*KERNEL\(S\|_OPTIONS\=\)\=" end="$" contains=arbKernelOption,arbLogical,arbNumber,arbDeprecated,arbComment keepend
syn region arbSolverStatement matchgroup=arbStatement start="^\s*SOLVER\(S\|_OPTIONS\=\)\=" end="$" contains=arbSolverOption,arbLogical,arbNumber,arbComment keepend
syn region arbReplaceStatement matchgroup=arbStatement start="[^<]\<R\(EPLACE\)*\>[^>]" end="\<W\(ITH\)*\>" contains=arbString nextgroup=arbString skipwhite
syn keyword arbDeprecated DIMENSIONS LINEAR_SOLVER READ_GMSH
syn keyword arbOption DEFAULT HSL_MA28 INTEL_PARDISO INTEL_PARDISO_OOC SUITESPARSE_UMF
syn match arbLocationString "\<\(ASSOCIATED WITH\|AT\|BOUNDARY OF\|COMMON\|COMPOUND\|DOMAIN OF\|GMSH\|INTERSECTION\|PART OF\|UNION\|WITHIN BOX\)\>"

" formerly in equations.in (some syntax common to constants.in)
syn match arbStatement "\<\(CELL_\|FACE_\|NODE_\|NONE_\)\=\(CONDITION\|CONSTANT\|DERIVED\|EQUATION\|LOCAL\|NEWTIENT\|OUTPUT\|REGION\(_CONSTANT\|_LIST\)\=\|TRANSIENT\|UNKNOWN\|VARIABLE\)\>" nextgroup=arbName skipwhite
syn match arbDeprecated "\<\(CELL_\|FACE_\|NONE_\)\=\(\(IN\)\=DEPENDENT\|FIELD\)\>"
syn match arbStatement "\<\(\(NON\)\=NEWTIENT\|STEADY-\=STATE\|\(NON\)\=TRANSIENT\)_SIMULATION\>"
syn keyword arbStatement VARIABLE
syn match arbStatement "\<\(COMPOUND\|VARIABLE\)_OPTIONS\>"
syn keyword arbOption clearoptions magnitude negative nocheck positive
syn match arbOption "\<\(\(centring\)\=\(mesh\)\=\|\(no\)\=\(compound\|component\)\=\)\(input\|output\)\>"
syn match arbOption "\<\(\(centring\)\=\(mesh\)\=\|\(no\)\=\)\(dat\|vtk\)output\>"
syn match arbOption "\<\(compound\|\(no\)\=component\)\=element\(node\(limited\)\=\)\=data\>"
syn match arbOption "\<\(no\)\=\(component\)\=\(output\|stepoutput\(noupdate\)\=\)\>"
syn match arbOption "\<\(bell\|convergence\|output\|stop\)condition\>"
syn match arbOption "\<\(dynamic\|static\)\=magnitude\(multiplier\)\=\>"
syn match arbOption "\<\(no\)\=deriv\(ative\)\=\>"
syn match arbOption "\<newtstep\(max\|min\)\>"
syn keyword arbDeprecated check_minw kernel_method limit_mask_to_icell minimum_minw weight_separation_multiplier contained
syn match arbDeprecated "\<\(minimum\|maximum\)_\(boundary\|domain\)_separation\>" contained
syn match arbDeprecated "\<polynomial\(_average\)\=_order\>" contained
syn keyword arbKernelOption boundarynodeseparations checkminw hyperbolicb kernelmethod limitkernelmasktosharednodes minimumminw mls none optimisation separationmultipliedtrialkernels shiftboundaryweightcentre shifthyperbolicdistance simple weightseparationmultiplier contained
syn match arbKernelOption "\<\(partial\)\=hyperbolickernel\>" contained
syn match arbKernelOption "\<\(auto\)\=\(minimum\|maximum\)\(cell\)\=separation\>" contained
syn match arbKernelOption "\<polynomial\(average\|cell\)\=order\>" contained
syn keyword arbSolverOption backstepping default hslma28 intelpardiso intelpardisoooc lambdalimitfalseroot lambdalimitfalserootfactor linearsolver none pardiso pardisoiterative suitesparse contained
syn match arbSystemVar "<\(adjacentface\(down\|up\)cell\|all \(cells\|faces\)\|boundaries\|boundary cells\|\(nob\)\=celljfaces\|cellvol\|domain\( faces\)\=\|facearea\|facedx\|huge\(ish\)\=\|icell\|jface\|limiter\(contgrad\|tolerance\)\|newtstep\|noloop\|pi\|timestep\|tiny\(ish\)\=\|\(down\|up\)windfaceicells\)>"
syn match arbSystemVar "<\(adjacent\)\=\(cell\|face\)\=icells>"
syn match arbSystemVar "<cell\(kernel\|x\)\[l=\(\d\|:\)\]>"
syn match arbSystemVar "<face\(kernel\|norm\|tang[12]\|x\)\[l=\(\d\|:\)\]>"
syn match arbSystemVar "<delta\[l=\(\d\|:\)\]>"
syn keyword arbExpression dot ddot heaviside signum
syn match arbExpression "\<cell\(\(to\|from\)\(cell\|face\|node\)\)\=\(ave\|delta\|div\(grad\)\=\|grad\|if\|limiter\|link\|max\|min\|product\|sum\)\>" nextgroup=arbExpressionBracket
syn match arbExpression "\<face\(\(to\|from\)\(cell\|face\|node\)\)\=\(ave\|delta\|grad\|if\|link\|max\|min\|product\|sum\)\>" nextgroup=arbExpressionBracket
syn match arbExpression "\<node\(\(to\|from\)\(cell\|face\|node\)\)\=\(ave\|delta\|grad\|if\|link\|max\|min\|product\|sum\)\>" nextgroup=arbExpressionBracket
syn match arbExpression "\<none\(if\|max\|min\)\>"
syn region arbExpressionBracket matchgroup=arbExpression start="\[" end="\]" contained contains=arbExpressionOption,arbNumber oneline
syn keyword arbExpressionOption advection lastface linear lower noderivative upper contained
syn match arbExpressionOption "\<\(adjacentcells\(evenweighting\)\=\|\(down\|last\|up\)cell\|\(limited\)\=harmonic\)\>" contained
syn match arbExpressionOption "\<l=[1-6:]" contained
syn match arbGeneralReplacements "<<\(cartesian\|cylindrical\|dim[1-3]\|steadystate\|transient\)comment>>"
syn match arbGeneralReplacements "<<\(cartesian\|cylindrical\|radiusdim[1-3]\|steadystate\|transient\)flag>>"
syn match arbGeneralReplacements "<<radius_\(c\|f\|n\)>>"
syn match arbGeneralReplacements "<<reflect=[1-3]>>"

" Taken directly from Fortran syntax file
syn keyword arbFortranFunction alog alog10 amax0 amax1 amin0 amin1 amod cabs ccos cexp clog csin csqrt dabs dacos dasin datan datan2 dcos dcosh ddim dexp dint dlog dlog10 dmax1 dmin1 dmod dnint dsign dsin dsinh dsqrt dtan dtanh float iabs idim idint idnint ifix isign max0 max1 min0 min1 sngl
syn keyword arbFortranFunction algama cdabs cdcos cdexp cdlog cdsin cdsqrt cqabs cqcos cqexp cqlog cqsin cqsqrt dcmplx dconjg derf derfc dfloat dgamma dimag dlgama erf erfc gamma iqint qabs qacos qasin qatan qatan2 qcmplx qconjg qcos qcosh qdim qerf qerfc qexp qgamma qimag qlgama qlog qlog10 qmax1 qmin1 qmod qnint qsign qsin qsinh qsqrt qtan qtanh
syn keyword arbFortranFunction abs acos aimag aint anint asin atan atan2 char cmplx conjg cos cosh exp ichar index int log log10 max min nint sign sin sinh sqrt tan tanh
syn keyword arbFortranFunction adjustl adjustr all allocated any associated bit_size btest ceiling count cshift date_and_time digits dot_product eoshift epsilon exponent floor fraction huge iand ibclr ibits ibset ieor ior ishft ishftc lbound len_trim matmul maxexponent maxloc maxval merge minexponent minloc minval modulo mvbits nearest pack precision present product radix random_number random_seed range repeat reshape rrspacing
syn keyword arbFortranFunction scale scan selected_int_kind selected_real_kind set_exponent shape size spacing spread sum system_clock tiny transpose trim ubound unpack verify
syn keyword arbFortranFunction null cpu_time

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
  HiLink arbExpression Function
  HiLink arbExpressionOption arbExpression
  HiLink arbFortranFunction Function
  HiLink arbKernelOption Type
  HiLink arbSolverOption Type
  HiLink arbLocationString Statement
  HiLink arbLogical Boolean
  HiLink arbNumber Number
  HiLink arbOperator Operator
  HiLink arbOption Type
  HiLink arbStatement Statement
  HiLink arbString String
  HiLink arbString2 String
  HiLink arbSystemVar Special
  HiLink arbTodo Todo
  HiLink arbUnit String
  HiLink arbUserVar None
  HiLink arbReplaceStatement None
  HiLink arbGeneralReplacements Special
  delcommand HiLink
endif

let b:current_syntax = "arb"
