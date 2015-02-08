" Vim syntax file for arb finite volume solver
" Language:     arb
" Version:      0.50
" URL:          http://www.chemeng.unimelb.edu.au/people/staff/daltonh/downloads/arb
" Modified:     2015/01/28

" For version 5.x: Clear all syntax items
" For version 6.x: Quit if a syntax file is already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match

" common
syn match arbComment "#.*$"
syn match arbNumber "\<\d\+\.\=\d*\([DEde][-+]\=\d\+\>\)\="
syn match arbOperator "\(+\|-\|\*\|/\|\^\)"
syn region arbUserRegion matchgroup=arbRegionName start="<" end=">" contains=arbRegionName keepend oneline
syn match arbRegionName ".*" contained
syn region arbUnit matchgroup=arbString start="\[" end="\]" contains=arbString keepend oneline
syn match arbString ".*" contained
syn match arbQuotedString /"[^"]*"/ contained

" constants.in (some syntax common to equations.in)
syn keyword arbStatement DEFAULT_OPTIONS OVERRIDE_OPTIONS END MSH_FILE NEWTRESTOL NEWTSTEPMAX NEWTSTEPSTART NEWTSTEPOUT ON TIMESTEPMAX TIMESTEPOUT TIMESTEPSTART VERSION NEWTSTEPDEBUGOUT TIMESTEPADDITIONAL GENERAL_REPLACEMENTS KERNEL_OPTIONS SOLVER_OPTIONS INCLUDE INCLUDE_ROOT INCLUDE_WORKING
syn keyword arbStatement MSH_FILE nextgroup=arbQuotedString skipwhite
syn keyword arbDeprecated DIMENSIONS READ_GMSH LINEAR_SOLVER
syn keyword arbOption default hslma28 intelpardiso intelpardisoooc suitesparseumf
syn match arbLocationString "\<\(ASSOCIATED WITH\|AT\|BOUNDARY OF\|UNION\|INTERSECTION\|COMPOUND\|COMMON\|DOMAIN OF\|GMSH\|WITHIN BOX\|PART OF\)\>"

" equations.in (some syntax common to constants.in)
syn match arbStatement "\<\(CELL_\|FACE_\|NODE_\|NONE_\)\=\(CONDITION\|CONSTANT\|DERIVED\|EQUATION\|LOCAL\|OUTPUT\|REGION\(_CONSTANT\|_LIST\)\=\|TRANSIENT\|UNKNOWN\)\>"
syn match arbDeprecated "\<\(CELL_\|FACE_\|NODE_\|NONE_\)\=\(\(IN\)\=DEPENDENT\|FIELD\)\>"
syn match arbStatement "\<\(STEADY-\=STATE\|TRANSIENT\)_SIMULATION\>"
syn keyword arbOption negative noderivative positive
syn match arbOption "\<\(\(\(centring\)\=\(mesh\)\=\)\|\(\(no\)\=\(component\)\=\)\=\)\(input\|output\)\>"
syn match arbOption "\<\(component\)\=element\(node\(limited\)\=\)\=data\>"
syn match arbOption "\<\(no\)\=\(component\)\=stepoutput\(noupdate\)\=\>"
syn match arbOption "\<\(bell\|convergence\|output\|stop\)condition\>"
syn match arbSystemRegion "<\(adjacentface\(down\|up\)cell\|all \(cells\|faces\|nodes\)\|boundaries\|boundary nodes\|boundary cells\|\(nob\)\=celljfaces\|cellvol\|domain\( faces\| nodes\)\=\|facearea\|facedx\|huge\(ish\)\=\|icell\|jface\|knode\|limiter\(contgrad\|tolerance\)\|newtstep\|noloop\|pi\|timestep\|tiny\(ish\)\=\|\(down\|up\)windfaceicells\)>"
syn match arbSystemRegion "<\(adjacent\)\=\(cell\|face\)\=icells\>"
syn match arbSystemRegion "<cell\(kernel\|x\)" nextgroup=arbRegionBracket
syn match arbSystemRegion "<face\(kernel\|norm\|tang[12]\|x\)" nextgroup=arbRegionBracket
syn match arbSystemRegion "<node\(kernel\|x\)" nextgroup=arbRegionBracket
syn match arbSystemRegion "<delta" nextgroup=arbRegionBracket
syn keyword arbExpression dot ddot heaviside signum
syn match arbExpression "\<cell\(\(to\|from\)\(cell\|face\|node\)\)\=\(ave\|delta\|div\(grad\)\=\|grad\|if\|limiter\|link\|max\|min\|product\|sum\)\>" nextgroup=arbExpressionBracket
syn match arbExpression "\<face\(\(to\|from\)\(cell\|face\|node\)\)\=\(ave\|delta\|grad\|if\|link\|max\|min\|product\|sum\)\>" nextgroup=arbExpressionBracket
syn match arbExpression "\<node\(\(to\|from\)\(cell\|face\|node\)\)\=\(ave\|delta\|grad\|if\|link\|max\|min\|product\|sum\)\>" nextgroup=arbExpressionBracket
syn match arbExpression "\<none\(if\|max\|min\)\>"
syn region arbRegionBracket matchgroup=arbSystemRegion start="\[" end="\]>" contained contains=arbExpressionOption,arbNumber oneline
syn region arbExpressionBracket matchgroup=arbExpression start="\[" end="\]" contained contains=arbExpressionOption,arbNumber oneline
syn keyword arbExpressionOption advection lastface linear lower noderivative upper contained
syn match arbExpressionOption "\<\(adjacentcells\(evenweighting\)\=\|\(down\|last\|up\)cell\|\(limited\)\=harmonic\)\>" contained
syn match arbExpressionOption "\<l=[0-6:]" contained

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
  HiLink arbExpression Special
  HiLink arbExpressionOption Type
  HiLink arbFortranFunction Function
  HiLink arbLocationString Statement
  HiLink arbMshFile None
  HiLink arbNumber Number
  HiLink arbOperator Operator
  HiLink arbOption Type
  HiLink arbQuotedString String
  HiLink arbRegionName None
  HiLink arbStatement Statement
  HiLink arbString String
  HiLink arbSystemRegion Identifier
  delcommand HiLink
endif

let b:current_syntax = "arb"
