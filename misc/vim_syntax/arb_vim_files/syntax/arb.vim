" Vim syntax file for arb finite volume solver
" Language:     arb
" Version:      0.55
" Modified:     2016/03/03
" URL:          http://people.eng.unimelb.edu.au/daltonh/downloads/arb/
" Maintainer:   Christian Biscombe

" NOTE match patterns starting with \%(\) are used to prevent following syntax elements from appearing in the omni completion list

" For version 5.x: Clear all syntax items
" For version 6.x: Quit if a syntax file is already loaded
if version < 600
  syntax clear
elseif exists("b:current_syntax")
  finish
endif

syn case match

syn match arbNumber "\<\d\+\.\=\d*\%([DEde][-+]\=\d\+\>\)\="
syn match arbOperator "\%(+\|-\|\*\|/\|\^\|=\)" " for some reason, / here adds 'match' to the omni completion list - a bug in syntaxcomplete.vim (v. 13.0)
syn region arbName start="<" end=">" contains=arbSystemFlag,arbUserFlag contained nextgroup=arbUnit skipwhite
syn region arbUserVar start="<" end=">" contains=arbSystemFlag,arbUserFlag
syn region arbUserFlag start="<<" end=">>"
syn region arbUnit start="\[" end="\]" contained
syn match arbUnquotedString "\S\+" contained
syn region arbString start="'" end="'" contained
syn region arbString start=/"/ end=/"/ contained
syn match arbFilename "\%(\w\|/\)\+" contained
syn region arbExpression start=/"/ end=/"/ contains=arbDeprecated,arbFunction,arbFortranFunction,arbNumber,arbOperator,arbStatement,arbSystemFlag,arbSystemVar,arbUserFlag,arbUserVar
syn match arbLogical "\.\%(true\|false\)\." contained

syn match arbComment "#.*$" contains=arbTodo
syn match arbOmniList "\%(BEGIN_COMMENTS\|START_COMMENTS\|END_COMMENTS\|STOP_COMMENTS\)"
syn region arbComment matchgroup=arbStatement start="^\s*\zs\%(BEGIN\|START\)_\%(COMMENTS\=\|SKIP\)\>" end="^\s*\zs\%(END\|STOP\)_\%(COMMENTS\=\|SKIP\)\>"
syn sync match arbCommentSync grouphere arbComment "\%(BEGIN\|START\)_\%(COMMENTS\=\|SKIP\)"
syn sync match arbCommentSync groupthere NONE "\%(END\|STOP\)_\%(COMMENTS\=\|SKIP\)"
syn region arbTodo start="\%(FIXME\|TODO\|XXX\)" end="$" contained

syn keyword arbStatement COMPOUND_OPTIONS DEFAULT_OPTIONS END GENERAL GENERAL_OPTIONS GENERAL_REPLACEMENTS GLUE_FACES NEWTIENT_SIMULATION NONNEWTIENT_SIMULATION NEWTRESTOL NEWTSTEPADDITIONAL NEWTSTEPDEBUGOUT NEWTSTEPMAX NEWTSTEPMIN NEWTSTEPOUT NEWTSTEPSTART ON OVERRIDE_OPTIONS REPLACEMENTS STEADYSTATE_SIMULATION TIMESTEPADDITIONAL TIMESTEPMAX TIMESTEPMIN TIMESTEPOUT TIMESTEPSTART TRANSIENT_SIMULATION NONTRANSIENT_SIMULATION VARIABLE_OPTIONS VERSION
syn match arbStatement "\%(\)\<STEADY-STATE_SIMULATION\>"
syn keyword arbStatement INCLUDE INCLUDE_FROM INCLUDE_ROOT INCLUDE_WORKING MSH_FILE nextgroup=arbString,arbFilename skipwhite
syn match arbStatement "\<\%(EXTERNALS\=\)\>" nextgroup=arbString,arbFilename skipwhite
syn keyword arbStatement C CANCEL D DEFAULT R REPLACE W WITH nextgroup=arbString,arbUnquotedString,arbComment skipwhite
syn match arbStatement "\<\%(INFO_AUTHOR\|INFO_DATE\|INFO_DESCRIPTION\|INFO_FILENAME\|INFO_RUNDATE\|INFO_RUNHOST\|INFO_RUNVERSION\|INFO_TITLE\|INFO_VERSION\)\>\%(+\|-\)\=" nextgroup=arbString,arbUnquotedString skipwhite
syn keyword arbStatement ERROR INFO WARNING nextgroup=arbString skipwhite
syn match arbOmniList "\%(KERNEL_OPTIONS\|SOLVER_OPTIONS\)"
syn region arbKernelStatement matchgroup=arbStatement start="\<KERNEL\%(S\|_OPTIONS\=\)\=" end="$" contains=arbComment,arbDeprecated,arbKernelOption,arbLogical,arbNumber,arbOperator,arbSystemFlag,arbUserFlag keepend
syn region arbSolverStatement matchgroup=arbStatement start="\<SOLVER\%(S\|_OPTIONS\=\)\=" end="$" contains=arbComment,arbLogical,arbNumber,arbOperator,arbSolverOption,arbSystemFlag,arbUserFlag keepend
syn keyword arbDeprecated DIMENSIONS READ_GMSH
syn match arbDeprecated "\<LINEAR_SOLVER\%(\s\+\%(DEFAULT\|HSL_MA28\|INTEL_PARDISO\%(_OOC\)\=\|SUITESPARSE_UMF\)\)\=\>"

syn keyword arbStatement      CONDITION      CONSTANT      DERIVED      EQUATION      LOCAL      NEWTIENT      OUTPUT      REGION_CONSTANT      REGION_LIST      TRANSIENT      UNKNOWN      VARIABLE nextgroup=arbName,arbNameError skipwhite
syn keyword arbStatement CELL_CONDITION CELL_CONSTANT CELL_DERIVED CELL_EQUATION CELL_LOCAL CELL_NEWTIENT CELL_OUTPUT CELL_REGION_CONSTANT CELL_REGION_LIST CELL_TRANSIENT CELL_UNKNOWN CELL_VARIABLE nextgroup=arbName,arbNameError skipwhite
syn keyword arbStatement FACE_CONDITION FACE_CONSTANT FACE_DERIVED FACE_EQUATION FACE_LOCAL FACE_NEWTIENT FACE_OUTPUT FACE_REGION_CONSTANT FACE_REGION_LIST FACE_TRANSIENT FACE_UNKNOWN FACE_VARIABLE nextgroup=arbName,arbNameError skipwhite
syn keyword arbStatement NODE_CONDITION NODE_CONSTANT NODE_DERIVED NODE_EQUATION NODE_LOCAL NODE_NEWTIENT NODE_OUTPUT NODE_REGION_CONSTANT NODE_REGION_LIST NODE_TRANSIENT NODE_UNKNOWN NODE_VARIABLE nextgroup=arbName,arbNameError skipwhite
syn keyword arbStatement NONE_CONDITION NONE_CONSTANT NONE_DERIVED NONE_EQUATION NONE_LOCAL NONE_NEWTIENT NONE_OUTPUT NONE_REGION_CONSTANT                  NONE_TRANSIENT NONE_UNKNOWN NONE_VARIABLE nextgroup=arbName,arbNameError skipwhite
syn match arbDeprecated "\<\%(CELL_\|FACE_\|NODE_\|NONE_\)\=\%(\%(IN\)\=DEPENDENT\|FIELD\)\>" nextgroup=arbName,arbNameError skipwhite
syn keyword arbStatement      CONSTANT_REGION      DERIVED_REGION      EQUATION_REGION      GMSH_REGION      NEWTIENT_REGION      OUTPUT_REGION      REGION      SETUP_REGION      STATIC_REGION      TRANSIENT_REGION      UNKNOWN_REGION nextgroup=arbName,arbNameError skipwhite
syn keyword arbStatement CELL_CONSTANT_REGION CELL_DERIVED_REGION CELL_EQUATION_REGION CELL_GMSH_REGION CELL_NEWTIENT_REGION CELL_OUTPUT_REGION CELL_REGION CELL_SETUP_REGION CELL_STATIC_REGION CELL_TRANSIENT_REGION CELL_UNKNOWN_REGION nextgroup=arbName,arbNameError skipwhite
syn keyword arbStatement FACE_CONSTANT_REGION FACE_DERIVED_REGION FACE_EQUATION_REGION FACE_GMSH_REGION FACE_NEWTIENT_REGION FACE_OUTPUT_REGION FACE_REGION FACE_SETUP_REGION FACE_STATIC_REGION FACE_TRANSIENT_REGION FACE_UNKNOWN_REGION nextgroup=arbName,arbNameError skipwhite
syn keyword arbStatement NODE_CONSTANT_REGION NODE_DERIVED_REGION NODE_EQUATION_REGION NODE_GMSH_REGION NODE_NEWTIENT_REGION NODE_OUTPUT_REGION NODE_REGION NODE_SETUP_REGION NODE_STATIC_REGION NODE_TRANSIENT_REGION NODE_UNKNOWN_REGION nextgroup=arbName,arbNameError skipwhite

syn keyword arbOption bellcondition clearoptions convergencecondition derivative noderivative dynamicmagnitude dynamicmagnitudemultiplier elementdata elementnodedata elementnodelimiteddata inputscale inputinversescale magnitude negative newtstepmax newtstepmin nocheck outputcondition outputscale outputinversescale positive reflect staticmagnitude stopcondition translate
syn match arbOption "\%(\)\<\%(no\)\=deriv\>"
syn keyword arbOption centringinput centringmeshinput centringoutput centringmeshoutput centringdatoutput centringmeshdatoutput centringvtkoutput centringmeshvtkoutput datoutput nodatoutput input noinput meshinput meshoutput meshdatoutput meshvtkoutput output nooutput stepoutput nostepoutput stepoutputnoupdate nostepoutputnoupdate vtkoutput novtkoutput
syn keyword arbOption  compoundelementdata  compoundelementnodedata  compoundelementnodelimiteddata  compoundinput  nocompoundinput  compoundoutput  nocompoundoutput  compoundstepoutput  nocompoundstepoutput  compoundstepoutputnoupdate  nocompoundstepoutputnoupdate
syn keyword arbOption componentelementdata componentelementnodedata componentelementnodelimiteddata componentinput nocomponentinput componentoutput nocomponentoutput componentstepoutput nocomponentstepoutput componentstepoutputnoupdate nocomponentstepoutputnoupdate
syn keyword arbKernelOption automaximumseparation boundarynodeseparations checkminw hyperbolicb hyperbolickernel kernelmethod limitkernelmasktosharednodes maximumseparation maximumcellseparation minimumminw minimumseparation mls none optimisation partialhyperbolickernel polynomialorder polynomialaverageorder polynomialcellorder polynomialnodeorder separationmultipliedtrialkernels shiftboundaryweightcentre shifthyperbolicdistance simple weightseparationmultiplier zerononorientedweights contained
syn keyword arbSolverOption backstepping default hslma28 intelpardiso intelpardisoooc lambdalimitfalseroot lambdalimitfalserootfactor lambdamin linearsolver mgmres none pardiso pardisoiterative stickylambda stickylambdaincrease suitesparse weightlargeequationerrors weightlargeequationerrorsfactor contained
syn match arbDeprecated "\<\%(check_minw\|kernel_method\|limit_mask_to_icell\|minimum_minw\|weight_separation_multiplier\)\>" contained
syn match arbDeprecated "\<\%(minimum\|maximum\)_\%(boundary\|domain\)_separation\>" contained
syn match arbDeprecated "\<polynomial\%(_average\)\=_order\>" contained

syn match arbSystemVar "\%(\)<\%(all\|boundary\|domain\) \%(cells\|faces\|nodes\)>" 
syn match arbSystemVar "<\%(adjacentcellicells\|adjacentcellsignns\|adjacentfacedowncell\|adjacentfaceicells\|adjacentfaceothercell\|adjacentfaceupcell\|boundaries\|celldxkernel\|celldxmax\|celldxmin\|cellicells\|cellknodes\|cellvol\|centralkernel\|crosskernel\|celljfaces\|domain\|downwindfaceicells\|facearea\|facedx\|facedxkernel\|facedivop\|facefromcelldirection\|faceicells\|faceknodes\|glueface\|huge\|hugeish\|icell\|jface\|kernelsum\|knode\|lastface\|limitercontgrad\|limitertolerance\|newtientdelta\|newtres\|newtstep\|nobcelljfaces\|nocadjacentcellicells\|nodedxkernel\|nodeicells\|noloop\|pi\|random\|separation\|separationcentre\d*\|timestep\|tiny\|tinyish\|transientdelta\|upwindfaceicells\)>"
syn match arbSystemVar "<\%(celldx\|cellfromcellx\|cellfromfacex\|cellkernel\|cellkernelregion\|celltoicellr\|celltoseparationicellr\|celltoseparationicellreflect\|celltoseparationicellrsquared\|cellx\)\[l=\%(\d\|:\)\]>"
syn match arbSystemVar "<\%(facedxup\|facedxdown\|facedxunit\|facekernel\|facekernelregion\|facenorm\|facereflect\|facereflectnorm\|facetang1\|facetang2\|facetoicellr\|facex\)\[l=\%(\d\|:\)\]>"
syn match arbSystemVar "<\%(nodekernel\|nodekernelregion\|nodex\)\[l=\%(\d\|:\)\]>"
syn match arbSystemVar "<\%(delta\)\[l=\%(\d,\d\|:\)\]>"
syn match arbNameError "\%(\)<\%(all\|boundary\|domain\) \%(cells\|faces\|nodes\)>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<\%(adjacentcellicells\|adjacentcellsignns\|adjacentfacedowncell\|adjacentfaceicells\|adjacentfaceothercell\|adjacentfaceupcell\|boundaries\|celldxkernel\|celldxmax\|celldxmin\|cellicells\|cellknodes\|cellvol\|centralkernel\|crosskernel\|celljfaces\|domain\|downwindfaceicells\|facearea\|facedx\|facedxkernel\|facedivop\|facefromcelldirection\|faceicells\|faceknodes\|glueface\|huge\|hugeish\|icell\|jface\|kernelsum\|knode\|lastface\|limitercontgrad\|limitertolerance\|newtientdelta\|newtres\|nobcelljfaces\|nocadjacentcellicells\|nodedxkernel\|nodeicells\|noloop\|pi\|random\|separation\|separationcentre\d*\|tiny\|tinyish\|transientdelta\|upwindfaceicells\)>" contained nextgroup=arbUnit skipwhite
syn match arbSystemVar "<\%(newtstep\|timestep\)\%(\[r=\d\+\]\)\=>"
syn match arbNameError "<\%(celldx\|cellkernel\|cellkernelregion\|celltoicellr\|celltoseparationicellr\|celltoseparationicellreflect\|celltoseparationicellrsquared\|cellx\)\[l=\%(\d\|:\)\]>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<\%(facedxup\|facedxdown\|facedxunit\|facekernel\|facekernelregion\|facenorm\|facereflect\|facereflectnorm\|facetang1\|facetang2\|facetoicellr\|facex\)\[l=\%(\d\|:\)\]>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<\%(nodekernel\|nodekernelregion\|nodex\)\[l=\%(\d\|:\)\]>" contained nextgroup=arbUnit skipwhite
syn match arbNameError "<\%(delta\)\[l=\%(\d,\d\|:\)\]>" contained nextgroup=arbUnit skipwhite
syn match arbSystemFlag "<<\%(axialdim\|batchercomment\|cartesiancomment\|cartesianflag\|cylindricalcomment\|cylindricalflag\|dim1comment\|dim2comment\|dim3comment\|nobatchercomment\|radialdim\|radius_c\|radius_f\|radius_n\|radiusdim1flag\|radiusdim2flag\|radiusdim3flag\|reflect=[1-3]\|steadystatecomment\|steadystateflag\|transientcomment\|transientflag\)>>"

syn keyword arbFunction all associatedwith at boundaryof common compound domainof dot ddot gmsh heaviside intersection none normal signum surrounds union variable withinbox contained nextgroup=arbFunctionParen
syn keyword arbFunction cellave cellboundangle celldelta celldiv celldivgrad cellfromcellave                  cellfromfaceave cellfromfacediv cellfromfacedivgrad cellfromfacegrad cellfromnodeave cellfromnodegrad cellgrad cellif celllimiter celllink cellmagnitude cellmax cellmin cellnewtonupdate cellproduct cellsum celltocelllink celltofacelink celltonodelink contained nextgroup=arbFunctionBracket,arbFunctionParen
syn keyword arbFunction faceave faceboundangle facedelta facediv facedivgrad facefromcellave facefromcellgrad                                                                                                       facegrad faceif facelimiter facelink facemagnitude facemax facemin facenewtonupdate faceproduct facesum facetocelllink facetofacelink                contained nextgroup=arbFunctionBracket,arbFunctionParen
syn keyword arbFunction nodeave nodeboundangle nodedelta nodediv nodedivgrad nodefromcellave nodefromcellgrad                                                                                                       nodegrad nodeif nodelimiter nodelink nodemagnitude nodemax nodemin nodenewtonupdate nodeproduct nodesum                                              contained nextgroup=arbFunctionBracket,arbFunctionParen
syn keyword arbFunction noneave noneboundangle nonedelta nonediv nonedivgrad                                                                                                                                        nonegrad noneif nonelimiter nonelink nonemagnitude nonemax nonemin nonenewtonupdate noneproduct nonesum                                              contained nextgroup=arbFunctionBracket,arbFunctionParen
syn keyword arbFunction cellvofd cellvofphiadjust cellvofphishape expand facevofphi contained nextgroup=arbFunctionBracket,arbFunctionParen
syn region arbFunctionBracket matchgroup=arbFunction start="\[" end="\]" contained contains=arbFunctionOption,arbNumber,arbOperator,arbSystemFlag,arbSystemVar,arbUserFlag,arbUserVar nextgroup=arbFunctionParen
syn region arbFunctionParen start="(" end=")" contained contains=arbFunction,arbFunctionBracket,arbFunctionParen,arbFunctionParameter,arbFortranFunction,arbNumber,arbOperator,arbSystemFlag,arbSystemVar,arbUserFlag,arbUserVar
syn keyword arbFunctionOption adjacentcells adjacentcellsevenweighting adjacentcellsweighted advection best box circle cube cylinder downcell dxunit ellipse ellipsoid exact exactpiecewise faceseparation glueface harmonic harmonicweighted highorder lastcell lastface lastfacenoglue limitedharmonic linear linearone lineartwo lower maximumseparation minimumseparation noderivative nodeseparation nonlimitedharmonic noseparation othercell parabolic rectangle reflect separationcentre sphere square upcell upper contained
syn match arbFunctionOption "\%(\)\<sep\%(aration\)\=cent\%(er\|re\)\=\d*" contained
syn match arbFunctionOption "\%(\)\<\%(max\|min\)separation\>" contained
syn match arbFunctionOption "\<\%(l\|r\)\ze=\%(\d\|:\)" contained
syn keyword arbFunctionParameter bound condition d default dt expression faceseparationflag false flux fromregion limiter localregion phif phitol region remoteregion toregion true variable contained
syn match arbFunctionParameter "\<\%(axis\|centre\|gradient\|normal\|size\)\[l=\%(\d\|:\)\]" contained
syn match arbFunctionParameter "\<\%(phi\)\%(\[r=1\]\)\=" contained
syn keyword arbFortranFunction abs acos aint anint asin atan atan2 bit_size ceiling cos cosh dble digits dim dprod epsilon exp exponent floor fraction huge iachar ichar index int len len_trim log log10 max maxexponent min minexponent mod modulo nearest nint precision radix range real rrspacing scale scan set_exponent sign sin sinh spacing sqrt system_clock tan tanh tiny verify contained
syn keyword arbDeprecated AT COMMON COMPOUND GMSH INTERSECTION SURROUNDS UNION VARIABLE contained
syn match arbDeprecated "\<\%(ASSOCIATED WITH\|BOUNDARY OF\|DOMAIN OF\|PART OF\|WITHIN BOX\)\>" contained
syn match arbDeprecated "\<\%(associated with\|boundary of\|domain of\)\>" contained nextgroup=arbFunctionBracket,arbFunctionParen

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
  HiLink arbFilename arbString
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
  HiLink arbUnquotedString arbString
  HiLink arbSystemFlag arbSystemVar
  HiLink arbSystemVar Special
  HiLink arbTodo Todo
  HiLink arbUnit String
  HiLink arbUserFlag String
  HiLink arbUserVar None
  delcommand HiLink
endif

let b:current_syntax = "arb"
