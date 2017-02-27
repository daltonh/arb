# example of how to set the ARB_DIR environment variable and include the bin directory into your path under csh/tcsh etc
setenv ARB_DIR "/Users/daltonh/arb/development/arb_develop_v0.57" # set this to the install location of arb - note that this variable purposely isn't used anywhere except for in this script
setenv PATH "${PATH}:${ARB_DIR}/bin"
