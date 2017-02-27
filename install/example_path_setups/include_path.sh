# example of how to set the ARB_DIR environment variable and include the bin directory into your path under sh/bash etc
# include using 'source include_path.csh'
export ARB_DIR="/Users/daltonh/arb/development/arb_develop_v0.57" # set this to the install location of arb - note that this variable purposely isn't used anywhere except for in this script
export PATH="${PATH}:${ARB_DIR}/bin"
