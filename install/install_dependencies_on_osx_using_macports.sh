#!/bin/bash

# this script just installs the macports packages that are required for arb
# this file is specific to osx
# run as sudo, as in 'sudo ./install_dependencies_on_osx_using_macports.sh' after you have:
# 1) installed Xcode with the command line utilities, including opening up the GUI first and accepting the licence agreements, and then doing `xcode-select --install` on the command line
# 2) installed macports following instructions at https://guide.macports.org/#installing

# other things you might like to do after this install has completed:
# 1) install gmsh using their binaries from http://geuz.org/gmsh/#Download
# 2) get syntax highlighting working in vim - see misc/vim_syntax
# 3) install some other linear solvers - see (e.g.) src/contributed/pardiso
# 4) install ifort (if you have a licence)
# 5) install paraview as an alternative post-processor to gmsh
# 6) install wxmaxima for a maxima GUI interface
#       port install wxmaxima

######################
osxfullversion=`sw_vers -productVersion`
osxversion=${osxfullversion:3:2} # extracts substring starting at 3 (0 referenced) and of length 2
echo "INFO: the osx full version number is $osxfullversion, and osx version is $osxversion"

if [ $osxversion -lt 12 ] ; then
  port install maxima # see below for alternatives
fi
# basic arb dependencies
port install gnuplot wget
# install gfortran
port install gcc49 +gfortran gdb
# install SuiteSparse
port install SuiteSparse
# for graphviz and perl graphviz modules (GraphViz2)
port install graphviz p5-graphviz2
# alias module gone from macports
#p5-data-alias

# now as one big list to avoid multiple dialogues
#port install maxima gnuplot wget gcc49 +gfortran gdb SuiteSparse python27 py27-matplotlib py27-numpy py27-scipy py27-pandas py27-numexpr py27-wxpython-2.8 py27-pydot py27-cycler graphviz p5-graphviz2 p5-data-alias
# maxima not working right now under macports, trying wxmaxima binary instead
# p5-data-alias port gone
# port install gnuplot wget gcc49 +gfortran gdb SuiteSparse python27 py27-matplotlib py27-numpy py27-scipy py27-pandas py27-numexpr py27-wxpython-2.8 py27-pydot py27-cycler graphviz p5-graphviz2 p5-data-alias
#port install gnuplot wget gcc49 +gfortran gdb SuiteSparse python27 py27-matplotlib py27-numpy py27-scipy py27-pandas py27-numexpr py27-wxpython-2.8 py27-pydot py27-cycler graphviz p5-graphviz2

# for Lachlan's plot and track scripts
port install python27 py27-matplotlib py27-numpy py27-scipy py27-pandas py27-numexpr py27-wxpython-2.8 py27-pydot py27-cycler
#port select --list python # just for information
port select --set python python27

# some notes on macports/python
#https://astrofrog.github.io/macports-python/
#https://guide.macports.org
#http://truongtx.me/2014/02/25/mac-os-install-python-pip-virtualenv-using-macports/

# the following directories and link are the only non-ports things done in this install script
# finally, link the particular macports gfortran version name to a standard name within an accessible bin directory
echo "INFO: setting up a link to gfortran within /usr/local/bin";
if [ ! -d "/usr/local/bin" ] ; then 
  echo "INFO: first creating /usr/local/bin directory";
  mkdir -p "/usr/local/bin" ;
fi ;
# find all of the gfortran versions, and link to the latest one
gfortran_versions=( `ls /opt/local/bin/gfortran-mp-* 2> /dev/null` );
echo "INFO: found the following gfortran versions: ${gfortran_versions[@]}";
if [ ${#gfortran_versions[@]} -ne 0 ]; then
  if [ -h "/usr/local/bin/gfortran" ] ; then 
    rm /usr/local/bin/gfortran;
  fi;
  echo "INFO: linking from ${gfortran_versions[${#gfortran_versions[@]}-1]} to /usr/local/bin/gfortran";
  ln -s ${gfortran_versions[${#gfortran_versions[@]}-1]} /usr/local/bin/gfortran;
  echo "UNINSTALL: to get rid of this link do rm /usr/local/bin/gfortran";
fi;

# this is info from the macports install of gdb:
# You will need to make sure /System/Library/LaunchDaemons/com.apple.taskgated.plist has the '-p' option, e.g.
#             <key>ProgramArguments</key>
#             <array>
#                     <string>/usr/libexec/taskgated</string>
#                     <string>-sp</string>
#             </array>
#
# Due to kernel caching, you usually need to restart Mac OS X for this option to effect.
if [ $osxversion -lt 12 ] ; then
  echo "INFO: doing a change to /System/Library/LaunchDaemons/com.apple.taskgated.plist to get ggdb (gnu debugger) to work, as advised by macports install - will apparently often require osx restart"
  /usr/bin/perl -p -i -e "s/<string>-s<\/string>/<string>-sp<\/string>/" /System/Library/LaunchDaemons/com.apple.taskgated.plist
fi

if [ $osxversion -ge 12 ] ; then
  echo "WARNING: the macports version of maxima for sierra is currently broken.  Instead, install the binary version following the instructions provided at install/maxima_osx_binary_install"
fi

echo "INFO: you can now add the arb/bin directory to your path and make alternative linear solvers using the include_globally.sh script";
