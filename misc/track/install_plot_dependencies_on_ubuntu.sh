#!/bin/bash

# script that does the installations required for Lachlan's plot_step_gui
# only for ubuntu
# run as sudo

apt-get install python-pip python-dev python-wxgtk2.8 python-wxtools wx2.8-doc wx2.8-examples wx2.8-headers wx2.8-i18n python-matplotlib
pip install --upgrade numpy
pip install --upgrade pandas
pip install --upgrade numexpr
