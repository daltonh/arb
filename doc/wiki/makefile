# arb finite volume solver
# Copyright 2009-2015 Dalton Harvie (daltonh@unimelb.edu.au)
# 
# This file is part of arb finite volume solver, referred to as `arb'.
# 
# arb is a software package designed to solve arbitrary partial
# differential equations on unstructured meshes using the finite volume
# method.  Primarily it consists of fortran source code, perl source
# code and shell scripts.  arb replies on certain third party software
# to run, most notably the computer algebra system maxima
# <http://maxima.sourceforge.net/> which is released under the GNU GPL.
# 
# The copyright of arb is held by Dalton Harvie.
# 
# arb is released under the GNU GPL.  arb is free software: you can
# redistribute it and/or modify it under the terms of the GNU General
# Public License (version 3) as published by the Free Software Foundation.
# You should have received a copy of the GNU General Public Licence
# along with arb (see file licence/gpl.txt after unpacking).  If not,
# see <http://www.gnu.org/licences/>.
# 
# arb is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
# FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public Licence
# for more details.
# 
# For full details of arb's licence see the licence directory.
# 
# The current homepage for the arb finite volume solver project is
# <http://people.eng.unimelb.edu.au/daltonh/downloads/arb>.
#
#-------------------------------------------------------------------------
# makefile for arb wiki
# daltonh, v0.54

rootdir = .
bodydir = $(rootdir)/body
bodycssfile = $(rootdir)/css/arb_body.css

# find all of the markdown files within the body directory and first level of body subdirectories
markdownbodyfiles := $(wildcard $(bodydir)/*.md) $(wildcard $(bodydir)/*/*.md)
htmlbodyfiles := $(patsubst %.md,%.html,$(markdownbodyfiles))

all : createhtmlbodyfiles

# print out some info
print :
	@echo "Compiling arb wiki with options:"
	@echo " markdownbodyfiles: $(markdownbodyfiles)"
	@echo " htmlbodyfiles: $(htmlbodyfiles)"

# this idea courtesy of http://lincolnmullen.com/blog/make-and-pandoc/
createhtmlbodyfiles : $(htmlbodyfiles)

# define generic rule to create the htmlbodyfiles
%.html : %.md
	pandoc -S -c ../../$(bodycssfile) --toc $< -o $@

# clean all files
clean : clean_html
# clean all files created during setup process
clean_html :
	rm $(htmlbodyfiles)
