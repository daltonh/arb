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
# makefile for arb doc
# daltonh, v0.54

# assembler needs some variables passed to it
arbdir = "ERROR: not set";
docdir = "ERROR: not set";
rootvar = "ERROR: not set";
contentdir = "ERROR: not set";
htmldir = "ERROR: not set";

assemblerdir = $(docdir)/src/assembler

#bodydir = $(wikidir)/body
#bodycssfile = $(wikidir)/css/arb_body.css

# find all of the markdown files within the body directory and first level of body subdirectories
#markdownfiles := $(wildcard $(htmldir)/*.md) $(wildcard $(htmldir)/*/*.md)
#markdownfiles := $(wildcard $(htmldir)/*/*.md)
markdownfiles :=  $(shell find $(htmldir) -type f -name '*.md')
listfiles :=  $(shell find $(htmldir) -type f -name 'list')
htmlfiles := $(patsubst %.md,%.html,$(markdownfiles))
htmlcontentfiles := $(patsubst %/index.md,%/contents.html,$(markdownfiles))

#all : print createlistfiles createhtmlfiles
all : print createhtmlfiles

# print out some info
print :
	@echo "Compiling arb wiki with options:"
	@echo " arbdir: $(arbdir)"
	@echo " docdir: $(docdir)"
	@echo " rootvar: $(rootvar)"
	@echo " contentdir: $(contentdir)"
	@echo " htmldir: $(htmldir)"
	@echo " markdownfiles: $(markdownfiles)"
	@echo " listfiles: $(listfiles)"
	@echo " htmlfiles: $(htmlfiles)"
	@echo " htmlcontentfiles: $(htmlcontentfiles)"

# this idea courtesy of http://lincolnmullen.com/blog/make-and-pandoc/
createhtmlfiles : $(htmlfiles)

#@echo " doing createhtmlfiles"

# get vimhl script and compile using haskel compiler
#$(assemblerdir)/vimhl.hs :
#	cd $(assemblerdir) && wget https://github.com/lyokha/vim-publish-helper/archive/master.zip
#	unzip master.zip

# compile vimhl.hs script using haskel compiler
$(assemblerdir)/vimhl : $(assemblerdir)/vimhl.hs
	ghc --make "$<" -o "$@"

#%.html : %.md

%.html : %.md $(docdir)/src/makefile $(assemblerdir)/assembler.pl $(assemblerdir)/assembler_pre.html $(assemblerdir)/assembler_post.html $(assemblerdir)/vimhl
	@echo "  INFO: recreating $@"
#cd $(assemblerdir) && ./assembler.pl $< $(rootvar) $(htmldir) $(arbdir) >>debug.log
	cd "$(assemblerdir)" && ./assembler.pl "$<" '$(rootvar)' "$(htmldir)" "$(arbdir)" | pandoc -S -F "$(assemblerdir)/vimhl" -o "$@"
#cd "$(assemblerdir)" && ./assembler.pl "$<" '$(rootvar)' "$(htmldir)" "$(arbdir)" > "$@"




# 
#createlistfiles : $(listfiles)
#	@echo " doing createlistfiles"

#@$(assemblerdir)/assembler.pl $< | pandoc -S -F $(assemblerdir)/vimhl -o $@
	
#@touch $@

# define generic rule to create the htmlfiles
#%.html : %.md
#	pandoc -S -c ../../$(bodycssfile) --toc $< -o $@
#%.html : %.md makefile $(assemblerdir)/assembler.pl $(assemblerdir)/assembler_pre.html $(assemblerdir)/assembler_post.html $(assemblerdir)/vimhl
#	@echo "  INFO: creating $@"
#	@touch $@
#@assembler/assembler.pl $< | pandoc -S -o $@
#@assembler/assembler.pl $< | pandoc -S -c $(bodycssfile) --toc -o $@

#@$(assemblerdir)/assembler.pl $< | pandoc -S -F $(assemblerdir)/vimhl -o $@

# clean all files
#clean : clean_html
# clean all files created during setup process
#clean_html :
#	rm $(htmlfiles)
