#!/usr/bin/perl -w
# file src/contributed/higham88_sonest/donest_generator.pl
#
# Copyright 2009-2014 Dalton Harvie (daltonh@unimelb.edu.au)
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
# perl script to extract the fortran 77 subroutine sonest from file 674 included with the higham88 paper, and then modify this routine
#  to use double precision rather than reals

use strict;
my ($line);

my $found_sonest = 0;
open(SONEST,"<674") or die "ERROR: file 674 does not exist in this directory.\n";
while($line=<SONEST>) {
  chompm($line);
  if (!($found_sonest)) {
    if ($line =~ /^\s*SUBROUTINE SONEST\s*/) {
      $found_sonest = 1;
      open(DONEST,">donest.f") or die "ERROR: could not open file donest.f for writing\n";
    } else { next; }
  }

# if we are here then we are in the guts of the routine - do any replacements and write to donest.f
  if (!($line =~ /^C/)) { # exclude comments from replacments
# some one-off larger replacements
    if ($line =~ /^\s*SUBROUTINE SONEST\s*/) { $line = $line."\n      IMPLICIT DOUBLE PRECISION (A-H,O-Z)"; } # include implicit double precision specifier
    if ($line =~ /^\s*PARAMETER \(ZERO/) { $line = '      PARAMETER (ZERO = 0.0D0, ONE = 1.0D0, TWO = 2.0D0)'; } # change specific line
# now do smaller replacements
    $line =~ s/ SONEST / DONEST /;
    $line =~ s/(^\s*)(REAL)(\s+)/$1DOUBLE PRECISION$3/;
    $line =~ s/REAL\s*\(/DBLE(/g;
    $line =~ s/ISAMAX\s*\(/IDAMAX(/g;
    $line =~ s/SASUM\s*\(/DASUM(/g;
    $line =~ s/SCOPY\s*\(/DCOPY(/g;
  }

# print line to donest
  print DONEST "$line\n";

# look for end of subroutine and end if successful
  if ($line =~ /^\s*END\s*$/) {
    print "SUCCESS: file donest.f created\n";
    exit
  }

}

die "ERROR: file donest.f was not created successfully\n";

exit;
  
#-------------------------------------------------------------------------------
# chomp and remove mac linefeads too if present

sub chompm {
  use strict;
  chomp($_[0]);  # remove linefeed from end
  $_[0]=~tr/\r//d; # remove control m from mac files
}

#-------------------------------------------------------------------------------
