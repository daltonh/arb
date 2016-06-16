#!/usr/bin/perl -w
#
# little script to put licence in each of the files listed in licenser.in
# daltonh, 261010
# 
# usage:
# 1) edit arb_licence.txt in licence directory to reflect current status
# 2) list files that require licence to be added in file licenser.in, relative to working directory
# 3) if the file is being licensed for the first time, set variable first = 1
# 4) run script
#
# script looks for start of licence text with # Copyright, and ends with #------------
# comment characters are automatically chosen
#
use strict;
# the following turns on autoflush to these files
use IO::Handle;
STDERR->autoflush(1);
STDOUT->autoflush(1);

my $first = 0; # includes filename for first time licence is added to file
my $workingdir = "../../../";
my $licencefile = $workingdir."licence/arb_licence.txt";
my $tmpfile = $workingdir."tmp/licenser.tmp";

# read in licence file
open(LICENCETEXT, "<".$licencefile) or die "error opening $licencefile\n";
<LICENCETEXT>;
<LICENCETEXT>;
my @licence = <LICENCETEXT>; # read complete licence file into array
close(LICENCETEXT);
print "read in licence file $licencefile\n";

# work through filenames in licenser.in file
open(FILENAMES, "<licenser.in");

my $filename;
my $line;
my $commentchar;
my $writingnow;
my $donecopyright;
my $systemcall;

while($filename=<FILENAMES>) { chompm($filename);
  if ($filename =~ /^#/) {next;}
  if ($filename =~ /\.f90$/) {
    $commentchar = "!";
  } elsif ($filename =~ /\.f$/) {
    $commentchar = "c";
  } elsif ($filename =~ /\.tex$/) {
    $commentchar = "%";
  } else {
    $commentchar = "#";
  }
  print "processing filename = $filename with commentchar = $commentchar";
  open(INFILE, "<".$workingdir.$filename) or die " : error opening file\n";
  open(TEMP, ">".$tmpfile) or die "error opening $tmpfile\n";
  $writingnow = 1;
  $donecopyright = 0;
  while ($line = <INFILE>) {
    if ($writingnow) {
      if (!($donecopyright) && $line =~ /^$commentchar Copyright/) { $writingnow = 0; $donecopyright = 1;}
      else {print TEMP $line;}
    } else {
      if ($line !~ /^$commentchar/) { print "\nERROR: could not find bounding line in file\n"; exit; }
      if ($line =~ /^$commentchar-------/) {
        $writingnow = 1;
# include filename if first is specified
        if ($first) {print TEMP "$commentchar file $filename\n$commentchar\n";}
#       if ($first) {print TEMP "$commentchar-----------------------------------------------------------------------\n"}
# write licence in here
        foreach $line (@licence) {
          print TEMP "$commentchar ".$line;
        }
        print TEMP "$commentchar\n";
        print TEMP "$commentchar-------------------------------------------------------------------------\n"
      }
    }
  }
  close(TEMP);
  close(INFILE);
  if (!($donecopyright)) { print "\nERROR: copyright was not added\n"; exit; }
  if (!($writingnow)) { print "\nERROR: did not find end of copyright statement\n"; exit; }
# copy over file if all is successful
  $systemcall="cp $tmpfile ".$workingdir.$filename;
  (!(system("$systemcall"))) or do {print "\nERROR: could not $systemcall\n"; die;};
  print " success\n";
}

exit;

#-------------------------------------------------------------------------------
# chomp and remove mac linefeads too if present

sub chompm {
  use strict;
  chomp($_[0]);  # remove linefeed from end
  $_[0]=~tr/\r//d; # remove control m from mac files
}

#-------------------------------------------------------------------------------
