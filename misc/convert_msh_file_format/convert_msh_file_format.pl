#!/usr/bin/env perl
#
# simple script which is passed a msh file and produces an arb input file which can be used to convert the file to other formats
# 

use strict;
use warnings;
use File::Path qw(mkpath); # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
my $vtk=0; # whether converting to vtk file (for e.g. paraview)
my $dat=0; # whether converting to dat file (tecplot)
my $run=0; # whether to run arb as well
my $arboptions="-q"; # options to pass to arb when running

# read through all command line arguments
foreach my $argument ( @ARGV )  # first loop looks for distributable files and help request
{
  if ( $argument eq '-h' || $argument eq '--help' ) {
    usage();
  } elsif ( $argument eq '-p' || $argument eq '--paraview' || $argument eq '-v' || $argument eq '--vtk' ) {
    $vtk=1;
  } elsif ( $argument eq '--noparaview' || $argument eq '--novtk' ) {
    $vtk=0;
  } elsif ( $argument eq '-t' || $argument eq '--tecplot' || $argument eq '-d' || $argument eq '--dat' ) {
    $dat=1;
  } elsif ( $argument eq '--notecplot' || $argument eq '--nodat' ) {
    $dat=0;
  } elsif ( $argument eq '-r' || $argument eq '--run' ) {
    $run=1;
  } elsif ( $argument eq '--norun' ) {
    $run=0;
  } elsif ( $argument =~ /\.msh/) {
    my $msh_file=$argument;
    print "INFO: found filename $msh_file\n";
    my $convert_arb_file="convert_msh_file_format.arb";
    open(OUTFILE, ">$convert_arb_file") or die "ERROR: could open $convert_arb_file for writing\n";
    print OUTFILE "MSH_FILE \"$msh_file\" input,nooutput\n";
    if ($vtk) { print OUTFILE "MSH_FILE \"output/output.msh\" noinput,centringvtkoutput\n"; }
    if ($dat) { print OUTFILE "MSH_FILE \"output/output.msh\" noinput,centringdatoutput\n"; }
    open(INFILE, "<$msh_file");
    my $reading_info=0;
    my %variable_data=();
    my $line;
    while ($line=<INFILE>) { chompm($line);
      if ($line=~/^\$arb(\S*)Data\s*$/) {
        $reading_info=1;
        %variable_data=();
      } elsif ($line=~/^\$Endarb(\S*)Data\s*$/) {
        $reading_info=0;
#       print "at printing variable_data = ".%variable_data."\n";
        if ($variable_data{"rank"} eq "scalar") {
          print OUTFILE "\U$variable_data{centring}"."_CONSTANT $variable_data{name} \"0.d0\" input,output\n";
        } else {
          my $basename = $variable_data{"name"}; # note, this name will be in a standard format for a compound
          chop($basename); # removes >
          if ($basename =~ /\]$/) { $basename=$`.","; } else { $basename = $basename."["; } # noting that only if [] are present, they will only contain a r=\d reference in standardised format
          print "name = ".$variable_data{"name"}.": basename = $basename\n";
          if ($variable_data{"rank"} eq "vector") {
            foreach my $n ( 1 .. 3) {
              print OUTFILE "\U$variable_data{centring}"."_CONSTANT ".$basename."l=$n]> \"0.d0\" input,output\n";
            }
          } else {
            foreach my $n1 ( 1 .. 3) {
              foreach my $n2 ( 1 .. 3) {
                print OUTFILE "\U$variable_data{centring}"."_CONSTANT ".$basename."l=$n1,$n2]> \"0.d0\" input,output\n";
              }
            }
          }
        }
      } elsif ($reading_info && $line=~/^\"(\S*?)\=(.*)"$/) {
        $variable_data{$1}=$2;
#       print "set $1 = $variable_data{$1}\n";
      }
    }
    close(INFILE);
    close(OUTFILE);
    print "INFO: created $convert_arb_file\n";

    if ($run) {
# sanity check that we are in an arb directory
      print "INFO: running arb to do the conversion\n";
      if (! -d "src") { die "ERROR: could not find the src directory.  Are you in an arb directory?\n"; } # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
      if (! -f "arb") { die "ERROR: could not find the arb script.  Are you in an arb directory?\n"; } # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
      my $systemcall="./arb $arboptions $convert_arb_file";
      (!(system("$systemcall"))) or die "ERROR: could not $systemcall\n";
    }

  } else {
    die "ERROR: problem with command line argument $argument\n";
  }
}

exit;

#*******************************************************************************
# usage function

sub usage {
  print "\nusage:\n\n".
        "convert_msh_file_format.pl [options] file_to_convert.msh\n\n".
        "where file_to_convert.msh is the name of the msh file that is to be converted,".
        "which must not be output.*.msh (as this is used as the output file name)\n".
        "Options are processed in order, so file type needs to be specified before the\n".
        "corresponding msh file on the command line, but multiple files can be processed\n".
        "This script will produce convert_msh_file_format.arb which can be run by arb to do the conversion\n\n".
        "options include:\n".
        " -h or --help = produce this message\n".
        " -p or --paraview or -v or --vtk = produce vtk output suitable for paraview\n".
        " --noparaview or --novtk = do not produce vtk output (default)\n".
        " -t or --tecplot or -d or --dat = produce dat output suitable for tecplot\n".
        " --notecplot or --nodat = do not produce dat output (default)\n".
        " -r or --run = run arb after producing each convert_msh_file_format.arb\n".
        " --norun = do not run but just produce convert_msh_file_format.arb (default)\n";
  exit;
}

#*******************************************************************************
# chomp and remove mac linefeads too if present

sub chompm {
  use strict;
  chomp($_[0]);  # remove linefeed from end
  $_[0]=~tr/\r//d; # remove control m from mac files
}

#*******************************************************************************
