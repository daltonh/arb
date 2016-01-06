#!/usr/bin/env perl
# file misc/plot_step/plot_step.pl
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
# perl script to create gnuplot commands for plotting output.step file contents

# usage: plot_step.pl [x_data:]y1_data1[,y1_data2,...][:y2_data1,y2_data2,...]
#
# plot_step.pl 2 ! will plot column 2 (vertical) against column 1 (horizontal) data
# plot_step.pl 5:2 ! will plot column 2 (vertical) against column 5 (horizontal) data
# plot_step.pl 1:2,3 ! will plot columns 2 and 3 on same vertical axis against column 1
# plot_step.pl 1:2,3:4,5 ! will plot columns 2 and 3 on same vertical axis, and 4 and 5 on second vertical axis, both against column 1
#
# alternatively columns may be referred to by their names, however this whole line will need to be quoted to pass the shell
# plot_step.pl '<Re>:<t>' ! plot <Re> on vertical axis versus <t> on horizontal
#
# Note:
# 1) if two y axis are to be used, two colons (:) must be present
# 2) columns are numbered from 1 (NB, perl @cols, @names and @units arrays start at 0) 
# 3) if data is not specified for x, then column 1 will be used, or column 2 for y, or if 2nd : present, column 3 for y2

use strict;
use warnings;
use List::Util qw(min max);
use File::Path qw(mkpath); # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
use File::Glob ':glob'; # deals with whitespace better
my $ostype_local;

my $n = 0;
while ( $n <= $#ARGV ) {
  if ($ARGV[$n] eq '-h' || $ARGV[$n] eq '--help') {
    usage();
  }
  $n = $n + 1;
}

print "plot_step.pl, an arb misc accessory that calls gnuplot to graph output_step.csv results\n";
print "FOR HELP: plot_step.pl -h\n";

# find a portable ostype: should be darwin or linux
if (!($ENV{'OSTYPE'})) {
  $ostype_local=chompm(lc(`uname -s`));
} else { 
  $ostype_local=lc($ENV{'OSTYPE'});
}

print "PLOTSTEP INFO: current operating system is $ostype_local\n";

my $tmp_dir="tmp/plot_step";
my $plot_step_dir="misc/plot_step";
my $plot_file="$tmp_dir/plot_step.plot";
my $plot_file_pre="$plot_step_dir/plot_step_pre_$ostype_local.plot";
my $plot_file_post="$plot_step_dir/plot_step_post_$ostype_local.plot";
my $gnuplot = "gnuplot";
my $output_file = 0; # this get set if writing to file is requested
my $title = 0;
my $key = 0; # position of key
my @gnuplot_options=(); # an array of options to pass to gnuplot
my @range=(); # range for each data_set

my ($step_file, $line, $command_line_chunk, $command);
my @names=(); # NB, $names[0] is the variable name for the first column
my @units=(); # ditto units
my $refresh = 0; # if refresh is true then plot gets continually updated rather than only being constructed once
my $batcher = 0; # if batcher is true then plot batcher_output/run_*/output_step.csv files on the one plot
my $refresh_interval = 5; # seconds between plot refreshes
my $label_characters = 50; # maximum number of y label characters before a line break
my $pointinterval = 0; # default interval between points when using linespoints

# loop through command line bits looking for file name ($step_file) and plotting command ($command)
$n = 0;
while ( $n <= $#ARGV ) {
  if ($ARGV[$n] eq '-f' || $ARGV[$n] eq '--file') {
    $n = $n + 1;
    if ($n > $#ARGV) {die "PLOTSTEP ERROR: step file name missing on command line\n";}
    $step_file = $ARGV[$n];
  } elsif ($ARGV[$n] eq '-r' || $ARGV[$n] eq '--refresh') {
    $refresh = 1;
  } elsif ($ARGV[$n] =~ /^(-r|--refresh)(\d+)$/) {
    $refresh = 1;
    $refresh_interval = $2;
  } elsif ($ARGV[$n] eq '-b' || $ARGV[$n] eq '--batcher') {
    $batcher = 1;
  } elsif ($ARGV[$n] eq '--linespoints') {
    push(@gnuplot_options,"set style data linespoints");
    if (!($pointinterval)) {$pointinterval = -1;} # a negative here specifies that this should be calculated based on the number of data points
  } elsif ($ARGV[$n] eq '--keybelow') {
    $key = "below";
  } elsif ($ARGV[$n] eq '--keywithin') {
    $key = "within";
  } elsif ($ARGV[$n] eq '--key') {
    $n = $n + 1;
    if ($n > $#ARGV) {die "PLOTSTEP ERROR: key string missing on command line\n";}
    $key = $ARGV[$n];
  } elsif ($ARGV[$n] eq '--pointinterval') {
    $n = $n + 1;
    if ($n > $#ARGV) {die "PLOTSTEP ERROR: pointinterval missing on command line\n";}
    $pointinterval = $ARGV[$n];
  } elsif ($ARGV[$n] eq '-o' || $ARGV[$n] eq '--output') {
    $n = $n + 1;
    if ($n > $#ARGV) {die "PLOTSTEP ERROR: output file name missing on command line\n";}
    $output_file = $ARGV[$n];
  } elsif ($ARGV[$n] eq '--gnuplotoption') {
    $n = $n + 1;
    if ($n > $#ARGV) {die "PLOTSTEP ERROR: gnuplot option missing on command line\n";}
    push(@gnuplot_options,$ARGV[$n]);
  } elsif ($ARGV[$n] eq '--pre') {
    $n = $n + 1;
    if ($n > $#ARGV) {die "PLOTSTEP ERROR: pre file name missing on command line\n";}
    $plot_file_pre=$ARGV[$n];
  } elsif ($ARGV[$n] eq '--post') {
    $n = $n + 1;
    if ($n > $#ARGV) {die "PLOTSTEP ERROR: post file name missing on command line\n";}
    $plot_file_post=$ARGV[$n];
  } elsif ($ARGV[$n] eq '--title') {
    $n = $n + 1;
    if ($n > $#ARGV) {die "PLOTSTEP ERROR: title missing on command line\n";}
    $title=$ARGV[$n];
  } elsif ($ARGV[$n] eq '--logy') {
    push(@gnuplot_options,"set log y; set format y '%.1e' ");
  } elsif ($ARGV[$n] eq '--logy2') {
    push(@gnuplot_options,"set log y2; set format y2 '%.1e' ");
  } elsif ($command) {
    die "PLOTSTEP ERROR: multiple commands seem to be given on the command line?  Maybe you need to quote the command\n";
  } else {
    $command = $ARGV[$n];
  }
  $n = $n + 1;
}

# check that we are called from the working directory
if (!(-d 'misc/plot_step')) { die "PLOTSTEP ERROR: plot_step.pl must be called from an arb working directory (even if a different step file is being referenced)\n"; }

# check that we can find the step_file
if (!($step_file)) {
  if ($batcher) {
    $step_file="batcher_output/run_0/output_step.csv";
  } else {
    $step_file="output/output_step.csv";
  }
  if (!(-e $step_file)) { die "PLOTSTEP ERROR: could not find the step file $step_file: plot_step.pl reads in this file after a simulation has been successfully run\n"; }
} else {
  if (!(-e $step_file)) { die "PLOTSTEP ERROR: could not find the step file $step_file\n"; }
}

# check that gnuplot 4.4+ is available when batcher plots are requested
if($batcher) {
  my $gnuplot_version=qx(gnuplot --version);
  if ($gnuplot_version =~ /gnuplot\s(\S+)\s/) {
  $gnuplot_version=$1;
    if ($gnuplot_version < 4.4) { die
    "PLOTSTEP INFO: --batcher option only compatible with gnuplot 4.4+\n"
    ."local version of gnuplot is $gnuplot_version\n";
    }
  }
}

if ($refresh) {
  if ($output_file) { die "PLOTSTEP ERROR: cannot request refresh and output simultaneously\n"; }
  print "PLOTSTEP INFO: plot will be continually refreshed with refresh interval ".$refresh_interval."s: type ^c to quit\n";
}

# create/clear out tmp directory
if (! -d $tmp_dir) { mkpath($tmp_dir) or die "PLOTSTEP ERROR: could not create $tmp_dir\n"; } # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
if (-e $plot_file) { unlink($plot_file) or die "PLOTSTEP ERROR: could not remove $plot_file\n"; }

# check that pre and post plot files exist for this system type
if (! -e $plot_file_pre) { die "PLOTSTEP ERROR: $plot_file_pre does not exist for this system type\n"; }
if (! -e $plot_file_post) { die "PLOTSTEP ERROR: $plot_file_post does not exist for this system type\n"; }

if ($batcher) {
# check whether arb output variable ordering is identical in all run_* directories
  my @reference_names;
  my @test_names;
  my $iteration=0;
  foreach my $step_dir (bsd_glob("batcher_output/run_*")) {
    if($iteration==1) { #note that @names refers to run_0 @names (i.e. not run_1 @names) when this statement is true
      @reference_names = @test_names; #store run_0 @names for comparison with other run_* @names
    }

    @test_names=(); #clear array for loop testing
    # read through step file creating array of variable names versus columns
    open(STEPFILE, "<$step_dir/output_step.csv");
    while ($line=<STEPFILE>) {
      chompm($line);
      if ($line =~ /^\s*["']{0,1}</) {
        while ($line =~ /^\s*[,]{0,1}\s*["']{0,1}(<.+?>)["']{0,1}/) {
          push(@test_names,$1);
          $line = $';
        }
        last;
      }
    }
    close(STEPFILE);
    if ($iteration >= 1) {
      my $j=0;
      foreach my $test_name (@test_names) { 
        if (!($test_name eq $reference_names[$j])) {
          die "\nERROR: the number of output variables (or possibly their ordering) "
          ."in $step_dir/output_step.csv is different from that in batcher_output/run_0/output_step.csv\n"
          ."--batch option is only valid for identical output variable lists\n";
        }
        $j++;
      }
    }
    $iteration++;
  }
}

# now for both batcher and nonbatcher, read through it creating array of variable names versus columns
open(STEPFILE, "<$step_file");
print "PLOTSTEP INFO: $step_file contains the following column names\n";
while ($line=<STEPFILE>) {
  chompm($line);
  if (!($title)) { if ($line =~ /#\s*TITLE\s*=\s*(.+?)\s*$/) {$title = $1; next;}; } # grab title if it's there
  if ($line =~ /^\s*["']{0,1}</) {
    while ($line =~ /^\s*[,]{0,1}\s*["']{0,1}(<.+?>)["']{0,1}/) {
      push(@names,$1); # first name is $names[0]
      $line = $';
    }
    last;
  }
}
while ($line=<STEPFILE>) {
  chompm($line);
  if ($line =~ /^\s*["']{0,1}\[/) {
    while ($line =~ /^\s*[,]{0,1}\s*["']{0,1}\[(.*?)\]["']{0,1}/) {
      push(@units,$1);
      if ($units[$#units] eq "1") { $units[$#units] = ''; } # 1 symbolises a nondimensional number in arb, so remove the units for plotting purposes
      $line = $';
    }
    last;
  } elsif ($line) { last; } # any other material specifies that no units are going to be found in this file
}
# if pointinterval is to be calculated, count the number of data lines remaining here
if ($pointinterval < 0) {
  my $datapoints=0;
  while ($line=<STEPFILE>) { ++$datapoints; }
  $pointinterval = int($datapoints/25)+1;
  print "PLOTSTEP INFO: setting pointinterval = $pointinterval based on $datapoints datapoints\n";
}
close(STEPFILE);

if ($#names < 0) { die "PLOTSTEP ERROR: no data was found in $step_file\n"; }
for my $n ( 0 .. $#names ) {
  print "  variable name = $names[$n] ";
  if (nonempty($units[$n])) { print "[$units[$n]] "; }
  print "for column = ".scalar($n+1)."\n";
}

# check that a command was issued
if (!($command)) { die "PLOTSTEP ERROR: column specifications were not passed to plot_step.pl\n"; }

print "PLOTSTEP INFO: command line plot specification passed in: $command\n";

my @cols=(); # this will be a 2 dimensional array specifying a column of data, relative to 0 (unlike gnuplot)
             # first index is data set (ie x, y1 or y2) and second index is the subset (ie T1, T2)
my $key_char;
my $data_set=1; # we have the possibility of three data sets (x:y:y2)
my $last_element;
my $column;
my $name;
my $perlcol;

# read in all variable names/column numbers into up to three lots of data_sets
while ($command =~ /^\s*(<|\d{1,}|:|,|\[)/) {
  $key_char = $1;
  $command = $';
  if ($key_char eq ":") { $data_set+=1; if ($data_set > 3) { die "PLOTSTEP ERROR: too many colons (:) are in the plot specification\n"; }}
  elsif ($key_char eq ",") {} # just ignore commas
  elsif ($key_char eq "<" || $key_char =~ /\d{1,}/) {
    $last_element = $#{$cols[$data_set]}; # this will be the currently last element in the array (-1 if array is not set apparently)
#   print "last_element before = $last_element\n";
    if ($key_char eq "<") {
      if ($command !~ /(.+?)>/) { die "PLOTSTEP ERROR: error in command syntax: could not find matching <> delimiters\n"; }
      $name = "<$1>";
      $command = $';
      for $column ( 0 .. $#names+1 ) {
        if ($column > $#names) {
          print "WARNING: $name was not found in $step_file: ignored\n";
        }
        elsif ($name eq $names[$column]) {
          $cols[$data_set][$last_element+1] = $column;
          print "  found $name corresponding to column ".scalar($column+1)."\n";
          last;
        }
      }
    } else {
      $perlcol = $key_char-1; # $cols starts at zero, gnuplot starts at 1
      if ($perlcol > $#names) { die "PLOTSTEP ERROR: column $key_char is larger than number of columns in $step_file\n"; }
      else {
        print "  found column $key_char corresponding to name $names[$perlcol]\n";
        $cols[$data_set][$last_element+1] = $perlcol;
      }
    }
  } elsif ($key_char eq "[") {
    if ($command !~ /(.*?)\]/) { die "PLOTSTEP ERROR: error in command syntax: could not find matching range [] delimiters\n"; }
    $range[$data_set] = "[".$1."]";
    $command = $';
  }
}

# now check that data is assigned for each data set, and if not, assign defaults
my $xdata_set = 1;
my $ydata_set = 2;
my $y2data_set = 0;
if ($data_set == 1) { # only in this case do the indicies have to be reversed
  $xdata_set = 2;
  $ydata_set = 1;
}
if ($data_set == 3) { $y2data_set = 3; }
# and now check that each set has data
# x data
if ($#{$cols[$xdata_set]} < 0) {
  $cols[$xdata_set][0] = 0;
  print "WARNING: x data column not set: setting to $names[0]\n";
} elsif ($#{$cols[$xdata_set]} > 0) {
  die "PLOTSTEP ERROR: too many x data columns specified: only one allowed\n";
}
# y data
if ($#{$cols[$ydata_set]} < 0) {
  $cols[$ydata_set][0] = min(1,$#names);
  print "WARNING: y data column not set: setting to $names[$cols[$ydata_set][0]]\n";
}
# y2 data
if ($y2data_set) {
  if ($#{$cols[$y2data_set]} < 0) {
    $cols[$y2data_set][0] = min(2,$#names);
    print "WARNING: y2 data column not set: setting to $names[$cols[$y2data_set][0]]\n";
  }
}

# open plot file
open(PLOTFILE, ">$plot_file");

# write material from pre template file to plot_file
open(TEMPLATEFILE, "<$plot_file_pre") or die "PLOTSTEP ERROR: cannot open $plot_file_pre\n";;
while ($line=<TEMPLATEFILE>) { print PLOTFILE $line; }
close(TEMPLATEFILE);

# intro comment
print PLOTFILE "# plot_step.pl generated statements follow\n";

# for output file overwrite standard terminal types
if ($output_file) {
  my ($output_file_type) = $output_file =~ /\.(\S{1,4})$/; # match 1 to 4 letter file extensions
  if ($output_file_type eq 'pdf') {
# now using cairo libraries for both png and pdf, on darwin and linux
#   print PLOTFILE "set terminal pdfcairo colour enhanced font \"Helvetica,10\" size 12cm,9cm linewidth 3 dashed\n";
    if ($ostype_local eq 'darwin') {
# scaling on osx seems a little screwy
      print PLOTFILE "set terminal pdfcairo colour enhanced font \"Helvetica,24\" size 28cm,22cm linewidth 6 dashed\n";
#     print PLOTFILE "set terminal pdf colour enhanced fsize 10 size 20cm,16cm dashed\n";
#     print PLOTFILE "set terminal pdfcairo colour enhanced font \"Helvetica,10\" size 14cm,11cm linewidth 2 dashed\n";
#     print PLOTFILE "set terminal pdfcairo colour enhanced font \"Helvetica,10\" size 20cm,16cm linewidth 2 dashed\n";
# set terminal pdf colour enhanced fsize 12 size 20cm,16cm dashed
    } else {
      print PLOTFILE "set terminal pdfcairo colour enhanced font \"Helvetica,12\" size 28cm,22cm linewidth 6 dashed\n";
#     print PLOTFILE "set terminal pdfcairo colour enhanced fsize 12 size 20cm,16cm\n";
#     print PLOTFILE "set terminal pdfcairo colour enhanced font \"Helvetica,10\" size 20cm,16cm linewidth 2 dashed\n";
    }
#set terminal pdf colour enhanced fsize 12 size 20cm,16cm
#set terminal pdf monochrome dashed enhanced fsize 12 size 20cm,16cm
  } elsif ($output_file_type eq 'png') {
#   print PLOTFILE "set terminal png\n";
    print PLOTFILE "set terminal pngcairo colour enhanced font \"Helvetica,12\" size 28cm,22cm linewidth 2 dashed\n";
  } else {
    die "PLOTSTEP ERROR: could not determine output file type from $output_file: file type = $output_file_type\n";
  }
  print PLOTFILE "set output \"$output_file\"\n";
}

# title
if ($title) {print PLOTFILE "set title \"$title\" noenhance\n";}

# set axis titles and ranges
# xlabel
print PLOTFILE "set xlabel \'$names[$cols[$xdata_set][0]]";
if (nonempty($units[$cols[$xdata_set][0]])) { print PLOTFILE " [$units[$cols[$xdata_set][0]]]"; }
print PLOTFILE "\' noenhanced\n";
if (nonempty($range[$xdata_set])) { print PLOTFILE "set xrange $range[$xdata_set]\n"; }

# ylabel
my $sub_set;
my $characters=0;
print PLOTFILE "set ylabel \'";
for $sub_set ( 0 .. $#{$cols[$ydata_set]} ) {
  if ($sub_set > 0) {print PLOTFILE ", ";}
  if ($characters+length($names[$cols[$ydata_set][$sub_set]]) > $label_characters) { print PLOTFILE "\'.\"\\n\".\'"; $characters=0; }
  print PLOTFILE "$names[$cols[$ydata_set][$sub_set]]";
  $characters = $characters+length($names[$cols[$ydata_set][$sub_set]]);
}
if (nonempty($units[$cols[$ydata_set][0]])) { print PLOTFILE " [$units[$cols[$ydata_set][0]]]"; } # units are taken from first variable
print PLOTFILE "\' noenhanced\n";
if (nonempty($range[$ydata_set])) { print PLOTFILE "set yrange $range[$ydata_set]\n"; }

if ($y2data_set) {
  my $characters=0;
  print PLOTFILE "set y2label \'";
  for $sub_set ( 0 .. $#{$cols[$y2data_set]} ) {
    if ($sub_set > 0) {print PLOTFILE ", ";}
    if ($characters+length($names[$cols[$y2data_set][$sub_set]]) > $label_characters) { print PLOTFILE "\'.\"\\n\".\'"; $characters=0; }
    print PLOTFILE "$names[$cols[$y2data_set][$sub_set]]";
    $characters = $characters+length($names[$cols[$y2data_set][$sub_set]]);
  }
  if (nonempty($units[$cols[$y2data_set][0]])) { print PLOTFILE " [$units[$cols[$y2data_set][0]]]"; } # units are taken from first variable
  print PLOTFILE "\' noenhanced\n";
  print PLOTFILE "set y2tics nomirror\nset ytics nomirror\n";
  if (nonempty($range[$y2data_set])) { print PLOTFILE "set y2range $range[$y2data_set]\n"; }
}

# place key, optionally setting the correct number of columns
my $gnuplot_maxcolumns=$#{$cols[$ydata_set]}+1;
if ($y2data_set) { $gnuplot_maxcolumns = $gnuplot_maxcolumns + $#{$cols[$y2data_set]}+1; }
if (!($key)) { if ($gnuplot_maxcolumns > 3) {$key = "below";} else {$key = "within";} }
if ($key eq "below") {
  print PLOTFILE "set key below horizontal maxcolumns $gnuplot_maxcolumns noenhanced\n";
} elsif ($key eq "within") {
  print PLOTFILE "set key noenhanced\n";
} else {
  print PLOTFILE "$key\n"; # otherwise pass whole string to gnuplot
}

if (@gnuplot_options) {
  while (@gnuplot_options) {
    print PLOTFILE pop(@gnuplot_options)."\n";
  }
}

if ($batcher) { # batcher specific plot configuration
  # write plot statements
  # add workaround for lack of native "ls -v" on mac
  print PLOTFILE 'list=system("ls -1d batcher_output/run_* | sed \'s/^\([^0-9]*\)\([0-9]*\)/\1 \2/\' | sort -k2,2n | tr -d \' \'")'."\n";
  print PLOTFILE 'number_of_runs=words(list)'."\n"; #find out the number of runs undertaken
  print PLOTFILE 'run_directory(i)=word(list,i)'."\n"; #function to iterate over the run directories

  print PLOTFILE "\n\nplot";

  my $ls = 1;
  my $first = 1;
  for $data_set ( 1 .. 3 ) {
    if ($data_set ne $ydata_set && $data_set ne $y2data_set) { next; }
    for $sub_set ( 0 .. $#{$cols[$data_set]} ) {
#     if (!($data_set == 1 && $sub_set == 0)) {print PLOTFILE ",";}
      if ($first) { $first = 0; } else { print PLOTFILE ",";}
      print PLOTFILE " \\\nNaN w points pt -1 title '$names[$cols[$data_set][$sub_set]]',";
      print PLOTFILE " \\\nfor [i=1:number_of_runs:1] run_directory(i).\"/output_step.csv\" using ",$cols[$xdata_set][0]+1,":",$cols[$data_set][$sub_set]+1;
#     print PLOTFILE " ls $ls";
      print PLOTFILE " ls i";
      if ($pointinterval) { print PLOTFILE " pi $pointinterval"; }
#     if ($data_set > 1) { print PLOTFILE " axes x1y2";}
      if ($data_set == $y2data_set) { print PLOTFILE " axes x1y2";}
      #print PLOTFILE " title \'$names[$cols[$data_set][$sub_set]]\'";
      print PLOTFILE " title \"run_\".(i-1)";
      ++$ls;  if ($ls > 24) { $ls = $ls - 24; } # maximum number of linestyles
    }
  }
} else { # non-batcher plot specifications
  print PLOTFILE "\nplot";
  my $ls = 1;
  my $first = 1;
  for $data_set ( 1 .. 3 ) {
    if ($data_set ne $ydata_set && $data_set ne $y2data_set) { next; }
    for $sub_set ( 0 .. $#{$cols[$data_set]} ) {
      if ($first) { $first = 0; } else { print PLOTFILE ",";}
#     if (!($data_set == 1 && $sub_set == 0)) {print PLOTFILE ",";}
      print PLOTFILE " \\\n\'$step_file\' using ",$cols[$xdata_set][0]+1,":",$cols[$data_set][$sub_set]+1;
      print PLOTFILE " ls $ls";
      if ($pointinterval) { print PLOTFILE " pi $pointinterval"; }
      if ($data_set == $y2data_set) { print PLOTFILE " axes x1y2";}
      print PLOTFILE " title \'$names[$cols[$data_set][$sub_set]]\'";
      ++$ls;  if ($ls > 24) { $ls = $ls - 24; } # maximum number of linestyles
    }
  }
}
print PLOTFILE "\n\n";

# write material from post template file to plot_file
open(TEMPLATEFILE, "<$plot_file_post") or die "PLOTSTEP ERROR: cannot open $plot_file_post\n";
while ($line=<TEMPLATEFILE>) { print PLOTFILE $line; }
close(TEMPLATEFILE);

if ($output_file) { print PLOTFILE "unset output\n"; }

# include refresh statements at the end if required
if ($refresh) { print PLOTFILE "\npause $refresh_interval\nreread\n"; }

close(PLOTFILE);

# now do the plot
my ($systemcall);
$systemcall="$gnuplot -raise $plot_file"; # raise option will raise the plot when it is first done, but noraise option will keep it in the background on replotting - thanks gnuplot - too easy
system("$systemcall");

exit;

#-------------------------------------------------------------------------------
# chomp and remove mac linefeads too if present

sub chompm {
  use strict;
  chomp($_[0]);  # remove linefeed from end
  $_[0]=~tr/\r//d; # remove control m from mac files
  return $_[0];
}

#-------------------------------------------------------------------------------
# usage function
sub usage {
  print "plot_step.pl, an arb misc accessory that calls gnuplot to graph output_step.csv results\n\n";
  print "USAGE: call from working directory using \'plot_step.pl [options] [<x_data>:]<y_data>[:<y2_data]\'\n";
  print "  Note:  two colons must be given for y2 data to be specified\n";
  print "OPTIONS:\n";
  print " -f|--file <inputfile.csv>: call from any working directory with specific reference to another file\n";
  print " -r|--refresh: continuously refresh the plot, using the default refresh interval (5s)\n";
  print " -rN|--refreshN: continuously refresh the plot, using an interval of Ns\n";
  print " -b|--batcher: plot data from batcher_output/run_* directories\n";
  print " -o|--output <outputfile.[pdf|png]>: create image rather than displaying plot.  Supports pdf and png via cairo libraries.\n";
  print " --pre <prefile.plot>: alternative prepend gnuplot plotting command file\n";
  print " --post <postfile.plot>: alternative postpend gnuplot plotting command file\n";
  print " --title \"A title\": specify a title\n";
  print " --key \"set key some gnuplot specification\": specify a format statement that is used instead of the default key format (eg, for a key position)\n";
  print " --keywithin: place key within plot\n";
  print " --keybelow: place key within plot\n";
  print " --linespoints: include points as well as lines on the graph\n";
  print " --pointinterval n: this specifies the skip between points when drawing linespoints.  A negative value (default) tries to use about 25 points across the graph\n";
  print " --logy: log y axis\n";
  print " --logy2: log y2 axis\n";
  print " --gnuplotoption \"set key some gnuplot option\": some option that is passed to gnuplot just before the plot command. ".
    "For multiple options use multiple --gnuplotoption's\n";
  print "\n To specify a range for an axis, include it in the relevant list of variables using gnuplot syntax: eg '<u>[0:10]:<t>[0:]'\n";
  print " The plot file that this script produces is tmp/plot_step/plot_step.plot:  You can use this as a basis for further customisation\n";
  die;
}
#-------------------------------------------------------------------------------
# little subroutine that tests whether a variable has been defined and/or holds anything

sub empty {
  use strict;
  if (!(defined($_[0]))) {
    return 1;
  } elsif ($_[0] eq "") {
    return 1;
  } else {
    return 0;
  }
}

#-------------------------------------------------------------------------------
# opposite of empty

sub nonempty {
  use strict;
  if (empty($_[0])) { return 0; } else { return 1; }
}

#-------------------------------------------------------------------------------
