#!/usr/bin/env perl
# This file is called filter.pl
# from Lachlan, 12/8/14

# this is an idea for filtering out specific views.
#./filter.pl '<ls>' '<ls_phi>'
#will take all the output.*.msh files in current directory and make a series of files called filtered.*.msh that contain only <ls> and <ls_phi>.
#The --tar option will zip them all up so that it's more feasible to send the data over ssh.

# modified to work with all files that glob to output*.msh, daltonh, 12/8/14

use strict;
use warnings;
use File::Glob ':glob';
use Getopt::Long;

my $tar;
GetOptions(
"tar" => \$tar,
);

# add shell arguments to views array
my @views = ();
foreach (@ARGV) {
  push (@views,$_);
}

my $output_filename = '';
my @tar_list = ();
my $switch = 0;
my $data_type = '';

foreach my $filename (bsd_glob("{output*.msh}")) { 
  (my $output_filename = $filename) =~ s/output/filtered/g;
  push (@tar_list,$output_filename);
  open(DATA,"<",$filename);
  open(OUTPUT,">",$output_filename);

  while (<DATA>) {
    if (/^\$(Data|ElementData|ElementNodeData)/) {
      $data_type = $1;
      process_data();
      $switch = 0;
    } elsif (/^\$(arbData|arbElementData|arbElementNodeData)/) {
      skip_data()
    } else {
      print OUTPUT;
    }
  }

  sub process_data {
    my $line;
    while ($line = <DATA>, $line !~ /^\$(EndData|EndElementData|EndElementNodeData)/) {
      foreach (@views) {
        if ($line =~ /^\"$_/) {
          print OUTPUT "\$$data_type\n1\n";
          $switch = 1;
        }
      }
      if ($switch) {
        print OUTPUT "$line";
      }
    }
    if ($switch) {
      print OUTPUT "\$End$data_type\n";
    }
  }

  sub skip_data {
    my $line;
    while ($line = <DATA>, $line !~ /^\$End(arbElementData|arbData|arbElementNodeData)/) {
      #do nothing
    }
  }
  close(DATA);
  close(OUTPUT);
}


if ($tar) {
  my $tar_arguments = join(' ', @tar_list);
  print "INFO: runnning tar\n";
  system("tar -cvzf filtered.tar.gz $tar_arguments");
}
