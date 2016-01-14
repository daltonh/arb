#!/usr/bin/env perl

use strict;
use warnings;
use Data::Dumper;
use Thread;
use File::Glob ':glob'; # deals with whitespace better

use lib './misc/batcher';
use common qw(arbthread chompm empty nonempty protect protectarray error_stop);

my $run_id; # this will be passed to the script as a command line option
$run_id = int($ARGV[0]); # batcher.pl will call via `run_and_collect $run_id`

my %present_case = ();
my $run_record_dir;
our $output_dir = 'batcher_output';
our %output = ();
my ($n, $ndir, $prune_output_structure);
my @case = (); # read in case using eval, deals with the possibility that batcher_setup.pm may have changed during the time the job has spent in the pbs queue

our $input_dir="$output_dir/input_files"; # this will contain copies of the original arb, geo and msh input files

# here we extract the required information using eval
my $infile = "batcher_output/run_$run_id/batcher_pbs_variables.txt";
open(FILE,"<",$infile);
undef $/; # $/ is the input file record separator
eval <FILE>;
close FILE;
$/="\n"; # $/ is the input file record separator

my $stopfile = 'batcher_stop';
$case[$n] = \%present_case; # this is a case that is passed to the thread

&arbthread(\@case, $n, $ndir, $run_record_dir);
