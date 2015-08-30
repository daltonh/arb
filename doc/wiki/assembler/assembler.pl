#!/usr/bin/perl -w
# perl script to assemble the contents pages for the arb wiki, using pandoc
# daltonh, 30/8/15

# usage
# call from wiki working directory for now
# input markdown page, and output html assembled page as in:
# ./assembler/assembler <body/introduction/contents.md >body/introduction/index.html

use strict;
use warnings;
use Data::Dumper;
use File::Path qw(mkpath rmtree); # for File::Path version < 2.08, http://perldoc.perl.org/File/Path.html
use File::Copy qw(move copy);
use File::Glob ':glob'; # deals with whitespace better
my ($line);
my $pandoc="pandoc -t html -f markdown";

my $assembler_dir="assembler";
my $assembler_file_pre="$assembler_dir/assembler_pre.html";
my $assembler_file_post="$assembler_dir/assembler_post.html";

# write material from pre template file to assembler_file
open(ASSEMBLERFILE, "<$assembler_file_pre") or die "ASSEMBLERFILE ERROR: cannot open $assembler_file_pre\n";;
while ($line=<ASSEMBLERFILE>) { print $line; }
close(ASSEMBLERFILE);

# straight copy
#while ($line=<>) { print $line; }
# copy through pandoc
while ($line=<>) { print $line; }

# write material from pre template file to assembler_file
open(ASSEMBLERFILE, "<$assembler_file_post") or die "ASSEMBLERFILE ERROR: cannot open $assembler_file_post\n";;
while ($line=<ASSEMBLERFILE>) { print $line; }
close(ASSEMBLERFILE);

exit;
