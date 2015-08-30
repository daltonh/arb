#!/opt/local/bin/perl -w
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
use Cwd 'getcwd';
my ($line);
my $pandoc="pandoc -t html -f markdown";

use lib './assembler/pandoc-perl/lib';
#use common qw(arbthread chompm empty nonempty protect protectarray error_stop copy_back_input_files);

use Pandoc;
#use Config::Onion;
# my $config = Config::Onion->load('pandoc')->get;
#my $config = Config::Onion->load('assembler/pandoc-perl/lib/Pandoc.pm')->get;
#Pandoc->new($config)->exec(@ARGV);

my $assembler_dir="assembler";
my $assembler_file_pre="$assembler_dir/assembler_pre.html";
my $assembler_file_post="$assembler_dir/assembler_post.html";

# run through file doing any replacements
my $cwd = getcwd;
my $version;
open(SETUPEQS,"<working_dir/src/setup_equations.pl") or die "problem opening setup_equations.pl\n";
while ($line=<SETUPEQS>) {
  if ($line=~/\$version\s*=\s*"\s*(.+)\s*"\s*/) { $version=$1; last; }
}
close(SETUPEQS);
print "<!-- cwd file from assemnbler $cwd: arb version = $version -->"; 

# write material from pre template file to assembler_file
open(ASSEMBLERFILE, "<$assembler_file_pre") or die "ASSEMBLERFILE ERROR: cannot open $assembler_file_pre\n";;
while ($line=<ASSEMBLERFILE>) {
  $line =~ s/<<linkrootdir>>/$cwd/g;
  print $line;
}
close(ASSEMBLERFILE);

# input file
#while ($line=<>) { print $line; }
my $markdownfile;
while ($line=<STDIN>) { $markdownfile=$markdownfile.$line; }

# straight copy
#print $markdownfile;
# copy through pandoc
# eg: print Pandoc->convert( '*hello*' );
#print $htmlfile;
#print Pandoc->convert( $markdownfile );

# now swapping linkrootdir
$markdownfile =~ s/<<linkrootdir>>/$cwd/g;

# do format conversion using pandoc
my $htmlfile=Pandoc->convert( $markdownfile );

#$htmlfile =~ s/linkrootdir/$cwd/g;
print $htmlfile;

# scan for directories
my @content_dirs=glob("body/*/index.md");
my $markdowncontents;
my $content_name;
foreach my $content_dir ( @content_dirs ) {
  ($content_name) = $content_dir =~ /body\/(.*)\/index.md/;
  my $content_link = $content_dir;
  $content_dir =~ s/.md$/.html/;
  $markdowncontents=$markdowncontents."* [$content_name]($cwd/$content_dir)\n";
}
my $htmlcontents=Pandoc->convert( $markdowncontents );

# write material from pre template file to assembler_file
open(ASSEMBLERFILE, "<$assembler_file_post") or die "ASSEMBLERFILE ERROR: cannot open $assembler_file_post\n";;
while ($line=<ASSEMBLERFILE>) {
  $line =~ s/<<linkrootdir>>/$cwd/g;
  $line =~ s/<<version>>/$version/g;
  $line =~ s/<<contents>>/$htmlcontents/g;
 print $line; }
close(ASSEMBLERFILE);

exit;
