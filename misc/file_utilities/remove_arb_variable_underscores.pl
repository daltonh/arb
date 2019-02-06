#!/usr/bin/env perl
# little script that replaces all spaces within variable and region names within a specified file
# call using:
#  ./remove_arb_variable_underscores.pl filename.arb
# after using this you need to remove the *.bak file, which is just a copy of the original
# WATCH OUT though, there are some cases where this search/replace can go wayward:  eg R "<phi" W "<foo" becomes R "<phi"_W_"<foo"

use strict;
use warnings;

my $file=$ARGV[0]; # first argument is the filename to work on

foreach my $file ( @ARGV ) {
# bak replacement grabbed from https://stackoverflow.com/questions/6994947/how-to-replace-a-string-in-an-existing-file-in-perl
  rename($file, $file.'.bak') or die "Could no rename file $file to $file.bak\n";
  open(IN, '<'.$file.'.bak') or die "Could not open input file $file.bak\n";
  open(OUT, '>'.$file) or die "Could not open output file $file\n";
  while(<IN>)
  {
  # while ($_ =~ /<[^>]* /) {
  #   $_ =~ s/(<[^>]*) /$1_/g;
    while ($_ =~ /([^<]|^)<[^<>\[]* /) { # idea: match only a single < followed by anything up to a space, except for > and [ which close the variable name
      $_ =~ s/(([^<]|^)<[^<>\[]*) /$1_/g; # have to keep repeating the search as the search string is used in subsequent replacements
    }
    print OUT $_;
  }
  close(IN);
  close(OUT);
}
