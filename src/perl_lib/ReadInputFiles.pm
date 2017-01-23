# routines to read arb input files

package ReadInputFiles;

use strict;
use warnings;
use Exporter 'import';
#our $VERSION = '1.00';
our @EXPORT  = qw(read_input_files);

our $code_blocks; # this array needs to be readable by other subroutines, so make global



sub test_variable_scope {
  print "scope_test = $scope_test\n";
}

#--------------------------------------------------------------
# parse all *.arb files

sub read_input_files {

  use FileHandle;
  use List::Util qw( min max );
  use Data::Dumper;
  use Storable qw(dclone);
  use File::Find; # for find
  use File::Spec; # for rel2abs
  my ($oline, $line, $type, $name, $cunits, $units, $multiplier, $mvar, $file_version,
    $mcheck, $typecheck, $tmp, $keyword, $centring, $otype, $match, $tmp1, $tmp2,
    $try_dir, $search, $replace, $working, $comments, $error, $region_constant,
    $condition, $key, $append, $cancel, $default, $masread, $lineremainder, $repeats, $include_type);

  my %region_list = (); # contains the centring and REGION_LIST most recently specified in the input file (as used for REGION_CONSTANT)
  my $default_options = ""; # default options prepended to each statement read in
  my $override_options = ""; # override options appended to each statement read in
  my $skip = 0; # flag to indicate whether we are in comments section or not
  my $indent = "   "; # amount to indent the unwrapped input file for each level of file inclusion

# open unwrapped input file that will be used as a record only, and can be used for subsequent runs
  open(UNWRAPPED_INPUT, ">$unwrapped_input_file");

# this will become a stack of recursively called arb code blocks (which could correspond to a new input file), starting with the root_input.arb file created by the arb script that contains INPUT_WORKING links to the arb files called by the user from the arb script
my @code_blocks;
$code_blocks[0]{"ref_name"}="root_input.arb";
$code_blocks[0]{"name"}="$build_dir/root_input.arb";
$code_blocks[$#code_blocks]{"abs_name"} = File::Spec->rel2abs($code_blocks[0]{"name"}); # abs_name is always the absolute path to the file
# initial include_path used for searching for the file is the working directory
# so, always the initial search path for files is the working_dir (even though root_input.arb sits in the build directory)
$code_blocks[0]{"include_path"}[0] = $working_dir;
$code_blocks[0]{"code_type"} = "solver"; # indicates whether code_block when left is either string or solver
$code_blocks[0]{"buffer"} = ""; # current buffer containing block as it is read in

  $code_blocks[$#code_blocks]{"handle"} = FileHandle->new(); # make a filehandle for the first file (taken from http://docstore.mik.ua/orelly/perl/cookbook/ch07_17.htm)
  my $handle = $code_blocks[$#code_blocks]{"handle"};
  open($handle, "<$code_blocks[$#code_blocks]{name}") or error_stop("problem opening arb input file $code_blocks[$#code_blocks]{name}");

  while (@code_blocks) { # we parse the input until we have removed (ie, dealt with) all code blocks on this stack

# make sure that the handle corresponds to the last file on the code_blocks stack
    $handle = $code_blocks[$#code_blocks]{"handle"};
    my $file = $code_blocks[$#code_blocks]{"ref_name"};
    my $code_type = $code_blocks[$#code_blocks]{"code_type"}; # indicates whether code_block when left is either string or solver

    while ($oline=get_next_code_line($handle)) { chompm($oline); if ($oline) { $oline=~s/^\s*//; $oline=~s/\s*$// }; # remove linefeed from end and space from the start and end

# create a file locator string that can be used in error messages etc to show where the parser is at, and also includes the original line from the file, only minus start/end space and linefeed character
#     my $filelinelocator = "file = $file: linenumber = $.: line = \'$oline\'";
# for the line locator variable (generally used in error messages) use absolute filename (for info messages generally use ref_name, which does not include path)
      my $filelinelocator = "file = $code_blocks[$#code_blocks]{abs_name}: linenumber = $.: line = \'$oline\'";

# now splitting input line at the include/replacements keywords (wherever they are) and only doing replacements before this
      my $lineremainder = '';
      if ($oline =~ /^\s*(.*?)(\s*((INCLUDE(|_[A-Z]+))|((GENERAL_|)REPLACEMENTS))($|#|\s))/i) {
        print DEBUG "INFO: found a line that possibly includes replacement statements: $filelinelocator\n";
        my $linestart = $1;
        $lineremainder = $2.$';
        $oline = '';
        if (!($linestart)) {
          print DEBUG "INFO: include/replacement string is bare, so do not perform any replacements on this line\n";
        } else {
          print DEBUG "INFO: attempting to remove any replace strings from the start of this line\n";
          while ($linestart =~ /^(\s*<<.*?>>\s*)/) { $oline=$oline.$1; $linestart=$'; }
          print DEBUG "INFO: after splitting and looking for replace strings: oline = $oline: linestart = $linestart; lineremainder = $lineremainder\n";
          if (nonempty($linestart)) {
# there is more than string replacements at the start
# to be consistent with previous behaviour do replacements on the entire string
            $oline=$oline.$linestart.$lineremainder;
            $lineremainder='';
            print DEBUG "INFO: search/replace going ahead on entire oline as preamble to include/replacement keywords contains more than only <<>> delimited strings\n";
          } else {
            print DEBUG "INFO: search/replace going ahead on preamble oline only as preamble to include/replacement keywords contains only <<>> delimited strings\n";
          }
        }
      }

# do file-specific replacements on file before anything else
      foreach my $n1 ( reverse( 0 .. $#code_blocks ) ) {
        foreach my $n2 ( 0 .. $#{$code_blocks[$n1]{"replacements"}} ) {
          replace_substrings($oline,$code_blocks[$n1]{"replacements"}[$n2]{"search"},$code_blocks[$n1]{"replacements"}[$n2]{"replace"});
        }
      }
      foreach my $n1 ( reverse( 0 .. $#general_replacements ) ) {
        replace_substrings($oline,$general_replacements[$n1]{"search"},$general_replacements[$n1]{"replace"});
      }

# and now reconstruct the entire line
      $oline = $oline.$lineremainder;
      if ($lineremainder) { print DEBUG "INFO: after replacements and reconstructing: oline = $oline\n"; }

      $line = $oline;
# keep a record of what arb is doing in UNWRAPPED_INPUT, commenting out any INCLUDE or GENERAL_REPLACMENTS statements so that this file could be read again by arb directly
      if ($line =~ /^\s*((INCLUDE(|_[A-Z]+))|((GENERAL_|)REPLACEMENTS))($|#|\s)/i) { print UNWRAPPED_INPUT $indent x $#code_blocks,"#(hash added during unwrap)$line\n"; } else { print UNWRAPPED_INPUT $indent x $#code_blocks,"$line\n"; }

# now process guts of statement
# for the time being, have to handle trailing comments on the input lines

# first check whether skip is active or there is a COMMENTS|SKIP statement or there is an empty line
      if ($skip && $line =~ /^\s*((STOP|END)_(COMMENT(S){0,1}|SKIP))($|#|\s)/i) { print "INFO: found \L$1\E statement in $file\n"; $skip=0; next; }
      elsif ($skip || $line =~ /^\s*($|#)/) {next;}
      elsif ($line =~ /^\s*((START|BEGIN)_(COMMENT(S){0,1}|SKIP))($|#|\s)/i) { print "INFO: found \L$1\E statement in $file\n"; $skip=1; next; }

# check for include statement, possibly opening new file
# ref: include ref: include_template ref: include_local ref: include_absolute ref: include_working ref: include_last
      elsif ($line =~ /^\s*INCLUDE(|_([A-Z]+))($|(\s*#)|\s)/i) {
        if (nonempty($2)) {$include_type = "\L$2";} else { $include_type = ''; }
# note to user re deprecation of INCLUDE_ROOT
        if ($include_type eq "root" || $include_type eq "from") {
          syntax_problem("INCLUDE_"."\U$include_type"." has been deprecated.  Use INCLUDE_TEMPLATE instead which searches through the templates directory tree for a specific file, and at the same time, adds the file's path to the include_path stack.  Or, if the include_path stack already includes the path for the template file, you can just use the INCLUDE command as this searches through the include_path stack.  INCLUDE_"."\U$include_type"." has been replaced by INCLUDE_TEMPLATE in this instance: $filelinelocator","warning");
          $include_type = "template";
        }
        if ($3 =~ /#/) {$line = '';} else {$line = $';}
        my $new_file = extract_first($line,$error); # extract filename from line of text
        if ($error) { error_stop("a valid file or directory name could not be determined from the following: $filelinelocator\n"); }

        if (empty($new_file)) {
          if (nonempty($include_type)) { syntax_problem("the include_path stack can only have its top level removed (popped) using the generic INCLUDE statement, otherwise some error occurred in: $filelinelocator"); }
# an empty include statement means to remove one level off the search_path
          if ($#{$code_blocks[$#code_blocks]{"include_path"}} > 0) {
# this means to pull an item from the stack
            pop(@{$code_blocks[$#code_blocks]{"include_path"}});
            print "INFO: an INCLUDE statement is removing (popping) an include_path from the stack, leaving: include_path = $code_blocks[$#code_blocks]{include_path}[0]\n";
            print UNWRAPPED_INPUT $indent x $#code_blocks,"#(comment generated during unwrap) after one has been removed, currently: include_path = @{$code_blocks[$#code_blocks]{include_path}}\n";
          } else {
            print "WARNING: an INCLUDE statement is attempting to remove an include_path from the stack, but there is only the local path left which cannot be removed: include_path = $code_blocks[$#code_blocks]{include_path}[0]\n";
            print UNWRAPPED_INPUT $indent x $#code_blocks,"#(comment generated during unwrap) after failed removal attempt, only the single local (unremovable) path is left: include_path = $code_blocks[$#code_blocks]{include_path}[0]\n";
          }
          next; # move to next statement
        }

# otherwise we are creating either a new include file or adding to the current file's include_paths
        my $found_name = ''; # this will hold the found file or directory
        my $found_type = ''; # this will specify whether what was found was a file or directory
        my $found_depth = 1; # this is used only for file::find used within the template include
        if (empty($include_type)) {
# if this is a plain INCLUDE statement then we cycle through the list of paths looking for the file or directory
          for my $search_path ( reverse( @{$code_blocks[$#code_blocks]{"include_path"}} ) ) {
            ($found_name,$found_type) = check_for_arbfile_or_dir($search_path.'/'.$new_file);
            if (nonempty($found_name)) {
              print DEBUG "INFO: found include $found_type $new_file at $found_name\n";
              last;
            }
          }
          if (empty($found_name)) {
            error_stop("could not find $new_file that is referenced in an INCLUDE statement in any of the current include paths (ie, the include path stack): $filelinelocator");
          }
        } elsif ($include_type eq "template") {
# the following would only check in the templates directory
#         ($found_name,$found_type) = check_for_arbfile_or_dir($template_dir.'/'.$new_file);
# whereas this cycles through all subdirectories of the templates directory, using a depth-prioritised search
# as file paths are relative to the build directory, don't chdir is required to reference filename
          find ({ wanted => sub { wanted($new_file,$found_name,$found_type,$found_depth); }, no_chdir => 1},$template_dir);
          if (empty($found_name)) {
            error_stop("could not find $new_file that is referenced in an INCLUDE_TEMPLATE statement in any of template directories: $filelinelocator");
          }
#         if (-f $found_name) { $found_type = 'file'; } elsif (-d $found_name) { $found_type = 'directory'; }
        } else {
          if ($include_type eq "absolute") {
            ($found_name,$found_type) = check_for_arbfile_or_dir('/'.$new_file);
          } elsif ($include_type eq "local") {
            ($found_name,$found_type) = check_for_arbfile_or_dir($code_blocks[$#code_blocks]{"include_path"}[0].'/'.$new_file);
          } elsif ($include_type eq "last") {
            ($found_name,$found_type) = check_for_arbfile_or_dir($code_blocks[$#code_blocks]{"include_path"}[$#{$code_blocks[$#code_blocks]{"include_path"}}].'/'.$new_file);
          } elsif ($include_type eq "working") {
            ($found_name,$found_type) = check_for_arbfile_or_dir($working_dir.'/'.$new_file);
          } else {
            error_stop("keyword INCLUDE_"."\U$include_type"." is not understood: $filelinelocator");
          }
          if (empty($found_name)) {
            error_stop("could not find $new_file that is referenced in an INCLUDE_"."\U$include_type"." statement: $filelinelocator");
          }
        }
        print "INFO: found the following include $found_type $new_file at $found_name\n";
        print DEBUG "INFO: found the following include $found_type $new_file at $found_name referenced from: $filelinelocator\n";

# here we extract the path from the full found_name if we have found a file, or remove found_name (and store in found_dir) if we have found a directory
        my $found_dir = '';
        if ($found_type eq 'file' && $found_name =~ /\//) {
          ($found_dir) = $found_name =~ /^(.*)\/.*?$/;
        } elsif ($found_type eq 'directory') {
          $found_dir = $found_name;
          $found_name = '';
        }

# check whether this path is on the top of the current files's include_path list, and if not, add it
        if (nonempty($found_dir)) {
          if ($found_dir ne $code_blocks[$#code_blocks]{"include_path"}[$#{$code_blocks[$#code_blocks]{"include_path"}}]) {
            push(@{$code_blocks[$#code_blocks]{"include_path"}},$found_dir);
            print "INFO: adding new path $found_dir to the include_path stack, making: include_path = @{$code_blocks[$#code_blocks]{include_path}}\n";
            print DEBUG "INFO: adding new path $found_dir to the include_path stack, making: include_path = @{$code_blocks[$#code_blocks]{include_path}}\n";
            print UNWRAPPED_INPUT $indent x $#code_blocks, "#(comment generated during unwrap) adding new include_path $found_dir, making include_path stack = @{$code_blocks[$#code_blocks]{include_path}}\n";
          } else {
            print UNWRAPPED_INPUT $indent x $#code_blocks, "#(comment generated during unwrap) not adding new include_path $found_dir, as it is already on the top of include_path stack = @{$code_blocks[$#code_blocks]{include_path}}\n";
          }
        }
            
# from here on, only concerned with actually including a file
        if (empty($found_name)) {next;}

# create the new include file
        $code_blocks[$#code_blocks+1]{"ref_name"} = $new_file; # ref_name is recorded as the name specified in the INCLUDE statement
        $code_blocks[$#code_blocks]{"name"} = $found_name; # name is the file name including the path, either relative to the build directory or absolute
        $code_blocks[$#code_blocks]{"abs_name"} = File::Spec->rel2abs($found_name); # abs_name is the file name including the absolute path to the file, now just used for user information (see below output file location statement)
        $code_blocks[$#code_blocks]{"include_path"}[0] = $code_blocks[$#code_blocks-1]{"include_path"}[$#{$code_blocks[$#code_blocks-1]{"include_path"}}]; # set local path to last path of calling file

        print "INFO: found INCLUDE $code_blocks[$#code_blocks]{ref_name} statement with include file identified as $code_blocks[$#code_blocks]{abs_name}: $filelinelocator\n";
        print DEBUG "INFO: found INCLUDE $code_blocks[$#code_blocks]{ref_name} statement with include file identified as $code_blocks[$#code_blocks]{abs_name}: $filelinelocator\n";
        print UNWRAPPED_INPUT $indent x $#code_blocks,"#(comment generated during unwrap)++++++++++++++++++++++++++++++++++++++++++++++++++++++++\n",$indent x $#code_blocks,"#(comment generated during unwrap) the following is INCLUDED from $code_blocks[$#code_blocks]{abs_name}";
# ref: FILENAME
# set simulation_info filename based on the first included file (which is the one that will be listed in root_input.arb)
        if (empty($simulation_info{"filename"})) {
          $simulation_info{"filename"} = $code_blocks[$#code_blocks]{"ref_name"};
          $simulation_info{"absfilename"} = $code_blocks[$#code_blocks]{"abs_name"};
        }

# now extract replacements
        while (!($line=~/^\s*(|#.*)$/)) {
          ($search,$replace,$cancel,$default) = extract_replacements($line,$file,$oline);
          if ($cancel) { syntax_problem("string replacements for individual files cannot be cancelled: $filelinelocator"); }
          if ($default) { syntax_problem("default string replacements for individual files are not implemented: $filelinelocator"); }
          if (nonempty($search)) {
            %{$code_blocks[$#code_blocks]{"replacements"}[$#{$code_blocks[$#code_blocks]{"replacements"}}+1]} = ( search => $search, replace => $replace );
          }
        }

        if ($code_blocks[$#code_blocks]{"replacements"}) {
          print "INFO: using the following search/replace combinations during the include of $code_blocks[$#code_blocks]{ref_name}";
          print UNWRAPPED_INPUT " with the following search/replace combinations";
          my $n1 = $#code_blocks;
          foreach my $n2 ( 0 .. $#{$code_blocks[$#code_blocks]{"replacements"}} ) {
            print ": replace $code_blocks[$n1]{replacements}[$n2]{search} with $code_blocks[$n1]{replacements}[$n2]{replace}";
            print UNWRAPPED_INPUT ": replace $code_blocks[$n1]{replacements}[$n2]{search} with $code_blocks[$n1]{replacements}[$n2]{replace}";
          }
          print "\n";
          print UNWRAPPED_INPUT "\n";
        } else {
          print "INFO: not using any search/replace combinations during the include of $code_blocks[$#code_blocks]{ref_name}\n";
          print UNWRAPPED_INPUT " without any search/replace combinations\n";
        }
# now setup handle and open file
        $code_blocks[$#code_blocks]{"handle"} = FileHandle->new(); # make a filehandle for this file
        $handle = $code_blocks[$#code_blocks]{"handle"};
        $file = $code_blocks[$#code_blocks]{"ref_name"};
        open($handle, "<$code_blocks[$#code_blocks]{name}") or error_stop("problem opening arb input file $code_blocks[$#code_blocks]{name}");
        next; # skip to reading the next line (in the included file)

# extract any general replacements, pushing them onto the back of the existing list
# replacements are performed in reverse order, so latest replacement definitions take precedence
      } elsif ($line =~ /^\s*(GENERAL_|)REPLACEMENTS($|\s*#|\s)/i) {
        print "INFO: found GENERAL_REPLACEMENTS statement in file = $file\n";
        if ($2 =~ /#/) {$line = '';} else {$line = $';}
        while (!($line=~/^\s*$/ || $line =~ /^\s*#/)) {
          ($search,$replace,$cancel,$default) = extract_replacements($line,$file,$oline);
          if (nonempty($search)) {
# if the replacement is a default replacement, first need to check whether string is part of an existing input file replacement
            if ($default) {
              foreach my $n1 ( reverse( 0 .. $#code_blocks ) ) {
                foreach my $n2 ( 0 .. $#{$code_blocks[$n1]{"replacements"}} ) {
                  if ($search eq $code_blocks[$n1]{"replacements"}[$n2]{"search"}) { # found search string in input file replacements
                    print "INFO: general replacement (default) search string $search cancelled as the string is already present as an input file string replacement\n";
                    last;
                  }
                }
              }
            }
# v0.42 general replacements of each search string is unique
            my $n = search_general_replacements($search);
            if ($n < 0) { # indicates that string is not found in general_replacements
              if ($cancel) {
                print "WARNING: general replacement search string $search cancelled before being allocated\n";
              } else { # add a new one
                %{$general_replacements[$#general_replacements+1]} = ( search => $search, replace => $replace );
                print "INFO: added general replacements search and replace pair: search = $search: replace = $replace\n";
              }
            } else { # found search as an existing general replacement
              if ($cancel) {
                splice(@general_replacements,$n,1);
                print "INFO: cancelling previous general replacements search string: search = $search\n";
              } elsif ($default) {
                print "INFO: general replacement (default) search string $search cancelled as the string is already present as a general string replacement\n";
              } else {
                $general_replacements[$n]{"replace"}=$replace;
                print "INFO: replaced general replacements search and replace pair: search = $search: replace = $replace\n";
              }
            }

          }
        }
        print DEBUG "INFO: just processed GENERAL_REPLACEMENTS statement: general_replacements = ".Dumper(@general_replacements)."\n";
        next;
      }

# remove any in-line comments now (so from now on, do not have to consider # in any string matching)
      if ($line =~ /^(.*?)#/) { $comments = $'; $line = $1 } else { $comments = ''; }

# check for EXTERNALS statement, which accepts a list of external fortran external files
      if ($line =~ /^\s*EXTERNAL(|S)(\s+)/i) { 
				$line = $';
				while ($tmp = extract_first($line,$error)) {
# check that there weren't any errors, and that text was properly quoted
          if ($error) { error_stop("some type of syntax problem with the EXTERNAL statement.  Should the text be quoted?:\nfile = $file: line = $oline"); }
					create_external_file($tmp);
				}
				next;
			}

# check for END statement
      if ($line =~ /^\s*END($|\s)/i) { print "INFO: found END statement in file = $file\n"; last; }

#---------------------
# ref: deprecated syntax
# check for deprecated syntax and correct if possible
      if ($line =~ /^\s*(CELL_|FACE_|NODE_|NONE_|)(INDEPENDENT|FIELD|DEPENDENT)($|\s)/i) {
        my $deprecatedtype = "\U$2";
        if ($deprecatedtype eq "DEPENDENT") {
          $type = "DERIVED";
        } else {
          $type = "UNKNOWN";
        }
        $line = $`.$1.$type.$3.$';
        syntax_problem("$deprecatedtype type has been deprecated, use $type instead.\nfile = $file\noriginal line = $oline\ncorrected line = $line","warning");
      }

      if ( $line =~ /^\s*(READ_GMSH)($|\s)/i ) {
        error_stop("READ_GMSH keyword has been deprecated, use MSH_FILE instead.\nfile = $file: line = $oline");
      }

      if ( $line =~ /^\s*(LINEAR_SOLVER)($|\s)/i ) {
        error_stop("LINEAR_SOLVER keyword has been deprecated, use SOLVER_OPTIONS linearsolver=default (eg) instead.\nfile = $file: line = $oline");
      }

# check for the user ERROR or WARNING statements that signifies that there is a known problem with a file
      if ($line =~ /^\s*(ERROR|WARNING|INFO)(\s|$)/i) {
        $key="\L$1"; $line=$';
# pull out any string that follows this keyword if there is one
        $tmp = extract_first($line,$error);
        if (empty($tmp)) { $tmp = "no explanation provided"; }
        if ($key eq "error") { error_stop("user error statement found in $file: $tmp"); }
        else { print "\U$key",": user ","\L$key"," statement found in $file: $tmp\n"; }
      }

# check on file version
      elsif ($line =~ /^\s*VERSION\s+(\S*)/i) {
        $file_version = $1;
        if (abs($file_version - $version) > 1.e-7) {
          if ($file_version < $minimum_version) {
            error_stop("version mismatch between $file and the current version (of setup_equations.pl)\n".
                "  You may be able to increase the version number in $file (from $file_version to $version), however some features of the language ".
                "syntax have changed since version $file_version so you should check the input files");
          } else {
            print "WARNING: version mismatch between $file and the current version (of setup_equations.pl)\n".
                "  You should be able to increase the version number in $file (from $file_version to $version) safely without altering the input ".
                "file syntax, however this error indicates that additional language features are now available\n";
          }
        }
        print FORTRAN_INPUT "VERSION $file_version\n"; # also tell the fortran program about this
        next;
      }

# ref: INFO
# look for userable simulation info statements
      elsif ($line =~ /^\s*INFO_(TITLE|DESCRIPTION|AUTHOR|DATE|VERSION)(|\+|-)\s+/i) {
        $key="\L$1"; $line=$';
        if ($2 eq "+") { $append=1; } elsif ($2 eq "-") {$append=-1;} else { $append=0; }
        $tmp = extract_first($line,$error);
# check that there weren't any errors, and that text was properly quoted
        if ($error || $line =~ /\S/) { error_stop("some type of syntax problem with the INFO_"."\U$key"." string.  Should the text be quoted?:\nfile = $file: line = $oline"); }
# a plus signifies to append the string to the current value
# a minus means set the string only if it is empty.
# Otherwise, set the string to the new value ($append=0)
        if ($append eq 1) {
          $simulation_info{$key} = $simulation_info{$key}.$tmp;
        } elsif (!($append) || empty($simulation_info{$key})) {
          $simulation_info{$key} = $tmp;
        }
        next;
      }

# look for transient/steady-state simulation keyword
      elsif ($line =~ /^\s*(TRANSIENT|STEADY-STATE|STEADYSTATE|NONTRANSIENT)_SIMULATION($|\s)/i) {
        print DEBUG "INFO: $1_SIMULATION set from line: $oline\n";
        print "INFO: $1_SIMULATION set directly\n";
        if ($1 =~ /^TRANSIENT$/i) { set_transient_simulation(1); } else { set_transient_simulation(0); }
        next;
      }

# look for newtient simulation keyword
      elsif ($line =~ /^\s*((NON|)NEWTIENT)_SIMULATION($|\s)/i) {
        print DEBUG "INFO: $1_SIMULATION set from line: $oline\n";
        print "INFO: $1_SIMULATION set directly\n";
        if ($1 =~ /^NEWTIENT$/i) { $newtient=1; } else { $newtient=0; }
        next;
      }

# ref: default options
# set or reset any default options for variables (these go before the individual options so any relevant individual options take precedence over these)
# also, each DEFAULT_OPTIONS statement clears previous DEFAULT_OPTIONS statements
      elsif ($line =~ /^\s*DEFAULT_OPTIONS\s*($|\s)/i) {
        $default_options = $';
        ($default_options) = $default_options =~ /^\s*(.*?)\s*$/; # greedy space matches at the front and back remove all leading and trailing space
        if (empty($default_options)) {
          print DEBUG "INFO: default options have been removed via:\nfile = $file: line = $oline\n";
          print "INFO: default options have been removed\n";
        } else {
          print DEBUG "INFO: default options have been set to $default_options via:\nfile = $file: line = $oline\n";
          print "INFO: default options have been set to $default_options\n";
        }
        next;
      }

# ref: override options
# set or reset any override options (these go at the end of the individual options so override any individual options)
# also, each OVERRIDE_OPTIONS statement clears previous OVERRIDE_OPTIONS statements
      elsif ($line =~ /^\s*OVERRIDE_OPTIONS\s*($|\s)/i) {
        $override_options = $';
        ($override_options) = $override_options =~ /^\s*(.*?)\s*$/; # greedy space matches at the front and back remove all leading and trailing space
        if (empty($override_options)) {
          print DEBUG "INFO: override options have been removed via:\nfile = $file: line = $oline\n";
          print "INFO: override options have been removed\n";
        } else {
          print DEBUG "INFO: override options have been set to $override_options via:\nfile = $file: line = $oline\n";
          print "INFO: override options have been set to $override_options\n";
        }
        next;
      }

      elsif ( $line =~ /^\s*(MSH_FILE|((KERNEL|SOLVER|GENERAL)(|_OPTION(|S)))|ITERRESTOL|ITERRESRELTOL|ITERSTEP(MAX|CHECK)|NEWTRESTOL|NEWTSTEP(MAX|MIN|OUT|DEBUGOUT)|TIMESTEP(MAX|MIN|OUT|ADDITIONAL)|TIMESTEPSTART|NEWTSTEPSTART|GLUE_FACES|((TIME|NEWT)STEP_REWIND))($|\s)/i ) {
# these are commands that need to be transferred unaltered to the arb input file
        $keyword = "\U$1";
        $line = $'; $line =~ s/^\s*//;
        if ($keyword =~ /^((KERNEL|SOLVER|GENERAL)(|_OPTION(|S)))$/) {$keyword = "$2_OPTIONS";} # standardise the statement for fortran input
# if this is a GLUE_FACES then check if reflect=? has been specified and if so, set general_replacement string automatically
# not setting this in the arb input file has caught me out so many times that it is now automatic, but can be overwritten after the GLUE_FACES command if need be
        if ($keyword =~ /^GLUE_FACES/i) {
          $tmp = $line;
# strip any region definitions from line, just in case a region contains a reflect=N statement in it...
          while ($tmp =~ /\s*<(.+?)>\s*/) { $tmp = $`." ".$'; }
          while ($tmp =~ /\s*reflect\s*=\s*(\d)\s*/i) {
            my $search="<<reflect=$1>>";
            my $replace="reflect=$1";
            $tmp = $tmp = $`." ".$';
            my $n = search_general_replacements($search);
            if ($n < 0) { error_stop("some type of error with the reflect specification in the following: $oline"); }
            else { $general_replacements[$n]{"replace"}=$replace; }
            print "INFO: based on a GLUE_FACES statement setting $search general_replacements string to $replace\n";
          }
        }
        print FORTRAN_INPUT $keyword; if (nonempty($line)) {print FORTRAN_INPUT " ".$line}; print FORTRAN_INPUT "\n"; # print line to fortran input file
        next;
      }

# # read in glue_face
# # glue_faces are processed in reverse order, and once a face is glued it won't be glued to another
# # so for duplicate glue_face commands, the last one takes precedence, although all are stored in both the perl and fortran
# # for options they are processes as per variable options, moving from left to right - hence the rightmost takes precedence, and the order is important
#       elsif ($line =~ /^\s*GLUE_FACES($|\s)/i) {
#         $line = $';
#         if ($line =~ /^\s*(<.+?>)\s*/) {
#           $glue_face[$#glue_face+1]{"region"}[1] = $1;
#           $line = $';
#         } else { error_stop("a valid region name was not recognised for the following GLUE_FACES definition\nfile = $file: line = $oline"); }
# # see if this glue_face has been defined before, and if so, overwrite previous definition
#         if ($line =~ /^\s*(<.+?>)\s*/) {
#           $glue_face[$#glue_face]{"region"}[2] = $1;
#           $line = $';
#         } else { $glue_face[$#glue_face]{"region"}[2] = $glue_face[$#glue_face]{"region"}[1]; } # if no second region is specified then we set this to the first region (ie, for reflection boundaries)
#         ($glue_face[$#glue_face]{"options"}) = $line =~ /^\s*(.*)\s*$/;
# # now clean up by removing any leading, repeated or trailing commas
#         $glue_face[$#glue_face]{"options"} =~ s/(^\,+\s*)|(\s*\,+$)//;
#         $glue_face[$#glue_face]{"options"} =~ s/\s*\,+\s*/,/g;
#         print "INFO: GLUE_FACE definition between $glue_face[$#glue_face]{region}[1] and $glue_face[$#glue_face]{region}[2] with options $glue_face[$#glue_face]{options} processed\n";
#         print DEBUG "INFO: GLUE_FACE definition between $glue_face[$#glue_face]{region}[1] and $glue_face[$#glue_face]{region}[2] with options $glue_face[$#glue_face]{options} processed\n";
#       }

# read in region_list
      elsif ($line =~ /^\s*(CELL_|FACE_|NODE_|)REGION_LIST($|\s)/i) {
        $line = $';
# reset most recently read region list
        %region_list = ();
# find centring
        $region_list{"centring"} = '';
        if ($1) { $region_list{"centring"} = "\L$1"; ($region_list{"centring"}) = $region_list{"centring"} =~ /^(\S+?)_/; }
# read in regions and store in an array
        while ($line =~ /^\s*(<.+?>)\s*/) { 
          $line = $';
          push(@{$region_list{"regions"}},examine_name($1,'regionname')); # the regions hash contains an array of region names
        }
        if ($line !~ /^\s*$/) {error_stop("there is a syntax error in the following REGION_LIST statement:\nfile = $file: line = $oline");}
        if (empty($region_list{"regions"})) {error_stop("the following REGION_LIST statement contains no regions:\nfile = $file: line = $oline");}
# print some info about this
        $tmp = "INFO: found ";
        if (nonempty($region_list{"centring"})) { $tmp = $tmp."$region_list{centring} centred"; } else { $tmp = $tmp."unknown centring"; }
        $tmp = $tmp." REGION_LIST containing the regions: @{$region_list{regions}}\n";
        print $tmp; print DEBUG $tmp;
        next;
      }

#-----------------------
# user variables, by type and name
# ref: VARIABLE
      elsif ($line =~ /^\s*(CELL_|FACE_|NODE_|NONE_|)(CONSTANT|REGION_CONSTANT|TRANSIENT|NEWTIENT|DERIVED|UNKNOWN|EQUATION|OUTPUT|CONDITION|LOCAL|VARIABLE)($|\s)/i) {
        $line = $';
        $type = "\L$2"; # NB: $2 cannot be empty
        if ($type eq "variable") { $type = ''; }; # the variable keyword is just a placeholder - set type to an empty string
        if ($type eq "region_constant") { $type = "constant"; $region_constant = 1; } else { $region_constant = 0; }
        $centring = ""; # now centring can be grabbed from last definition
        if ($1) { $centring = "\L$1"; ($centring) = $centring =~ /^(\S+?)_/; }

        if ($line =~ /^\s*(<.+?>)($|\s)/) { $name = $1; $line = $'; }
        else { error_stop("problem reading in the variable name from the following line:\nfile = $file: line = $oline");}
        print DEBUG "INFO: found user variable in input file: name = $name: type = $type: centring = $centring\n";
        $name = examine_name($name,"name");
        print DEBUG "  coverting user defined name to consistent name = $name\n";

# see if this name has already been defined, and if so, find position of the variable in the input file
        $masread = -1; # variable masread starts at 0 if any variables are defined
        foreach $mcheck ( 0 .. $#asread_variable ) {
          if ($asread_variable[$mcheck]{"name"} eq $name) { # variable has been previously defined, and position in file is based on first definition
            $masread = $mcheck;
            last;
          }
        }

# check for CANCEL keyword
        if ($line =~ /^(\s*)CANCEL(\s|$)/) {
          if ($masread >= 0) {
            print "INFO: cancelling variable $name\n";
            print DEBUG "INFO: cancelling variable $name\n";
            splice(@asread_variable, $masread, 1);
# also have to adjust the reference to the variables from the regions
            for my $nregion ( 0 .. $#region ) {
              if ($region[$nregion]{"last_variable_masread"} >= $masread) { $region[$nregion]{"last_variable_masread"}=$region[$nregion]{"last_variable_masread"}-1; }
            }
# TODO: get rid of options if variable is cancelled
          } else {
            print "WARNING: attempting to cancel variable $name that hasn't been defined yet - CANCEL ignored\n";
            print DEBUG "WARNING: attempting to cancel variable $name that hasn't been defined yet - CANCEL ignored\n";
          }
          next;
        }

# now create or update variable type and centring
        if ($masread >= 0) {
          $asread_variable[$masread]{"redefinitions"}++; 
          print "INFO: a secondary definition statement (repeat number $asread_variable[$masread]{redefinitions}) for variable $name has been found in file = $file\n";
          print DEBUG "INFO: a secondary definition statement (repeat number $asread_variable[$masread]{redefinitions}) for variable $name has been found based on:\nfile = $file: line = $oline\n";
# a variable has been identified, now check whether the type has changed
          if (nonempty($type)) {
            if ($type ne $asread_variable[$masread]{"type"} && nonempty($asread_variable[$masread]{"type"})) {
              print "NOTE: changing variable $name from type $asread_variable[$masread]{type} to $type\n";
              print DEBUG "NOTE: changing variable $name from type $asread_variable[$masread]{type} to $type based on:\nfile = $file: line = $oline\n";
              $asread_variable[$masread]{"typechanges"}++;
            }
            $asread_variable[$masread]{"type"} = $type;
          } else {
            $type = $asread_variable[$masread]{"type"};
          }
# a variable has been identified, now check whether the centring has changed
          if (nonempty($centring)) {
            if ($centring ne $asread_variable[$masread]{"centring"} && nonempty($asread_variable[$masread]{"centring"})) {
              print "NOTE: changing the centring of variable $name from $asread_variable[$masread]{centring} to $centring\n";
              print DEBUG "NOTE: changing the centring of variable $name from $asread_variable[$masread]{centring} to $centring based on \nfile = $file: line = $oline\n";
              $asread_variable[$masread]{"centringchanges"}++;
# also clear previous region
              if (nonempty($asread_variable[$masread]{"region"})) {
                print "NOTE: during change of centring type region specification of $asread_variable[$masread]{region} deleted for variable $name\n";
                print DEBUG "NOTE: during change of centring type region specification of $asread_variable[$masread]{region} deleted for variable $name\n";
                $asread_variable[$masread]{"region"} = '';
              }
            }
            $asread_variable[$masread]{"centring"} = $centring;
          } else {
            $centring = $asread_variable[$masread]{"centring"};
          }
          $asread_variable[$masread]{"comments"}=$asread_variable[$masread]{"comments"}." ".$comments;
          $asread_variable[$masread]{"filename"}=$asread_variable[$masread]{"filename"}." ".$file;
          $asread_variable[$masread]{"absfilename"}=$asread_variable[$masread]{"absfilename"}." ".$code_blocks[$#code_blocks]{"abs_name"};
        } else {
          print "INFO: a primary definition statement for variable $name has been found in file = $file\n";
          print DEBUG "INFO: a primary definition statement for variable $name has been found based on:\nfile = $file: line = $oline\n";
# otherwise create a new variable
          $masread=$#asread_variable+1;
          print DEBUG "INFO: creating new variable number $masread with name $name based on \n:file = $file: line = $oline\n";
# and set basic info, empty if necessary
          $asread_variable[$masread]{"name"}=$name;
          $asread_variable[$masread]{"type"}=$type;
          if (empty($centring)) { $centring = 'none'; } # default centring if not previously set
          $asread_variable[$masread]{"centring"}=$centring;
          $asread_variable[$masread]{"rindex"}=examine_name($name,"rindex"); # this is based on name so doesn't change with repeat definitions
          $asread_variable[$masread]{"comments"}=$comments;
          $asread_variable[$masread]{"region"}='';
          foreach $repeats (keys(%statement_repeats)) {
            $asread_variable[$masread]{$repeats}=0;
          }
          $asread_variable[$masread]{"options"} = '';
          $asread_variable[$masread]{"filename"}=$file;
          $asread_variable[$masread]{"absfilename"}=$code_blocks[$#code_blocks]{"abs_name"};
        }

# units and multiplier (optional)
# first determine whether any specification has been made
        $units = ''; $multiplier = '';
        if ($line =~ /^\s*\[(.*?)\]/) {
# units specification is present to determine what they are and strip them from the front of $line
          $cunits = $1;
          $line = $';
# split units if multiplier is present
          if ($cunits =~ /\*/) { ($multiplier,$units) = $cunits =~ /(.*?)\*(.*)/; }
          else { $multiplier=""; $units=$cunits;}
          $multiplier =~ s/e|E|D/d/; # convert single and floats to double precision, regardless of case
          if (nonempty($units)) { $asread_variable[$masread]{"units"}=$units; }
          if (nonempty($multiplier)) { $asread_variable[$masread]{"multiplier"}=$multiplier; }
        }
        if (!($asread_variable[$masread]{"units"})) { $asread_variable[$masread]{"units"} = "1"; }
        if (!($asread_variable[$masread]{"multiplier"})) { $asread_variable[$masread]{"multiplier"} = "1.d0"; }

# equations or numerical constants
# look for either a single (CONSTANT) or list (REGION_CONSTANT) of numbers, or otherwise an expression for this variable
# first delete any orphaned initial_equations or constant_lists
        if (!($type eq "transient" || $type eq "newtient") && nonempty($asread_variable[$masread]{"initial_equation"})) {
          print "NOTE: deleting initial equation for $type variable $name that must have been left over from a previous definition\n";
          print DEBUG "NOTE: deleting initial equation for $type variable $name that must have been left over from a previous definition\n";
          delete $asread_variable[$masread]{"initial_equation"};
        }
        if ($type ne "constant" && nonempty($asread_variable[$masread]{"constant_list"})) {
          print "NOTE: deleting constant list (numerical value) for $type variable $name that must have been left over from a previous constant definition\n";
          print DEBUG "NOTE: deleting constant list (numerical value) for $type variable $name that must have been left over from a previous constant definition\n";
          delete $asread_variable[$masread]{"region_list"};
          delete $asread_variable[$masread]{"constant_list"};
        }
# look for numerical constants which must start with either +-. or a digit
        if ( $type eq "constant" && $line =~ /^\s*[\+\-\d\.]/ ) { # to use a numerical constant value the type must be known at read-in time
          print DEBUG "INFO: assuming a numerical constant is entered in the following:\nfile = $file: line = $oline\n";
          if (nonempty($asread_variable[$masread]{"equation"}) || nonempty($asread_variable[$masread]{"initial_equation"})) {
            print "NOTE: resetting CONSTANT $name from an equation form to a numerical form\n";
            delete $asread_variable[$masread]{"equation"}; # preference is to delete these key/values as they then won't be included in %variable (for restart purposes)
            delete $asread_variable[$masread]{"initial_equation"};
          }
          my $n = 1;
          delete $asread_variable[$masread]{"region_list"};
          delete $asread_variable[$masread]{"constant_list"};
          if ($region_constant) {
            if (empty($region_list{"regions"})) { error_stop("a $centring REGION_CONSTANT appears before a REGION_LIST has been defined:\nfile = $file: line = $oline");}
            if ($centring eq "none") { error_stop("attempting to set a none centred constant $name using a REGION_CONSTANT statement: use a NONE_CONSTANT statement instead:\nfile = $file: line = $oline");}
            if (nonempty($region_list{"centring"}) && $region_list{"centring"} ne $centring) { error_stop("the $centring centring of a REGION_CONSTANT is not consistent with the $region_list{centring} centring of the preceeding REGION_LIST:\nfile = $file: line = $oline");}
            @{$asread_variable[$masread]{"region_list"}} = @{$region_list{"regions"}}; # set region_list to that of most recent REGION_LIST
            $n = scalar(@{$region_list{"regions"}}); # returning the number of elements in this array
          }
          while ($line =~ /^\s*([\+\-\d\.][\+\-\ded\.]*)(\s+|$)/i) { # numbers must start with either +-. or a digit, so options cannot start with any of these
            $line = $'; $match = "\L$1";
            if ($match !~ /\d/) { error_stop("a numerical constant was not valid in the following line, indicating some type of syntax error:\nfile = $file: line = $oline"); };
# make sure all constants are written in double precision
            $match =~ s/e/d/;
            if ($match !~ /d/) { $match = $match."d0"; }
            if ($match !~ /\./) { $match =~ s/d/.d/; }
            push(@{$asread_variable[$masread]{"constant_list"}},$match); # assemble list of numerical constants
          }
          if (empty($asread_variable[$masread]{"constant_list"})) { error_stop("no numerial constants were read in from the following line, indicating some type of syntax error:\nfile = $file: line = $oline"); };
          print DEBUG "INFO: found the following constant_list for $name: @{$asread_variable[$masread]{constant_list}}\n";
          if ($region_constant) { print DEBUG "INFO: found the following region_list for $name: @{$asread_variable[$masread]{region_list}}\n"; }
          if ($n ne @{$asread_variable[$masread]{"constant_list"}} ) {
            if ($region_constant) {
              error_stop("the following REGION_CONSTANT line has ".scalar(@{$asread_variable[$masread]{constant_list}})." numerical entries, whereas the preceeding REGION_LIST has $n entries - these should match:\nfile = $file: line = $oline");
            } else { error_stop("a single numerical constant could not be read from the following CONSTANT line:\nfile = $file: line = $oline"); }
          }
        } elsif ( $line =~ /^\s*["']/ ) {
          print DEBUG "INFO: assuming an expression (rather than a numerical constant) is entered in the following:\nfile = $file: line = $oline\n";
          delete $asread_variable[$masread]{"region_list"};
          delete $asread_variable[$masread]{"constant_list"};
# read in expressions, noting that only if the expression is nonempty do we overide previously stored expression
# this allows initial_equation to be reset independently of the equation for transient/newtient variables
          $tmp1 = extract_first($line,$error);
          if ($error) { error_stop("some type of syntax problem with the (first) expression in the following $type $name variable definition:\nfile = $file: line = $oline"); }
          if (($type eq "transient" || $type eq "newtient") && $line =~ /^\s*["']/) { # to set the intial expression the type must be known at read-in time
            $tmp2 = extract_first($line,$error);
            if ($error) { error_stop("some type of syntax problem with the second expression in the following $type $name variable definition:\nfile = $file: line = $oline"); }
# if we are here then tmp1 corresponds to the initial_equation, and tmp2 to the equation
# Note that later when these equations are processed (in organise_user_variables), empty and undef now have different meanings
# depending on variable type and r index, empty ("") now means to repeat the full equation, whereas undef means to give it a value of zero
            my $previous_equation = ""; if (nonempty($asread_variable[$masread]{"initial_equation"})) { $previous_equation = $asread_variable[$masread]{"initial_equation"}; }
            $asread_variable[$masread]{"initial_equation"} = expand_equation($tmp1,$asread_variable[$masread]{"name"},$previous_equation,$oline,$asread_variable[$masread]{"selfreferences"});
            print DEBUG "INFO: setting the $type $name initial_equation to $asread_variable[$masread]{initial_equation} based on:\nfile = $file: line = $oline\n";
# incase we need to only set the initial_equation of a variable, keeping the previous equation value, only set equation if it is actually nonempty (ie, not "")
            if (nonempty($tmp2)) {
              my $previous_equation = ""; if (nonempty($asread_variable[$masread]{"equation"})) { $previous_equation = $asread_variable[$masread]{"equation"}; }
              $asread_variable[$masread]{"equation"} = expand_equation($tmp2,$asread_variable[$masread]{"name"},$previous_equation,$oline,$asread_variable[$masread]{"selfreferences"});
              print DEBUG "INFO: setting the $type $name equation to $asread_variable[$masread]{equation} based on:\nfile = $file: line = $oline\n";
            }
          } else {
# if we are here then tmp1 corresponds to the equation
# save previous equation for possible selfreference replacement, passing an empty string if it hasn't been previously defined
            my $previous_equation = ""; if (nonempty($asread_variable[$masread]{"equation"})) { $previous_equation = $asread_variable[$masread]{"equation"}; }
            $asread_variable[$masread]{"equation"} = expand_equation($tmp1,$asread_variable[$masread]{"name"},$previous_equation,$oline,$asread_variable[$masread]{"selfreferences"});
            print DEBUG "INFO: setting the $type $name equation to $asread_variable[$masread]{equation} based on:\nfile = $file: line = $oline\n";
          }
# check/set defaults for these later, after all variable definitions have been read in
        }

# region
        if ( $line =~ /^\s*ON(\s*)(<.+?>)\s*/i ) {
          if ($2 eq "<none>" ) { # the '<none>' region cancels the previously defined region
            $asread_variable[$masread]{"region"} = '';
          } else {
            $asread_variable[$masread]{"region"} = examine_name($2,'regionname');
          }
          $line = $';
        }

# store raw options in the asread_variable array now
# variable and compound option lists will be assembled later
        $line =~ s/^\s*//; # remove any leading space from the line
        if (nonempty($line) || nonempty($default_options) || nonempty($override_options)) {
          $line = $default_options.','.$line.','.$override_options;
          $line =~ s/(^\,+\s*)|(\s*\,+$)//;
          $line =~ s/\s*\,+\s*/,/g;
          if (empty($asread_variable[$masread]{"options"})) {
            $asread_variable[$masread]{"options"} = $line;
          } else {
            $asread_variable[$masread]{"options"} = $asread_variable[$masread]{"options"}.",".$line;
          }
          print DEBUG "INFO: adding options $line to: name = $name: masread = $masread: options = $asread_variable[$masread]{options}\n";
# now clean up by removing any leading, repeated or trailing commas
          $line = ''; # nothing is now left in the line
        }
      }

#-----------------------
# now processing user regions too, in much the same way as the user variables
# ref: REGION

      elsif ( $line =~ /^\s*((FACE|CELL|NODE)_|)((STATIC|SETUP|GMSH|CONSTANT|TRANSIENT|NEWTIENT|DERIVED|EQUATION|OUTPUT|UNKNOWN)_|)REGION($|\s)/i ) {
        $line = $';
        $centring = ""; # now centring can be grabbed from last definition
        if ($2) { $centring = "\L$2"; }
        $type = ""; # as can type
        if ($4) { $type = "\L$4";}
        if ($type eq "static") {$type="setup";} # setup is the name used in the fortran and perl to denote a user region that is not dynamic, but for the end user, static is easier to comprehend (for the fortran and perl static means !dynamic, which ewals setup, gmsh and system types)

# grab region name
        if ($line =~ /^\s*(<.+?>)($|\s)/) { $name = $1; $line = $'; }
        else { error_stop("problem reading in the region name from the following line:\nfile = $file: line = $oline");}
        print DEBUG "INFO: found user region in input file: name = $name: centring = $centring: type = $type\n";
        $name = examine_name($name,"regionname");
        print DEBUG "  coverting user defined name to consistent name = $name\n";

# see if this name has already been defined, and if so, find its index
        $masread = find_region($name);
        if ($masread >= 0 && ( $region[$masread]{"type"} eq "system" || $region[$masread]{"type"} eq "internal" ) ) {
          error_stop("an attempt is being made to define a region which has a name ($name) that is reserved for system or internal region, in the following:\nfile = $file: line = $oline");
        }

#       $masread = -1; # variable masread starts at 0 if any variables are defined
#       foreach $mcheck ( 0 .. $#region ) {
#         if ($region[$mcheck]{"name"} eq $name) { # region has been previously defined, and position in file is based on first definition
#           $masread = $mcheck;
#           last;
#         }
#       }

# check for CANCEL keyword
        if ($line =~ /^(\s*)CANCEL(\s|$)/) {
          if ($masread >= 0) {
            print "INFO: cancelling region $name\n";
            print DEBUG "INFO: cancelling region $name\n";
            splice(@region, $masread, 1)
          } else {
            print "WARNING: attempting to cancel region $name that hasn't been defined yet - CANCEL ignored\n";
            print DEBUG "WARNING: attempting to cancel region $name that hasn't been defined yet - CANCEL ignored\n";
          }
          next;
        }

# now create or update region type and centring
        if ($masread >= 0) {
          $region[$masread]{"redefinitions"}++; 
          print "INFO: a secondary definition statement (number $region[$masread]{definitions}) for region $name has been found in file = $file\n";
          print DEBUG "INFO: a secondary definition statement (number $region[$masread]{definitions}) for region $name has been found based on:\nfile = $file: line = $oline\n";
# a variable has been identified, now check whether the centring has changed
          if (nonempty($centring)) {
            if ($centring ne $region[$masread]{"centring"} && nonempty($region[$masread]{"centring"})) {
              print "NOTE: changing the centring of region $name from $region[$masread]{centring} to $centring\n";
              print DEBUG "NOTE: changing the centring of region $name from $region[$masread]{centring} to $centring based on \nfile = $file: line = $oline\n";
            }
            $region[$masread]{"centring"} = $centring;
          } else {
            $centring = $region[$masread]{"centring"};
          }
# same with type
          if (nonempty($type)) {
            if ($type ne $region[$masread]{"type"} && nonempty($region[$masread]{"type"})) {
              print "NOTE: changing the type of region $name from $region[$masread]{type} to $type\n";
              print DEBUG "NOTE: changing the type of region $name from $region[$masread]{type} to $type based on \nfile = $file: line = $oline\n";
            }
            $region[$masread]{"type"} = $type;
          } else {
            $type = $region[$masread]{"type"};
          }
          $region[$masread]{"comments"}=$region[$masread]{"comments"}." ".$comments;
          $region[$masread]{"filename"}=$region[$masread]{"filename"}." ".$file;
          $region[$masread]{"absfilename"}=$region[$masread]{"absfilename"}." ".$code_blocks[$#code_blocks]{"abs_name"};
        } else {
          print "INFO: a primary definition statement for region $name has been found in file = $file\n";
          print DEBUG "INFO: a primary definition statement for region $name has been found based on:\nfile = $file: line = $oline\n";
# otherwise create a new region
          $masread=$#region+1;
          print DEBUG "INFO: creating new region number $masread with name $name based on \n:file = $file: line = $oline\n";
# and set basic info, empty if necessary
          $region[$masread]{"name"}=$name;
          $region[$masread]{"centring"}=$centring; # maybe blank
          $region[$masread]{"type"}=$type; # maybe blank
          $region[$masread]{"comments"}=$comments;
          $region[$masread]{"redefinitions"}=0;
          $region[$masread]{"part_of"}='';
          $region[$masread]{"options"}='';
          $region[$masread]{"location"}{"description"}='';
          $region[$masread]{"initial_location"}{"description"}='';
          $region[$masread]{"last_variable_masread"}=$#asread_variable; # this determines when a region will be evaluated, for dynamic regions - it will be -1 if no variables are defined yet
          $region[$masread]{"filename"}=$file;
          $region[$masread]{"absfilename"}=$code_blocks[$#code_blocks]{"abs_name"};
        }

# extract the location string, and if two are present, also an initial_location string (to be used for transient and newtient dynamic regions)
        if ( $line =~ /^\s*["']/ ) {
          $tmp1 = extract_first($line,$error);
          if ($error) { error_stop("some type of syntax problem with a location string in the following region definition:\nfile = $file: line = $oline"); }
          if (nonempty($region[$masread]{"location"}{"description"})) {
            print "NOTE: changing the location of region $name\n";
            print DEBUG "NOTE: changing the location of region $name\n";
          }
          if ( $line =~ /^\s*["']/ ) {
            $tmp2 = extract_first($line,$error);
            if ($error) { error_stop("some type of syntax problem with a location string in the following region definition:\nfile = $file: line = $oline"); }
            if (nonempty($region[$masread]{"initial_location"}{"description"})) {
              print "NOTE: changing the initial_location of region $name\n";
              print DEBUG "NOTE: changing the initial_location of region $name\n";
            }
            $region[$masread]{"location"}{"description"} = $tmp2;
            $region[$masread]{"initial_location"}{"description"} = $tmp1;
            print DEBUG "INFO: extracting region $name location and initial_location string from the following:\nfile = $file: line = $oline\n";
          } else {
            $region[$masread]{"location"}{"description"} = $tmp1;
            print DEBUG "INFO: extracting region $name location string from the following:\nfile = $file: line = $oline\n";
          }
        }

# ON keyword
        if ($line =~ /ON(\s+(<.+?>)($|\s+)|($|\s+))/i) {
          $line = $`.$';
          if ($2) {
            $tmp = examine_name($2,'regionname'); # standardise name here
            if (nonempty($region[$masread]{"part_of"})) {
              print "NOTE: changing the ON region for region $name to $tmp\n";
              print DEBUG "NOTE: changing the ON region for region $name to $tmp\n";
            }
            $region[$masread]{"part_of"} = $tmp;
            print DEBUG "INFO: found ON region $tmp for region $name\n";
          }
          else { $region[$masread]{"part_of"} = ''; print DEBUG "INFO: cancelling any possible ON region for region $name\n"; }
        }
          
# region options
        $line =~ s/^\s*//; # remove any leading space from the line
        if (nonempty($line)) {
          $region[$masread]{"options"} = $region[$masread]{"options"}.",".$line;
          $region[$masread]{"options"} =~ s/(^\,+\s*)|(\s*\,+$)//;
          $region[$masread]{"options"} =~ s/\s*\,+\s*/,/g;
          print DEBUG "INFO: adding options to: region = $region[$masread]{name}: masread = $masread: options = $region[$masread]{options}\n";
          $line = ''; # nothing is now left in the line
        }

        print "INFO: region statement has been read: name = $name: number = $masread: centring = $region[$masread]{centring}: ".
          "type = $region[$masread]{type}: location = $region[$masread]{location}{description}: ".
          "initial_location = $region[$masread]{initial_location}{description}: part_of = $region[$masread]{part_of}\n"; 
        print DEBUG "INFO: region statement has been read: name = $name: number = $masread: centring = $region[$masread]{centring}: ".
          "type = $region[$masread]{type}: location = $region[$masread]{location}{description}: ".
          "initial_location = $region[$masread]{initial_location}{description}: part_of = $region[$masread]{part_of}\n"; 

        next;

#-----------------------
      } else {
# finally if the line doesn't match any of the above, then stop - it may mean that something is not as intended
        error_stop("the following line in $file makes no sense:\n line = $oline");
      }

    } # end of loop for this input file

    close($handle);
    if ($#code_blocks) { print UNWRAPPED_INPUT $indent x $#code_blocks,"#(comment generated during unwrap) INCLUDE finished for $code_blocks[$#code_blocks]{name}\n",$indent x $#code_blocks,"#(comment generated during unwrap)--------------------------------------------------------\n"; }
    pop(@code_blocks);
  } # end of loop for all input files

  close(UNWRAPPED_INPUT);

# dump all of the simulation info into the fortran file, and output to the screen and debug
  $sub_string{"simulation_info"} = '';
  foreach $key ( keys(%simulation_info)) {
    print DEBUG "SIMULATION INFO: "."\U$key"." = $simulation_info{$key}\n";
    if ($key ne "description") { print "SIMULATION INFO: "."\U$key"." = $simulation_info{$key}\n"; }
    print FORTRAN_INPUT "INFO_\U$key"." \"$simulation_info{$key}\"\n"; # also tell the fortran program about this
    $sub_string{"simulation_info"} = $sub_string{"simulation_info"}."! SIMULATION INFO: "."\U$key"." = $simulation_info{$key}\n"; # and write the same to equation_module.f90
  }

}
