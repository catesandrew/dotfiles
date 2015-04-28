#!/usr/bin/perl -w
# Usage: ./fdupes.pl <start directory>

use strict;
use Term::ReadKey;
use File::Find;

# testing - 0 for interactive mode, 1 to skip all deletion etc
my $testing = 0;

# skip files smaller than 100 bytes. Set to zero if you like...
my $minsize = 100;

my $filecount = my $bytecount = my $fileschecked = my $wasted = 0;
my %files = ();
&usage unless (@ARGV);

sub wanted {
  return unless -f;
  my $filesize = (stat($_))[7];
  $bytecount += $filesize;
  return unless $filesize > $minsize; # skip small files
  $filecount++;
  push @{$files{$filesize}}, $File::Find::name;
}

find(\&wanted, $ARGV[0] || ".");

# update progress display 1000 times maximum
my $update_period = int($filecount/1000)+1;

if ($fileschecked % $update_period == 0) {
  print "Progress: $fileschecked/$filecount\r";
  # note \r does carriage return, but NO LINE FEED
  # for progress display
}

my @dupesets;
  # list of lists - @{$dupesets[0]} = (file1, file2)
  # where file1 and file2 are dupes
foreach my $size (keys %files) {
  my @entries = @{$files{$size}};
  my $samesizecount = scalar @entries;
  if (@{$files{$size}} == 1) {  # unique size
    $fileschecked++;
    next;
  }

  # duplicates by file size.. Check if files are the same
  while (my $base = shift @entries) {
    # get first entry in list under filesize
    my @dupes = ();
    my $count = 0;
    while ($count <= $#entries) {
      # go through all @entries
      my $compare = $entries[$count];
      if (&same($base, $compare)) {
        # remove "compare" from list so it can't be used
        # on next run
        splice(@entries, $count,1);
        # removed "compare" from list - update progress
        if (++$fileschecked % $update_period == 0) {
          print "Progress: $fileschecked/$filecount\r";
        }
        if (@dupes) {
          # already have some dupes - just add duplicate
          # #n to list
          push @dupes, $compare;
          $wasted += $size;
        } else {
          # no dupes yet - include base file and duplicate
          # #1 in list
          push @dupes, ($base, $compare);
          $wasted += $size;
        }
      } else {
        $count++;
        # only increase counter if not a dupe - note splice
        # will break $array[$position] loop otherwise
      }
    }
    if (@dupes) {
      push @dupesets, \@dupes;
    }
    # "base" file removed from list of files to check - update
    # progress meter
    if (++$fileschecked % $update_period == 0) {
      print "Progress: $fileschecked/$filecount\r";
    }
  }
}
if (@dupesets) {
  my @deletelist = ();
  # at least one set of duplicates exists

  # number of sets of duplicates
  my $dupesetcount = scalar(@dupesets);

  my $dupesetcounter = 0;
  foreach my $setref (@dupesets) {
    if ($testing) {
      print @$setref, "\n";
      next;
    }
    $dupesetcounter++;
    my @dupes = @$setref;
    print "Duplicates found ($dupesetcounter / $dupesetcount)",
        "... Should I keep...\n";
    my $count = 0;
    # print up list of options of which file to keep
    while ($count <= $#dupes) {     # go through all @entries
      my $entry = $dupes[$count];
      print $count + 1, " : $entry\n";
      $count++;
    }

    # alternative options - keep all files, skip to end
    print "0: All\n";
    print "A: Skip all remaining duplicates\n";

    # use ReadKey to get user input
    ReadMode 4; # Turn off controls keys
    my $key = '';
    while (not defined ($key = ReadKey(-1))) {
      # No key yet
    }
    ReadMode 0; # Reset tty mode before exiting

    if ($key eq 'A') {
      # skip any remaining dupes and get to deletion bit
      last;
    }

    # not a number or 'A' - default to zero (ie keep all files)
    $key = '0' unless ($key =~ /^\d+$/);

    if ($key == 0) {      # ALL - don't delete anything
      #print "you chose: ALL\n";
    } elsif (defined $dupes[$key-1]) {
      print "you chose: ", $dupes[$key-1], "\n";
      my @list_to_delete = @dupes;
      # remove file to keep from list
      splice(@list_to_delete, $key-1, 1);
      # add rest to deletelist
      push @deletelist, @list_to_delete;
    } else {
      #print "you chose: invalid number... (nothing will",
      #   " be deleted)\n";
    }
    print "\n";
  }

  # confirm deletion if any files are needing deleting
  if (@deletelist) {
    print "\n------------------------\n";
    print "list of files to delete:\n";
    foreach (@deletelist) {
      print "$_\n";
    }
    print "\nAre you *sure* you want to delete all these files?",
      " (Y/N)\n";
    ReadMode 4; # Turn off controls keys
    my $key = '';
    while (not defined ($key = ReadKey(-1))) {
      # No key yet
    }
    ReadMode 0; # Reset tty mode before exiting
    if (lc($key) eq 'y') {
      print "deleting\n";
      unlink @deletelist;
    } else {
      print "wussing out\n";
    }
  }

  1 while $wasted =~ s/^([-+]?\d+)(\d{3})/$1,$2/;
  print "$wasted bytes in duplicated files\n";
}


# routine to check equivalence in files. pass 1 checks first
# "line" of file (up to \n char), rest of file checked if 1st
# line matches
sub same {
  local($a, $b) = @_;
  open(A, $a) || die;
  open(B, $b) || die;
  if (<A> ne <B>) {   # FIRST LINE is not the same
    return 0;     # not duplicates
  } else {        # try WHOLE FILE
    local $/ = undef;
    return <A> eq <B>;
  }
}

sub usage {
  print "Usage: $0 <start directory>\n";
  exit;
}

