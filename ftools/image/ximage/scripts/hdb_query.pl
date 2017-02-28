#!/usr1/local/bin/perl5
#
#  hdb_query.pl - Query database using xbrowse_extract.pl,
#                 transforming the pipe-delimited output
#                 into comma-delimited format parsable by XGTARG
# Usage:
#   hdbquery.pl table=table [position=name_or_position]
#               [coordinates=csys] [equinox=year] [radius=arcmin]
#               [name_resolver=NED/SIMBAD]
#               [infile=input_list] [outfile=output_file]
#               [fields=STANDARD/ALL/field1,field2,field3]
#               [debug]
#
#  Version 1.0 -- Aug. 24, 1998
#  Micah Johnson
#
#  Version 1.1 -- Sept. 16, 1998
#  Micah Johnson
#  Quote parameters being passed to browse_extract.pl
#  And prevent endless loop when operation fails (Kept looking for |)
#
#  Version 1.2 -- Nov. 30, 1999
#  Run xbrowse_extract.pl instead of browse_extract.pl to freeze
#  behavior in case changes to script are not backwards-compatible
#
#  Version 1.3 -- Mar. 3, 2000
#  Fix bug in finding ra and dec column numbers
#
#  Version 1.4 -- Aug. 7, 2000
#  Fix bug in treatment of ambiguous column names
#  For example 'ra' matched 'ra' and 'ra_dec'
#  With modifications only exact matches are accepted
#
#  Version 1.5 -- Sept. 19, 2001
#  browse_extract.pl now returns 50 results by default unless
#  given a resultmax value.  Automatically set resultmax to 
#  zero to emulate former behavior of returning all results
#
#  Version 1.6 -- Apr. 9, 2002
#  browse_extract.pl reverted back to old behavior, so tacking
#  on resultmax=0 unnecessary
#

#$local_url = "/cgi-bin/W3Browse/w3nquery.pl";
#$local_host = "legacy.gsfc.nasa.gov";

if ($local_url) { push @sitedep, "url=$local_url"; }
if ($local_host) { push @sitedep, "host=$local_host"; }

#
# Trap fields, outfile, and debug options
#

$i = 0;
$list = 0;
while ( $i < @ARGV ) {
   ($key, $val) = split /\=/, $ARGV[$i];
   if ($key =~ /^fields/i) {
      @fields = split /\,/, $val;
      splice (@ARGV, $i, 1);
   } elsif ($key =~ /^outfile/i) {
      $outfile = $val;
      splice (@ARGV, $i, 1);
   } elsif ($key =~ /^debug/i) {
      $debug = 1;
      splice (@ARGV, $i, 1);
   } elsif ($key =~ /^table/i) {
      if ( $val eq "" ) {
         $list = 1;
      }
      $i++;
   } else {
      $i++;
   }
}
if (@fields == 0) {
   $custom_fields = 0;
} elsif (@fields <= 1 && $fields[0] =~ /^(all|standard)/i) {
   $custom_fields = 0;
   push @ARGV, "fields=$1";
} else {
   $custom_fields = 1;
   push @ARGV, "fields=all";
}

#
#  Enclose parameter values in quotes
#
$browse_args = "";
foreach $arg (@sitedep, @ARGV) {
   if ( $arg =~ /^([a-z]*)=(.*)/ ) {
      $browse_args .= "$1=\"$2\" ";
   } else {
      $browse_args .= "$args ";
   }
}
#$browse_args = join ' ', @sitedep, @ARGV;

if ($debug) { print "xbrowse_extract.pl $browse_args\n"; }
open QUERY, "xbrowse_extract.pl $browse_args |";
@results = <QUERY>;
close QUERY;

#
#  An argument of table= indicates that a list of tables is desired.
#  Print and exit
#
if ($list) {
   foreach $line (@results) {
      print $line;
   }
   exit;
}

#
# Assume first line is column labels
#

$line = shift @results;
while ( $line && $line =~ /^[^|]/ ) { $line = shift @results; }
@columns = split /\|/, $line;
pop @columns;
shift @columns;
for ( $i = 0; $i < @columns; $i++ ) {
   $col_order[$i] = $i
}

#
# Find ra and dec columns
#

$racol = -1;
$decol = -1;
for ( $i = 0; $i < @columns; $i++ ) {
   if ($debug) { print "column: $i *$columns[$i]*\n"; }
   if ($racol < 0 && $columns[$i] =~ /^ra *$/i) {
      $racol = $i;
   } elsif ($decol < 0 && $columns[$i] =~ /^dec *$/i) {
      $deccol = $i;
   }
}
if ($debug) { print "RA column: $racol\n"; }
if ($debug) { print "Dec column: $deccol\n"; }

#
# ra and dec are made to be the first two columns
# unless column order is specified
#

if ($custom_fields) {
   @col_order = ();
   foreach $entry (@fields) {
      for ( $i = 0; $i < @columns; $i++ ) {
         if ( $columns[$i] =~ /^$entry *$/i ) {
            push @col_order, $i;
            last;
         }
      }
   }
   if ( $debug ) {
      for ( $i = 0; $i < @col_order; $i++ ) {
         print "field: $i $col_order[$i] $columns[$col_order[$i]]\n"; 
      }
   }
} else {
   $i = 0;
   while ( $i < @col_order ) {
      if ( $racol == $col_order[$i] ) {
         $radec[0] = splice(@col_order, $i, 1);
      } elsif ( $deccol == $col_order[$i] ) {
         $radec[1] = splice(@col_order, $i, 1);
      } else {
         $i++;
      }
   }
   if ( $racol >= 0 && $deccol >= 0 ) {
      unshift @col_order, @radec;
   }
}

#
# Open outfile
#

if ($outfile) {
   open OUTFILE, ">$outfile";
   $fh = OUTFILE;
} else {
   $fh = STDOUT;
}

#
# Construct column header line based on column order
#

$line = "";
for ( $i = 0; $i < @col_order; $i++ ) {
  $line .= "$columns[$col_order[$i]],";
}
chop $line;
print $fh "#$line\n";

#
# Throw out line after column headers
#

shift @results;

#
# Process remaining output
#

foreach $line (@results) {
   if ($line =~ /^\|/) {
      @columns = split /\|/, $line;
      pop @columns;
      shift @columns;
      $line = "";
      for ( $i = 0; $i < @col_order; $i++ ) {
        $entry = $columns[$col_order[$i]];
        if ( $col_order[$i] == $racol ) {
           @hms = split /\s/, $entry;
           $raval = 15*(abs($hms[0]) + $hms[1]/60 + $hms[2]/3600);
           if ( $hms[0] < 0 ) { $raval = - $raval; }
           $entry = sprintf "%12.8f", $raval;
        } elsif ( $col_order[$i] == $deccol ) {
           @dms = split /\s+/, $entry;
           $decval = abs($dms[0]) + $dms[1]/60 + $dms[2]/3600;
           if ( $dms[0] < 0 ) { $decval = - $decval; }
           $entry = sprintf "%12.8f", $decval;
        }
        $line .= "$entry,";
      }
      chop $line;
      print $fh "$line\n";
   }
   else {
      print $fh "#$line";
   }
}

# Equinox parser
#     $radec[0] =~ /^ra\((.*)\)/;
#     print $fh "$1\n";
