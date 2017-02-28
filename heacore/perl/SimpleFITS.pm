#!/usr/bin/perl
#
# SimpleFITS
# $Source: /headas/headas/heacore/perl/SimpleFITS.pm,v $
# $Revision: 1.5 $
# $Date: 2013/11/22 20:35:37 $
#
# A perl library which simplifies using the Perl-CFITSIO interface.
# This is basically an object which wraps around the cumbersome
# CFITSIO library calls to make mundane things simpler.
#
# Usage:
#   use SimpleFITS;
#
# How to use it:
#
# 1. Open a file with following
#      $fits = SimpleFITS->open("<file");  # read-only access to existing file
#      $fits = SimpleFITS->open(">file");  # create a file
#      $fits = SimpleFITS->open("+<file"); # read-write access to existing file
#    There are other options for opening a file (see documentation below)
# 2. Create a new table in an open file with
#      $fits -> createtab("extname")  
# 3. Add columns with
#      $fits -> insertcol({TTYPE => ["NAME", "Description"], # TTYPEn keyword
#                          TFORM => "1D",                    # TFORMn keyword
#                          Tkey  => [value, "Comment", TYPE]}) # Tkeyn keywords
#    and delete columns with
#      $fits -> delcol("colname");
#
#   All keywords will have the column number appended.  TYPE is the
#   (optional) CFITSIO data type of the *keyword*, such as TDOUBLE.
#   The data type of the column is given by the TFORMn keyword.
# 4. Read columns with
#      $fits -> readcol("NAME",{rows=>$rowrange,type=>TYPE},$data) 
#            ($data is a reference to the data array)               # or
#      @data = $fits->readcol("NAME",{rows=>$rowrange,type=>TYPE})
#            (@data is the data array)
#   $rowrange is either
#      [] - read all rows
#      SCALAR - read single row (row numbers start with 1)
#      [$start,$stop] - read from row $start to $stop
#    Both "rows" and "type" are optional and can be omitted (see below
#    for more detailed documentation).
#    
#    Also, calculate any derived quantity using the CFITSIO calculator
#    expression evaluator.
#      $fits -> calculate("expression",$options,$data)  #or
#      @data = $fits->calculate("expression",$options)
#    where "expression" is any CFITSIO calculator expression.
#  
# 5. Write columns with
#      $fits -> writecol("NAME",{rows=>$rowrange,type=>TYPE},$data)
#    with the same options as readcol.
#      $data - an array reference to the data
# 6. Read keywords with
#      $fits -> readkey("KEYWORD",$value,$comment,TYPE)  # or
#      $value = $fits -> readkey("KEYWORD")
#   $value is the keyword value, $comment is the comment string
#   TYPE is optional; and allows typecasting from the native keyword type.
#
# 7. Write keywords with
#      $fits -> writekey("KEYWORD",$value,$comment,TYPE)
#   The meanings are the same as for readkey()
#
# 8. Read pixel values from an image HDU with
#      $fits -> readpix(...options..., $data)
#            ($data is a reference to the data array)               # or
#      @data = $fits->readpix(...options...)
#            (@data is the data array)
#   where the "...options..." is a hash which gives pixel ranges
#   for each dimension, optional data type, optional null value. 
#   See documentation below for more detailed information.
#   The default, if no pixel ranges are specified, is to read the
#   entire image.
#
# 9. Get the CFITSIO status with
#      $fits -> status()
#
# 10. Reset the CFITSIO status with
#      $fits -> setstatus($value)
#
# 11. Retrieve the number of HDUs and the current HDU number with,
#      $fits -> nhdu()
#      $fits -> curhdu()
#     and the current HDU type code (IMAGE_HDU, BINARY_TBL) with,
#      $fits -> curhdutype()
#
# 12. Determine number of rows and columns of a 2D image or table with
#      $fits -> nrows()
#      $fits -> ncols()
#     and the dimensions of an image HDU with
#      $fits -> imgdims()
#
# 13. Insert rows in a binary table with
#      $fits -> insertrows($nrows)           # at end of table
#      $fits -> insertrows($nrows,$firstrow) # at another position
#     and delete rows in a binary table with
#      $fits -> delrows($nrows)              # from end of table
#      $fits -> delrows($nrows,$firstrow)    # at another position
#      $fits -> delrows([a,b])               # delete rows a to b
#      $fits -> delrows([a])                 # delete row a
# 
# 13. Retreive the CFITSIO file handle with
#      $handle = $fits->handle()
#   (allows calling CFITSIO routines directly)
#
# 
# 14. Move to an extension
#      $fits -> move("NAME")  # Move to named extension
#      $fits -> move(1)       # Move to extension number 1 (absolute)
#      $fits -> move(+1)      # Move to next extension (relative)
#      $fits -> move(-1)      # Move to previous extension (relative)
# 
# 15. Close the file with
#      $fits -> close()
#
#      
# Methods that don't return a data value, return $self.  Thus it is
# possible to chain together multiple calls,
# 
#     $status = $fits
# 	->createtab("$extname")
# 	->writekey("NAXIS2",1)
# 	->insertcol({TTYPE => ["START", "GTI Start Time"], 
# 		     TFORM => "1D", TUNIT => "s"})
# 	->insertcol({TTYPE => ["STOP",  "GTI Stop Time"],
# 		     TFORM => "1D", TUNIT => "s"})
# 	->writecol("START",{},$start)
# 	->writecol("STOP", {},$stop)
# 	->status();
#
#   The final call to status() reads the CFITSIO status after making
#   all the calls.  Since CFITSIO routines will return immediately if
#   the status is non-zero, $status should contain the first error 
#   encountered while running the chain.
#
#   To reset the status after a non-fatal error, use setstatus(0).
#
# MODIFICATIONS
# 
# $Log: SimpleFITS.pm,v $
# Revision 1.5  2013/11/22 20:35:37  craigm
# Bug fix to to writecol() defend against $data being a scalar; now fixed; untested! --CM
#
# Revision 1.4  2013/09/23 18:13:45  craigm
#
#
# Fixes and improvements to SimpleFITS
#
# writecol() now accepts "nulval" parameter (still waiting on
# Astro::FITS::CFITSIO to have fits_write_colnull() capability)
#
# writecol() now accepts "grow" parameter to grow table to match data
# size.
#
# parsekeys() now returns hash ref instead of array.  insertcol() has
# been modified to use this new return value.
#
# Revision 1.3  2011/09/22 14:11:32  irby
# New functions submitted by C. Markwardt:
# * 'delcol' to delete an existing column;
# * 'calculate' to calculate a column-based expression and return it;
#   Modified function:
# * 'insertcol' has a new special keyword '=' which allows one to specify
#   an initializer expression for a new (or existing) column
#
# Revision 1.2  2011/04/11 15:42:37  irby
# Submitted by Craig Markwardt: Fix bug when calling readpix() with more
# than 1 requested pixel.
#
# Revision 1.1  2010/12/08 19:04:12  irby
# Relocate SimpleFITS.pm from swift/gen/lib/perl to heacore/perl.  Previous
# history may be accessed using "cvs log SimpleFITS.pm" in swift/gen/lib/perl.
#
# Revision 1.15  2010/10/21 21:00:45  craigm
# Fix to {nulval} handling which avoids warnings when $options-{nulval} is not defined --CM
#
# Revision 1.14  2010/05/26 00:25:01  craigm
#
#
# New version of SimpleFITS:
#
#  * new function imgdims() returns dimensions of an image HDU
#
#  * new function readpix() reads pixel values (rows, columns or subsets)
#
#  * new function delrows() to delete rows
#
#  * new function curhdutype() to return type of current HDU
#
#  * readcol() now allows "nulval" option to specify null value
#
#  * ncols() and nrows() now return the number of rows and columns in a
#    2D image, as well as for tables.
#
#  * documentation: document the alertnate syntaxes patterns of
#    nrows($n) versus nrows(); this pattern applies for curhdutype(),
#    curhdu(), nrows(), ncols()
#
# Revision 1.13  2009/10/02 23:41:35  craigm
# Change from extver=undef to extver=0 when calling movnam_hdu; thanks to Alex Padgett --CM
#
# Revision 1.12  2009/03/04 22:11:28  craigm
# Add function 'curhdu' to determine the current HDU number --CM
#
# Revision 1.11  2009/02/12 23:38:59  craigm
# Change to readkey() so that $value and $comment are not changed if the keyword does not exist --CM
#
# Revision 1.10  2007/10/25 14:40:04  craigm
# When files are opened with '+<filename' do not attempt to create them --CM
#
# Revision 1.9  2006/03/16 14:24:37  rwiegand
# Updated loadtable to deal with vector columns.
#
# Revision 1.8  2006/02/16 20:03:32  craigm
# Special case: bit vectors are handled like bytes by CFITSIO --CM
#
# Revision 1.7  2006/02/06 23:16:31  craigm
# Fix a bug when the 'access' hash is not provided; document some code --CM
#
# Revision 1.6  2006/01/19 02:19:07  craigm
# Add special cases for HISTORY and COMMENT keywords -- CM
#
# Revision 1.5  2006/01/19 02:11:10  craigm
# Add 'access' option to the open() method -- CM
#
# Revision 1.4  2005/12/01 14:18:44  rwiegand
# Added routine to store a table into an array of hash references.
#
# Revision 1.3  2005/11/28 22:46:38  craigm
# Changes to SimpleFITS to allow moving to an extension when open()ing the file; the standard open_diskfile(), open_data(), open_table() and open_image() CFITSIO routines are permitted; also, one can move to a particular extension by name or number when opening the file --CM
#
# Revision 1.2  2005/06/15 21:09:45  rwiegand
# Made n{hdu,rows,cols} return $self even when in an error state.
#
# Revision 1.1  2005/05/20 23:24:41  craigm
# Transfer SimpleFITS.pm from swift/bat/lib/perl to swift/gen/lib/perl (RCS version 1.5) -- CM
#
# Revision 1.5  2005/05/19 23:18:23  craigm
# Make the MODIFICATIONS section more harmonious with the CVS log -- CM
#
# Revision 1.4  2005/05/19 23:16:40  craigm
#
#  19 May 2005 CM
#     More changes by Bob Wiegand including readonly,readwrite,create,
#     optional output arguments for the nxxx() routines, and other
#     conceptual fixes.
#  19 May 2005 CM
#     Previous major interface changes - now documented!
#  29 Jan 2005 CM
#     Incorporate Bob Wiegand's changes (use strict improvements)
#     Documentation
#  02 Sep 2004 CM
#     Correct bug in the nhdus() function
#
#

package SimpleFITS;
use strict;

use Astro::FITS::CFITSIO qw(:longnames :constants);

# =========================================================================
#
# open($inspec,%args) - open a file
#   $inspec = "<file" open existing file for reading
#   $inspec = ">file" create file for writing
#   $inspec = "+<file" open existing file for read/write
#   %args = hash containing optional named arguments, one of
#                type => "file"
#                type => "diskfile"
#                type => "data"
#                type => "table"
#                type => "image"
#                   indicates the desired type of file or extension to
#                   open.
#                ext => "NAME"
#                ext => 5
#                ext => +2
#                   move to the requested HDU (see move() method).  If
#                   the extension does not exist, then the file is
#                   closed before return.
#                access => "create"   create a new file;
#                access => "readonly" or READONLY   open a file for reading;
#                access => "readwrite" or READWRITE open a file for update;
#
# Returns new SimpleFITS object, or zero upon failure.
#
sub open {
    my ($first,$inspec,%args) = @_;
    my ($fits,$mode, $modestr, $filename, $type, $access);
    my $self = {};

    $type = "file";
    $type = "$args{type}" if ($args{type});
    $access = "";
    $access = "$args{access}" if ($args{access});

    my $status = 0;
    # Parse the "mode" string (<,>,+<) and the file name
    if ($inspec =~ m/^([+<>]*)([^+<>].*)/) {
	$modestr = $1;
	$filename = $2;

        $self->{filename} = $filename;
	# Cases where a file should be created
	if ($modestr eq ">" || 
	    ($access eq "create")) {
	    $fits = Astro::FITS::CFITSIO::create_file($filename,$status);
	    
	} else {
	    # Other cases where a file should be opened with read or
	    # read-write access

	    # Parse the read/write mode
	    if (($modestr eq "<") || ($access eq "readonly") || 
		($access eq READONLY)) {
		$mode = READONLY;
	    } elsif (($modestr eq "+<") || 
		     ($access eq "readwrite") || 
		     ($access eq "write") || 
		     ($access eq READWRITE)) {
		$mode = READWRITE;
	    } else {
		# Default when no mode string is present (read-only)
		$mode = READONLY;
	    }

	    # Decide on how to open the file
	    if ($type eq "diskfile") {
		Astro::FITS::CFITSIO::fits_open_diskfile($fits,"$filename",$mode,$status);
	    } elsif ($type eq "data") {
		Astro::FITS::CFITSIO::fits_open_data($fits,"$filename",$mode,$status);
	    } elsif ($type eq "table") {
		Astro::FITS::CFITSIO::fits_open_table($fits,"$filename",$mode,$status);
	    } elsif ($type eq "image") {
		Astro::FITS::CFITSIO::fits_open_image($fits,"$filename",$mode,$status);
	    } else {
		$fits = Astro::FITS::CFITSIO::open_file("$filename",$mode,$status);
	    } 

#	    print "fits=$fits filename=$filename mode=$mode status=$status\n";
	}	
    }

    $self->{fits} = $fits;
    $self->{status} = $status;

    bless $self;

    # Move to the requested extension, if given
    if ($args{ext}) {
	$self->move($args{ext});
	$self->close() if ($self->status);
    }
    
    return $self;
}


# =========================================================================
#  readonly("filename")   - open file read-only
#  readwrite("filename")  - open a file for reading and writing
#  create("filename")     - create a new file
#
#  These are short-cut routines that do not require the "<" or ">"
#  syntax for opening files.
#
#  RETURNS: a SimpleFITS object reference
#
sub readonly
{
    my ($first, $inspec) = @_;
    return $first->open('<' . $inspec);
}

sub readwrite
{
    my ($first, $inspec) = @_;
    return $first->open('+<' . $inspec);
}

sub create
{
    my ($first, $inspec) = @_;
    return $first->open('>' . $inspec);
}

# =========================================================================
# 
# close() - close an already-opened file
#
# This file closes a file.  The current CFITSIO status is
# always ignored, so the file is always closed.
#
# Returns $self.
#
sub close {
    my ($self) = @_;
    my ($fits);
    my $status = 0;

    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);

    $fits->close_file($status);
    undef $self->{fits};
    return $self;
}



# =========================================================================
#
# status() - return CFITSIO status
#  
# Returns the most recent CFITSIO error status code
#
sub status {
    my ($self) = @_;
    return NULL_INPUT_PTR unless ($self);
    if (@_ > 1) {
        $_[1] = $self->{status};
        return $self;
    }
    return $self->{status};
}

# =========================================================================
#
# setstatus($value) - reset CFITSIO status
#
#   $value - new value of status code
#
# Returns $self
#
sub setstatus {
    my ($self,$value) = @_;

    return 0 unless ($self);
    $self->{status} = $value;

    return $self;
}

# =========================================================================
#
# handle() - return CFITSIO file handle
#  
# Returns the CFITSIO file handle
#
sub handle { 
    my ($self) = @_;
    return 0 unless ($self);
    return $self->{fits};
}

# =========================================================================
#
# nhdu($n) - return the number of HDUs in the file
#
# $n - (optional) upon return, $n has number of HDUs
#
# Returns:
#   * if $n is passed, then function returns self
#   * if no arguments are passed, function returns number of HDUs
#
sub nhdu {
    my ($self) = @_;
    my ($nhdu);

    $nhdu = -1;
    return 0 unless ($self);
    my $fits = $self->{fits};
    return $self unless ($fits);

    my $status = $self->{status};
    if ($status) {
	return (@_ > 1) ? $self : -1;
    }

    $fits->get_num_hdus($nhdu,$status);
    $self->{status} = $status;

    if (@_ > 1) {
        $_[1] = $nhdu;
       return $self;
    }

    return $nhdu;
}

# =========================================================================
#
# curhdu($n) - Return the current HDU number (1=primary HDU)
#
# $n - (optional) upon return, $n has current HDU number
#
# Returns:
#   * if $n is passed, then function returns self
#   * if no arguments are passed, function returns current HDU number
#
sub curhdu {
    my ($self) = @_;
    my ($curhdu);

    $curhdu = -1;
    return 0 unless ($self);
    my $fits = $self->{fits};
    return $self unless ($fits);

    my $status = $self->{status};
    if ($status) {
	return (@_ > 1) ? $self : -1;
    }

    $fits->get_hdu_num($curhdu);
    $self->{status} = $status;

    if (@_ > 1) {
        $_[1] = $curhdu;
       return $self;
    }

    return $curhdu;
}



# =========================================================================
#
# curhdutype($n) - Return HDU type code of the current HDU
#
# $n - (optional) upon return, $n has type code (e.g. IMAGE_HDU, etc)
#
# Returns:
#   * if $n is passed, then function returns self
#   * if no arguments are passed, function returns type code
#   * -1 indicates error condition
#
sub curhdutype {
    my ($self) = @_;
    my ($hdutype);

    $hdutype = -1;
    return 0 unless ($self);
    my $fits = $self->{fits};
    return $self unless ($fits);

    my $status = $self->{status};
    if ($status) {
	return (@_ > 1) ? $self : -1;
    }

    $fits->get_hdu_type($hdutype,$status);
    $self->{status} = $status;

    if (@_ > 1) {
        $_[1] = $hdutype;
       return $self;
    }

    return $hdutype;
}



# =========================================================================
#
# nrows($n) - return the number of rows in a table or 2D image
#
# $n - (optional) upon return, $n has number of rows
#
# Returns:
#   * if $n is passed, then function returns self
#   * if no arguments are passed, function returns number of rows
#
sub nrows {
    my ($self) = @_;
    my $nrows = -1;

    return 0 unless ($self);
    my $fits = $self->{fits};
    return $self unless ($fits);

    my $status = $self->{status};
    if ($status) {
	return (@_ > 1) ? $self : -1;
    }

    if ($self->curhdutype() == IMAGE_HDU) {
      my ($naxes);
      $fits->get_img_size($naxes,$status);
      $nrows = $naxes->[1] if ($status == 0);
    } else {
      $fits->get_num_rows($nrows,$status);
    }
    $self->{status} = $status;

    if (@_ > 1) {
        $_[1] = $nrows;
        return $self;
    }

    return $nrows;
}

# =========================================================================
#
# ncols() - return the number of columns in a table or 2D image
#
# $n - (optional) upon return, $n has number of columns
#
# Returns:
#   * if $n is passed, then function returns self
#   * if no arguments are passed, function returns number of columns
#
#
sub ncols {
    my ($self) = @_;
    my ($ncols);

    $ncols = -1;
    return 0 unless ($self);
    my $fits = $self->{fits};
    return $self unless ($fits);

    my $status = $self->{status};
    if ($status) {
	return (@_ > 1) ? $self : -1;
    }

    $ncols = -1;
    if ($self->curhdutype() == IMAGE_HDU) {
      my ($naxes);
      $fits->get_img_size($naxes,$status);
      $ncols = $naxes->[0] if ($status == 0);
    } else {
      $fits->get_num_cols($ncols,$status);
    }
    if (@_ > 1) {
        $_[1] = $ncols;
        return $self;
    }

    $self->{status} = $status;
    return $ncols;
}


# =========================================================================
#
# imgdims() - return the dimensions of an image
#
# Returns:
#   * an array giving the dimensions of the image
#   * an empty array () upon error
#
#
sub imgdims {
    my ($self) = @_;
    my ($naxes);

    return 0 unless ($self);
    my $fits = $self->{fits};
    return $self unless ($fits);

    my $status = $self->{status};
    if ($status) {
      return ();
    }

    $fits->get_img_size($naxes,$status);
    $self->{status} = $status;
    if ($status) {
      return ();
    }

    # Return as an array
    return @$naxes;
}



# =========================================================================
#
# colnum($colname) - return the column number corresponding to the given name
#
#  $colname - name of the column, or -1 if not found
#
# Returns integer FITS column number
#
sub colnum {
    my ($self,$colname) = @_;
    my ($colnum);

    return 0 unless ($self);
    my $fits = $self->{fits};
    return $self unless ($fits);

    my $status = $self->{status};
    if ($status) { return -1; }

    $colnum = -1;
    $fits->get_colnum(CASEINSEN,$colname,$colnum,$status);

    if ($status) { return -1; }
    return $colnum;
}

# =========================================================================
#
# colname($colnum) - return the name corresponding to a given column number
#
#  $colnum - FITS column number
#
# Returns column name, or undef if not found
#
sub colname {
    my ($self,$colnum) = @_;
    my ($colname,$keyname);

    return undef unless ($self);
    my $fits = $self->{fits};
    return $self unless ($fits);

    my $status = $self->{status};
    if ($status) { return undef; }

    if ($colnum =~ m/^[0-9]+$/) {
	# Column number given
	$keyname = "TTYPE$colnum";
	$colname = $self->readkey($keyname);
	if ($self->{status}) {
	    $self->{status} = 0;
	    return undef;
	}
    } else {
	# Not a number, assume it is the name
	$colname = $colnum;
    }

    return $colname;
}


# =========================================================================
#
# move($hdu) - Move to a new HDU
#
#  $hdu = "NAME" - move to extension named "NAME"
#  $hdu =  5     - move to absolute extension number 5
#  $hdu =  +2    - move forward by two HDUs
#
# Note that $hdus are numberd starting with 1 as the primary HDU.
#
# Returns $self
#
sub move {
    my ($self,$hdu) = @_;

    return 0 unless ($self);
    my $fits = $self->{fits};
    return $self unless ($fits);

    my $status = $self->{status};
    if ($status) { return $self; }

    if ($hdu =~ m/^[-+][0-9]+$/) {
	$fits->movrel_hdu($hdu,undef,$status);
    } elsif ($hdu =~ m/^[0-9]+$/) {
	$fits->movabs_hdu($hdu,undef,$status);
    } else {
	$fits->movnam_hdu(ANY_HDU,$hdu,0,$status);
    }

    $self->{status} = $status;
    return $self;
}

# =========================================================================
#
# readheader($header) - read entire header and return it
#
#   $header - upon return, a reference to an array of strings
#             containing the current HDU's header
#
# Returns $self
#
sub readheader {
    my ($self,$header, %args) = @_;
    my ($fits, $status, $value, $comment, $dtype, $type);
    
    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);

    $status = $self->{status};
    if ($status) { return $self; }

    ($header, $status) = $fits->read_header();
    $self->{status} = $status;

    if ($args{clean}) {
       while (my ($k, $v) = each(%$header)) {
           if ($v =~ /^'/) {
               $v =~ s/^'//;
               $v =~ s/\s*'$//;
               $header->{$k} = $v;
           }
       }
    }

    $_[1] = $header;
    
    return $self;
}

# =========================================================================
# 
# readkey($keyname,$value,$comment,$type) - Read keyword value
#
#   $keyname - scalar string containing keyword name
#   $value - (optional) upon return, contains keyword value
#   $comment - (optional) upon return, contains comment string
#   $type - (optional) contains desired type for value returned in $value.
#           (a suitable default is used otherwise)
#
# Returns $self.  Note that if the read fails, then $value and/or $comment
#   are not changed upon return.
#
# Alternate calling sequence:
#   $value = readkey($keyname)
#
# In this sequence, the data value is returned explicitly.  The
# comments are ignored.
#

sub readkey {
    my ($self,$keyname) = @_;
    my ($fits, $status, $value, $comment, $dtype, $type);
    
    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);

    $status = $self->{status};
    if ($status) { return $self; }

    if ($_[4]) {
	$type = $_[4];
    } else {
	$fits->read_keyword($keyname,$value,$comment,$status);
	Astro::FITS::CFITSIO::fits_get_keytype($value,$dtype,$status);
	$type = TSTRING;
	if ($dtype eq 'L')    { $type = TLOGICAL; } 
	elsif ($dtype eq 'I') { $type = TINT; }
	elsif ($dtype eq 'F') { $type = TDOUBLE; }
    }
    $fits->read_key($type,$keyname,$value,$comment,$status);
    $self->{status} = $status;

    if ($#_ <= 1) {
	return $value;
    }

    if ($status == 0) {
      $_[2] = $value;
      $_[3] = $comment;
    }
	     
    return $self;
}

# =========================================================================
# 
# vtype($value) - (Utility routine) determine data type of $value
#
# Returns suitable CFITSIO type descriptor for $value
#
sub vtype {
    my ($val) = @_;
    my ($strval);
    
    $strval = "$val";
    if (ref($val) eq "SCALAR") {
      return TLOGICAL;
    } elsif ($strval =~ m/^[-+]?[0-9]+$/) {
	return TLONG;
    } elsif ($strval =~ /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/) {
	# m/^[-+]?[0-9.]+[eE]?([-+][0-9]+)?/     my old one
	return TDOUBLE;
    }
    return TSTRING;

}

# =========================================================================
# 
# writekey($keyname,$value,$comment,$type) - write keyword value
#
#   $keyname - scalar string containing keyword name
#              'HISTORY' and 'COMMENT' are treated specially
#              to add a new history or comment card.
#   $value - new keyword value
#            any scalar value is permitted
#            if you want to set a logical value ('T'rue or 'F'alse)
#            then either pass \1 or \0, respectively; or 
#            pass 1 or 0, respectively and set $type=TLOGICAL
#   $comment - (optional) comment string
#   $type - (optional) desired type for the stored form of $value.
#           (a suitable default is used otherwise)
#
# Returns $self
#
sub writekey {
    my ($self,$keyname,$value) = @_;
    my ($fits, $status, $comment, $type);

    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};
    
    if ($status) { return $self; }

    if ($keyname eq "HISTORY") {
	$fits->write_history($value, $status);
    } elsif ($keyname eq "COMMENT") {
	$fits->write_comment($value, $status);
    } else {
        my ($val1);
	$val1 = "$value";

	$comment = '';
	if ($_[3]) { $comment = "$_[3]"; }
	if ($_[4]) { $type = $_[4]; } else { $type = vtype($value); }

	## Special case for logical booleans
	if ($type == TLOGICAL && ref($value) eq "SCALAR") {
	  $val1 = $$value ? 1 : 0;
	}

	$fits->update_key($type,$keyname,$val1,$comment,$status);
    }

    $self->{status} = $status;

    return $self;
}


# =========================================================================
# 
# delkey($keyname) - delete a keyword
#
#   $keyname - scalar string containing keyword name
#
# Returns $self
#
sub delkey {
    my ($self,$keyname) = @_;
    my ($fits, $status);

    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};
    
    if ($status) { return $self; }

    $fits->delete_key($keyname,$status);
    $self->{status} = $status;

    return $self;
    
}


# =========================================================================
# 
# readpix($options,$data)
#
#   Read pixel values from image HDU.  The user can specify the range
#   of each dimension. The default is to read the entire image.  Pixel
#   values use the FITS convention where the first pixel is labeled as
#   1.
#
#
#   $options is a hash reference  (e.g. {option1 => value1, option2 => value2})
#      options include:
#        axis<n> => [a,b]      # For axis #n, take pixel range a-b
#        axis<n> => [a]        # For axis #n, take pixel a only
#        axes    => [[a1,b1],[a2,b2],..] # Pixel ranges for each axis
#          EXAMPLE: axes => [[1,10], undef, [2]]
#            (for first dimension, take pixel range 1-10;
#             for second dimension, take all pixels (the default);
#             for third dimension, take pixel 2 only)
#        type => (any CFITSIO data type)
#        nulval => null value (must same data type as data)
#   $data - upon return, an array reference to the data
#
# Returns $self
#
# Alternate calling sequence:
#   @data = $fits->readcol($options)
# where @data is the data array
#

sub readpix {
    my ($self,$options) = @_;
    my ($fits, $status);
    my (@dims,@fpixel,@lpixel,@inc);
    my ($ndims, $type, $data);
    my ($n);

    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};
    
    if ($status) { return $self; }

    @dims = $self->imgdims();   # Image dimensions...
    $ndims = $#dims;            # ... and number of dimensions
    @fpixel = (1) x ($ndims+1); # First pixel = 1 (default)
    @inc = (1) x ($ndims+1);    # Increment = 1
    @lpixel = @dims;            # Last pixel = dim (default)

    # Parse $options->{axis$n} and $options->{axes}
    foreach $n (1 .. $ndims-1) {
      my ($rng);
      undef($rng);
      # {axis1 => [1,20]}
      $rng = $options->{axis$n} 
	if (ref($options->{axis$n}) eq "ARRAY");
      # {axes => [[1,20],[2,3]]}
      $rng = $options->{axes}->[$n-1] 
	if (ref($options->{axes}->[$n-1] eq "ARRAY"));
      next if (! defined($rng));

      $fpixel[$n-1] = $rng->[0];
      if (defined($rng->[1])) {
	$lpixel[$n-1] = $rng->[1];
      } else {
	$lpixel[$n-1] = $fpixel[$n-1];
      }
    }
    

    $type = $options->{type};
    if (! defined($type) ) {
      my ($bitpix);
      $fits->get_img_equivtype($bitpix,$status) unless $status;
      if ($status) {
	$self->{status} = $status;
	return $self;
      }
      $type = TBYTE if ($bitpix == BYTE_IMG);
      $type = TSHORT if ($bitpix == SHORT_IMG);
      $type = TLONG if ($bitpix == LONG_IMG);
      $type = TLONGLONG if ($bitpix == LONGLONG_IMG);
      $type = TFLOAT if ($bitpix == FLOAT_IMG);
      $type = TDOUBLE if ($bitpix == DOUBLE_IMG);
    }
    if (! defined($type) ) {
      $self->{status} = BAD_DATATYPE;
      return $self;
    }
    
    $fits->read_subset($type,
		       \@fpixel, \@lpixel, \@inc,
		       (defined($options->{nulval})? $options->{nulval} : undef),
		       $data, undef, $status);
    $self->{status} = $status;

    if ($#_ <= 1) {
	return @$data;
    }
    $_[2] = $data;

    return $self;
}


# =========================================================================
# 
# readcol($column,$options,$dataref)
#   $column - column "NAME" or SCALAR number
#   $options is a hash reference  (e.g. {option1 => value1, option2 => value2})
#      options include:
#         rows => 
#           [] - read all rows
#           SCALAR - read single row (row numbers start with 1)
#           [$start,$stop] - read from row $start to $stop
#           DEFAULT: [] (all rows)
#         nulval => null value (must be same type as data being read)
#         type => (any CFITSIO data type)
#   $dataref - upon return, an array reference to the data
#
# Returns $self
#
# Alternate calling sequence:
#   @data = $fits->readcol("NAME",$options)
# where @data is the data array
#

sub readcol {
    my ($self,$column,$options) = @_;
    my ($rrows,$type);
    my ($fits, $status, $colnum, $nrows);
    my ($typecode, $repeat, $width, $r, $nel, $rowstart, $data, $comment);
    my @rows;

    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};
    
    if ($status) { return $self; }

    if ($column =~ m/^[0-9]$/) {
	$colnum = $column;
    } else {
	$fits->get_colnum(CASEINSEN,$column,$colnum,$status);
    }

    # Default value: all rows
    $rrows = $options->{rows};
    $rrows = [] unless($rrows);

    $fits->get_num_rows($nrows,$status) unless $status;
    $fits->get_coltype($colnum,$typecode,$repeat,$width,$status) unless $status;
    if ($status) {
	$self->{status} = $status;
	return $self;
    }

    if ($typecode == TSTRING) { $repeat /= $width; }
    # Special case: bit vectors are handled like bytes by CFITSIO
    if ($typecode == TBIT && $options->{type} != TBIT) {
	$repeat /= 8;
    }
    $type = $options->{type};
    if (!defined($options->{type})) { $type = $typecode; }

    $r = ref($rrows);
    if ($r eq "SCALAR") {
	@rows = ($$rrows,$$rrows);
    } elsif ($r eq "ARRAY") {
	@rows = @$rrows;
	if ($#rows == -1) { @rows = (1, $nrows); }
	if ($#rows ==  0) { @rows = ($rows[0], $rows[0]); }
    } elsif ($rrows) {
	@rows = ($rrows, $rrows);
    } else {
	@rows = (1, $nrows);
    }

    $rowstart = $rows[0];
    $nel = ($rows[1] - $rows[0] + 1)*$repeat;

    $fits->read_col($type,$colnum,$rowstart,1,$nel,
		    (defined($options->{nulval})? $options->{nulval} : undef),
		    $data,undef,$status);
    $self->{status} = $status;

    if ($#_ <= 2) {
	return @$data;
    }
    $_[3] = $data;

    return $self;
}

# =========================================================================
# 
# writecol($column,$options,$data)
#   $column - column "NAME" or SCALAR number
#   $options is a hash reference  (e.g. {option1 => value1, option2 => value2})
#      options include:
#         rows => 
#           [] - write all rows
#           SCALAR - write single row (row numbers start with 1)
#           [$start,$stop] - write from row $start to $stop
#           DEFAULT: [] (all rows)
#         type => (any CFITSIO data type)
#         nulval => null value of values in $data
#                   (translated to TNULLn upon output)
#         grow => 0 or 1 - grow table beyond nrows?
#   $data - an array reference to the data
#
# Returns $self
#
sub writecol {
    my ($self,$column,$options,$data) = @_;
    my ($rrows,$type);
    my ($fits, $status, $colnum, $nrows);
    my ($typecode, $repeat, $width, $r, $nel, $rowstart, $comment);
    my @rows;

    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};
    
    if ($status) { return $self; }

    if ($column =~ m/^[0-9]$/) {
	$colnum = $column;
    } else {
	$fits->get_colnum(CASEINSEN,$column,$colnum,$status);
    }

    $fits->get_num_rows($nrows,$status) unless $status;
    $fits->get_coltype($colnum,$typecode,$repeat,$width,$status) unless $status;
    if ($status) {
	$self->{status} = $status;
	return $self;
    }

    if ($typecode == TSTRING) { $repeat /= $width; }
    $type = $options->{type};
    if (!defined($options->{type})) { $type = $typecode; }

    $rrows = $options->{rows};
    $rrows = [] unless($rrows);

    $r = ref($rrows);
    if ($r eq "SCALAR") {
	@rows = ($$rrows,$$rrows);
    } elsif ($r eq "ARRAY") {
	@rows = @$rrows;
	if ($#rows == -1) { @rows = (1, $nrows); }
	if ($#rows ==  0) { @rows = ($rows[0], $rows[0]); }
    } elsif ($rrows) {
	@rows = ($rrows, $rrows);
    } else {
	@rows = (1, $nrows);
    }

    # Compute last row according to the data
    my ($n_data,$n_datarows);
    # Number of data points to be written (scalar is only one element)
    $n_data = ref($data) ? scalar(@$data) : 1;

    # Number of data rows to be written (modified by repeat)
    $n_datarows = int(($n_data+$repeat-1)/$repeat);
    
    # Last data row to be written.  See if we need to grow table.
    my $lastdatarow = ($rows[0] + $n_datarows - 1);
    if ($options->{grow} && $lastdatarow > $rows[1]) {
      $rows[1] = $lastdatarow;
    }

    $rowstart = $rows[0];
    $nel = ($rows[1] - $rows[0] + 1)*$repeat;

    # Which writing routine we use depends on whether a null value was 
    # specified or not.
    if (! defined($options->{nulval}) ) {
      $fits->write_col($type,$colnum,$rowstart,1,$nel,$data,$status);
    } else {
      $fits->write_colnull($type,$colnum,$rowstart,1,$nel,$data,
			   $options->{nulval}, $status);
    }
    $self->{status} = $status;

    return $self;
}

# =========================================================================
# 
# parsekeys() - parse keyword parameters
#
#   parsekeys($value) - indicates the keyword value
#   parsekeys([$value,"Comment",$type]) - indicates
#      the keyword value, comment, and data type
#
# Returns the array ($value,$comment,$type)
#   where $comment and $type may be undefined
#
sub parsekeys {
    my ($arg) = @_;
    my ($rref);

    $rref = ref($arg);
    if ($rref eq "SCALAR") {      # \SCALAR
      print "SCALAR=$$arg ".vtype($$arg)."\n";
	return {value=>"$$arg",comment=>undef,type=>vtype($$arg)};
    } elsif ($rref eq "ARRAY") {  # ARRAY
	return {value=>"$$arg[0]", comment=>$$arg[1], type=>$$arg[2]};
    } else {                      # scalar
	return {value=>"$arg",comment=>undef,type=>vtype($arg)};
    }
}

# =========================================================================
# 
# createtab($extname) - Create empty extension
#    $extname - string "NAME" of extension
#
# The extension initially is empty, with no rows or columns.
#
# Returns $self
#
sub createtab {
    my ($self,$extname) = @_;
    my ($fits, $status, $colnum, $nrows);
    my @rows;

    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};
    
    if ($status) { return $self; }

    $fits->create_tbl(BINARY_TBL,0,0,undef,undef,undef,$extname,$status);
    $self->{status} = $status;

    return $self;
}


# =========================================================================
# 
# insertcol($colkeys,$column) - Insert new column
#
#   $colkeys - hash reference to column keywords
#   $column - (optional) column number to insert at
#             (default: last column of table)
# 
# $colkeys = {TTYPE => $keydesc,
#             TFORM => $keydesc,
#             "="   => $expression, # (optional) initialization expression
#             <other keys> => $keydesc}
#
# At least TTYPE and TFORM are required, but other keywords
# like TUNIT, TLMAX can be specified.  The task automatically
# adds the column number.  See the example at the top of this
# module on how to add columns.
#
# The user may optionally specify an initialization expression for the
# column using the special descriptor "=".  See 'fhelp calc_express'
# for allowed calculator expressions.
#
# where $keydesc = $value for the keyword value
#                  (and default comment and data type)
#       $keydesc = [$value, $comment, $type]
#                  where $comment is a scalar string
#                  and $type is the CFITSIO data type
#                  either $comment or $type can be undef
#       $expression = optional initialization expression.
#                  This should be a scalar string expression
#                  defined according to the CFITSIO calculator
#                  syntax (see 'fhelp calc_express').
#                  Default: initialized with null values.
#
# Returns $self
#   

sub insertcol {
    my ($self,$colkeys,$column) = @_;
    my ($fits, $status, $colnum, $ncols);
    my ($ttype,$type,$comment,$tform,$value);
    my ($typecode, $repeat, $width, $r, $nel, $rowstart);
    my @rows;
    my %defcomments;

    return 0 unless ($self);
    $defcomments{TUNIT} = "physical unit of field";
    $defcomments{TSCAL} = "data scale";
    $defcomments{TZERO} = "data offset";
    $defcomments{TNULL} = "data null value";
    $defcomments{TDIM}  = "Array dimensions";
    $defcomments{TDMIN} = "Minimum column data value";
    $defcomments{TDMAX} = "Maximum column data value";
    $defcomments{TLMIN} = "Minimum legal value";
    $defcomments{TLMAX} = "Maximum legal value";

    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};

    if ($status) { return $self; }
    if (!defined($colkeys)) {return $self; }
    if (!defined($$colkeys{TTYPE}) || !defined($$colkeys{TFORM})) { 
	$self->{status} = -1;
	return $self;
    }

    $fits->get_num_cols($ncols,$status);

    if (!defined($column)) {
	$colnum = $ncols+1;
    } elsif ($column =~ m/^[0-9]$/) {
	$colnum = $column;
    } else {
	$fits->get_colnum(CASEINSEN,$column,$colnum,$status);
    }
    
    if ($status) {
	$self->{status} = $status;
	return $self;
    }
    
    $ttype = parsekeys($$colkeys{TTYPE});
    if (! $ttype->{value} ) { 
	$self->{status} = -1;
	return $self;
    }
    $tform = parsekeys($$colkeys{TFORM});
    $self->{status} = $status;

    if (defined($$colkeys{"="})) {
      # Calculate the column based on a user expression
      my $expr = $$colkeys{"="};
      Astro::FITS::CFITSIO::fits_calculator($fits,"$expr",$fits,
			 $ttype->{value},$tform->{value},$status);
    } else {
      # Insert a new blank column
      $fits->insert_col($colnum,$ttype->{value},$tform->{value},$status);
    }
    if ($ttype->{comment}) { 
      $self->writekey("TTYPE$colnum",$ttype->{value},
		      $ttype->{comment},TSTRING);
    }
    foreach my $key (keys %$colkeys) {
	if ($key ne "TTYPE" && $key ne "TFORM" && $key ne "=") {

	    my $tkey = parsekeys($$colkeys{$key});
	    if (!defined($tkey->{comment}) && $defcomments{$key}) { 
		$tkey->{comment} = $defcomments{$key};
	    }

	    $self->writekey("$key$colnum",$tkey->{value},
			    $tkey->{comment},$tkey->{type});
	}
    }

    return $self;
}


# =========================================================================
# 
# delcol($column) - Delete existing column
#
#   $column - column number or name to delete
# 
# Returns $self
#   
sub delcol {
    my ($self,$column) = @_;
    my ($fits, $status, $colnum, $ncols);


    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};
    
    if ($status) { return $self; }

    if ($column =~ m/^[0-9]$/) {
	$colnum = $column;
    } else {
	$fits->get_colnum(CASEINSEN,$column,$colnum,$status);
    }

    $fits->delete_col($colnum, $status);

    $self->{status} = $status;
    return $self;
}

# =========================================================================
# 
# insertrows($nrows,$firstrow) - Insert new rows into a FITS table
#
#   $nrows - number of rows to insert
#   $firstrow - (optional) row number to insert rows after
#               (default: last row of table)
#
# Returns $self
#   
sub insertrows {
    my ($self,$nrows,$firstrow) = @_;
    my ($fits, $status, $ntabrows);

    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};
    
    if ($status) { return $self; }

    $fits->get_num_rows($ntabrows,$status);
    if ($status) {
	$self->{status} = $status;
	return $self;
    }

    $firstrow = $ntabrows unless($firstrow);
    $fits->insert_rows($firstrow,$nrows,$status);
    
    $self->{status} = $status;
    return $self;
}


# =========================================================================
# 
# delrows($nrows,$firstrow) - Delete rows of a FITS table
# delrows([a,b]) - delete row range [a,b] (inclusive)
# delrows([a])   - delete row a only
#
#   $nrows - number of rows to delete
#   $firstrow - (optional) first row to delete (first row is 1)
#               (default: delete last rows of table)
#
# Returns $self
#   
sub delrows {
    my ($self,$nrows,$firstrow) = @_;
    my ($fits, $status, $ntabrows);
    my ($firstrow1, $nrows1);

    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};
    
    if ($status) { return $self; }

    if (ref($nrows) eq "ARRAY") {
      $firstrow1 = $nrows->[0];
      if (defined($nrows->[1])) {
	$nrows1    = $nrows->[1] - $nrows->[0] + 1;
      } else {
	$nrows1    = 1;
      }
    } else {
      $fits->get_num_rows($ntabrows,$status);
      if ($status) {
	$self->{status} = $status;
	return $self;
      }

      $nrows1 = $nrows;
      if (defined($firstrow)) {
	$firstrow1 = $firstrow;
      } else {
	$firstrow1 = $ntabrows-$nrows+1;
      }
    }

    $fits->delete_rows($firstrow1,$nrows1,$status);
    
    $self->{status} = $status;
    return $self;
}

# =========================================================================
# 
# calculate($expr,$options,$dataref) - calculate column-based expression
#   $expr - column-based expression to calculate
#   $options is a hash reference  (e.g. {option1 => value1, option2 => value2})
#      options include:
#         rows => 
#           [] - read all rows
#           SCALAR - read single row (row numbers start with 1)
#           [$start,$stop] - read from row $start to $stop
#           DEFAULT: [] (all rows)
#         nulval => null value (must be same type as data being read)
#         type => (any CFITSIO data type)
#   $dataref - upon return, an array reference to the calculated data
#
# Returns $self
#
# Alternate calling sequence:
#   @data = $fits->calculate("expr",$options)
# where @data is the calculated data array
sub calculate {
    my ($self,$expr,$options) = @_;
    my ($rrows,$type);
    my ($fits, $status, $colnum, $nrows);
    my ($typecode, $repeat, $r, $nel, $rowstart, $data, $naxis, $anynul);
    my @rows;

    return 0 unless ($self);
    $fits = $self->{fits};
    return $self unless ($fits);
    $status = $self->{status};
    
    if ($status) { return $self; }

    # Default value: all rows
    $rrows = $options->{rows};
    $rrows = [] unless($rrows);

    $fits->get_num_rows($nrows,$status) unless $status;
    $fits->test_expr($expr,$typecode,$repeat,$naxis,undef,$status) 
      unless $status;
    if ($status) {
	$self->{status} = $status;
	return $self;
    }

    # Special case: bit vectors are handled like bytes by CFITSIO
    if ($typecode == TBIT && $options->{type} != TBIT) {
	$repeat /= 8;
    }
    $type = $options->{type};
    if (!defined($options->{type})) { $type = $typecode; }

    $r = ref($rrows);
    if ($r eq "SCALAR") {
	@rows = ($$rrows,$$rrows);
    } elsif ($r eq "ARRAY") {
	@rows = @$rrows;
	if ($#rows == -1) { @rows = (1, $nrows); }
	if ($#rows ==  0) { @rows = ($rows[0], $rows[0]); }
    } elsif ($rrows) {
	@rows = ($rrows, $rrows);
    } else {
	@rows = (1, $nrows);
    }
    
    $rowstart = $rows[0];
    $nel = ($rows[1] - $rows[0] + 1)*$repeat;

    $fits->calc_rows($type, $expr, $rowstart, $nel, 
		     (defined($options->{nulval})? $options->{nulval} : undef),
		     $data, $anynul, $status);
    $self->{status} = $status;

    if ($#_ <= 2) {
        # @data = $fits->calculate($expr,$options);
        return @$data;
    }
    # $fits->calculate($expr,$options,$dataref);
    $_[3] = $data;

    return $self;
}


# =========================================================================
#
# loadtable($dataref) - load a FITS table into an array of hash references
#
# The keys of the hashes are the upper cased column names
# Example: given a FITS table
#	NAME	RA	DEC
#	Alpha	13.3	-4.3
#	Beta	150.1	5.6
# the passed array reference will have these two hash references
#	{ NAME => 'Alpha', RA => 13.3, DEC => -4.3 },
#	{ NAME => 'Beta', RA => 150.1, DEC => 5.6 },
# added on output.
#
sub loadtable
{
    my ($self, $dataref) = @_;

    return 0 unless ($self);

    my $fits = $self->{fits};
    return $self unless ($fits);

    my $status = $self->{status};

    my $nrows;
    my $ncols;

    $fits->get_num_rows($nrows, $status) unless $status;
    $fits->get_num_cols($ncols, $status) unless $status;

    my @column;
    for (my $i = 1; not $status and $i <= $ncols; ++$i) {
        my $colname;
        my $comment;
        my $type;
        my $repeat;
        my $width;
        my @data;
        $fits->read_key_str("TTYPE$i", $colname, $comment, $status);
        $colname =~ s/^'//;
        $colname =~ s/\s*'$//;

        $fits->get_eqcoltype($i, $type, $repeat, $width, $status)
                unless $status;

        my $readstr = $type == TSTRING || $type == TBIT;
        my $nelem = $readstr ? $nrows : $nrows * $repeat;
		if ($readstr) {
			$repeat = 1;
		}
        my $nulls;
        if ($status) {
        }
        elsif ($readstr) {
            $fits->read_col_str($i, 1, 1, $nelem, 0,
                    \@data, $nulls, $status)
        }
        else {
            $fits->read_col($type, $i, 1, 1, $nelem, 0,
                    \@data, $nulls, $status)
        }

        if (not $status and $type == TBIT) {
            @data = map { substr($_, 0, $repeat) } @data;
        }

        my %column = (
            index => $i,
            name => uc($colname),
            type => $type,
            repeat => $repeat,
            data => \@data,
        );

        push(@column, \%column);
    }

    if ($status) {
        $self->{status} = $status;
    }
    else {
        for (my $i = 0; $i < $nrows; ++$i) {
            my %record = map {
				my $repeat = $_->{repeat} || 1;
				my $value = ($repeat == 1)
						? $_->{data}[$i]
						: [ @{ $_->{data} }[$i*$repeat .. ($i+1)*$repeat - 1] ];
				($_->{name} => $value);
			} @column;
            push(@$dataref, \%record);
        }
    }

    return $self;
}



1;

