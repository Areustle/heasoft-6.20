
# Fix up input level 1 files
#
# This module fixes up the files which have pipeline errors.  Namely,
# that they have the wrong INSTRUME keyword.
#
# Also, the TIMEZERO keyword is set to zero, in case the keyword is
# not present in the data.
# 
# $self - class name (ignored)
# @globlist - list of wildcard patterns, of files to be changed
#
# RETURNS: nothing
#
# EXCEPTIONS: * failed to unzip file
#             * failed to open file for writing
#             * failed to write INSTRUME keyword
#
#
# MODIFICATIONS:
#   02 Sep 2004 CM
#     Reset TIMEZERO keyword to zero, in case it is missing.
#     Revised method of scanning the input files
#
package BAT::fixup;

sub do {
    my ($self,@globlist) = @_;
    my ($globspec, $file, $rootfile, $nhdu, $status, $fits, $extnum);
    my @files;

    foreach $globspec (@globlist) {
	@files = glob($globspec);
	foreach $file (@files) {
	    # Gunzip if necessary
	    if ($file =~ m/(.*)\.gz$/) {
		$rootfile = $1;
		system("gunzip $file");
		die "Gunzip of $file failed" if ($?);
		$goodfile = $rootfile;
	    } else {
		$goodfile = $file;
	    }

	    $fits = SimpleFITS->open("+<$goodfile");  # Read/write mode
	    $status = $fits->status();
	    die "ERROR: could not open $goodfile (status=$status)" if ($status);

	    $nhdu = SimpleFITS->nhdu();
	    foreach $i (1 .. $nhdu) {
		$fits->move($i);
		$fits->writekey("INSTRUME", "BAT");
		$fits->writekey("TIMEZERO", 0.0);
	    }
	    $status = $fits->status();
	    $fits->setstatus(0)->close(); # Reset status so we can close file
	    undef $fits;

	    die "ERROR: could not write INSTRUME keyword to $goodfile (status=$status)" 
		if ($status != 0 && $status != 107);

	} # End of per-file loop
	
    } # End of per-glob loop


    return undef;
}


1;
