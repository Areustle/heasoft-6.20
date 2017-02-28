#!/usr1/local/bin/perl
##########################################
# Skvbatch.pl is a front end to (x)webquery.pl 
# to return FITS files from SkyView from 
# the command line.
#
# Calling sequence and parameter information
# can be found on the SkyView web site:
#     http://skyview.gsfc.nasa.gov/batchpage.html
#
##########################################

# Loop over the argument list and look to see
# if file has been defined 
$i=0;
foreach (@ARGV) {
   last if (/^file=/i);
   $i++;
}

# if a filename has been specified, make it the filename
# and splice it out of the argument list
$filename = splice(@ARGV,$i,1);

# Read in the argument list and put the
# values in quotes
$new = 'xwebquery.pl url="/cgi-bin/pskcall" host="skys.gsfc.nasa.gov" ' . '\'' . join('\' \'',@ARGV) . '\'';

# Exexcute web query
@arr = `$new`;

# Put all the blocks togehter
$data = join('',@arr);

# Read past the HTTP header to get to the real data
# this is a kludge such that this works with the NCSA and APACHE
# web servers
if (index($data,"\n\r\n") == -1) {
    $locat = index($data, "\n\n") + 2;
} else {
    $locat = index($data, "\n\r\n") + 3;
}

if ($locat == 1) {$locat = 0;}
			       
# Either write to a file or standard output
if (length($filename)) {
   # get the filename from the argument
   ($key,$truename) = split('=',$filename);

   # Open the file
   open(WWWOUT,">$truename");

   # Print out only from SIMPLE on
   print(WWWOUT substr($data, $locat));
} else {
   # Print to standard outuput
   print substr($data, $locat);
}

# And that's all there is


