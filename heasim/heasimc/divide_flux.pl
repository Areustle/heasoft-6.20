#! /usr/bin/perl

# If there's no output direcotry, then make it.
if (! -d "output"){
    mkdir "output";
}

# Get the number of flux subdivisions from the command line, i.e. calling sequence is:
#   ./divide_flux 5      - to have 5 subexposures equal in flux

my $N_flux = 1;
if ( defined $ARGV[0] ){
    $N_flux = $ARGV[0];
}


my $mission="astro-h";                                               # mission
my $instrume="sxi";                                                  # instrume
my $cal_dir="sxi";                                                   # master calibration data directory                                                   
my $resp_dir="$ENV{'HEASIM_SUPPORT'}/$mission/$cal_dir/response";    # response directory
my $psf_dir="$ENV{'HEASIM_SUPPORT'}/$mission/$cal_dir/psf";          # psf directory
my $vig_dir="$ENV{'HEASIM_SUPPORT'}/$mission/$cal_dir/vignette";     # vignette directory
my $back_dir="$ENV{'HEASIM_SUPPORT'}/$mission/$cal_dir/background";  # int-bkg directory

my $arf="$resp_dir/sxt-i_140505_ts02um_int01.8r.arf";
my $rmf="$resp_dir/ah_sxi_20120702.rmf";
my $back="none";
my $vignette="$vig_dir/SXT_VIG_140618.txt";
my $psf="$psf_dir/sxt-i_EEF_4p5keV_140617.txt";
my $mdbfile="$ENV{LHEA_DATA}/heasim.mdb";

my $ra=151.8606;
my $dec=16.1085;
my $pa=0.0;
my $expose=10000.;
my $root_name="burst";
my $logfile="$root_name.log";

my $input_src_file="plaw_burst1.txt";
my $sub_input_src_file="subflux_$input_src_file";

# If the logfile already exists, delete it.
if (-e "$logfile") {unlink "$logfile";}

# If the sub-divided flux filename already exists, delete it.
if (-e "$sub_input_src_file") {unlink "$sub_input_src_file";}

# Open $input_src_file for reading, $sub_input_src_file for writing.
open(my $FILE1, '<', $input_src_file);
open(my $FILE2, '>', $sub_input_src_file);
print $FILE2 "# ALL ORIGINAL FLUXES HAVE BEEN DIVIDED BY $N_flux.\n\n";

# Copy input source file to new source file, but divide fluxes by N_flux
while(my $line = <$FILE1>){
    chomp $line;
    $line =~ s/^\s+//;                     # Remove white space at beginning
    my $firstchar = substr($line, 0, 1);   # Get the first character in the line

    if (length $line > 0){                 # If not a blank line
	if ($firstchar != "#") {           # If not a comment

	    my @fields = split ",", $line; # Split the line into comma-delimited fields
	    @fields[5] /= $N_flux;         # Flux is field 5; divide by N_flux
	    
	    # Append all fields into a new line
	    my $new_line = "";
	    for my $field (@fields) {$new_line .= "$field,";}

	    # Remove the comma from the end of the new line
	    $new_line = substr($new_line, 0, -1);
	    
	    # Print both the original line commented out, and the new line, to file
	    print $FILE2 "# $line\n";
	    print $FILE2 "  $new_line\n\n";

	} else { # Otherwise this line is a comment, just copy to new file as is.
	    print $FILE2 "$line\n";
	}
    }
}

# Close the files
close $FILE1;
close $FILE2;

# Set the constant parameters
my @pset_commands = "";
push @pset_commands,"punlearn heasim";
push @pset_commands,"pset heasim mission=$mission";
push @pset_commands,"pset heasim instrume=$instrume";
push @pset_commands,"pset heasim rapoint=$ra";
push @pset_commands,"pset heasim decpoint=$dec";
push @pset_commands,"pset heasim roll=$pa";
push @pset_commands,"pset heasim exposure=$expose";
push @pset_commands,"pset heasim flagsubex=no";
push @pset_commands,"pset heasim psffile=$psf";
push @pset_commands,"pset heasim vigfile=none";
push @pset_commands,"pset heasim rmffile=$rmf";
push @pset_commands,"pset heasim arffile=$arf";
push @pset_commands,"pset heasim intbackfile=none";
push @pset_commands,"pset heasim debug=no";
push @pset_commands,"pset heasim clobber=yes";
push @pset_commands,"pset heasim mdbfile=$mdbfile";
push @pset_commands,"pset heasim insrcdeffile=$sub_input_src_file";

# Execute the psets
for my $command (@pset_commands) {system($command)};

# Write paramters to logfile
system("plist heasim 2>&1 | tee -a $logfile");


# Initialize a string of commands to merge the fits files later
my $merge_line = "";


# Loop over the flux subdivisions, calling heasim
for (my $NN=1; $NN <= $N_flux; $NN++){

    # Write a header to screen and to log, telling which block we're doing
    open(my $FILE3, '>>', $logfile);
    print "\n\n\n########## BEGINNING SUB-FLUX SIMULATION $NN of $N_flux ##########";
    print $FILE3 "\n\n\n########## BEGINNING SUB-FLUX SIMULATION $NN of $N_flux ##########";
    close $FILE3;

    # Execute the psets that change during each loop
    system("pset heasim outfile=output/$root_name$NN.evt");
    system("pset heasim seed=$NN");

    # Call heasim, pipe output to screen and to log
    system("heasim mode=hl 2>&1 | tee -a $logfile");

    # Append the output file name into the merge string
    $merge_line .= "output/$root_name$NN.evt.fits,"

}

# Remove the comma from the end of the merge line
$merge_line = substr($merge_line, 0, -1);

# If the final output files already exist, delete them
if (-e "output/$root_name.evt.fits")  {unlink "output/$root_name.evt.fits";}
if (-e "output/$root_name.sort.evt.fits")  {unlink "output/$root_name.sort.evt.fits";}

# Merge all the output fits files into one
system("ftmerge $merge_line output/$root_name.evt.fits");

# Sort the fits file by time
system("ftsort output/$root_name.evt.fits[EVENTS] output/$root_name.sort.evt.fits time method=heap memory=yes copyall=yes");

# Rename the sorted fits file to main fits file
system("mv output/$root_name.sort.evt.fits output/$root_name.evt.fits");

