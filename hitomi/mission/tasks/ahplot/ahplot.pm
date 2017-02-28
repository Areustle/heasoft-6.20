package ahplot ;

use strict ;
use warnings;
use diagnostics;


# Standard Modules
use File::Spec ;
use File::Spec::Functions qw ( rel2abs catfile ) ;
use File::Basename ;
use Cwd ;

# astro H specific modules
use ahlog ;
use ahapp ;
use ahgen qw (:ALL) ;
use File::Copy;
use List::Util qw( min max ); # Finding minimum and maximum of an array
use Scalar::Util qw(looks_like_number);

BEGIN {
      use Exporter () ;

      our ($VERSION, @ISA, @EXPORT, @EXPORT_OK, %EXPORT_TAGS) ;

      # set the version for version checking
      # $VERSION = 1.00 ;

      # if using RCS/CVS, this may be preferred
      $VERSION = sprintf "%d.%03d", q$Revision: 1.5 $ =~ /(\d+)/g;

      @ISA = qw(Exporter);
     
      # Functions exported by default.
      @EXPORT = qw(@pointing_timerange
                   @calculate_y);

      #%EXPORT_TAGS = ( ); # eg: TAG => [ qw!name1 name2! ],
      %EXPORT_TAGS = (ALL => \@EXPORT_OK ); # eg: TAG => [ qw!name1 name2! ],

      # your exported package globals go here,
      # as well as any optionally exported functions
      @EXPORT_OK = qw($debug
                      
                      );
      }

# Not sure why this needs to be here.
our @EXPORT_OK;

#############################################################################

sub parse_timerange {

 # This Routine parses the time ragne parameter it can be eithere and gti file
 # name or a 

 my $time_range = shift;
 # Array of start and stop times (File may contain multiple time range)
 my @start_time = ();
 my @stop_time = ();

  # Parse the file name in gtifile[0] = name gtifile[1] = extension
  # if time range is in form start,stop parse_file name will
  # return the string start,stop in gtifile[0]
  my @gtifile = ahgen::parse_file_name($time_range);

  if (-e $gtifile[0]) {  #time_range is a GTI extenion 
    @start_time = ahgen::read_column($gtifile[0],$gtifile[1],"START");
    @stop_time = ahgen::read_column($gtifile[0],$gtifile[1],"STOP");
  } else { # time_range is range in this format start,stop
    my @times = split(",",$time_range);
    if (scalar(@times) != 2) {
      ahlog::ah_err "For comma delimited timerange, only a single start and stop time are accepted (tstart,tstop) \n";
      exit 0;
    }
    $start_time[0] = $times[0];
    $stop_time[0] = $times[1];
  }

  return (\@start_time,\@stop_time);
}
#############################################################################

sub timerange_rownums  {

  # Determine row numbers in infile that correspond to a time range  
  # provided as either as a GTI extension in a file or a list of two 
  # timerange in this form start_time,stop_time

  my $infile = shift;
  my ($start_time, $stop_time) = @_;

  # read in the time column in infile
  my @infile_parsed = ahgen::parse_file_name($infile); 
  my @time_in =  ahgen::read_column($infile_parsed[0],$infile_parsed[1],"TIME");

  # Obtain the row numbers in infile that correspond to @start_time and 
  # @stop_time
  my $rownum = 0; 
  my $row_start = -1;
  my $row_stop = -1;  
  my @row_pointing_start = ();
  my @row_pointing_stop = ();
  for (my $ii=0; $ii<scalar(@{$start_time});++$ii) {
    $row_start = -1;
    $row_stop = -1;
    for (my $jj=0; $jj<scalar(@time_in);++$jj) {

      # Locate row numbers
      $rownum = $jj+1;
      if (($time_in[$jj] >= $start_time->[$ii]) && ($row_start < 0)) {
        $row_start = $rownum;
      }
      if (($time_in[$jj] >= $stop_time->[$ii]) && ($row_stop < 0)) {
        $row_stop = $rownum-1; 
      }

      # If both rows are found append and break out of the loop
      # over time ranges.  Only append if row_stop>row_start 
      if (($row_start > 0) && ($row_stop > 0)) {
        if ($row_start < $row_stop) {
          push(@row_pointing_start,$row_start);
          push(@row_pointing_stop,$row_stop);
        } 
        last;
      } 
    }

    # If start/stop row weren't found set them to the first/last row  
    if ($row_start < 0) { push(@row_pointing_start,1); }
    if ($row_stop < 0) { push(@row_pointing_stop,scalar(@time_in)); }
  }

  # Return the start and stop row numbers
  return (\@row_pointing_start,\@row_pointing_stop);

}


sub remove_nulls {

  # This routine removes NULL elements from an input array
  # using perls splice function

  my @array = @_;

  for (my $ii=0; $ii<scalar(@array); ++$ii) {
    if (uc $array[$ii] eq "NULL") { 
      splice @array,$ii,1;
      --$ii;
    }
  }
  
  return(@array); 

}

sub calculate_gti_y_position_parameters {
  
  # This routine determine the first y postion of a gti time
  # interval plot (y_0) the spacing between different gti plots (dy)
  # and the bottom border of the plot on the y axis  
 
  # Read input data array
  my @y_array = @_;
  my @out = ();

  # Determine the max and min values for this array
  my $y_max = max grep {looks_like_number($_)} @y_array;
  my $y_min = min grep {looks_like_number($_)} @y_array;

  # Determine difference between max and min
  my $delta_y = $y_max - $y_min;

  # y_0 intitial y position of the first set of GTI tick marks
  my $y_0 = $y_max + (0.1 * $delta_y);

  # difference in height between the different GTI tick marks
  my $dy = 0.1 * $delta_y; 
  
  # bottom border of the plot window
  my $plot_bottom_border = $y_min - $dy;

  # fill in output array
  $out[0] = $plot_bottom_border;
  $out[1] = $y_0;
  $out[2] = $dy;
  
  return @out;
   
}
  
sub  write_gti_pco {
  
  # get input variables 
  my $outfile = shift;
  my $qdp = shift;
  my $offset = shift;
  my $tstart = shift;
  my $tstop = shift;
  my ($ymin, $ymax) = @_;
  my $panel_number = 0;

  # Write plot command file ahplot.pco this will be used by fplot to create 
  # the final plot.  It contains commands for QDP/PLT 
  my $pltfile    ="ahplot.pco";
  # Check if the plot command file exist if so delete it
  if (-e $pltfile) {
    unlink $pltfile 
  }

  ahapp::add_temp_file($pltfile);
  open PLT,">$pltfile";
  print PLT "line step\n";
  my $mark_string = "mark 2 on ";
  print PLT "error on \n";
  if (uc $offset eq "YES") { print PLT "R X 0 $tstop-$tstart \n"; }
  for (my $ii=0; $ii<scalar(@{$ymin}); ++$ii) {
    $panel_number = $ii+2;
    if ($ymin->[$ii] == $ymax->[$ii]) { 
      $ymin->[$ii] = $ymin->[$ii] - (0.1 * $ymin->[$ii]);
      $ymax->[$ii] = $ymax->[$ii] + (0.1 * $ymax->[$ii]);
    }
    print PLT "R Y" . $panel_number . " $ymin->[$ii] $ymax->[$ii] \n";
    print PLT "grid Y" . $panel_number . " 2 \n";
    $mark_string = $mark_string . $panel_number. " ";
  }
  print PLT $mark_string;
  print PLT "cs 0.5 \n";
  if (uc $qdp eq "NO") {
    print PLT "cpd $outfile/cps \n";
    print PLT "plot\n"; # force pgplot to plot the file
    print PLT "quit\n"; # force pgplot to quit
  }
  close PLT;

}

sub prepare_gti_plot_data {

  my $gtifile = shift;
  my $infile_extn = shift;
  my $xcol = shift;
  my $dxcol = shift;
  my $outfile = shift;
  my ($y0, $dy, $ycols) = @_;


  my $yvalues_string = "";
  my $ycols_string = ""; # String input for ycol parameter of gtiplot
  my $numycols = scalar(@{$ycols}); # The number of y columns

  my @gtifilelist = ();
  my $status = 0;

  # Construct input to ycol parameter (eg. COl1,COL2,COL3)
  for (my $ii=0; $ii<$numycols; ++$ii) {
    $ycols_string = $ycols_string . "$ycols->[$ii],";
  } 

  # Parse list of input GTI files
  @gtifilelist = ahgen::get_file_list($gtifile);
  
  my $numgtifiles = scalar(@gtifilelist);

  # Create a "gti file" for each gti time range provided by the user.
  # These gti files contain the gti information in a format that can
  # be easily plotted.  The gti files are combined into one single file 
  # after they have been generated.  
  my $merge_list = "";
  # Run GTIplot to determine GTI ranges 
  for (my $ii=0; $ii<$numgtifiles; ++$ii) {
    $yvalues_string = "";
    # Construct input string for y value parameter
    for (my $jj=0; $jj<$numycols; ++$jj) {
      my $yval = $y0->[$jj] + (($ii+1) * $dy->[$jj]);
      $yvalues_string = $yvalues_string . "$yval,";
    }  

    ahapp::add_temp_file("gti$ii.fits");
    ahgen::run_ftool("gtiplot",
                     "infile=$gtifilelist[$ii]",
                     "outfile=gti$ii.fits",
                     "yvalue=$yvalues_string",
                     "xcol=$xcol",
                     "dxcol=$dxcol",
                     "ycol=$ycols_string",
                     "clobber=YES");
    if ($ii != 0) {
      $merge_list = $merge_list . ",gti$ii.fits[GTI]";
    } else {
      $merge_list = "gti$ii.fits[GTI]";
    }
  }

  # We need to merge the gti files together because each additional
  # GTI range needs to be plotted at a different Y axis value
  ahapp::add_temp_file($outfile);
  $status = ahgen::run_ftool("ftmerge",
                   "infile=$merge_list",
                   "outfile=$outfile",
                   "clobber=YES");
  if ($status) {
    ahlog::ah_err "Failed to merge list of gti files \n";
    return $status;
  }

  # Set extension name of output file so it can be easily merged with infile
  $status = ahgen::set_keyword($outfile,"GTI","EXTNAME",$infile_extn,"name of this binary table extension");

}


sub write_null_row {

  # This function adds an additional row to a fits file 
  # with NULLS in the user sepcified columns.  This routine
  # expects the extension to be included in the filename 

  my $filename = shift;
  my @null_columns= @_;
  my $status = 0;

  ahapp::add_temp_file("nullrow.fits");

  # Copy first row of the input fits file to file nullrow.fits
  $status = ahgen::run_ftool("ftcopy",
                             "infile=$filename\[#ROW==1\]",
                             "outfile=nullrow.fits",
                             "copyall=NO",
                             "clobber=YES");

  # Change the value of the specified columns to null
  for (my $ii=0; $ii<scalar(@null_columns); ++$ii) {
    ahgen::run_ftool("ftcalc",
                     "infile=nullrow.fits",
                     "outfile=nullrow.fits",
                     "column=$null_columns[$ii]",
                     "expression=#null",
                     "rows=1",
                     "clobber=yes");
  }

  # merge nullrow.fits with input file
  $status = ahgen::run_ftool("ftmerge",
                             "infile=$filename,nullrow.fits",
                             "outfile=$filename",
                             "clobber=YES");
  
  if ( $status ) {
    ahlog::ah_err "Failed to add NULL Row \n";
    return $status;
  } 
 
  return $status; 

}
