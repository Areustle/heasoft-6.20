package Clock::SwiftClock;

require Exporter;
@ISA = qw(Exporter);
@EXPORT = qw(
   sCC
   sLeap
);

use strict;

# Input MJD (UT) and return number of leap seconds between that time and the Swift epoch (2001.0 UTC)
# 08Feb2007:
# Note that the convention (per SwiftClock.C) is that a leapsecond 'has occurred' starting at 00:00:01
# so we have to add $dsec (one second expressed in days) to the MJD values for consistency
sub sLeap{
  my $mjd = @_[0];
  my $swep = 51910;
  my $xep = 49353; #RXTE epoch, for testing only
  my $dsec = 1.157407407e-05; #one second as a fraction of a day

  use vars qw($line $taiutc $TAI @leapsMJD @leapsecs @MJDoffset @leapcoeff $i $dt0 $dt $numlines);

  $taiutc="tai-utc.dat";
  if (-f $ENV{"TIMING_DIR"}."/$taiutc") {$TAI=$ENV{"TIMING_DIR"}."/$taiutc"}
  elsif (-f $ENV{"LHEA_DATA"}."/$taiutc"){$TAI=$ENV{"LHEA_DATA"}."/$taiutc"}
  else {die "===>  Could not find $taiutc in TIMING_DIR or LHEA_DATA"}

  $numlines=0;
  open(TAIFH, $TAI);
  while(<TAIFH>){
    chomp($line=$_);
    $line =~ /.*JD 24(\d+).5  TAI-UTC= *(\d+.\d+).*MJD - (\d+.\d*).*X (\d*.\d*) *S/;
    push @leapsMJD, $1;
    push @leapsecs, $2;
    push @MJDoffset, $3;
    push @leapcoeff, $4;
    $numlines++
  }
  close(TAIFH);
  $numlines--;



  #for the epoch
  $i=$numlines;
  while ( ($swep < $leapsMJD[$i] + $dsec) && $i){ $i-- }
  if ( $i > 12 ) {$dt0=$leapsecs[$i]}else{
    $dt0 = $leapsecs[$i] + ($swep - $MJDoffset[$i]) * $leapcoeff[$i]
  }

  #for the input MJD
  $i=$numlines;
  while ( ($mjd < $leapsMJD[$i] + $dsec) && $i){ $i-- }
  if ( $i > 12 ) {$dt=$leapsecs[$i]}else{
    $dt = $leapsecs[$i] + ($mjd - $MJDoffset[$i]) * $leapcoeff[$i]
  }

  return ($dt-$dt0);
}

{
# Using 'static variables' to ensure that a FITS clock file
# is only read once since multiple reads slows things down
# drastically.
my $nrows = 0;
my @tstart = 0;
my @tstop = 0;
my @c0 = 0;
my @c1 = 0;
my @c2 = 0;
my @tz = 0;
my $isfits = "no";

my $isascii = "no";
my @m0 = ();
my @m1 = ();
my @m2 = ();
my @end = ();


# Input:  $t        Time for which correction is desired (MET in s (TT))
#         $tsys     Timesystem for correction (TTCF / UTCF)
#         $leaps    Leapseconds since Swift epoch (2001.0) to be
#                   added (if necessary) to $tCorr
#                    If less than -990 (eg -999) then compute for each time
#                    Ignored if $tsys eq "U"
#         $swcofile Swift Clock Correction file; FITS or ascii OK.
#                     Absolute path or relative to TIMING_DIR or LHEA_DATA
# Output: $tZero    Timezero (always 0.0 for Swift)
#         $tCorr    Correction (in microseconds)
#         $tLast    Last valid MET time for the specified clock file
#                   (only when $t = "last", otherwise -1)
#
# 16Feb2007:
# Allow for the swcofile to begin with ftp: or http:
#
# 09Feb2009:
# Fixed timezero return (was a no-op when swcofile is FITS, though
#                        inconsequential since always 0.0 for Swift)
#
# If input time is "last" then return tCorr for last table entry (and tLast)
#
# Robustified error handling
#
# 08Dec2009:
# Changed ascii table section to only read file once
# 02Jul2012
# Ascii extrapolation was being done from second-to-last file entry
#
sub sCC{
  my ($t,$tsys,$leaps,$swcofile,$tZero,$tCorr,$tLast) = @_;
  use Astro::FITS::CFITSIO qw(:constants :longnames);

  use vars qw($t0 $modt $m0 $m1 $m2 $end $SCC $error $utoffset $subday);
  use vars qw($hdutype $anynul $i $t1 $lastMET);

  if (substr($swcofile,0,5) eq "http:" or substr($swcofile,0,4) eq "ftp:") {$SCC = $swcofile}
  else{
    if (-f $ENV{"TIMING_DIR"}."/$swcofile") {$SCC=$ENV{"TIMING_DIR"}."/$swcofile"}
    elsif (-f $ENV{"LHEA_DATA"}."/$swcofile"){$SCC=$ENV{"LHEA_DATA"}."/$swcofile"}
    elsif ($swcofile =~ /(.+)\+\d+$/) {$SCC = $1}
    else {$SCC = $swcofile}
    -f $SCC or die "===> Error verifying existence of $SCC";
  }

  $error = 0;
  $utoffset = -999999.0;
  $lastMET = -1;

  if ($nrows == 0 && $tstart[0] == 0){
    my $fits = Astro::FITS::CFITSIO::open_file($SCC, READONLY, $error);
    if (not $error) {
      $fits->movabs_hdu(2, $hdutype, $error);
      my $header = $fits->read_header;
      unless ($header->{TELESCOP} =~ /SWIFT/ and
	      $header->{EXTNAME} =~ /CLOCK_CORRECT/) {
	die "==>  $SCC is not a proper FITS Swift Clock Correction file";
      }
      $fits->get_num_rows($nrows, $error);
      $fits->read_col_dbl(1, 1, 1, $nrows, 0, \@tstart, $anynul, $error);
      $fits->read_col_dbl(2, 1, 1, $nrows, 0, \@tstop, $anynul, $error);
      $fits->read_col_dbl(3, 1, 1, $nrows, 0, \@tz, $anynul, $error);
      $fits->read_col_dbl(4, 1, 1, $nrows, 0, \@c0, $anynul, $error);
      $fits->read_col_dbl(5, 1, 1, $nrows, 0, \@c1, $anynul, $error);
      $fits->read_col_dbl(6, 1, 1, $nrows, 0, \@c2, $anynul, $error);
      $fits->close_file($error);
      if ($error) {die "problem reading $SCC"}
      $isfits = "yes";
    }else{$error = 0}
  }

  if ($isfits eq "yes"){
    if ($t =~ /^last$/i){
      # determine correction for final table entry
      $t1 = ($tstop[$nrows-1] - $tstart[$nrows-1])/86400.;
      $utoffset = $c0[$nrows-1] + $c1[$nrows-1]*$t1 + $c2[$nrows-1]*$t1*$t1;
      $t0 = $tz[$nrows-1];
      $t = $lastMET = $tstop[$nrows-1]; #set $t in case it's passed to sLeap() below
    }else{
      for ($i=0;$i<$nrows;$i++){
	if ($t >= $tstart[$i] && $t <= $tstop[$i]){
	  $t1 = ($t-$tstart[$i])/86400.;
	  $utoffset = $c0[$i] + $c1[$i]*$t1 + $c2[$i]*$t1*$t1;
	  $t0 = $tz[$i];
	  last;
	}
      }
    }
  }else{ #clock file must be ascii
    if ($isascii eq "no"){ # first time through; read in clock file
      open(CCFH, $SCC);
      while(<CCFH>){
	($m0,$m1,$m2,$end)=split;
	if ($m0 < 0.0 && $end < 0.0){last}
	push @m0,$m0;
	push @m1,$m1;
	push @m2,$m2;
	push @end,$end;
	$nrows++;
      }
      close(CCFH);
      $isascii = "yes";
    }
    if ($t =~ /^last$/i){ #last tCorr requested
      for ($i=0;$i<$nrows;$i++){
	if ($end[$i] < 0.0){$subday = $m0[$i]}
	$utoffset = $m0[$i] + $m1[$i]*$end[$i] + $m2[$i]*$end[$i]*$end[$i];
	$t = $lastMET = 86400.0*($subday+$end[$i]); #set $t for sLeap() below
      }
    }else{
      for ($i=0;$i<$nrows;$i++){
	if ($end[$i] < 0.0){
	  $subday = $m0[$i];
	  $modt = $t / 86400.0 - $subday;
	  $t0 = $m1[$i];
	}else{
	  if ($modt < $end[$i]) {
	    $utoffset = $m0[$i] + $m1[$i]*$modt + $m2[$i]*$modt*$modt;
	    last;
	  }
	}
      }
    }
  }

  if ( $error || ($utoffset == -999999.0) ){
    $t0 = 0.0;
    $utoffset = 0.0;
    $error++;
  }

  # This isn't very Perl-like, but I want to track the original C code
  # rather than return a list
  @_[4] = $t0;
  @_[5] = -1*$utoffset; #UTCF in microseconds <= $tCorr
  # NOTE: corr is measured UT offset, so UTCF is opposite in sign
  @_[6] = $lastMET; # <= $tLast

  if (not $error){
    # Unless converting to UTC we need TTCF instead of UTCF so
    # we have to "undo" any leapseconds inherent in the UT offset
    if ($tsys =~ /^t/i){
      if ($leaps >= -990){ # -999 is used to request on-the-fly calc
	@_[5] += (1e6 * $leaps);
      }else{ #compute leapseconds on the fly
	my $swep = 51910;
	my $mjd = $t/86400.0 + $swep;
	my $leapcor = &sLeap($mjd); #sec
	@_[5] += (1e6 * $leapcor);
      }
    }
  }

  return $error;
}
} #extra block to enclose 'static' variables with subroutine definition

1;
