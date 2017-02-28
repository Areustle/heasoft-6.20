package libswxrtgrblc;

# Miscellaneous functions for processing Swift light curves

#use strict;
use vars qw(@ISA @EXPORT $VERSION);
use Exporter;
$VERSION = "1.0";
@ISA = qw( Exporter );

@EXPORT = qw( &chat &error &warnhi &warnlo &debug &genCmd &runSystem &parseFitsList
              &runSystemNoChat &getListParam &getStrParam &getTmpFile &dumpListToTxt  
              &readLC &extractLC &writeLC &getEmptyLC &readXImageDetectFile &lcStats
              &getFltParam &getIntParam &getBoolParam &convertRAStringToDegrees
              &convertDecStringToDegrees &swifttime FLT_NULL PI );

use HEACORE::PIL;
use HEACORE::HEAUTILS;
use Astro::FITS::CFITSIO qw( :longnames :constants );
use Env;
use Errno;
use File::Spec::Functions;

# constants
use constant PI                 => 3.14159265358979;
use constant FLT_NULL           => -2.22e-28; # !trouble unless something==this

# some useful PIL constants
use constant PIL_NO_FILE        => -3003;
use constant PIL_FILE_NO_RD     => -3014;

# don't use these variable names in your code
# not that you would, since they are awful
our $libswxrtgrblcPrefix  = "";
our $libswxrtgrblcChatter = 0;
our %libswxrtgrblcTmpNames = ( );

sub setchat ( $ ) {
    $libswxrtgrblcChatter = shift;
}

sub setprefix ( $ ) {
    $libswxrtgrblcPrefix = shift;
}

#
# genCmd -
#
#       generates a shell command. first arg is taskname, second is
#       hash reference, where keys are parameter names, and values are
#       parameter values
#
sub genCmd ( $$ ) {

    my $toolname = shift;
    my $params   = shift;

    my $cmd = $toolname;
    foreach my $param ( sort keys %{$params} ) {
        $cmd .= " $param=\"$params->{$param}\"";
    }
    return $cmd;
}

#
# getTmpFile -
#
#       returns a unique, non-extant temporary file with a given extension
#       the only argument is the extension (e.g. "fits"). a '.' will 
#       be placed between the root name and the extension.
#
sub getTmpFile( $ ) {
    my $ext = shift;
    my $nam = join '.', 'tmpfil', $$, int( rand( 65535 ) ), $ext;
    if ( exists $ENV{HEADAS_TMPDIR} && defined $ENV{HEADAS_TMPDIR} ) {
        $nam = catfile( $ENV{HEADAS_TMPDIR}, $nam );
    }
    while ( -e $nam || exists $libswxrtgrblcTmpNames{$nam} ) {
        $nam = join '.', 'tmpfil', $$, int( rand( 65535 ) ), $ext;
        if ( exists $ENV{HEADAS_TMPDIR} && defined $ENV{HEADAS_TMPDIR} ) {
            $nam = catfile( $ENV{HEADAS_TMPDIR}, $nam );
        }
    }
    $libswxrtgrblcTmpNames{$nam} = 1;
    return $nam;
}

#
# chat - 
#
#       prints a list of messages - newlines are the callers responsibility
#       message is only printed if the first argument is >= the global 
#       $chatter variable. prepends the task name and version to each line.
#
#       Inputs:
#               - chat level
#               - message list
#
#       Outputs: None (except to terminal)
#
sub chat {
    my $lev = shift;
    my $tmp = join "", @_;
    chomp $tmp;
    my @tmp2 = split /\n/, $tmp, -1;
    my @msg = ( );
    foreach my $line ( @tmp2 ) {
        push @msg, "${libswxrtgrblcPrefix}Info: $line\n";
    }
    if ( @msg && $lev <= $libswxrtgrblcChatter ) {
        print @msg;
    }
}

sub error {
    my $lev = shift;
    my $tmp = join "", @_;
    chomp $tmp;
    my @tmp2 = split /\n/, $tmp, -1;
    my @msg = ( );
    foreach my $line ( @tmp2 ) {
        push @msg, "${libswxrtgrblcPrefix}Error: $line\n";
    }
    if ( @msg && $lev <= $libswxrtgrblcChatter ) {
        print @msg;
    }
}

sub warnlo {
    my $lev = shift;
    my $tmp = join "", @_;
    chomp $tmp;
    my @tmp2 = split /\n/, $tmp, -1;
    my @msg = ( );
    foreach my $line ( @tmp2 ) {
        push @msg, "${libswxrtgrblcPrefix}Warning: $line\n";
    }
    if ( @msg && $lev <= $libswxrtgrblcChatter ) {
        print @msg;
    }
}

sub warnhi {
    my $lev = shift;
    my $tmp = join "", @_;
    chomp $tmp;
    my @tmp2 = split /\n/, $tmp, -1;
    my @msg = ( );
    foreach my $line ( @tmp2 ) {
        push @msg, "${libswxrtgrblcPrefix}WARNING: $line\n";
    }
    if ( @msg && $lev <= $libswxrtgrblcChatter ) {
        print @msg;
    }
}

sub debug {
    my $lev = 5;
    my $tmp = join "", @_;
    chomp $tmp;
    my @tmp2 = split /\n/, $tmp, -1;
    my @msg = ( );
    foreach my $line ( @tmp2 ) {
        push @msg, "${libswxrtgrblcPrefix}Debug: $line\n";
    }
    if ( @msg && $lev <= $libswxrtgrblcChatter ) {
        print @msg;
    }
}

#
# runSystem -
# 
#       Runs a system command with output logging, and error checking
#       adapted from Xan.pm
#
#       Inputs:
#               - base command name
#               - arguments list
#
#       Outputs:
#               - reference to the list of output lines from the command
#               - return status of the command
#
sub runSystem {
    
    my $base = $_[ 0 ];
    my $cmd  = join " ", @_;
    my @out  = ( );
    chat( 3, "running: $cmd\n" );
    eval {
        #open( PIPE, "$cmd 2>&1 |" ) || return;
        local $SIG{__DIE__} = sub { return @_; };
        open( PIPE, "$cmd 2>&1 |" ) or die;
        while ( <PIPE> ) { 
            print $_ unless $libswxrtgrblcChatter < 3; 
            push @out, $_;
        }
        #close( PIPE ) || return;
        close( PIPE ) or die;
    };
    my $rc = $?;
    if ( $rc == 0xff00 ) {
        warnhi( 1, "$base failed, error was: $!\n" );
        warnhi( 1, "command line was: $cmd\n" );
    } elsif ( $rc > 0x80 ) {
        my $nrc = ( $rc >> 8 );
        warnhi( 1, "command $base exited with non-zero status: $nrc\n" );
        warnhi( 1, "command line was: $cmd\n" );
    } elsif ( $rc != 0 ) {
        warnhi( 1, "command $base failed with core dump: $rc\n" );
        warnhi( 1, "command line was: $cmd\n" );
    }
    if ( wantarray ) { 
        return ( $rc, \@out ); 
    } else { 
        return $rc;
    }
}

#
# runSystemNoChat -
# 
#       Runs a system command with error checking and returns the output
#       by way of a list reference
#
#       Inputs:
#               - base command name
#               - arguments list
#
#       Outputs:
#               - reference to the list of output lines from the command
#               - return status of the command
#
sub runSystemNoChat {
    
    my $base = $_[ 0 ];
    my $cmd  = join " ", @_;
    my @out = ( );
    eval {
        #open( PIPE, "$cmd 2>&1 |" ) || return;
        local $SIG{__DIE__} = sub { return @_; };
        open( PIPE, "$cmd 2>&1 |" ) or die;
        @out = <PIPE>;
        #close( PIPE ) || return;
        close( PIPE ) or die;
    };
    my $rc = $?;
    if ( $rc == 0xff00 ) {
        warnhi( 1, "$base failed, error was: $!\n" );
        warnhi( 1, "command line was: $cmd\n" );
    } elsif ( $rc > 0x80 ) {
        my $nrc = ( $rc >> 8 );
        warnhi( 1, "command $base exited with non-zero status: $nrc\n" );
        warnhi( 1, "command line was: $cmd\n" );
    } elsif ( $rc != 0 ) {
        warnhi( 1, "command $base failed with core dump: $rc\n" );
        warnhi( 1, "command line was: $cmd\n" );
    }
    if ( wantarray ) { 
        return ( $rc, \@out ); 
    } else { 
        return $rc;
    }
}

#
# getEmptyLC -
#
#       returns an empty lightcurve hash
#
sub getEmptyLC {
    # empty light curve template
    my %emptyLC = (
        'time'    => [ ],       # center of time bin (sec)
        'rate'    => [ ],       # rate (in units of ratunit)
        'rateerr' => [ ],       # error (also in units of ratunit)
        'fracexp' => [ ],       # fraction exposure for the bin
        'timedel' => [ ],       # full width of the bin (sec)
        'uplimit' => [ ],       # is this bin an upper limit?
        'start'   => [ ],       # GTI start times
        'stop'    => [ ],       # GTI stop times
        'tstart'  => 0.0,       # start time
        'tstop'   => 0.0,       # stop time
        'tzero'   => 0.0,       # time that bin centers are relative to
        'deltat'  => 0.0,       # likely the same as timedel
        'ratunit' => 'count/s'  # unit of rate
    );
    return %emptyLC;
}

#
# extractLC -
#
sub extractLC {

    my ( $evt, $base, $odir, $pimin, $pimax, $tmin, 
         $tmax, $lcthresh, $reg, $savereg, $bin, $olc, $ospec, $oevt ) = @_;

    # create a region file
    my $regf;
    if ( $savereg ) {
        $regf = catfile( $odir, "$base.reg" );
    } else {
        $regf = getTmpFile( "reg" );
    }
    my $ret = open REG, ">$regf";
    if ( !$ret ) {
        error( 1, "failed to open region file $regf\n" );
        return -1;
    }
    chomp $reg;
    print REG $reg . "\n";
    close REG;
    
    # create a text GTI file
    my $gti = getTmpFile( "gti" );
    $ret = open GTI, ">$gti";
    if ( !$ret ) { 
        error( 1, "failed to open gti text file $gti\n" );
        return -1;
    }
    printf GTI "%1.13e %1.13e\n", $tmin, $tmax;
    close GTI;
   
    # run extractor
    # note that we try to keep the command line as small as possible
    # for fear we run into the 1000 char limit in xanlib
    my %extractor = (
        exitnow    => 'no',
        filename   => "${evt}\[PI=$pimin:$pimax\]",
        imgfile    => 'NONE',
        fitsbinlc  => $olc,
        binf       => 1,
        binlc      => $bin,
        lcthresh   => 0.0,
        lcthwarn   => 3.0,
        lctzero    => 'yes',
        regionfile => $regf,
        timefile   => $gti,
        unbinlc    => 'NONE',
        xcolf      => 'X',
        ycolf      => 'Y',
        tcol       => 'TIME',
        ecol       => 'PI',
        ccol       => 'NONE',
        gcol       => 'NONE',
        gtitxt     => 'NONE',
        xronwn     => 'NONE',
        events     => 'EVENTS',
        gti        => 'GTI',
        timeorder  => 'no',
        timeref    => 40000.0,
        eventkey   => 'NONE',
        phamax     => 'TLMAX',
        xfkey      => 'TLMAX',
        yfkey      => 'TLMAX',
        copyall    => 'yes',
        clobber    => 'yes'
    );
    
    # run once with energy filtering
    $extractor{eventsout} = defined $oevt ? $oevt : 'NONE';
    $extractor{phafile}   = 'NONE';

    my $cmd = genCmd( 'extractor', \%extractor );

    my ( $stat, $out ) = runSystem( $cmd );

    my $totcts;
    foreach my $line ( @{$out} ) {
        if ( $line =~ /Fits light curve has\s+([\d\.eE\+-]+)/ ) {
            $totcts = $1 * 1.0;
        }
    }
    
    # extract full channel spectrum 
    if ( defined $ospec ) {
        delete $extractor{binlc};
        delete $extractor{lcthresh};
        delete $extractor{lcthwarn};
        delete $extractor{lctzero};
        delete $extractor{copyall};
        $extractor{wtmapb}    = 'yes';
        $extractor{wtmapfix}  = 'yes';
        $extractor{swmapx}    = 'no';
        $extractor{swmapy}    = 'no';
        $extractor{binh}      = 1;
        $extractor{wmapver}   = 2;
        $extractor{specbin}   = 1;
        $extractor{xcolh}     = 'X';
        $extractor{ycolh}     = 'Y';
        $extractor{xhkey}     = 'TLMAX';
        $extractor{yhkey}     = 'TLMAX';
        $extractor{filename}  = $evt;
        $extractor{phafile}   = $ospec;
        $extractor{eventsout} = 'NONE';
        $extractor{fitsbinlc} = 'NONE';

        $cmd = genCmd( 'extractor', \%extractor );

        $stat = runSystem( $cmd );
    }

    unlink $gti;
    if ( !defined $savereg ) {
        unlink $regf;
    }
    return ( $stat, $totcts );
}

#
# readLC -
#
# TODO: 1. check that columns exist.
#       2. allow for different column names
#       3. check for TIMEDEL and DEADC columns
#       4. read more than 1 GTI
#       5. check if DEADC has been applied
#
sub readLC {

    my ( $fn, $lcref ) = @_;

    my ( $fptr, $nrows, $col, $any, $code, $rpt, $wdt, $com );
    my $stat = 0;

    my $deadc;
    
    my %cols = (
        'TIME'    => $lcref->{'time'},
        'RATE*'   => $lcref->{rate},    # there should be only RATE or RATE1
        'ERROR*'  => $lcref->{rateerr},
        'FRACEXP' => $lcref->{fracexp}
    );
    
    # read the columns
    $fptr = Astro::FITS::CFITSIO::open_file( $fn, READONLY, $stat );
    return $stat unless $stat == 0;
    $fptr->movnam_hdu( BINARY_TBL, "RATE", 0, $stat );
    $fptr->get_num_rows( $nrows, $stat );
    foreach my $nam ( keys %cols ) {
        $fptr->get_colnum( CASEINSEN, $nam, $col, $stat );
        $fptr->get_coltype( $col, $code, $rpt, $wdt, $stat );
        $fptr->read_col( $code, $col, 1, 1, $nrows, 0, \@{$cols{$nam}}, $any, $stat );
    }
    $fptr->read_key_dbl( 'TSTART',   $lcref->{tstart}, $com, $stat );
    $fptr->read_key_dbl( 'TSTOP',    $lcref->{tstop},  $com, $stat );
    $fptr->read_key_dbl( 'TIMEDEL',  $lcref->{deltat}, $com, $stat );
    $fptr->read_key_dbl( 'TIMEZERO', $lcref->{tzero},  $com, $stat );
    $fptr->read_key_dbl( 'DEADC',    $deadc,           $com, $stat );

    # read the GTI - assume that it is called GTI or STDGTI
    $fptr->movnam_hdu( BINARY_TBL, "GTI", 0, $stat );
    if ( $stat != 0 ) {
        $stat = 0;
        $fptr->movnam_hdu( BINARY_TBL, "STDGTI", 0, $stat );
        return $stat unless $stat == 0;
    }
    $fptr->get_num_rows( $nrows, $stat );
    $fptr->get_colnum( CASEINSEN, 'START', $col, $stat );
    $fptr->get_coltype( $col, $code, $rpt, $wdt, $stat );
    $fptr->read_col( $code, $col, 1, 1, $nrows, 0, \@{$lcref->{start}}, $any, $stat );
    $fptr->get_colnum( CASEINSEN, 'STOP', $col, $stat );
    $fptr->get_coltype( $col, $code, $rpt, $wdt, $stat );
    $fptr->read_col( $code, $col, 1, 1, $nrows, 0, \@{$lcref->{stop}}, $any, $stat );
    
    $fptr->close_file( $stat );
    return $stat unless $stat == 0;

    # apply dead time correction
    if ( $deadc > 0.0 && $deadc <= 1.0 ) {
        for ( my $i = 0; $i < @{$lcref->{'time'}}; $i++ ) {
            $lcref->{rate}[ $i ]    /= $deadc;
            $lcref->{rateerr}[ $i ] /= $deadc;
        }
    }
    return $stat;
}

#
# writeLC -
#
#       writes columns to a light-curve. a template is used
#       to preserve the observational 
#       keywords. The template file is assumed to be a lightcurve
#       generated with either extractor, or xselect. It is also assumed
#       that the time system keywords, and MJDREF keywords are correct
#
sub writeLC {
    my ( $lc, $file, $ra, $dec, $trigtime, $lctempl ) = @_;
    
    my ( $fptr, $fptr2, $hdutype, $col, $code, $rpt, $wdt, $any );
    
    my $stat = 0;

    my $nrows;
    
    # keywords for primary header and rate table
    my @primkeys = ( 
        { name => 'TELESCOP', type => TSTRING, 
          comm => 'Telescope (mission) name' }, 
        { name => 'INSTRUME', type => TSTRING, 
          comm => 'Intrument name' }, 
        { name => 'OBJECT',   type => TSTRING,
          comm => 'Name of observed object' },
        { name => 'ORIGIN',   type => TSTRING,
          comm => 'Origin of fits file' }, 
        { name => 'TSTART',   type => TDOUBLE, val => $lc->{tstart},
          comm => 'Start time' }, 
        { name => 'TSTOP',    type => TDOUBLE, val => $lc->{tstop},
          comm => 'Stop time' }, 
        { name => 'TELAPSE',  type => TDOUBLE, val => $lc->{tstop} - $lc->{tstart},
          comm => 'Elapsed time' },
        { name => 'MJDREFI',  type => TINT,
          comm => 'MJD reference day' },
        { name => 'MJDREFF',  type => TDOUBLE, 
          comm => 'MJD reference (fraction of day)' },
        { name => 'TIMEREF',  type => TSTRING, 
          comm => 'Reference time' },
        { name => 'TIMESYS',  type => TSTRING, 
          comm => 'Time measured from' },
        { name => 'TIMEUNIT', type => TSTRING, 
          comm => 'Unit of time keywords' }
    );

    # keywords for rate table only (primkeys are also written)
    my @ratekeys = (
        { name => 'TIMEZERO',  type => TDOUBLE, val => $lc->{tzero},
          comm => 'Time zero' },
        { name => 'TASSIGN',   type => TSTRING,
          comm => 'Time assigned by clock' },
        { name => 'TIERRELA',  type => TDOUBLE,
          comm => '[s/s] relative errors expressed as rate' },
        { name => 'TIERABSO',  type => TDOUBLE,
          comm => '[s] timing precision in seconds' },
        { name => 'DEADAPP',   type => TLOGICAL, val  => 1,
          comm => 'Has DEADC been applied to data' },
        { name => 'HDUCLASS',  type => TSTRING, val  => 'OGIP',
          comm => 'Format conforms to OGIP/GSFC conventions' },
        { name => 'HDUCLAS1',  type => TSTRING, val  => 'LIGHTCURVE',
          comm => 'Extension contains a light curve' },
        { name => 'TARG_ID',   type => TLONG,
          comm => 'Target ID' },
        { name => 'PROCVER',   type => TSTRING,
          comm => 'Processing script version' },
        { name => 'SOFTVER',   type => TSTRING,  
          comm => 'HEASoft/Swift software version' },
        { name => 'CALDBVER',  type => TSTRING,  
          comm => 'CALDB index versions used' },
        { name => 'RA_OBJ',    type => TDOUBLE, val  => $ra,
          comm => '[deg] R.A. Object' },
        { name => 'DEC_OBJ',   type => TDOUBLE, val  => $dec,
          comm => '[deg] Dec Object' },
        { name => 'TRIGTIME',  type => TDOUBLE, val  => $trigtime,
          comm => '[s] MET TRIGger Time for Automatic Target' },
        { name => 'PHALCUT',   type => TINT, 
          comm => 'Minimum PI channel' },
        { name => 'PHAHCUT',   type => TINT,     
          comm => 'Maximum PI channel' },
        { name => 'DATAMODE',  type => TSTRING,
          comm => 'Datamode' }
    ); 
    
    # read the template header
    $fptr = Astro::FITS::CFITSIO::open_file( $lctempl, READONLY, $stat );
    return $stat unless $stat == 0;

    my $primheader = $fptr->read_header( $stat );
    
    $fptr->movabs_hdu( 2, ANY_HDU, $stat );
    
    my $rateheader = $fptr->read_header( $stat );
    
    $fptr->close_file( $stat );
    
    # setup output columns
    my $un = $lc->{ratunit};
    my @ttype = qw( TIME XAX_E RATE ERROR FRACEXP TIMEDEL ); # UPLIMIT );
    my @tform = qw(  D    D     E    E     E       D      ); #  E      );
    my @tunit =   ( 's', 's',  $un, $un,  '',     's'     ); # $un     );
    my $nfld  = scalar( @ttype );
    
    # create the lightcurve
    $fptr = Astro::FITS::CFITSIO::create_file( $file, $stat );
    return $stat unless $stat == 0;
    
    # null primary array
    $fptr->write_grphdr( 1, 8, 0, [ 0 ], 0, 1, 1, $stat );
    
    # write primary header keywords
    foreach my $key ( @primkeys ) {

        # if we know the val, then write it
        if ( exists $key->{val} ) {
            $fptr->write_key( $key->{type}, $key->{name}, 
                              $key->{val},  $key->{comm}, $stat );
        # otherwise, default to the rate table extension
        } elsif ( exists $rateheader->{$key->{name}} ) {
            $rateheader->{$key->{name}} =~ s/[\"\']//g;
            $fptr->write_key( $key->{type}, $key->{name},
                              $rateheader->{$key->{name}}, $key->{comm}, $stat );
        } elsif ( exists $primheader->{$key->{name}} ) {
            $primheader->{$key->{name}} =~ s/[\"\']//g;
            $fptr->write_key( $key->{type}, $key->{name},
                              $primheader->{$key->{name}}, $key->{comm}, $stat );
        }
    }
    my ( $datestring, $timeref );
    Astro::FITS::CFITSIO::fits_get_system_time( $datestring, $timeref, $stat );
    $fptr->write_key_str( 'DATE', $datestring, 'File creation date', $stat );

    # add the rate table
    $nrows = scalar( @{$lc->{'time'}} );
    $fptr->insert_btbl( $nrows, $nfld, \@ttype, \@tform, \@tunit, "RATE", 0, $stat );

    # add the rate table header keywords
    foreach my $key ( @primkeys, @ratekeys ) {

        # if we know the val, then write it
        if ( exists $key->{val} ) {
            $fptr->write_key( $key->{type}, $key->{name}, 
                              $key->{val},  $key->{comm}, $stat );
        # otherwise, default to the rate table extension
        } elsif ( exists $rateheader->{$key->{name}} ) {
            $rateheader->{$key->{name}} =~ s/[\"\']//g;
            $fptr->write_key( $key->{type}, $key->{name},
                              $rateheader->{$key->{name}}, $key->{comm}, $stat );
        } elsif ( exists $primheader->{$key->{name}} ) {
            $primheader->{$key->{name}} =~ s/[\"\']//g;
            $fptr->write_key( $key->{type}, $key->{name},
                              $primheader->{$key->{name}}, $key->{comm}, $stat );
        }
    }

    # generate more columns
    my @xax_e = ( );
    foreach my $timdel ( @{$lc->{timedel}} ) {
        push @xax_e, $timdel / 2.0;
    }

    # add the columns
    $fptr->get_colnum( CASEINSEN, 'TIME', $col, $stat );
    $fptr->write_col_dbl( $col, 1, 1, $nrows, \@{$lc->{'time'}}, $stat );
    
    $fptr->get_colnum( CASEINSEN, 'RATE', $col, $stat );
    $fptr->write_colnull_flt( $col, 1, 1, $nrows, \@{$lc->{rate}}, FLT_NULL, $stat );
    
    $fptr->get_colnum( CASEINSEN, 'ERROR', $col, $stat );
    $fptr->write_colnull_flt( $col, 1, 1, $nrows, \@{$lc->{rateerr}}, FLT_NULL, $stat );
    
    $fptr->get_colnum( CASEINSEN, 'FRACEXP', $col, $stat );
    $fptr->write_col_flt( $col, 1, 1, $nrows, \@{$lc->{fracexp}}, $stat );
    
    $fptr->get_colnum( CASEINSEN, 'TIMEDEL', $col, $stat );
    $fptr->write_col_flt( $col, 1, 1, $nrows, \@{$lc->{timedel}}, $stat );
    
    $fptr->get_colnum( CASEINSEN, 'XAX_E', $col, $stat );
    $fptr->write_col_flt( $col, 1, 1, $nrows, \@xax_e, $stat );
    
    # add checksum and datasum keywords
    $fptr->write_chksum( $stat );

    # write the GTIs
    $nrows = scalar( @{$lc->{start}} );
    @ttype = ( 'START', 'STOP' );
    @tform = (     'D',    'D' );
    @tunit = (     's',    's' );
    $nfld  = scalar( @ttype );
    $fptr->insert_btbl( $nrows, $nfld, \@ttype, \@tform, \@tunit, "GTI", 0, $stat );
    $fptr->get_colnum( CASEINSEN, 'START', $col, $stat );
    $fptr->write_col_dbl( $col, 1, 1, $nrows, \@{$lc->{start}}, $stat );
    $fptr->get_colnum( CASEINSEN, 'STOP', $col, $stat );
    $fptr->write_col_dbl( $col, 1, 1, $nrows, \@{$lc->{stop}}, $stat );

    $fptr->write_key_str( 'HDUCLASS', 'OGIP', 
                          'Format conforms to OGIP/GSFC conventions', $stat );
    $fptr->write_key_str( 'HDUCLAS1', 'GTI',
                          'Extension contains Good Time Intervals', $stat );
    $fptr->write_key_str( 'HDUCLAS2', 'STANDARD',
                          'Extension contains Good Time Intervals', $stat );
    foreach my $key ( @primkeys ) {

        # if we know the val, then write it
        if ( exists $key->{val} ) {
            $fptr->write_key( $key->{type}, $key->{name}, 
                              $key->{val},  $key->{comm}, $stat );
        # otherwise, default to the rate table extension
        } elsif ( exists $rateheader->{$key->{name}} ) {
            $rateheader->{$key->{name}} =~ s/[\"\']//g;
            $fptr->write_key( $key->{type}, $key->{name},
                              $rateheader->{$key->{name}}, $key->{comm}, $stat );
        } elsif ( exists $primheader->{$key->{name}} ) {
            $primheader->{$key->{name}} =~ s/[\"\']//g;
            $fptr->write_key( $key->{type}, $key->{name},
                              $primheader->{$key->{name}}, $key->{comm}, $stat );
        }
    }

    HDpar_stamp( $fptr, 1, $stat );
    $fptr->close_file( $stat );
    return $stat;
}
   

#
# lcStats -
#
#       gets light curve stats with lcstats,
#       supporting extended filename syntax through ftcopy
#
sub lcStats {

    my $file = shift;

    my $stat = 0;
    
    my $root = "";

    my %stats = (
        average  => 0.0,
        stddev   => 0.0,
        minimum  => 0.0,
        maximum  => 0.0,
        variance => 0.0,
        expvar   => 0.0,
        thirdmom => 0.0,
        avgdev   => 0.0,
        skewness => 0.0,
        kurtosis => 0.0,
        chisq    => 0.0,
        chiprob  => 0.0,
        kolmsmir => 0.0,
    );
    my %patts = (
        average  => qr/Average\ \(c\/s\)\ \.+\s*(\S+)/,
        stddev   => qr/Standard\ Deviation\ \(c\/s\)\.+\s*(\S+)/,
        minimum  => qr/Minimum\ \(c\/s\)\ \.+\s*(\S+)/,
        maximum  => qr/Maximum\ \(c\/s\)\.+\s*(\S+)/,
        variance => qr/Variance\ \(\(c\/s\)\*\*2\)\.+\s*(\S+)/,
        expvar   => qr/Expected\ Variance\ \(\(c\/s\)\*\*2\)\.+\s*(\S+)/,
        thirdmom => qr/Third\ Moment\ \(\(c\/s\)\*\*3\)\.+\s*(\S+)/,
        avgdev   => qr/Average\ Deviation\ \(c\/s\)\.+\s*(\S+)/,
        skewness => qr/Skewness\.+\s*(\S+)/,
        kurtosis => qr/Kurtosis\.+\s*(\S+)/,
        chisq    => qr/Chi-Square\.+\s*(\S+)/,
        chiprob  => qr/Chi-Square\ Prob\ of\ constancy\.+\s*(\S+)/,
        kolmsmir => qr/Kolm\.-Smir\.\ Prob\ of\ constancy\s*(\S+)/,
    );
    
    Astro::FITS::CFITSIO::fits_parse_rootname( $file, $root, $stat );
    return ( $stat, %stats ) unless $stat == 0;

    my $tmpfits = undef;
    if ( $root ne $file ) {
        $tmpfits = getTmpFile( "fits" );
        my %ftcopy = ( 
            infile      => $file,
            outfile     => $tmpfits,
            copyall     => 'yes',
            clobber     => 'no',
            chatter     => 4,
            history     => 'no'
        );
        my $cmd = genCmd( 'ftcopy', \%ftcopy );
        $stat = runSystem( $cmd );
        return ( $stat, %stats ) unless $stat == 0;

        $file = $tmpfits;
    }

    my %lcstats = (
        cfile1   => $file,
        window   => '-',
        dtnb     => 'INDEF',
        nbint    => 'INDEF',
        itre     => 0,
        tchat    => 10,
        lchat    => 0
    );
    my $cmd = genCmd( 'lcstats', \%lcstats );
    my $out;
    
    ( $stat, $out ) = runSystem( $cmd );
    unlink $tmpfits if defined $tmpfits;
    return ( $stat, %stats ) unless $stat == 0;

    foreach my $line ( @$out ) {
        foreach my $key ( sort keys %patts ) {
            if ( $line =~ $patts{$key} ) {
                $stats{$key} = $1 * 1.0;
                delete $patts{$key};
                last;
            }
        }
    }

    return ( $stat, %stats );
}

#
# readXImageDetectFile -
#
#       reads a detect file generated by XImage. the first arg is the
#       detect file, and the second is an array ref. into this array (ref)
#       is put a hash at each array position. a single array element will
#       look like (see XImage documentation for column descriptions):
#
#               $outRef->[0]{SRCRATE} = 2.0
#               $outRef->[0]{SRCRATE_ERR} = 1.414
#               etc.
#
#       each array element represents a single detected source. a status
#       variable is returned indicating the fitsio error code if any.
#
sub readXImageDetectFile {

    my ( $detFile, $outarr ) = @_;

    # output from XImage detect is fixed
    my @cols = qw( SRCRATE SRCRATE_ERR X Y VIGNET RA DEC ERRRAD HBOXSIZE PROB SNR );
    
    my ( $stat, $fptr, $nrows, $code, $rpt, $wid, $anynull, $cn );
    $stat = $anynull = 0;

    @{$outarr} = ( );
    chat( 3, "reading XImage detect file $detFile\n" );

    $fptr = Astro::FITS::CFITSIO::open_file( $detFile, READONLY, $stat );
    return $stat unless $stat == 0;
    $fptr->movnam_hdu( BINARY_TBL, 'SRCLIST', 0, $stat );
    $fptr->get_num_rows( $nrows, $stat );

    # get the column numbers and types and sort them
    my $colnums = ( );
    my @coltyps = ( );
    foreach my $col ( @cols ) {
        $fptr->get_colnum( CASEINSEN, $col, $cn, $stat );
        $fptr->get_coltype( $cn, $code, $rpt, $wid, $stat );
        push @colnums, $cn;
        push @coltyps, $code;
    }
    my @order = sort { $colnums[ $a ] <=> $colnums[ $b ] } 0..$#colnums;
    @colnums = @colnums[ @order ];
    @coltyps = @coltyps[ @order ];
    @cols    = @cols[ @order ];
    
    for ( my $i = 1; $i <= $nrows; $i++ ) {
        my $row = { };
        for ( my $j = 0; $j < @cols; $j++ ) {
            my @outval = ( );
            $fptr->read_col( $coltyps[ $j ], $colnums[ $j ], $i, 
                             1, 1, 0, \@outval, $anynull, $stat );
            $row->{$cols[ $j ]} = $outval[ 0 ];
        }
        push @{$outarr}, $row;
    }
    $fptr->close_file( $stat );
    @{$outarr} = ( ) unless $stat == 0;
    return $stat;
}

#
# getListParam - 
#
#       gets string parameter and converts to a list
#       if the string parameter starts with '@' by reading 
#       the file whose name follows the '@'. Or if the gotten
#       parameter is a comma separated list, creates a list
#       containing all the values
#
#       e.g., if the parameter gotten is '@files.txt', and the 
#       contents of files.txt is:
#
#               file1.fits
#               file2.fits
#
#       then the parameter will be set to [ file1.fits, file2.fits ]
#
#       Inputs:
#               - parameter name
#               - return parameter reference
#
#       Outputs:
#               - the return parameter is modified, will be a list
#               - a standard status var is returned
#
sub getListParam {
    
    my $name   = shift;
    my $parref = shift;
    my $status = 0;
    my $tmp;
    
    debug( "getting string parameter $name\n" );
    $status = PILGetString( $name, $tmp );
    if ( $status != 0 ) {
        error( 1, "error getting string parameter $name\n" );
        return $status;
    }
    my @tmparr = ( );
    $status = readAtParamList( $tmp, \@tmparr );
    if ( $status != 0 ) {
        error( 1, "failed to read list parameter $name\n" );
    } else {
        @{$parref} = @tmparr;
    }
    return $status;
}

#
# getStrParam -
#
#       gets string parameter
#
#       Inputs:
#               - parameter name
#               - return parameter reference
#
#       Outputs:
#               - the return paramater is modified
#               - a standard status var is returned
#
sub getStrParam {
    my $name   = shift;
    my $parref = shift;
    my $status = 0;
    debug( "getting string parameter $name\n" );
    $status = PILGetString( $name, $$parref );
    if ( $status != 0 ) {
        error( 1, "error getting string parameter $name\n" );
    }
    debug( "parameter $name is: $$parref\n" );
    return $status;
}

#
# getFltParam -
#
#       gets real parameter
#
#       Inputs:
#               - parameter name
#               - return parameter reference
#
#       Outputs:
#               - the return paramater is modified
#               - a standard status var is returned
#
sub getFltParam {
    my $name   = shift;
    my $parref = shift;
    my $status = 0;
    debug( "getting real parameter $name\n" );
    $status = PILGetReal( $name, $$parref );
    if ( $status != 0 ) {
        error( 1, "error getting real parameter $name\n" );
    }
    debug( "parameter $name is: $$parref\n" );
    return $status;
}

#
# getIntParam -
#
#       gets integer parameter
#
#       Inputs:
#               - parameter name
#               - return parameter reference
#
#       Outputs:
#               - the return paramater is modified
#               - a standard status var is returned
#
sub getIntParam {
    my $name   = shift;
    my $parref = shift;
    my $status = 0;
    debug( "getting int parameter $name\n" );
    $status = PILGetInt( $name, $$parref );
    if ( $status != 0 ) {
        error( 1, "error getting int parameter $name\n" );
    }
    debug( "parameter $name is: $$parref\n" );
    return $status;
}

#
# getBoolParam -
#
#       gets bool parameter
#
#       Inputs:
#               - parameter name
#               - return parameter reference
#
#       Outputs:
#               - the return paramater is modified
#               - a standard status var is returned
#
sub getBoolParam {
    my $name   = shift;
    my $parref = shift;
    my $status = 0;
    debug( "getting bool parameter $name\n" );
    $status = PILGetBool( $name, $$parref );
    if ( $status != 0 ) {
         error( 1, "error getting bool parameter $name\n" );
    }
    debug( "parameter $name is: $$parref\n" );
    return $status;
}

#
# readAtParamList -
#
#       reads an @-file if the first character of the input is '@',
#
#       Inputs:
#               - string input parameter
#               - reference to the output array
#
#       Outputs:
#               - clears and fills output array
#               - returns status variable
#
sub readAtParamList {

    my $inparam = shift;
    my $outarr  = shift;
    my $status  = 0;
    
    # support the @file construct
    if ( $inparam =~ /^\@/ ) {
        my $fn = $inparam;
        $fn =~ s/^\@//;
        if ( -f $fn ) {
            debug( "reading list parameter from file $fn\n" );
            open IF, "<$fn";
            if ( $@ ) {
                error( 1, "failed to open file $fn\n" );
                $status = PIL_FILE_NO_RD;
                return $status;
            }
            # clear the output array
            @{$outarr} = ( );
            while ( <IF> ) {
                chomp;
                s/^#.*//;
                s/^\s+//;
                s/\s+$//;
                next unless length;
                push @{$outarr}, $_;
                debug( "read $_ from $fn\n" );
            }
            close IF;
        } else {
            error( 1, "$fn is not a file\n" );
            $status = PIL_NO_FILE;
        }
    } else {
        @{$outarr} = ( );
        push @{$outarr}, $inparam;
    }
    return $status;
}

#
# parseFitsList -
#
#       parses a list of fits files, separated by commas - handles extended 
#       notation (e.g. junk.fits[EVENTS]). adapted from cfitsio
#
sub parseFitsList {
    my $inparam = shift;
   
    my @outarr = ( );

    my @splitstring = split //, $inparam;
    my $depth = 0;
    my $start = 0;
    my $end   = 0;
    foreach my $char ( @splitstring ) {
        if ( $char eq '[' or $char eq '(' or $char eq '{' ) {
            $depth++;
        } elsif ( $char eq '}' or $char eq ')' or $char eq ']' ) {
            $depth--;
        } elsif ( $depth == 0 and $char eq ',' or $char eq ' ' ) {
            my $fn = join '', @splitstring[ $start..( $end - 1 ) ];
            $fn =~ s/^\s+//;
            if ( length $fn ) {
                push @outarr, $fn;
                $start = $end + 1;
                debug( "read $fn from input parameter\n" );
            }
        }
        $end++;
    }
    if ( $start != $end ) {
        my $fn = join '', @splitstring[ $start..( $end - 1 ) ];
        $fn =~ s/^\s+//;
        if ( length $fn ) {
            push @outarr, $fn;
            $start = $end + 1;
            debug( "read $fn from input parameter\n" );
        }
    }
    return @outarr;
}

#
# convertRAStringToDegrees - 
#
#       converts RA in HH:MM:SS.SSSS, HH MM SS.SSSS, HHhMMmSS.SSSSs 
#       or DDD.DDDD to degrees
#
#       Inputs:
#               - input RA string
#
#       Outputs:
#               - output scalar RA in degrees
#               - status variable
#
sub convertRAStringToDegrees {
    
    my $inRa   = shift;
    my $ra     = -1;
    my $status = 0;
    if ( $inRa =~ /^(\d{2})([: h]|h\s+)(\d{2})([: m]|m\s+)(\d{2}(\.\d*)?)s?$/ ) {
        my $h = $1 * 3600.;
        my $m = $3 * 60.;
        my $s = $5 * 1.;
        $ra   = ( $h + $m + $s ) / (60. * 4.);
        debug( "converted $inRa to $ra degrees\n" );
    } elsif ( $inRa =~ /^(\d+\.?\d*|\.\d+|\d+\.)$/ ) {
        $ra = $inRa * 1.0;
    } else {
        error( 1, "invalid RA string in convertRAStringToDegrees\n" );
        error( 1, "Use HH:MM:SS.SS, HHhMMmSS.SSs, HH MM SS.SS or DDD.DD\n" );
        $status = -1;
    }
    return ( $status, $ra );
}

#
# convertDecStringToDegrees - 
#
#       converts Dec in [+-]DD:MM:SS.SSSS, [+-]DD MM SS.SSSS, [+-]DDdMMmSS.SSSSs
#       or DDD.DDDD to degrees
#
#       Inputs:
#               - input Dec string
#
#       Outputs:
#               - output scalar Dec in degrees
#               - status variable
#
sub convertDecStringToDegrees {

    my $inDec  = shift;
    my $dec    = -1;
    my $status = 0;
    if ( $inDec =~ /^([\+-]?\d{2})([: d]|d\s+)(\d{2})([: m]|m\s+)(\d{2}(\.\d*)?)s?$/ ) {
        my $d    = $1;
        my $m    = $3 / 60.;
        my $s    = $5 / 3600.;
        my $sign = ( $d < 0 || $d =~ /-00/ ) ? -1. : 1.;
        $dec     = $sign * ( $d * $sign + $m + $s ); 
        debug( "converted $inDec to $dec degrees\n" );
    } elsif ( $inDec =~ /^[\+-]?(\d+\.?\d*)|(\.\d+)|(\d+\.)$/ ) {
        $dec = $inDec * 1.0;
    } else {
        error( 1, "invalid Dec string in convertDecStringToDegrees\n" );
        error( 1, "Use DD:MM:SS.SS, DD MM SS.SS, DDdMMmSS.SSs or DDD.DD\n" );
        $status = -1;
    }
    return ( $status, $dec );
}

#
# swifttime -
#
#       runs the tool swifttime, and converts output (if $outform='t')
#       to the DATE-OBS format YYYY-MM-DDTHH:MM:SS.SS
#
sub swifttime {
    my ( $intime, $insys, $inform, $outsys, $outform ) = @_;

    my $doconv = undef;
    if ( lc( $outform ) eq 't'  ) {
        $doconv  = 1;
        $outform = 'c';
    }

    # run swifttime
    my $swifttime = {
        intime    => $intime,
        insystem  => $insys,
        informat  => $inform,
        outsystem => $outsys,
        outformat => $outform,
        swcofile  => 'CALDB',
        allowcorr => 'yes',
        chatter   => 3
    };
    my ( $stat, $out ) = runSystem( genCmd( 'swifttime', $swifttime ) );

    # error
    if ( $stat != 0 ) {
        return FLT_NULL;
    }
    
    # parse output
    my $outtime;
    foreach my $line ( @$out ) {
        chomp $line;
        if ( $line =~ /^Converted time:\s+(.+?)$/ ) {
            $outtime = "$1";
            last;
        }
    }

    # another error condition
    if ( !defined $outtime ) {
        return FLT_NULL;
    }
    
    # convert to 
    if ( $doconv ) {
        if ( $outtime =~ /^(\d{4})(\w{3})(\d{2})\s+at\s+(\d{2}:\d{2}:\d{2})/ ) {
            my ( $yr, $mo, $da, $time ) = ( $1, $2, $3, $4 );

            # convert month
            if    ( $mo =~ /^jan$/i ) { $mo = "01"; }
            elsif ( $mo =~ /^feb$/i ) { $mo = "02"; }
            elsif ( $mo =~ /^mar$/i ) { $mo = "03"; }
            elsif ( $mo =~ /^apr$/i ) { $mo = "04"; }
            elsif ( $mo =~ /^may$/i ) { $mo = "05"; }
            elsif ( $mo =~ /^jun$/i ) { $mo = "06"; }
            elsif ( $mo =~ /^jul$/i ) { $mo = "07"; }
            elsif ( $mo =~ /^aug$/i ) { $mo = "08"; }
            elsif ( $mo =~ /^sep$/i ) { $mo = "09"; }
            elsif ( $mo =~ /^oct$/i ) { $mo = "10"; }
            elsif ( $mo =~ /^nov$/i ) { $mo = "11"; }
            elsif ( $mo =~ /^dec$/i ) { $mo = "12"; }
            else {
                return FLT_NULL;
            }
            return "${yr}-${mo}-${da}T${time}";
        } else {
            return FLT_NULL;
        }
    } else {
        return $outtime;
    }
}

#
# dumpListToTxt -
#
sub dumpListToTxt {
   
    my @list = @_;
    my $tmpfile = getTmpFile( "txt" );
    my $ret = open TMP, ">$tmpfile";
    if ( !$ret ) {
        return "";
    }
    foreach my $line ( @list ) {
        chomp $line;
        print TMP "$line\n";
    }
    close TMP;
    return $tmpfile;
}

1;
