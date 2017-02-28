
=pod

=head1 NAME

HEAGen.pm

=head1 SYNOPSIS

use HEAGen::HEAGen qw( :all );

=head1 DESCRIPTION

This package provides many subroutines for creating HEASoft perl tasks,
and running HEASoft commands from them. It is based in part on xanlib,
"utils.pl", the heautils C library among others (Thanks to all those that
developed those libs).

=cut

package HEAGen::HEAGen;

use strict;

################
# package setup
################
use Exporter;
our $VERSION = "1.0";
our @ISA = qw( Exporter );

# export nothing by default
our @EXPORT = ( );

####################
# setup export tags
####################

=pod

=head1 EXPORT TAGS

=head3 :all

=over

Includes all tags (excluding :all)

=back

=head3 :task

=over

L<intro|"intro">, L<outtro|"outtro">, L<cleanup|"cleanup">,
L<getPlotDevice|"getPlotDevice">, L<chat|"chat">, L<chatnp|"chatnp">,
L<heading|"heading">, L<headingnp|"headingnp">, L<error|"error">,
L<warnhi|"warnhi">, L<warnlo|"warnlo">, L<debug|"debug">,
L<addOutFH|"addOutFH">, L<histStamp|"histStamp">, L<setTask|"setTask">,
L<setChat|"setChat">, L<setPrefix|"setPrefix">, L<setSysChat|"setSysChat">,
L<genCmd|"genCmd">, L<runSystem|"runSystem">,
L<runSystemNoChat|"runSystemNoChat">

=back

=head3 :par

=over

L<getListParam|"getListParam">, L<getStrParam|"getStrParam">,
L<getBoolParam|"getBoolParam">, L<getFileParam|"getFileParam">,
L<getIntParam|"getIntParam">, L<getFltParam|"getFltParam">,
L<parseFitsList|"parseFitsList">, L<bs|"bs">

=back

=head3 :coord

=over

L<convertRAStringToDegrees|"convertRAStringToDegrees">,
L<convertDecStringToDegrees|"convertDecStringToDegrees">,
L<rolldiff|"rolldiff">, L<angdist|"angdist"> L<xy2sky|"xy2sky">,
L<sky2xy|"sky2xy">, L<pointxform|"pointxform">

=back

=head3 :tmp

=over

L<getTmpFile|"getTmpFile">, L<dumpListToTxt|"dumpListToTxt">

=back

=head3 :tool

=over

L<ftcopy|"ftcopy">, L<ftpaste|"ftpaste">, L<ftappend|"ftappend">,
L<maketime|"maketime">, L<mgtime|"mgtime">, L<nh|"nh">

=back

=head3 :extractor

=over

L<setupExtractor|"setupExtractor">, L<extractor|"extractor">,
L<xselmdbval|"xselmdbval">

=back

=head3 :lightcurve

=over

L<lcmath|"lcmath">, L<lcadd|"lcadd">, L<lcsub|"lcsub">, L<lcstats|"lcstats">,
L<lcstats2|"lcstats2">, L<lnGamma|"lnGamma">, L<gammaP|"gammaP">
L<gammaQ|"gammaQ">, L<gammaPSeries|"gammaPSeries">, L<gammaQFrac|"gammaQFrac">

=back

=head3 :spec

=over

L<grppha|"grppha">, L<addarf|"addarf">, L<addrmf|"addrmf">, L<marfrmf|"marfrmf">

=back

=head3 :gti

=over

L<readGTI|"readGTI">, L<findGTIExt|"findGTIExt">, L<gtiOR|"gtiOR">,
L<gtiAND|"gtiAND">, L<gtiSUM|"gtiSUM">

=back

=cut

my $taskTags  = [ qw( intro outtro cleanup getPlotDevice
                      chat chatnp heading headingnp error warnhi warnlo debug addOutFH
                      histStamp setTask setChat setPrefix setSysChat
                      genCmd runSystem runSystemNoChat ) ];

my $parTags   = [ qw( getListParam getStrParam getBoolParam getFileParam
                      getIntParam getFltParam parseFitsList bs ) ];

my $coordTags = [ qw( convertRAStringToDegrees convertDecStringToDegrees
                      rolldiff angdist xy2sky sky2xy pointxform ) ];

my $tmpTags   = [ qw( getTmpFile dumpListToTxt ) ];

my $toolTags  = [ qw( ftcopy ftpaste ftappend maketime mgtime nh ) ];

my $extTags   = [ qw( setupExtractor extractor xselmdbval ) ];

my $curveTags  = [ qw( lcmath lcadd lcsub lcstats lcstats2
                       lnGamma gammaP gammaQ gammaPSeries gammaQFrac ) ];

my $specTags  = [ qw( grppha addarf addrmf marfrmf ) ];

my $gtiTags   = [ qw( readGTI findGTIExt ) ];

our %EXPORT_TAGS = (
                     task       => $taskTags,
                     par        => $parTags,
                     coord      => $coordTags,
                     tmp        => $tmpTags,
                     tool       => $toolTags,
                     extractor  => $extTags,
                     spec       => $specTags,
                     lightcurve => $curveTags,
                     gti        => $gtiTags
                   );

#######################################################
# add all the other ":class" tags to the ":all" class,
# deleting duplicates
#######################################################
{
    my %seen = ( );
    push @{$EXPORT_TAGS{all}},
        grep {!$seen{$_}++} @{$EXPORT_TAGS{$_}} foreach keys %EXPORT_TAGS;
}

##############################################
# make sure we are OK with exporting them all
##############################################
Exporter::export_ok_tags( keys %EXPORT_TAGS );

##############################################
# libs we need (it's a lot of code to import)
##############################################
use HEACORE::HEAUTILS;
use HEACORE::PIL;
use Astro::FITS::CFITSIO qw( :longnames :constants );

use IPC::Open3;
use Symbol qw( gensym );
use IO::File;
use Math::Trig;
use Env;
use Cwd;
use File::Spec::Functions qw( :ALL );
use File::Basename;

############################
# some useful PIL constants
############################
use constant PIL_NO_FILE        => -3003;
use constant PIL_FILE_NO_RD     => -3014;

####################
# package variables
####################
our $chatPrefix = "";
our $chatLevel  = 0;
our $sysChatLevel = 3;
our $taskName = "unknown task";
our $taskVers = "??";
our $extraOutFH = undef;

# xselect mission database (one instrument only)
our %MDB = ( );

=head1 SUBROUTINES

=cut

=head2 :task - Task subroutines

=cut

=head3 setTask

=over

Sets task name and version and prefix. The task name and version are used in
L<intro|"intro"> and L<outtro|"outtro">, and the combination C<<
${task_name}_${task_version} >> will be prefixed to L<chat|"chat">'d
output.

Inputs:

    - task name
    - version string

Outputs: none

=back

=cut

sub setTask( $$ ) {
    $taskName = shift;
    $taskVers = shift;
    setPrefix( "${taskName}_${taskVers}: " );
}

=head3 setChat

=over

Sets chatter level. Calls to L<chat|"chat"> with the level set equal to or
below the level set with L<setChat|"setChat"> will be printed to the
terminal (and optional additional filehandle - see L<addOutFH|"addOutFH">);

Inputs

    - chatter level

Outputs: none

=back

=cut

sub setChat ( $ ) {
    $chatLevel = shift;
}

=head3 setSysChat

=over

Sets chatter level for L<runSystem|"runSystem">. Output from command
executed by L<runSystem|"runSystem"> will only be printed if the chatter
level set by L<setChat|"setChat"> is greater than or equal to the chatter
set by L<setSysChat|"setSysChat">.

Inputs:

    - runSystem chatter level

Outputs: none

=back

=cut

sub setSysChat ( $ ) {
    $sysChatLevel = shift;
}

=head3 setPrefix

=over

Sets message that is prefixed to most output lines
(L<runSystem|"runSystem"> output excluded).

Inputs:

    - string to prefix chat'd lines with

Outputs: none

=back

=cut

sub setPrefix ( $ ) {
    $chatPrefix = shift;
}

=head3 intro

=over

Prints a nice intro message with task name, version and the date.
L<setTask|"setTask"> should have already be run.

Inputs: none

Outputs: none

=back

=cut

sub intro {
    my $time  = localtime( time );
    my $intro = "Starting $taskName v$taskVers at $time";
    headingnp( 1, $intro );
}

=head3 outtro

=over

Prints outtro message with success/failure. Success/failure is determined by
single argument (S< ==0 => success >, S< !=0 => failure >).

Inputs:

    - status variable

Outputs: none

=back

=cut

sub outtro( $ ) {
    my $stat = shift;
    my $time  = localtime( time );
    my $success = $stat == 0 ? "success" : "failure";
    my $outtro = "$taskName v$taskVers $success at $time";
    headingnp( 1, $outtro );
}

=head3 cleanup

=over

Removes unneeded files, in reverse order of creation.

Inputs:

    - list of files/directories to delete

Outputs: none

=back

=cut

sub cleanup {

    foreach my $cf ( reverse @_ ) {
        if ( -f $cf ) {
            debug( "cleaning un-needed file $cf\n" );
            unlink $cf;
        } elsif ( -d $cf ) {
            debug( "cleaning un-needed dir $cf\n" );
            rmdir $cf;
        }
    }
}

=head3 histStamp

=over

Runs HDpar_stamp on all HDUs of each file in the input list of files, skips
non-fits.

Inputs:

    - list of FITS files to update history keywords in

Outputs: none

=back

=cut

sub histStamp {

    my $stat = 0;
    foreach my $hf ( @_ ) {
        my $exists = 0;
        fits_file_exists( $hf, $exists, $stat );
        next unless( $stat == 0 && $exists > 0 );

        my $fptr = Astro::FITS::CFITSIO::open_file( $hf, READWRITE, $stat );
        my $nhdus;
        $fptr->get_num_hdus( $nhdus, $stat );
        foreach my $hdu ( 1..$nhdus ) {
            HDpar_stamp( $fptr, $hdu, $stat );
        }
        $fptr->close_file( $stat );
        return $stat unless $stat == 0;
    }
    return $stat;
}

=head3 addOutFH

=over

Adds an additional filehandle to write messages to. Single argument should be a
filehandle, or reference to a typeglob. E.g.:

    open LOG, ">$somefile" or die "open failed\n";
    addOutFH( \*LOG );

Inputs:

    - filehandle or reference to typeglob

Outputs: none

=back

=cut

sub addOutFH {
    $extraOutFH = shift;
}

=head3 chat

=over

Basically a printf($...). The first argument indicates the chatter level at
which the formatted message is to be printed (see L<setChat|"setChat"> and
L<setSysChat|"setSysChat">).  The remaining arguments are the format
string, and the filler arguments (same as printf). Also, each line (as
determined by splitting on '\n') will be pre-pended with the prefix set by
either L<setTask|"setTask"> or L<setPrefix|"setPrefix">, and the string
"Info: ".

For example, the following code:

    setTask( "mytask", "2.0" );
    setChat( 2 );
    chat( 1, "This is a %s\n", "test" );

will produce the following output:

    mytask_2.0: Info: This is a test

If an additional filehandle for output has been specified (see
L<addOutFH|"addOutFH">), the message will be printed both to STDOUT and the
extra filehandle.

Inputs:

    - chatter level
    - format string and arguments

Outputs: none

=back

=cut

sub chat {
    my $lev = shift;
    dochat( $lev, 'Info', \*STDOUT, \@_ );
}

=head3 chatnp

=over

Same as L<chat|"chat"> but task name/version not prepended.

Inputs:

    - chatter level
    - format string and arguments

Outputs: none

=back

=cut

sub chatnp {
    my $lev = shift;
    dochat( -$lev, '', \*STDOUT, \@_ );
}

=head3 heading

=over

Takes a chatter level, and a string argument (with no newlines) and
L<chat|"chat">'s the output surrounded by '---------------'

Inputs:

    - chatter level
    - heading string

Outputs: none

=back

=cut

sub heading( $$ ) {
    my $lev = shift;
    if ( $lev > $chatLevel ) {
        return;
    }
    my $str = shift;
    chomp $str;
    $str = "- $str -";
    while ( length( $str ) + length( $chatPrefix ) < 77 ) {
        $str = "-$str-";
    }
    my $xs = "-" x length( $str );
    $str = "$xs\n$str\n$xs\n";
    dochat( -$lev, '', \*STDOUT, [ "\n" ] );
    dochat( $lev, 'Info', \*STDOUT, [ $str ] );
}

=head3 headingnp

=over

Same as L<heading|"heading">, but ntask name/version not prepended

Inputs:

    - chatter level
    - heading string

Outputs: none

=back

=cut

sub headingnp( $$ ) {
    my $lev = shift;
    if ( $lev > $chatLevel ) {
        return;
    }
    my $str = shift;
    chomp $str;
    $str = "- $str -";
    while ( length( $str ) < 77 ) {
        $str = "-$str-";
    }
    my $xs = "-" x length( $str );
    $str = "$xs\n$str\n$xs\n";
    dochat( -$lev, '', \*STDOUT, [ "\n" ] );
    dochat( -$lev, '', \*STDOUT, [ $str ] );
}

=head3 error

=over

Same as L<chat|"chat">, but 'Error: ' instead of 'Info: ' is prepended to
each line, and the output is sent to STDERR (and any additional filehandle set
with L<addOutFH|"addOutFH">).

Inputs:

    - chatter level
    - format string and arguments

Outputs: none

=back

=cut

sub error {
    my $lev = shift;
    dochat( $lev, 'Error', \*STDERR, \@_ );
}

=head3 warnlo

=over

Same as L<chat|"chat">, but 'Warning: ' instead of 'Info: ' is prepended to
each line.  The output is sent to STDOUT (and any additional filehandle set
with L<addOutFH|"addOutFH">).

Inputs:

    - chatter level
    - format string and arguments

Outputs: none

=back

=cut

sub warnlo {
    my $lev = shift;
    dochat( $lev, 'Warning', \*STDOUT, \@_ );
}

=head3 warnhi

=over

Same as L<error|"error">, but 'WARNING: ' instead of 'Error: ' is prepended
to each line.

Inputs:

    - chatter level
    - format string and arguments

Outputs: none

=back

=cut

sub warnhi {
    my $lev = shift;
    dochat( $lev, 'WARNING', \*STDERR, \@_ );
}

=head3 debug

=over

Same as L<chat|"chat">, but 'Debug: ' instead of 'Info: ' is prepended to
each line, and no chatter level is specified (assumed to be 5).

Inputs:

    - format string and arguments

Outputs: none

=back

=cut

sub debug {
    dochat( 5, 'Debug', \*STDOUT, \@_ );
}

=head3 dochat (NOT EXPORTED)

=over

Does the parsing and printing for L<chat|"chat">, L<warnhi|"warnhi">,
L<warnlo|"warnlo">, L<error|"error">, L<debug|"debug">.

If C<< $lev < 0 >>, task name/version are not prepended, and the chatter level
is C<< abs( $lev ) >>.

=back

=cut

sub dochat {
    my ( $lev, $typeStr, $fh, $msgs ) = @_;

    #########################################
    # do nothing if level is not high enough
    #########################################
    if ( abs( $lev ) > $chatLevel ) {
        return;
    }

    ##################################################################
    # replace \n with \n${chatPrefix}$typeStr: except for the last \n
    # in the format string (first element in @$msgs)
    ##################################################################
    my $format = "";
    if ( $lev < 0 ) {
        $format = $msgs->[ 0 ];
        $lev = abs( $lev );
    } else {
        if ( $msgs->[ 0 ] eq "\n" ) {
            $format .= "${chatPrefix}$typeStr:\n";
        } else {
            foreach my $substr ( split /\n/, $msgs->[ 0 ] ) {
                if ( substr( $substr, -1, 1 ) eq "\r" ) {
                    $format .= "${chatPrefix}$typeStr: $substr";
                } else {
                    $format .= "${chatPrefix}$typeStr: $substr\n";
                }
            }
        }
    }

    ########################################################################
    # if we don't have any extra arguments and just print instead of printf
    ########################################################################
    if ( @$msgs < 2 ) {
        print $fh $format;
        if ( defined $extraOutFH ) {
            print $extraOutFH $format;
        }
        return;
    }

    #################################################
    # put the rest into the argument list for printf
    #################################################
    my @args = ( );
    for ( my $i = 1; $i < @$msgs; $i++ ) {
        push @args, $msgs->[ $i ];
    }

    #####################################
    # printf to the specified filehandle
    #####################################
    printf $fh $format, @args;
    if ( defined $extraOutFH ) {
        printf $extraOutFH $format, @args;
    }
}

=head3 genCmd

=over

Generates a shell command. first arg is taskname, second is hash reference,
where keys are parameter names, and values are parameter values.

Inputs:

    - executable name
    - HASH reference of parameter names/values

Outputs:

    - returns full command line

=back

=cut

sub genCmd( $$ ) {

    my $toolname = shift;
    my $params   = shift;

    my $cmd = $toolname;
    foreach my $param ( sort keys %{$params} ) {
        $cmd .= " $param=\"$params->{$param}\"";
    }
    return $cmd;
}

=head3 runSystem

=over

Runs a system command with output logging, and error checking.  Adapted from
Xan.pm (from DTS), similar to runcom from "utils.pl".

There are two ways to use this routine. The first argument is always the
command name to run (but can include arguments as well). The second argument
can be:

    1. A hash reference => genCmd is called to build a command line.

    2. The first in a string of arguments, joined to the first.

If the caller wants a list to be returned (C<< wantarray == true >>),
L<runSystem|"runSystem"> will return the return status from running the
command, and a list reference to the lines of output that the command produced,
and a list reference to the lines of STDERR output that the command produced.

If the caller does not want a list, just the return status from running the
command is returned.

Any output produced by running the command will also be printed.

For example (case 1. above), the following:

    runSystem( 'ftcopy', { infile => file1, outfile => file2, copyall => 'yes' } );

would run the command:

    ftcopy infile="file1" outfile="file2" copyall="yes"

Another example (case 2. above):

    runSystem( "cp", "file1", "file2" );

would run the command:

    cp file1 file2

=back

=cut

sub runSystem {

    #########################################################
    # if we are given a scalar and a hash reference,
    # assume we want to generate a command from the two
    # otherwise just run the command with whatever arguments
    # we were given
    #########################################################
    my $base = $_[ 0 ];
    my $cmd;
    if ( defined $_[ 1 ] && ref $_[ 1 ] eq 'HASH' ) {
        $cmd = genCmd( $_[ 0 ], $_[ 1 ] );
    } else {
        $cmd = join " ", @_;
    }

    ###################################
    # does the caller want the output?
    ###################################
    my $wantarray = wantarray;

    ##################
    # should we chat?
    ##################
    my $dochat = $chatLevel >= $sysChatLevel;

    ###############
    # do the stuff
    ###############
    chat( $sysChatLevel, "running: $cmd\n" );
    my ( $rc, $out, $err ) = doSystem( $cmd, $wantarray, $dochat );
    $rc = checkSystemRC( $rc, $base, $cmd );

    if ( $wantarray ) {
        return ( $rc, $out, $err );
    } else {
        return $rc;
    }
}

=head3 runSystemNoChat

=over

Same as L<runSystem|"runSystem">, but nothing is printed to the terminal.

=back

=cut

sub runSystemNoChat {

    #########################################################
    # if we are given a scalar and a hash reference,
    # assume we want to generate a command from the two
    # otherwise just run the command with whatever arguments
    # we were given
    #########################################################
    my $base = $_[ 0 ];
    my $cmd;
    if ( defined $_[ 1 ] && ref $_[ 1 ] eq 'HASH' ) {
        $cmd = genCmd( $_[ 0 ], $_[ 1 ] );
    } else {
        $cmd = join " ", @_;
    }

    ###################################
    # does the caller want the output?
    ###################################
    my $wantarray = wantarray;

    ###############
    # do the stuff
    ###############
    my ( $rc, $out, $err ) = doSystem( $cmd, $wantarray, 0 );

    $rc = checkSystemRC( $rc, $base, $cmd );

    if ( $wantarray ) {
        return ( $rc, $out, $err );
    } else {
        return $rc;
    }
}

=head3 doSystem (NOT EXPORTED)

=over

Actually runs a command. does this using IPC::open3, capturing stdout and
stderr and the return status.

=back

=cut

sub doSystem( $$$ ) {

    my ( $cmd, $wantarray, $dochat ) = @_;

    my @out = ( );
    my @err = ( );

    ##############################################################
    # use IPC::open3 to run the command, without spawning a shell
    # and with output capture from stdout and stderr
    ##############################################################
    local *CATCHERR = IO::File->new_tmpfile;
    my $pid = open3(gensym, \*CATCHOUT, ">&CATCHERR", $cmd);
    while( <CATCHOUT> ) {
        if ( $wantarray ) {
            push @out, $_;
        }
        print $_ if $dochat;
    }
    waitpid($pid, 0);
    my $rc = $?;

    seek CATCHERR, 0, 0;
    while( <CATCHERR> ) {
        if ( $wantarray ) {
            push @err, $_;
        }
        print STDERR $_ if $dochat;
    }
    return ( $rc, \@out, \@err );
}

=pod

=head3 checkSystemRC (NOT EXPORTED)

=over

Checks the return status of a command, warning if the command failed.

=back

=cut

sub checkSystemRC {
    my ( $rc, $base, $cmd ) = @_;
    my $nrc = $rc;
    if ( $rc == 0xff00 ) {
        warnhi( 1, "$base failed, error was: $!\n" );
        warnhi( 1, "command line was: $cmd\n" );
    } elsif ( $rc > 0x80 ) {
        $nrc >>= 8;
        warnhi( 1, "command $base exited with non-zero status: $nrc\n" );
        warnhi( 1, "command line was: $cmd\n" );
    } elsif ( $rc != 0 ) {
        warnhi( 1, "command $base failed with core dump: $rc\n" );
        warnhi( 1, "command line was: $cmd\n" );
    }
    return $nrc;
}

=head3 getPlotDevice

=over

Sets file extension based on plot device and tries to use said device by
plotting a simple plot with qdp and checking that it was successful (i.e. the
test output file exists). Returns plot device string, plot file extension and
status (non-zero if un-supported device).

Inputs:

    - plotdevice (MUST be filetype device - see below)
    - a directory to use for test output

Outputs:

    - status (zero for success, non-zero otherwise)
    - plotdevice (that definitely works if status==0)
    - file extension

Supported pgplot devices are (currently):

    PostScript devices:         /ps, /cps, /vps, /vcps
    Old PostScript devices:     /ops, /ocps, /ovps, /ovcps
    GIF-format devices:         /gif, /vgif
    Portable Pixel Map devices: /ppm, /vppm
    X-Window Dump devices:      /wd, /vwd

=back

=cut

sub getPlotDevice {

    my ( $plotftype, $outdir ) = @_;
    my $plotfext = "";
    my $stat = 0;

    ##########################################
    # try to fix the plot device if not valid
    ##########################################
    if ( $plotftype !~ /^\// ) {
        warnlo( 1, "trying to fix plot device by prepending '/'\n" );
        $plotftype = '/' . $plotftype;
    }

    #######################################################
    # get the filename extension based on plot device type
    #######################################################
    if ( $plotftype =~ /ps/ ) {
        $plotfext = "ps";
    } elsif ( $plotftype =~ /gif/ ) {
        $plotfext = "gif";
    } elsif ( $plotftype =~ /ppm/ ) {
        $plotfext = "ppm";
    } elsif ( $plotftype =~ /wd/ ) {
        $plotfext = "xwd";
    } else {
        error( 1, "unsupported plot device $plotftype\n" );
        return -1;
    }

    ############################
    # try it with a simple plot
    ############################
    my $cwd = getcwd( );
    chdir $outdir or die "failed to chdir to $outdir\n";

    my $qdptest = basename( getTmpFile( "qdp" ) );
    open QDP, ">$qdptest" or die "failed to open file $qdptest\n";
    print QDP "1.0 1.0\n2.0 4.0\n";
    close QDP;

    my $testplot = basename( getTmpFile( "$plotfext" ) );

    my $cmd = "qdp $qdptest 2>&1 > " . devnull( );

    chat( 3, "testing qdp with command: $cmd\n" );
    open QDP, "|$cmd" or die "failed to run command: $cmd\n";
    print QDP "/null\n";
    print QDP "cpd ${testplot}$plotftype\n";
    print QDP "plot\nquit\n";
    close QDP;

    if ( !-e $testplot ) {
        error( 1, "failed to create test plot using plot ",
               "device $plotftype!\n" );
        $stat = -1;
    }
    unlink $testplot, $qdptest;
    chdir $cwd;

    return ( $stat, $plotftype, $plotfext );
}

#------------------------------------------------------------------------------

=head2 :par - Parameter subroutines

=cut

=head3 getListParam

=over

Gets string parameter and converts to a list, if the string parameter starts
with '@' by reading the file whose name follows the '@'. Or if the parameter is
a comma or space delimited list, creates a list containing all the values.

E.g., if the parameter is '@files.txt', and the contents of files.txt is:

    file1.fits
    file2.fits

then the parameter will be set to [ file1.fits, file2.fits ]

Inputs:

    - parameter name
    - return parameter reference

Outputs:

    - the return parameter is modified, will be a list reference
    - a standard status var is returned

=back

=cut

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

=head3 getStrParam

=over

gets string parameter

Inputs:

        - parameter name
        - parameter reference

Outputs:

        - the input parameter reference is modified
        - a standard status var is returned

=back

=cut

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

=head3 getFileParam

=over

gets file parameter

Inputs:

        - parameter name
        - parameter reference

Outputs:

        - the input parameter reference is modified
        - a standard status var is returned

=back

=cut

sub getFileParam {
    my $name   = shift;
    my $parref = shift;
    my $status = 0;
    debug( "getting file parameter $name\n" );
    $status = PILGetFname( $name, $$parref );
    if ( $status != 0 ) {
        error( 1, "error getting file parameter $name\n" );
    }
    debug( "parameter $name is: $$parref\n" );
    return $status;
}

=head3 getFltParam

=over

gets real parameter

Inputs:

        - parameter name
        - parameter reference

Outputs:

        - the input parameter reference is modified
        - a standard status var is returned

=back

=cut

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

=head3 getIntParam

=over

gets integer parameter

Inputs:

        - parameter name
        - parameter reference

Outputs:

        - the input parameter reference is modified
        - a standard status var is returned

=back

=cut

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

=head3 getBoolParam

=over

gets boolean parameter

Inputs:

        - parameter name
        - parameter reference

Outputs:

        - the input parameter reference is modified
        - a standard status var is returned

=back

=cut

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

=head3 readAtParamList

=over

Reads an @-file if the first character of the input is '@'.

Inputs:

       - string input parameter
       - reference to the output array

Outputs:

       - clears and fills output array
       - returns status variable

=back

=cut

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
        @{$outarr} = parseFitsList( $inparam );
    }
    return $status;
}

=head3 parseFitsList

=over

Parses a list of fits files, separated by commas - handles extended
notation (e.g. junk.fits[EVENTS]). adapted from cfitsio

Inputs:

        - string to parse

Outputs:

        - list reference of substrings

=back

=cut

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

=head3 bs

=over

Converts bool to 'yes'/'no' for passing to FTOOLS

Inputs:

        - some value to test for truth

Outputs:

        - "yes" if input is true
        - "no" if input is false

=back

=cut

sub bs {
    if ( $_[ 0 ] ) {
        return 'yes';
    } else {
        return 'no';
    }
}

#------------------------------------------------------------------------------

=head2 :tmp - Temporary file handling

=cut

=head3 getTmpFile

=over

Returns a unique, non-extant temporary file with a given extension
the only argument is the extension (e.g. "fits"). a '.' will
be placed between the root name and the extension.

Inputs:

        - file extension, e.g. 'fits'

Outputs:

        - name of unique temporary file

=back

=cut

sub getTmpFile( $ ) {
    my $ext = shift;
    my $nam = join '.', 'tmpfil', $$, int( rand( 2048 ) ), $ext;
    if ( defined $ENV{HEADAS_TMPDIR} ) {
        $nam = catfile( $ENV{HEADAS_TMPDIR}, $nam );
    }
    while ( -e $nam ) {
        $nam = join '.', 'tmpfil', $$, int( rand( 2048 ) ), $ext;
        if ( defined $ENV{HEADAS_TMPDIR} ) {
            $nam = catfile( $ENV{HEADAS_TMPDIR}, $nam );
        }
    }
    return $nam;
}

=head3 dumpListToTxt

=over

Dumps a list to a text file. If the second argument is an array ref the first
arg is assumed to be a filename, to which the list referenced by the second
argument is dumped. Otherwise, a temp file is created with all arguments dumped
to it. Returns the name of the file created.

Inputs:

        - either a list to dump to a temporary file
        - or a filename and a list reference

Outputs:

        - filename of file created

=back

=cut

sub dumpListToTxt {
    my $outfile;
    my $list;
    if ( 2 == @_ && ref( $_[ 1 ] ) eq 'ARRAY' ) {
        $outfile = shift;
        $list    = shift;
    } else {
        $outfile = getTmpFile( "txt" );
        $list    = [ @_ ];
    }
    my $ret = open OUTF, ">$outfile";
    if ( !$ret ) {
        return "";
    }
    foreach my $line ( @$list ) {
        chomp $line;
        debug( "writing \"$line\" to $outfile\n" );
        print OUTF "$line\n";
    }
    close OUTF;
    return $outfile;
}

#------------------------------------------------------------------------------

=head2 :coord - Coordinate stuff

=cut

=head3 convertRAStringToDegrees

=over

Converts RA in HH:MM:SS.SSSS, HH MM SS.SSSS, HHhMMmSS.SSSSs
or DDD.DDDD to degrees

Inputs:

        - input RA string

Outputs:

        - output scalar RA in degrees
        - status variable

=back

=cut

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
    } elsif ( $inRa =~ /^(\d+\.?\d*)|(\.\d+)|(\d+\.)$/ ) {
        $ra = $inRa * 1.0;
    } else {
        error( 1, "invalid RA string in convertRAStringToDegrees\n" );
        error( 1, "Use HH:MM:SS.SS, HHhMMmSS.SSs, HH MM SS.SS or DDD.DD\n" );
        $status = -1;
    }
    return ( $status, $ra );
}

=head3 convertDecStringToDegrees

=over

Converts Dec in [+-]DD:MM:SS.SSSS, [+-]DD MM SS.SSSS, [+-]DDdMMmSS.SSSSs
or DDD.DDDD to degrees

Inputs:

        - input Dec string

Outputs:

        - output scalar Dec in degrees
        - status variable

=back

=cut

sub convertDecStringToDegrees {
    my $inDec  = shift;
    my $dec    = -1;
    my $status = 0;
    if ( $inDec =~ /^([\+-]?\d{2})([: d]|d\s+)(\d{2})([: m]|m\s+)(\d{2}(\.\d*)?)s?$/ ) {
        my $d    = $1;
        my $sign = ( $d < 0 ) ? -1. : 1.;
        my $m    = $3 / 60.;
        my $s    = $5 / 3600.;
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

=head3 angdist

=over

Calculates distance in arcsec between two ra/dec pairs. Returns undef if bad
inputs are given.

Inputs:

        - RA_1, DEC_1, RA_2, DEC_2

Outputs:

        - distance in arcsec between two input coordinate pairs

=back

=cut

sub angdist {
    unless ( $#_ == 3 ) {
        return undef;
    }
    my ( $ra1, $dec1, $ra2, $dec2 ) = @_;
    my $dtor = 3.141592654 / 180.0;
    my $r0   = $ra1  * $dtor;
    my $d0   = $dec1 * $dtor;
    my $r1   = $ra2  * $dtor;
    my $d1   = $dec2 * $dtor;
    my $dst  = sin( ( $d0 - $d1 ) / 2.0 )**2 +
    cos( $d0 ) * cos( $d1 ) * sin( ( $r1 - $r0 ) / 2.0 )**2;
    if ( $dst > 1.0 ) {
        $dst = 1.0;
    } elsif ( $dst < 0.0 ) {
        $dst = 0.0;
    }
    $dst = 2.0 * asin( sqrt( $dst ) ) / $dtor;
    return $dst * 3600.0;
}

=head3 rolldiff

=over

Calculates distance mod 360. Returns absolute value of separation. Useful for
comparing ROLL angles.

Inputs:

        - angle 1, angle 2

Outputs:

        - separation

=back

=cut

sub rolldiff {
    my ( $ang1, $ang2 ) = @_;
    $ang1 %= 360.0;
    $ang2 %= 360.0;
    while ( $ang1 < 0.0 ) { $ang1 += 360.0; }
    while ( $ang2 < 0.0 ) { $ang2 += 360.0; }
    my $dist = abs( $ang1 - $ang2 ) % 360.0;
    if ( $dist > 180.0 ) { $dist = 360.0 - $dist; }
    return $dist;
}

=head3 sky2xy

=over

Runs FTool sky2xy on file $evt, for position ($ra, $dec)
Returns status, x-pixel and y-pixel.

Inputs:

        - event file name (and extension!)
        - (RA, Dec)
        - Optional X/Y-column names

Outputs:

        - status variable
        - X
        - Y

=back

=cut

sub sky2xy {

    my ( $evt, $ra, $dec, $xcol, $ycol ) = @_;
    if ( !defined $xcol ) { $xcol = 'X'; }
    if ( !defined $ycol ) { $ycol = 'Y'; }
    my ( $stat, $out ) = runSystem( 'sky2xy',
                                    { infile    => $evt,
                                      xsky      => $ra,
                                      ysky      => $dec,
                                      xcol      => $xcol,
                                      ycol      => $ycol,
                                      sensecase => 'no',
                                      tchat     => 10,
                                      lchat     => 0 } );
    return $stat unless $stat == 0;

    my $patt = qr/Output pixel coordinates:\s+(\S+),\s+(\S+)/;
    my ( $xpx, $ypx );
    foreach my $line ( @$out ) {
        if ( $line =~ /$patt/ ) {
            $xpx = $1 * 1.0;
            $ypx = $2 * 1.0;
            last;
        }
    }
    if ( !defined $xpx || !defined $ypx ) {
        $stat = -1;
    }
    return ( $stat, $xpx, $ypx );
}

=head3 xy2sky

=over

Runs FTool xy2sky on file $evt, for position ($xpix, $ypix)
Returns status, RA and Dec.

Inputs:

        - event file name (and extension!)
        - (XPIX, YPIX)
        - Optional X/Y-column names

Outputs:

        - status variable
        - RA
        - Dec

=back

=cut

sub xy2sky {

    my ( $evt, $xpix, $ypix, $xcol, $ycol ) = @_;
    if ( !defined $xcol ) { $xcol = 'X'; }
    if ( !defined $ycol ) { $ycol = 'Y'; }
    my ( $stat, $out ) = runSystem( 'xy2sky',
                                    { infile    => $evt,
                                      xpix      => $xpix,
                                      ypix      => $ypix,
                                      xcol      => $xcol,
                                      ycol      => $ycol,
                                      sensecase => 'no',
                                      tchat     => 10,
                                      lchat     => 0 } );
    return $stat unless $stat == 0;

    my ( $ra, $dec );
    my $patt = qr/Output SKY coordinates:\s+(\S+),\s+(\S+)$/;
    foreach my $line ( @$out ) {
        if ( $line =~ /$patt/ ) {
            $ra  = $1 * 1.0;
            $dec = $2 * 1.0;
            last;
        }
    }
    if ( !defined $ra || !defined $dec ) {
        $stat = -1;
    }

    return ( $stat, $ra, $dec );
}

#
# pointxform -
#
#       runs pointxform
#
sub pointxform {

    my ( $from, $to, $x, $y, $teld, $algn, $att, $time, $fromworld ) = @_;

    $to   = uc( $to );
    $from = uc( $from );
    $fromworld = bs( $fromworld );

    ############################################################
    # add a new output type (output must be parsed differently)
    ############################################################
    my $realto = $to;
    if ( $to eq 'RADEC' ) {
        $realto = 'SKY';
    }
    my ( $stat, $out ) = runSystem( 'pointxform',
                                    { from       => $from,
                                      to         => $realto,
                                      x          => $x,
                                      y          => $y,
                                      teldeffile => $teld,
                                      alignfile  => $algn,
                                      attfile    => $att,
                                      time       => $time,
                                      fromworld  => $fromworld,
                                      chatter    => 0,
                                      cleanup    => 'yes' } );
    return $stat unless $stat == 0;

    my $xto = undef;
    my $yto = undef;
    my $patt;
    if ( $to eq 'RADEC' ) {
        $patt = qr/${realto}[^\[]+\[\s*([\d\.-]+),\s*([\d\.-]+)/;
    } else {
        $patt = qr/${realto}\s+([\d\.-]+),\s+([\d\.-]+)/;
    }
    foreach my $line ( @$out ) {
        if ( $line =~ /$patt/ ) {
            $xto = $1 * 1.0;
            $yto = $2 * 1.0;
        }
    }
    if ( !defined $xto || !defined $yto ) {
        error( 1, "failed to get source ${to}X/${to}Y in pointxform()\n" );
        $stat = -1;
    }
    return( $stat, $xto, $yto );
}

#------------------------------------------------------------------------------

###############################################################################
######################## FTOOLs stuff - :tool #################################
###############################################################################

#
# ftcopy -
#
#       runs ftcopy with clobber=yes and history=no
#
sub ftcopy {
    my ( $file1, $file2, $copyall, $chatter ) = @_;

    $copyall = bs( $copyall );
    if ( !defined $chatter || $chatter < 0 || $chatter > 5 ) {
        $chatter = 1;
    }
    return runSystem( 'ftcopy',
                       { infile  => $file1,
                         outfile => $file2,
                         copyall => $copyall,
                         clobber => 'yes',
                         chatter => $chatter,
                         history => 'no' } );
}

#
# ftappend -
#
#       runs ftappend
#
sub ftappend {

    my ( $infile, $outfile, $chatter, $history ) = @_;
    if ( !defined $chatter || $chatter < 0 || $chatter > 5 ) {
        $chatter = 1;
    }
    $history = bs( $history );
    return runSystem( 'ftappend',
                      { infile    => $infile,
                        outfile   => $outfile,
                        chatter   => $chatter,
                        history   => $history } );
}

#
# ftpaste -
#
#       runs ftpaste
#
sub ftpaste {

    my ( $infile, $pastefile, $outfile, $copyall, $chatter ) = @_;
    $copyall = bs( $copyall );
    if ( !defined $chatter || $chatter < 0 || $chatter > 5 ) {
        $chatter = 1;
    }

    return runSystem( 'ftpaste',
                      { infile    => $infile,
                        pastefile => $pastefile,
                        outfile   => $outfile,
                        copyall   => $copyall,
                        clobber   => 'yes',
                        chatter   => $chatter,
                        history   => 'no' } );
}

#
# nh -
#
#       runs the FTool 'nh', and returns the weighted average NH.
#
sub nh {

    my ( $ra, $dec, $eq, $map, $size, $disio ) = @_;

    $eq    = $eq ? $eq : 2000;
    $size  = $size ? $size : 3;
    $disio = $disio ? $disio : 1;

    #################################################
    # if bad input for map given, default to D&L map
    #################################################
    if ( $map < 0 || $map > 1 ) {
        $map = 1;
        warnhi( 1, "in nh(), map must be 0 or 1 - defaulting to 1\n" );
    }

    #################################################
    # define the output parsing pattern based on map
    #################################################
    my $patt;
    if ( $map == 0 ) {
        $patt = qr/LAB >> Weighted average nH \(cm\*\*-2\)\s*(\S+)/;
    } else {
        $patt = qr/DL >> Weighted average nH \(cm\*\*-2\)\s*(\S+)/;
    }

    #########
    # run nh
    #########
    my ( $stat, $out ) = runSystem( 'nh',
                                    { equinox => $eq,
                                      ra      => $ra,
                                      dec     => $dec,
                                      size    => 3,
                                      disio   => 1,
                                      usemap  => $map,
                                      tchat   => 10,
                                      lchat   => -1 } );
    ###################
    # parse the output
    ###################
    foreach my $line ( @$out ) {
        chomp $line;
        if ( $line =~ /$patt/ ) {
            return ( $stat, $1 * 1.0 );
        }
    }
    return ( -1, 0.0 );
}

#
# maketime -
#
#       Runs the maketime tool - assumes non-compact hk
#
sub maketime {

    my ( $mkf, $gtifile, $expr, $prefr, $postfr ) = @_;
    if ( !defined $prefr ) {
        $prefr  = 1.0;
        $postfr = 0.0;
    }

    ###############
    # run maketime
    ###############
    if ( $expr =~ /^@/ ) {
        local $/;
        my $filename = $expr;
        $filename =~ s/^@//;
        open FILE, "<$filename" or die "failed to open file $filename\n";
        my $lines = <FILE>;
        close FILE;
        chat( 2, "running maketime on $mkf with expression:\n$lines\n" );
    } else {
        chat( 2, "running maketime on $mkf with expression:\n$expr\n" );
    }
    return runSystem( 'maketime',
                      { infile  => $mkf,
                        outfile => $gtifile,
                        expr    => $expr,
                        name    => 'anything',
                        value   => 'anything',
                        time    => 'TIME',
                        start   => 'START',
                        stop    => 'STOP',
                        compact => 'no',
                        copykw  => 'yes',
                        histkw  => 'yes',
                        prefr   => $prefr,
                        postfr  => $postfr,
                        clobber => 'yes' } );
}

#
# mgtime -
#
#       runs the mgtime FTOOL with fairly standard inputs
#
#       Inputs:
#               string to pass as ingtis param
#                 OR
#               list reference containing list of files to merge
#
sub mgtime {

    my ( $inlist, $outgti, $op ) = @_;

    ####################################################
    # dump the input list to a tmp file if it is a list
    ####################################################
    my $ingtis;
    my $tmp = undef;
    if ( ref $inlist eq 'ARRAY' ) {
        $tmp = dumpListToTxt( @$inlist );
        $ingtis = '@' . $tmp;
    } else {
        $ingtis = $inlist;
    }

    #############
    # run mgtime
    #############
    my ( $stat, $out, $err ) = runSystem( 'mgtime',
                                          { ingtis   => $ingtis,
                                            outgti   => $outgti,
                                            merge    => $op,
                                            instarts => 'START',
                                            instops  => 'STOP',
                                            indates  => 'MJDREF',
                                            intimes  => ' ',
                                            outstart => 'START',
                                            outstop  => 'STOP' } );
    unlink $tmp if defined $tmp;
    if ( wantarray ) {
        return ( $stat, $out, $err );
    } else {
        return $stat;
    }
}

#------------------------------------------------------------------------------

###############################################################################
########################## Extractor stuff - :extractor #######################
###############################################################################

###############################################################################
# These subs are based in part on the Util::Extractor perl module used for the
# Suzaku/Swift/ASCA pipeline processing scripts. The original author of that
# module was Ed Pier, with contributions by Eric Winter and Richard Fink
###############################################################################

#
# setupExtractor -
#
#       reads Xselect mission database for a particular mission/instrument
#
sub setupExtractor {

    my $mission   = shift;
    my $detector  = shift;

    ##############################################
    # for Suzaku make sure the mission is correct
    # otherwise make sure it's upper case
    ##############################################
    if ( $mission =~ /astro-e2/i ) {
        $mission = 'SUZAKU';
    } else {
        $mission = uc( $mission );
    }

    ################################################
    # do nothing if mission/instrument are the same
    ################################################
    if ( $MDB{mission} && $mission eq $MDB{mission} &&
         $MDB{detector} && $detector eq $MDB{detector} ) {
        return 0;
    }

    ###############
    # wipe the MDB
    ###############
    %MDB = ( );

    ##################################################
    # save the mission and detector in the mission DB
    ##################################################
    $MDB{mission}   = $mission;
    $MDB{detector}  = $detector;

    ###########################################
    # path to default Xselect mission database
    ###########################################
    my $mdb;
    if ( exists $ENV{XSELECT_MDB} && -e $ENV{XSELECT_MDB} ) {
        $mdb = $ENV{XSELECT_MDB};
    } else {
        $mdb = catfile( $ENV{HEADAS}, 'bin', 'xselect.mdb' );
    }

    ############################
    # read the mission database
    ############################
    my %localMDB = ( );
    my %mdbkeys  = ( );
    unless ( open MDB, "<$mdb" ) {
        error( 1, "failed to open mission database $mdb\n" );
        return -1;
    }
    while ( <MDB> ) {

        ################################
        # skip comments and blank lines
        ################################
        chomp;
        s/^\s!.*$//;
        s/^\s*$//;
        next unless length;

        ####################################################################
        # lines are "<tag> <value>", where <tag> is a ':' separated string
        # containing (e.g.) MISSION:DETECTOR:key, and <value> is everything
        # after the first space
        ####################################################################
        my ( $tag, $value ) = /^\s*(\S*)\s*(.*)$/;

        ##############################
        # get the mission and the key
        ##############################
        my @splittag = split /:/, $tag;
        my $mis = shift @splittag;
        my $key = pop @splittag;

        #############################################
        # if this is the mission we're interested in
        # then save the key/instrument/value
        #############################################
        if ( $mis eq $mission ) {
            my $ins = join ':', @splittag;
            $ins =~ s/\*/.*?/;
            $localMDB{$key}{$ins} = $value;
            $mdbkeys{$key} = 1;
            debug( "in setupExtractor(): found %s=%s in xselect MDB\n",
                   $key, $value );
        }
    }
    close MDB;

    ##################################################
    # try to match the instrument for every key
    # starting with the most specific instrument spec
    ##################################################
MDBKEY:
    foreach my $key ( keys %mdbkeys ) {
        my @detector = split /:/, $detector;
        while ( 1 ) {
            my $inst = join ':', @detector;
            foreach my $ins ( keys %{$localMDB{$key}} ) {
                debug( "in setupExtractor(): trying to match "
                     . "$inst vs $ins for key $key\n" );
                if ( $inst =~ /^$ins$/ ) {
                    $MDB{$key} = $localMDB{$key}{$ins};
                    debug( "in setupExtractor(): using xselect mdb value "
                         . "$MDB{$key} for $key\n" );
                    next MDBKEY;
                }
            }
            last if !@detector;
            pop @detector;
        }
    }

    ##################################
    # check if we found any more keys
    ##################################
    if ( scalar( keys %MDB ) <= 2 ) {
        error( 1, "mission instrument combo $mission/$detector "
                . "not found in xselect mission database\n" );
        return -1;
    }
    return 0;
}

#
# extractor -
#
#       runs the extractor tool with a combination of (hopefully) reasonable
#       defaults, information from the xselect mission database, and input
#       parameters passed as arguments in a hash reference
#
sub extractor {

    my $args = shift;

    my @cleanup = ( );

    ###########################################################
    # setup filename parameter - if a list reference is passed
    # dump the list to a temp file no matter how many elements
    ###########################################################
    my $filename;
    if ( exists $args->{filename} && ref $args->{filename} eq 'ARRAY' ) {
        $filename = dumpListToTxt( @{$args->{filename}} );
        push @cleanup, $filename;
        $filename = '@' . $filename;
    } elsif ( exists $args->{filename} ) {
        $filename = $args->{filename};
    } else {
        error( 1, "filename argument required in extractor( )\n" );
        return -1;
    }

    #################################################
    # setup region file if a reference was passed in
    #################################################
    my $regionfile;
    if ( exists $args->{regionfile} && ref $args->{regionfile} eq 'ARRAY' &&
         @{$args->{regionfile}} > 0 ) {
        $regionfile = dumpListToTxt( @{$args->{regionfile}} );
        push @cleanup, $regionfile;
    } elsif ( exists $args->{regionfile} && !ref $args->{regionfile} ) {
        $regionfile = $args->{regionfile};
    } else {
        $regionfile = 'NONE';
    }

    ######################################################
    # setup additional selection filter (extractor style)
    ######################################################
    if ( exists $args->{selection} && $args->{selection} ne "" ) {
        if ( $args->{selection} !~ /^\[/ ) {
            $filename .= "[";
        }
        $filename .= $args->{selection};
        if ( $args->{selection} !~ /\]$/ ) {
            $filename .= "]";
        }
    }

    #########################################
    # determine which coordinate type to use
    #########################################
    my ( $X, $Y );
    if ( $MDB{imagecoord} =~ /sky/i && exists $MDB{x} && exists $MDB{y} ) {
        $X = $MDB{x};
        $Y = $MDB{y};
    } elsif ( $MDB{imagecoord} =~ /det/i &&
              exists $MDB{detx} && exists $MDB{dety}) {
        $X = $MDB{detx};
        $Y = $MDB{dety};
    } elsif ( $MDB{imagecoord} =~ /raw/i &&
              exists $MDB{rawx} && exists $MDB{rawy}) {
        $X = $MDB{rawx};
        $Y = $MDB{rawy};
    } else {
        error( 1, "%s coordinates not defined for %s/%s\n",
                  $MDB{imagecoord}, $MDB{mission}, $MDB{instrument} );
        return -1;
    }

    #####################################################################
    # setup the extractor run filling in values from the input arguments
    #####################################################################
    my %extractor = (
        exitnow    => 'no',
        filename   => $filename,
        eventsout  => exists $args->{eventsout} ? $args->{eventsout} : 'NONE',
        imgfile    => exists $args->{imgfile} ? $args->{imgfile} : 'NONE',
        binf       => 1,
        fullimage  => 'yes',
        phafile    => exists $args->{phafile} ? $args->{phafile} : 'NONE',
        specbin    => 1,
        wtmapb     => exists $args->{wtmapb} ? $args->{wtmapb} : 'yes',
        wtmapfix   => 'yes',
        swmapx     => 'no',
        swmapy     => 'no',
        binh       => 1,
        wmapver    => 2,
        fitsbinlc  => exists $args->{fitsbinlc} ? $args->{fitsbinlc} : 'NONE',
        qdpfile    => 'NONE',
        binlc      => exists $args->{binlc} ? $args->{binlc} : 1.0,
        lcthresh   => exists $args->{lcthresh} ? $args->{lcthresh} : 0.0,
        lcthwarn   => 3.0,
        lctzero    => exists $args->{lctzero} ? $args->{lctzero} : 'yes',
        unbinlc    => exists $args->{unbinlc} ? $args->{unbinlc} : 'NONE',
        regionfile => $regionfile,
        timefile   => exists $args->{timefile} ? $args->{timefile} : 'NONE',
        gtinam     => 'GTI',
        xcolf      => $X,
        ycolf      => $Y,
        zcolf      => 'NONE',
        xint       => 1.0,
        yint       => 1.0,
        tcol       => exists $MDB{tcol} ? $MDB{tcol} : 'TIME',
        ecol       => exists $MDB{ecol} ? $MDB{ecol} : 'PI',
        ccol       => exists $MDB{ccol} ? $MDB{ccol} : 'CCD_ID',
        gcol       => exists $MDB{gcol} ? $MDB{gcol} : 'GRADE',
        gstring    => exists $args->{gstring} ? $args->{gstring} : 'NONE',
        xcolh      => $X,
        ycolh      => $Y,
        gtitxt     => 'NONE',
        xronwn     => 'NONE',
        events     => exists $MDB{events} ? $MDB{events} : 'EVENTS',
        gti        => exists $MDB{gti} ? $MDB{gti} : 'GTI',
        timeorder  => exists $MDB{timeorder} ? $MDB{timeorder} : 'no',
        timeref    => 40000.0,
        eventkey   => 'NONE',
        phamax     => exists $MDB{phamax} ? $MDB{phamax} : 'PHA_BINS',
        xfkey      => 'NONE',
        yfkey      => 'NONE',
        xhkey      => 'NONE',
        yhkey      => 'NONE',
        copyall    => exists $args->{copyall} ? $args->{copyall} : 'yes',
        clobber    => 'yes'
    );

    #######################################
    # add lcstart if explicitely asked for
    #######################################
    if ( exists $args->{lcstart} ) {
        $extractor{lcstart} = $args->{lcstart};
    }

    #########
    # run it
    #########
    my ( $stat, $out, $err ) = runSystem( 'extractor', \%extractor );

    unlink @cleanup;

    if ( wantarray ) {
        return ( $stat, $out, $err );
    } else {
        return $stat;
    }
}

#
# xselmdbval -
#
#       returns the value stored in the mission database for the specified
#       key, if extant. returns undef otherwise.
#
#       Inputs: key
#
#       Outputs: value
#
sub xselmdbval {

    my $key = shift;
    if ( exists $MDB{$key} ) {
        return $MDB{$key};
    } else {
        return undef;
    }
}

#------------------------------------------------------------------------------

###############################################################################
###################### Light Curve stuff - :lightcurve ########################
###############################################################################

#
# lcadd -
#
#       calls lcmath with addsubr=yes
#
sub lcadd {

    my ( $file1, $file2, $outfile, $mul1, $mul2, $add1, $add2, $mode ) = @_;

    $mul1 = defined $mul1 ? $mul1 : 1.0;
    $mul2 = defined $mul2 ? $mul2 : 1.0;
    $add1 = defined $add1 ? $add1 : 0.0;
    $add2 = defined $add2 ? $add2 : 0.0;
    $mode = defined $mode ? $mode : 1;

    return lcmath( $file1, $file2, $outfile, $mul1,
                   $mul2, $add1, $add2, $mode, 1 );
}

#
# lcsub -
#
#       calls lcmath with addsubr=no
#
sub lcsub {

    my ( $file1, $file2, $outfile, $mul1, $mul2, $add1, $add2, $mode ) = @_;

    $mul1 = defined $mul1 ? $mul1 : 1.0;
    $mul2 = defined $mul2 ? $mul2 : 1.0;
    $add1 = defined $add1 ? $add1 : 0.0;
    $add2 = defined $add2 ? $add2 : 0.0;
    $mode = defined $mode ? $mode : 1;

    return lcmath( $file1, $file2, $outfile, $mul1,
                   $mul2, $add1, $add2, $mode, 0 );
}

#
# lcmath -
#
#       calls lcmath
#
sub lcmath {

    my ( $file1, $file2, $outfile, $mul1, $mul2,
         $add1, $add2, $mode, $addsub ) = @_;

    return runSystem( 'lcmath',
                      { infile   => $file1,
                        bgfile   => $file2,
                        outfile  => $outfile,
                        multi    => $mul1,
                        multb    => $mul2,
                        addsubr  => bs( $addsub ),
                        addi     => $add1,
                        addb     => $add2,
                        docor    => 'no',
                        emin     => 0,
                        emax     => 0,
                        err_mode => $mode,
                        tchat    => 10,
                        lchat    => 0 } );
}

#
# lcstats -
#
#       gets light curve stats with lcstats,
#       supporting extended filename syntax through ftcopy
#
sub lcstats {

    my $file = $_[ 0 ];
    my $stat = 0;
    my $out;

    ####################################################################
    # setup output stats and pattern matches for lcstats output parsing
    ####################################################################
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

    ###############################################
    # get the root filename, without any filtering
    ###############################################
    my $root = "";
    Astro::FITS::CFITSIO::fits_parse_rootname( $file, $root, $stat );
    return ( $stat, %stats ) unless $stat == 0;

    ###########################################################
    # compare to input filename, and if different make a copy
    # lcstats does not play nice with extended filename syntax
    ###########################################################
    my $tmpfits = undef;
    if ( $root ne $file ) {
        $tmpfits = getTmpFile( "fits" );
        my $stat = ftcopy( $file, $tmpfits, 1 );
        return ( $stat, %stats ) unless $stat == 0;
        $file = $tmpfits;
    }

    ###################################################
    # run lcstats with (hopefully) reasonable defaults
    ###################################################
    ( $stat, $out ) = runSystem( 'lcstats',
                                 { cfile1   => $file,
                                   window   => '-',
                                   dtnb     => 'INDEF',
                                   nbint    => 'INDEF',
                                   itre     => 0,
                                   tchat    => 10,
                                   lchat    => 0 } );
    unlink $tmpfits if defined $tmpfits;
    return ( $stat, %stats ) unless $stat == 0;

    ###################
    # parse the output
    ###################
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
# lcstats2 -
#
#       gets basic light curve statistics without calling lcstats. the
#       calculation is slower than lcstats proper, but if you have already read
#       the light curve for some reason, it is awfully convenient. third order
#       statistics could be added.
#
sub lcstats2 {

    my ( $time, $rate, $err, $exp ) = @_;
    my $stats = {
                  average  => 0.0,
                  stddev   => 0.0,
                  variance => 0.0,
                  expvar   => 0.0,
                  maximum  => 0.0,
                  minimum  => 1e20,
                  nbins    => 0.0,
                  avgdev   => 0.0,
                  chisqr   => 0.0,
                  chidof   => 0.0,
                  chiprob  => 1.0,
                };

    ######################
    # get the basic stats
    ######################
    my $avgexp = 0.0;
    for ( my $i = 0; $i < @$time; $i++ ) {

        # skip 0 exposure
        next if $exp->[ $i ] <= 0.0;

        # number of bins
        $stats->{nbins}   += 1.0;

        # average rate
        $stats->{average} += $rate->[ $i ];

        # expected variance
        $stats->{expvar}  += $err->[ $i ]**2.0;

        # max rate
        if ( $rate->[ $i ] > $stats->{maximum} ) {
            $stats->{maximum} = $rate->[ $i ];
        }

        # min rate
        if ( $rate->[ $i ] < $stats->{minimum} ) {
            $stats->{minimum} = $rate->[ $i ];
        }

        # average exposure
        $avgexp += $exp->[ $i ];
    }

    # finalize
    $stats->{chidof} = $stats->{nbins} - 1.0;
    if ( $stats->{nbins} > 0.0 ) {
        $stats->{average} /= $stats->{nbins};
        $stats->{expvar}  /= $stats->{nbins};
        $avgexp /= $stats->{nbins};
    } elsif ( $stats->{minimum} == 1e20 ) {
        $stats->{min} = 0.0;
    }

    #########################
    # get higher order stats
    #########################
    for ( my $i = 0; $i < @$time; $i++ ) {
        next if $exp->[ $i ] <= 0.0;

        # abs avg deviation, standard deviation and third moment
        $stats->{avgdev}   += abs( $rate->[ $i ] - $stats->{average} );
        $stats->{stddev}   += ( $rate->[ $i ] - $stats->{average} )**2.0;

        # compute the Chi^2 distribution
        if ( $stats->{expvar} != 0.0 ) {
            $stats->{chisqr} += ( $stats->{average} - $rate->[ $i ] )**2.0 /
                                $stats->{expvar} /
                                ( $avgexp / $exp->[ $i ] )**2.0;
        }
    }
    $stats->{chiprob} = gammaQ( ( $stats->{nbins} - 1.0 ) / 2.0,
                                $stats->{chisqr} / 2.0 );
    # finalize
    if ( $stats->{nbins} > 0.0 ) {
        $stats->{variance}  = $stats->{stddev} / $stats->{nbins};
        $stats->{stddev}    = sqrt( $stats->{variance} );
        $stats->{avgdev}   /= $stats->{nbins};
        $stats->{chisqr}   /= $stats->{nbins};
    }
    return $stats;
}

#
# lnGamma -
#
#       returns the log of the gamma function for X > 1, where X is
#       the first input argument.
#
#       This routine is taken from xronlib, as are the routines that it calls
#
sub lnGamma {

    my $X = shift;

    my @coeffs = ( 76.18009173, -86.50532033, 24.01409822,
                   -1.23739516, 0.120858003e-2, -0.5356382e-5 );

    my $stp = 2.50662827465;

    my $x = $X - 1.0;
    my $t = ( $x + 5.5 );
    my $s = 1.0;

    $t = ( $x + 0.5 ) * log( $t ) - $t;
    for ( my $i = 0; $i < @coeffs; $i++ ) {
        $x += 1.0;
        $s += $coeffs[ $i ] / $x;
    }
    return ( $t + log( $stp * $s ) );
}

#
# gammaP -
#
#       returns normalized incomplete Gamma function P(A,X)
#
sub gammaP {

    my $A = shift;
    my $X = shift;

    if ( $X < 0.0 || $A <= 0.0 ) {
        warnlo( 4, "bad argument to gammaP( )!\n" );
    }
    if ( $X < $A + 1.0 ) {
        return gammaPSeries( $A, $X );
    } else {
        my $gammaQ = gammaQFrac( $A, $X );
        return ( 1.0 - $gammaQ );
    }
}

#
# gammaQ -
#
#       returns normalized complement of incomplete Gamma function, Q(A,X)
#
sub gammaQ {

    my $A = shift;
    my $X = shift;

    if ( $X < 0.0 || $A <= 0.0 ) {
        warnlo( 4, "bad argument to gammaQ( )!\n" );
    }
    if ( $X < $A + 1.0 ) {
        my $gammaP = gammaPSeries( $A, $X );
        return ( 1.0 - $gammaP );
    } else {
        return gammaQFrac( $A, $X );
    }
}

#
# gammaQFrac -
#
#       returns normalized complement of incomplete Gamma function, Q(A,X),
#       calculated using the continued fraction representation
#
sub gammaQFrac {

    my $A = shift;
    my $X = shift;

    my $maxiter = 5000;
    my $eps     = 3.0e-7;
    my $g       = 0.0;
    my $gold    = 0.0;
    my $a0      = 1.0;
    my $a1      = $X;
    my $b0      = 0.0;
    my $b1      = 1.0;
    my $fac     = 1.0;
    my ( $an, $ana, $anf );
    for ( my $i = 1; $i <= $maxiter; $i++ ) {
        $an  = $i * 1.0;
        $ana = $an - $A;

        $a0  = ( $a1 + $a0 * $ana ) * $fac;
        $b0  = ( $b1 + $b0 * $ana ) * $fac;
        $anf = $an * $fac;
        $a1  = $X * $a0 + $anf * $a1;
        $b1  = $X * $b0 + $anf * $b1;
        if ( $a1 != 0.0 ) {
            $fac = 1.0 / $a1;
            $g   = $b1 * $fac;
            if ( abs( ( $g - $gold ) / $g ) < $eps ) {
                goto GCFDONE;
            }
            $gold = $g;
        }
    }
    warnlo( 4, "failed to converge in $maxiter "
             . "iterations in gammaPSeries( )\n" );

GCFDONE:
    my $gammaln = lnGamma( $A );
    my $gammacf = exp( -1.0 * $X + $A * log( $X ) - $gammaln ) * $g;
    if ( wantarray ) {
        return ( $gammacf, $gammaln );
    } else {
        return $gammacf;
    }
}

#
# gammaPSeries -
#
#       returns normalized incomplete Gamma function P(A,X), computed using
#       series representation
#
sub gammaPSeries {

    my $A = shift;
    my $X = shift;

    my $maxiter = 5000;
    my $eps     = 3.0e-7;

    if ( $X <= 0.0 ) {
        if ( wantarray ) {
            return ( 0.0, lnGamma( 0.0 ) );
        } else {
            return 0.0;
        }
    }
    my $ap  = $A;
    my $sum = 1.0 / $A;
    my $del = $sum;
    for ( my $i = 0; $i < $maxiter; $i++ ) {
        $ap  += 1.0;
        $del  = $del * $X / $ap;
        $sum += $del;
        if ( abs( $del ) < abs( $sum ) * $eps ) {
            goto GSERDONE;
        }
    }
    warnlo( 4, "failed to converge in $maxiter "
             . "iterations in gammaPSeries( )\n" );
GSERDONE:
    my $gammaln  = lnGamma( $A );
    my $gammaser = $sum * exp( -1.0 * $X + $A * log( $X ) - $gammaln );
    if ( wantarray ) {
        return ( $gammaser, $gammaln );
    } else {
        return $gammaser;
    }
}

###############################################################################
########################## Spectral stuff - :spec #############################
###############################################################################

#
# grppha -
#
#       calls grppha to group a single spectrum
#
sub grppha {

    my ( $infile, $outfile, $comm ) = @_;

    my $tempc = "";
    if ( $comm !~ /exit/ ) {
        $tempc = "exit";
    }
    return runSystem( 'grppha',
                      { infile  => $infile,
                        outfile => $outfile,
                        chatter => 5,
                        comm    => $comm,
                        tempc   => $tempc,
                        clobber => 'yes' } );
}

#
# addarf -
#
#       runs the tool addarf
#
sub addarf {

    my ( $arfs, $wgts, $outf ) = @_;
    my @dumplist = map( sprintf( "%s %.8g\n", $arfs->[$_], $wgts->[$_] ),
                        ( 0..$#{$arfs} ) );
    my $tmpf = dumpListToTxt( @dumplist );
    my ( $stat, $out, $err ) = runSystem( 'addarf',
                                          { list    => "\@$tmpf",
                                            out_ARF => $outf,
                                            clobber => 'yes',
                                            chatter => 9 } );
    unlink $tmpf;
    if ( wantarray ) {
        return ( $stat, $out, $err );
    } else {
        return $stat;
    }
}

#
# addrmf -
#
#       runs the tool addrmf
#
sub addrmf {

    my ( $rmfs, $wgts, $outf ) = @_;
    my @dumplist = map( sprintf( "%s %.8g\n", $rmfs->[$_], $wgts->[$_] ),
                        ( 0..$#{$rmfs} ) );
    my $tmpf = dumpListToTxt( @dumplist );
    my ( $stat, $out, $err ) = runSystem( 'addrmf',
                                          { list    => "\@$tmpf",
                                            rmffile => $outf,
                                            clobber => 'yes' } );
    unlink $tmpf;
    if ( wantarray ) {
        return ( $stat, $out, $err );
    } else {
        return $stat;
    }
}

#
# marfrmf -
#
#       runs the tool marfrmf
#
sub marfrmf {

    my ( $rmf, $arf, $outfile, $arfcol ) = @_;

    if ( !defined $arfcol ) {
        $arfcol = 'SPECRESP';
    }
    return runSystem( 'marfrmf',
                      { rmfil     => $rmf,
                        ebfil     => '%',
                        arfil     => $arf,
                        outfil    => $outfile,
                        qoverride => 'no',
                        qdivide   => 'no',
                        arfcol    => 'SPECRESP' } );
}

###############################################################################
############################# GTI stuff - :gti ################################
###############################################################################

#
# readGTI -
#
#       reads the first GTI from an input file.
#
#       returns status, and an array of hash references, each of which
#       has two hash elements, START and STOP.
#
sub readGTI {

    my $infile = shift;

    my $stat  = 0;
    my $fptr  = 0;
    my $hdu   = 0;
    my $nrows = 0;
    my $col   = 0;
    my $tzero = 0.0;
    my $msg   = "";

    chat( 2, "reading GTI from file $infile\n" );

    ###########################################
    # open the file and goto 1st GTI extension
    ###########################################
    $fptr = Astro::FITS::CFITSIO::open_file( $infile, READONLY, $stat );
    return $stat unless $stat == 0;

    $stat = findGTIExt( $fptr, \$hdu, $stat );
    return $stat unless $stat == 0;

    #########################
    # get the number of rows
    #########################
    if ( $fptr->get_num_rows( $nrows, $stat ) ) {
        error( 1, "fits_get_num_rows() failed on %s (%d)\n", $infile, $stat );
        return $stat;
    }

    ####################
    # read the TIMEZERO
    ####################
    if ( $fptr->read_key_dbl( "TIMEZERO", $tzero, undef, $stat ) ) {
        fits_read_errmsg( $msg );
        $stat  = 0;
        $tzero = 0.0;
    }

    ###################
    # read the columns
    ###################
    my @start = ( 0.0 ) x $nrows;
    my @stop  = ( 0.0 ) x $nrows;
    if ( $fptr->get_colnum( CASEINSEN, "START", $col, $stat ) ) {
        error( 1, "fits_get_colnum('START') failed on %s (%d)\n",
                  $infile, $stat );
        return $stat;
    }
    if ( $fptr->read_col_dbl( $col, 1, 1, $nrows, 0,
                              \@start, undef, $stat ) ) {
        error( 1, "fits_read_col_dbl('START') failed on %s (%d)\n",
                  $infile, $stat );
        return $stat;
    }
    if ( $fptr->get_colnum( CASEINSEN, "STOP", $col, $stat ) ) {
        error( 1, "fits_get_colnum('stop') failed on %s (%d)\n",
                  $infile, $stat );
        return $stat;
    }
    if ( $fptr->read_col_dbl( $col, 1, 1, $nrows, 0,
                              \@stop, undef, $stat ) ) {
        error( 1, "fits_read_col_dbl('STOP') failed on %s (%d)\n",
                  $infile, $stat );
        return $stat;
    }
    $fptr->close_file( $stat );

    ###################################
    # create the output data structure
    ###################################
    my $gti = [ ];
    my @indices = sort { $start[ $a ] <=> $start[ $b ] } 0..$#start;
    foreach my $i ( @indices ) {
        push @$gti, { START => $start[ $i ] + $tzero,
                      STOP  => $stop[ $i ]  + $tzero };
    }
    return ( $stat, $gti );
}

#
# findGTIExt -
#
#       finds a GTI extension and moves to it, given an input fptr
#       returns status variable, and changes second argument (must be a
#       reference) to be the HDU number that was moved to
#
sub findGTIExt {

    my ( $fptr, $gtihduREF, $stat ) = @_;

    my $nhdus;
    my $clas1;
    my $msg;

    if ( ref $gtihduREF ne 'SCALAR' ) {
        error( 1, "second argument to findGTIExt() must be a reference to a scalar\n" );
        return -1;
    }

    $$gtihduREF = 0;
    $fptr->get_num_hdus( $nhdus, $stat );
    for ( my $i = 1; $i <= $nhdus; $i++ ) {
        $fptr->movabs_hdu( $i, undef, $stat );
        $fptr->read_key_str( "HDUCLAS1", $clas1, undef, $stat );
        if ( $stat == KEY_NO_EXIST ) {
            $stat = 0;
            fits_read_errmsg( $msg );
            next;
        } elsif ( $clas1 =~ /GTI/ ) {
            $$gtihduREF = $i;
            last;
        }
    }
    if ( $$gtihduREF ) {
        $fptr->movabs_hdu( $$gtihduREF, undef, $stat );
    } else {
        $stat = -1;
    }
    return $stat;
}

=pod

=head1 NOTES

Not fully documented in pod yet.

=head1 BUGS

Highly unlikely.

=head1 SEE ALSO

utils.pl, xanlib, ...

=cut

1;

