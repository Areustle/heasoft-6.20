package HEACORE::HEAINIT;

use strict;
use Carp;
use vars qw($VERSION @ISA @EXPORT @EXPORT_OK $AUTOLOAD);

require Exporter;
require DynaLoader;
require AutoLoader;

@ISA = qw(Exporter DynaLoader);
# Items to export into callers namespace by default. Note: do not export
# names by default without a very good reason. Use EXPORT_OK instead.
# Do not simply export all your public functions/methods/constants.
@EXPORT = qw(
  headas_init
  headas_close
  headas_main
);
$VERSION = '0.01';

bootstrap HEACORE::HEAINIT $VERSION;

# Preloaded methods go here.

sub headas_main{
  my $toolptr = $_[0];
  my $logopen;
  my $status;

  my $argc = $#ARGV+2;
  my $argv = [$0, @ARGV];
  if (defined $ENV{HEADASOUTPUT}){
    unless ($ENV{HEADASOUTPUT} =~ /stdout/i){
      ($logopen = open(LOG,">>".$ENV{HEADASOUTPUT}))
	|| die "error opening $ENV{HEADASOUTPUT}: $!";
      select LOG;
    }
  }
  $status = headas_init($argc, $argv);

  unless ($status){
    # Call the tool subroutine with an exception handler
    eval{
      $status = &{$toolptr};
    };
    # Check for errors and report them
    if ($@){
      if ($status == 0) { $status = 1 }
      warn $@;
    }
  }

  headas_close($status);
  if ($logopen){ close LOG }

  return $status;
}

# Autoload methods go after =cut, and are processed by the autosplit program.

1;
__END__
# Below is the stub of documentation for your module. You better edit it!

=head1 NAME

HEAINIT - Perl extension for headas/heacore/heainit routines

=head1 SYNOPSIS

  use HEACORE::HEAINIT;

=head1 DESCRIPTION

This module provides a headas_main() routine which should be used in
constructing HEADAS Perl scripts.

=head1 AUTHOR

Ziqin Pan
M. Tripicco

=head1 SEE ALSO

perl(1).

=cut
