package HEACORE::HEAUTILS;

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
  HDpar_stamp
  HDpar_note
 _HDpar_note
  set_history
  set_toolname
  set_toolversion
  headas_clobberfile
  HDgtcalf
);
$VERSION = '0.01';

bootstrap HEACORE::HEAUTILS $VERSION;

# Preloaded methods go here.
sub HDpar_note {
     my ($par, $format, @rest) = @_;
     my $formatted = sprintf($format, @rest);
     _HDpar_note($par, $formatted);
}


# Autoload methods go after =cut, and are processed by the autosplit program.

1;
__END__
# Below is the stub of documentation for your module. You better edit it!

=head1 NAME

HEAUTILS - Perl extension for HEAdas utilites

=head1 SYNOPSIS

  use HEACORE::HEAUTILS;
  blah blah blah

=head1 DESCRIPTION

Stub documentation for HEAUTILS was created by h2xs. It looks like the
author of the extension was negligent enough to leave the stub
unedited.

Blah blah blah.

=head1 AUTHOR

Ziqin Pan, zpan@milkyway.gsfc.nasa.gov

=head1 SEE ALSO

perl(1).

=cut
